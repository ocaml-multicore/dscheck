open EffectHandlers
open EffectHandlers.Shallow

type 'a t = 'a Atomic.t

type _ eff +=
  | Make : 'a -> 'a Atomic.t eff
  | Get : 'a Atomic.t -> 'a eff
  | Set : ('a Atomic.t * 'a) -> unit eff
  | Exchange : ('a Atomic.t * 'a) -> 'a eff
  | CompareAndSwap : ('a Atomic.t * 'a * 'a) -> bool eff
  | FetchAndAdd : (int Atomic.t * int) -> int eff

module IntSet = Set.Make(
  struct
    let compare = Stdlib.compare
    type t = int
  end )

let _set_to_string s =
  IntSet.fold (fun y x -> (string_of_int y) ^ "," ^ x) s ""

type atomic_op = Make | Get | Set | Exchange | CompareAndSwap | FetchAndAdd | Finish

type trace_cell = { op : atomic_op; process_id : int; repr : Obj.t; enabled : IntSet.t }

type state_cell = { mutable enabled : IntSet.t; mutable dones : IntSet.t }

let state_stack = CCVector.create ()
let current_trace = CCVector.create ()

type process_data = {
  id : int;
  mutable event_counter : int;
  mutable resume_func : (unit, unit) handler -> unit;
  initial_func : (unit -> unit);
  mutable finished : bool;
}

let tracing = ref false

let make v = if !tracing then perform (Make v) else Atomic.make v

let get r = if !tracing then perform (Get r) else Atomic.get r

let set r v = if !tracing then perform (Set (r, v)) else Atomic.set r v

let exchange r v =
  if !tracing then perform (Exchange (r, v)) else Atomic.exchange r v

let compare_and_set r seen v =
  if !tracing then perform (CompareAndSwap (r, seen, v))
  else Atomic.compare_and_set r seen v

let fetch_and_add r n =
  if !tracing then perform (FetchAndAdd (r, n)) else Atomic.fetch_and_add r n

let incr r = ignore (fetch_and_add r 1)

let decr r = ignore (fetch_and_add r (-1))

let processes = CCVector.create ()

let current_process = ref 0
let finished_processes = ref 0

let current_enabled = ref IntSet.empty

let add_trace op repr =
  CCVector.push current_trace { op; process_id = !current_process; repr; enabled = !current_enabled }

let update_process_data f =
  let process_rec = CCVector.get processes !current_process in
  process_rec.resume_func <- f;
  process_rec.event_counter <- process_rec.event_counter + 1

let finish_process () =
  let process_rec = CCVector.get processes !current_process in
  process_rec.finished <- true;
  finished_processes := !finished_processes + 1

let handler runner =
  {
    retc =
      (fun _ ->
         (
           add_trace Finish (Obj.repr None);
           finish_process ();
           runner ()));
    exnc = (fun s -> raise s);
    effc =
      (fun (type a) (e : a eff) ->
         match e with
         | Make v ->
           Some
             (fun (k : (a, _) continuation) ->
                let m = Atomic.make v in
                add_trace Make (Obj.repr m);
                update_process_data (fun h -> continue_with k m h);
                runner ())
         | Get v ->
           Some
             (fun (k : (a, _) continuation) ->
                add_trace Get (Obj.repr v);
                update_process_data (fun h -> continue_with k (Atomic.get v) h);
                runner ())
         | Set (r, v) ->
           Some
             (fun (k : (a, _) continuation) ->
                add_trace Set (Obj.repr r);
                update_process_data (fun h -> continue_with k (Atomic.set r v) h);
                runner ())
         | Exchange (a, b) ->
           Some
             (fun (k : (a, _) continuation) ->
                add_trace Exchange (Obj.repr a);
                update_process_data (fun h -> continue_with k (Atomic.exchange a b) h);
                runner ())
         | CompareAndSwap (x, s, v) ->
           Some
             (fun (k : (a, _) continuation) ->
                add_trace CompareAndSwap (Obj.repr x);
                update_process_data (fun h ->
                    continue_with k (Atomic.compare_and_set x s v) h);
                runner ())
         | FetchAndAdd (v, x) ->
           Some
             (fun (k : (a, _) continuation) ->
                add_trace FetchAndAdd (Obj.repr v);
                update_process_data (fun h ->
                    continue_with k (Atomic.fetch_and_add v x) h);
                runner ())
         | _ ->
           Printf.printf "Unknown on %d\n" !current_process;
           None);
  }

let spawn f =
  let new_id = CCVector.length processes in
  let fiber_f h =
    continue_with (fiber f) () h in
  CCVector.push processes
    { id = new_id; event_counter = 0; resume_func = fiber_f; initial_func = f; finished = false }

(*let reset_processes () =
  Hashtbl.iter (fun _ v ->
      let fiber_f h =
        continue_with (fiber v.initial_func) () h in
      v.resume_func <- fiber_f;
      v.finished <- false;
    ) processes*)

let trace_counter = ref 0

let reconcile_trace_state () =
  (* Here we have a trace in [current_trace] and a state stack in [state_stack].
     What we need to ensure is that if the trace is longer than the state stack
     we create new state stack entries for each of the new states and populate
     their [enabled] and [dones]. Then we need to make sure that the last
     element in the state stack's [dones] contains the last process we ran. If
     [dones] is equal to [enabled] then we remove the last element from the state
     stack, snip the last element from the trace and repeat the whole process. *)
  let trace_len = CCVector.length current_trace in
  while trace_len > CCVector.length state_stack do
    let nth_trace = CCVector.get current_trace (CCVector.length state_stack) in
    let enabled = nth_trace.enabled in
    let dones = IntSet.empty in
    CCVector.push state_stack { enabled; dones };
  done;
  let rec update_state current_pos =
    let trace_ele = CCVector.get current_trace current_pos in
    let state_ele = CCVector.get state_stack current_pos in
    state_ele.dones <- IntSet.add (trace_ele.process_id) state_ele.dones;
    if IntSet.cardinal state_ele.enabled == IntSet.cardinal state_ele.dones then
      (* we've completed everything at this state. Now we need to nuke this node
         and update the next element in the state stack *)
      begin
        CCVector.remove_and_shift state_stack current_pos;
        if current_pos > 0 then
          update_state (current_pos-1)
        else
          begin
            (* We're done *)
            ()
          end
      end
    else
      ()
  in update_state ((CCVector.length current_trace)-1)

let trace_start_func = ref (fun () -> ())
let last_time = ref (Sys.time ())

let print_progress () =
  CCVector.to_seq state_stack |> OSeq.zip_index |> OSeq.iter (fun (x,y) ->
    Printf.printf "%d %d %d\n" x (IntSet.cardinal y.enabled) (IntSet.cardinal y.dones)
  )

let trace f =
  trace_start_func := f;
  f (); (* initial start *)
  tracing := true;
  (* cache the number of processes in case it's expensive*)
  let num_processes = CCVector.length processes in
  (* current number of ops we are through the current run *)
  let rec run_trace () =
    if !finished_processes == num_processes then
      (* we now have no more processes we can run, so exit *)
      ()
    else
      (* is the current trace length longer than our state stack? That is are
         we now at a depth in the trace that is longer than anything we've
         explored before?*)
      begin
        current_enabled := CCVector.to_seq processes
                           |> OSeq.zip_index
                           |> Seq.filter (fun (_,proc) -> not proc.finished)
                           |> Seq.map (fun (id,_) -> id)
                           |> IntSet.of_seq;
        let (process_id_to_run, process_to_run) = if CCVector.length current_trace >= CCVector.length state_stack then
            (* We need to essentially continue exploring the state so we can
               add to the trace and reconcile later on. *)
            let process_id_to_run = IntSet.min_elt !current_enabled in
            let process_to_run = CCVector.get processes process_id_to_run in
            (process_id_to_run, process_to_run)
          else
            (* We've already been in a state this deep (or just created one). There
               should always be a state remaining in the [enabled] - [dones] set.
               Pick the smallest state in that set to run. *)
            begin
              let last_state = CCVector.get state_stack (CCVector.length current_trace) in
              let process_id_to_run = IntSet.diff last_state.enabled last_state.dones
                                      |> IntSet.min_elt in
              let process_to_run = CCVector.get processes process_id_to_run in
              (process_id_to_run, process_to_run)
            end
        in
        current_process := process_id_to_run;
        process_to_run.resume_func (handler run_trace)
      end
  in
  let progress_iterations = 1000000 in
  let am_done = ref false in
  while not !am_done do
    tracing := true;
    run_trace ();
    reconcile_trace_state ();
    CCVector.clear current_trace;
    CCVector.clear processes;
    finished_processes := 0;
    tracing := false;
    !trace_start_func ();
    trace_counter := !trace_counter + 1;
    if !trace_counter mod progress_iterations == 0 then begin
      let new_time = Sys.time () in
      let duration = new_time -. !last_time in
      let top_state = CCVector.get state_stack 0 in
      let percent_done = (100 * (IntSet.cardinal top_state.dones)) / (IntSet.cardinal top_state.enabled) in
      Printf.printf "Ran %d traces (%f traces/second) (~%d%%) (%d %d)\n%!" !trace_counter ((float_of_int progress_iterations) /. duration) percent_done (IntSet.cardinal top_state.dones) (IntSet.cardinal top_state.enabled);
      print_progress ();
      last_time := new_time
    end;
    (* last, check for completion *)
    if CCVector.length state_stack == 0 then
      am_done := true
  done;
  tracing := false

let clear () = ()
