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

let string_of_set s =
  IntSet.fold (fun y x -> (string_of_int y) ^ "," ^ x) s ""

type atomic_op = Make | Get | Set | Exchange | CompareAndSwap | FetchAndAdd | Finish

type trace_cell = { op : atomic_op; process_id : int; repr : Obj.t option ; enabled : IntSet.t }

type state_cell = { enabled : IntSet.t; mutable backtrack : IntSet.t; mutable dones : IntSet.t }

let state_stack = CCVector.create ()

let tracing = ref false
let current_trace = CCVector.create ()
let current_process = ref 0
let finished_processes = ref 0
let current_enabled = ref IntSet.empty

type process_data = {
  id : int;
  mutable next_op: atomic_op option;
  mutable next_repr: Obj.t option;
  mutable resume_func : (unit, unit) handler -> unit;
  initial_func : (unit -> unit);
  mutable finished : bool;
}

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

let add_trace op repr_opt =
  CCVector.push current_trace { op; process_id = !current_process; repr = repr_opt; enabled = !current_enabled }

let update_process_data f =
  let process_rec = CCVector.get processes !current_process in
  let last_trace = CCVector.get current_trace ((CCVector.length current_trace)-1) in
  process_rec.resume_func <- f;
  process_rec.next_repr <- last_trace.repr;
  process_rec.next_op <- Some(last_trace.op)

let finish_process () =
  let process_rec = CCVector.get processes !current_process in
  process_rec.finished <- true;
  finished_processes := !finished_processes + 1

let handler runner =
  {
    retc =
      (fun _ ->
         (
           add_trace Finish None;
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
                add_trace Make (Some (Obj.repr m));
                update_process_data (fun h -> continue_with k m h);
                runner ())
         | Get v ->
           Some
             (fun (k : (a, _) continuation) ->
                add_trace Get (Some (Obj.repr v));
                update_process_data (fun h -> continue_with k (Atomic.get v) h);
                runner ())
         | Set (r, v) ->
           Some
             (fun (k : (a, _) continuation) ->
                add_trace Set (Some (Obj.repr r));
                update_process_data (fun h -> continue_with k (Atomic.set r v) h);
                runner ())
         | Exchange (a, b) ->
           Some
             (fun (k : (a, _) continuation) ->
                add_trace Exchange (Some (Obj.repr a));
                update_process_data (fun h -> continue_with k (Atomic.exchange a b) h);
                runner ())
         | CompareAndSwap (x, s, v) ->
           Some
             (fun (k : (a, _) continuation) ->
                add_trace CompareAndSwap (Some (Obj.repr x));
                update_process_data (fun h ->
                    continue_with k (Atomic.compare_and_set x s v) h);
                runner ())
         | FetchAndAdd (v, x) ->
           Some
             (fun (k : (a, _) continuation) ->
                add_trace FetchAndAdd (Some (Obj.repr v));
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
    { id = new_id; next_op = None; next_repr = None; resume_func = fiber_f; initial_func = f; finished = false }

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
    let backtrack = IntSet.empty in
    let dones = IntSet.empty in
    CCVector.push state_stack { enabled; backtrack; dones };
  done;
  (* move forwards down the trace and calculate the last access times,
     populating the backtrack set as we go *)
  let last_obj_access = Hashtbl.create 3 in
  let last_proc_hb = CCVector.make (CCVector.length processes) 0 in
  let rec backtrack pos =
    if pos == trace_len then
      ()
    else
      let trace_ele = CCVector.get current_trace pos in
      begin match trace_ele.repr with
        | None -> Printf.printf "No last_access set\n"
        | Some(x) -> begin
            Printf.printf "Setting last_access[%d] to %d\n" (Obj.magic x) pos;
            Hashtbl.replace last_obj_access x pos;
          end;
      end;
      CCVector.set last_proc_hb trace_ele.process_id pos;
      IntSet.iter (fun p ->
          let proc = CCVector.get processes p in
          let ptr = proc.next_repr in
          let last_access = match ptr with
            | None -> 0
            | Some(v) -> begin match Hashtbl.find_opt last_obj_access v with
                | None -> 0
                | Some(x) -> x end in
          Printf.printf "Last access for proc: %d ptr: %d was: %d\n" p (Option.value ~default:0 (Obj.magic ptr)) last_access;
          if last_access > 0 then
            (* object was accessed earlier, need to work out if that state
               happens-before for this process. That is, whether this access
               could be placed before that state or not. *)
            let last_state_proc = CCVector.get last_proc_hb p in
            Printf.printf "Last state proc[%d] was %d\n" p last_state_proc;
            if last_state_proc < last_access then begin
              (* we can safely add this to the backtrack state at (last_access-1) *)
              let state = CCVector.get state_stack (last_access-1) in
              Printf.printf "Adding %d to backtrack at %d\n" p (last_access-1);
              state.backtrack <- IntSet.add p state.backtrack
            end
            else
              (* first access of object *)
              ()
        ) trace_ele.enabled;
      backtrack (pos+1)
  in backtrack 0;
  let rec update_state current_pos =
    let trace_ele = CCVector.get current_trace current_pos in
    let state_ele = CCVector.get state_stack current_pos in
    state_ele.dones <- IntSet.add (trace_ele.process_id) state_ele.dones;
    state_ele.backtrack <- IntSet.add (trace_ele.process_id) state_ele.backtrack;
    if current_pos > 0 then
      update_state (current_pos-1)
    else
      ()
  in update_state (trace_len-1)

let trace_start_func = ref (fun () -> ())
let last_time = ref (Sys.time ())

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
               should always be a state remaining in the [backtrack] - [dones] set.
               Pick the smallest state in that set to run. *)
            begin
              let last_state = CCVector.get state_stack (CCVector.length current_trace) in
              let backtrack_or_enabled = if (IntSet.cardinal last_state.backtrack) == 0 then last_state.enabled else last_state.backtrack in
              Printf.printf "state: %d, enabled: %s, backtrack: %s, dones: %s\n" (CCVector.length current_trace) (string_of_set last_state.enabled) (string_of_set last_state.backtrack) (string_of_set last_state.dones);
              let process_id_to_run = IntSet.diff backtrack_or_enabled last_state.dones
                                      |> IntSet.min_elt in
              Printf.printf "picked %d\n" process_id_to_run;
              let process_to_run = CCVector.get processes process_id_to_run in
              (process_id_to_run, process_to_run)
            end
        in
        current_process := process_id_to_run;
        process_to_run.resume_func (handler run_trace)
      end
  in
  let progress_iterations = 1 in
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
      last_time := new_time
    end;
    (* last, check for completion *)
    if CCVector.length state_stack == 0 then
      am_done := true
  done;
  tracing := false

let clear () = ()
