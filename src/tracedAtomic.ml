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

let print_set t =
  IntSet.iter (fun x -> Printf.printf "%d," x) t

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

let processes = Hashtbl.create 10

let current_process = ref 0
let finished_processes = ref 0

let add_trace op repr =
  let enabled = Hashtbl.to_seq processes
                |> Seq.filter (fun (_,proc) -> not proc.finished)
                |> Seq.map (fun (id,_) -> id)
                |> IntSet.of_seq in
  CCVector.push current_trace { op; process_id = !current_process; repr; enabled }

let update_process_data f =
  let process_rec = Hashtbl.find processes !current_process in
  process_rec.resume_func <- f;
  process_rec.event_counter <- process_rec.event_counter + 1

let finish_process () =
  let process_rec = Hashtbl.find processes !current_process in
  process_rec.finished <- true;
  finished_processes := !finished_processes + 1

let handler runner =
  {
    retc =
      (fun _ ->
         (
           Printf.printf "Finished process %d\n" !current_process;
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
                Printf.printf "Process %d: Make\n" !current_process;
                let m = Atomic.make v in
                add_trace Make (Obj.repr m);
                update_process_data (fun h -> continue_with k m h);
                current_process :=
                  (!current_process + 1) mod Hashtbl.length processes;
                runner ())
         | Get v ->
           Some
             (fun (k : (a, _) continuation) ->
                Printf.printf "Process %d: Get\n" !current_process;
                add_trace Get (Obj.repr v);
                update_process_data (fun h -> continue_with k (Atomic.get v) h);
                current_process :=
                  (!current_process + 1) mod Hashtbl.length processes;
                runner ())
         | Set (r, v) ->
           Some
             (fun (k : (a, _) continuation) ->
                Printf.printf "Process %d: Set\n" !current_process;
                add_trace Set (Obj.repr r);
                update_process_data (fun h -> continue_with k (Atomic.set r v) h);
                current_process :=
                  (!current_process + 1) mod Hashtbl.length processes;
                runner ())
         | Exchange (a, b) ->
           Some
             (fun (k : (a, _) continuation) ->
                Printf.printf "Process %d: Exchange\n" !current_process;
                add_trace Exchange (Obj.repr a);
                update_process_data (fun h -> continue_with k (Atomic.exchange a b) h);
                current_process :=
                  (!current_process + 1) mod Hashtbl.length processes;
                runner ())
         | CompareAndSwap (x, s, v) ->
           Some
             (fun (k : (a, _) continuation) ->
                Printf.printf "Process %d: CAS\n" !current_process;
                add_trace CompareAndSwap (Obj.repr x);
                update_process_data (fun h ->
                    continue_with k (Atomic.compare_and_set x s v) h);
                current_process :=
                  (!current_process + 1) mod Hashtbl.length processes;
                runner ())
         | FetchAndAdd (v, x) ->
           Some
             (fun (k : (a, _) continuation) ->
                Printf.printf "Process %d: FetchAndAdd\n" !current_process;
                add_trace FetchAndAdd (Obj.repr v);
                update_process_data (fun h ->
                    continue_with k (Atomic.fetch_and_add v x) h);
                current_process :=
                  (!current_process + 1) mod Hashtbl.length processes;
                runner ())
         | _ ->
           Printf.printf "Unknown on %d\n" !current_process;
           None);
  }

let spawn f =
  let new_id = Hashtbl.length processes in
  let fiber_f h =
    continue_with (fiber f) () h in
  Hashtbl.add processes new_id
    { id = new_id; event_counter = 0; resume_func = fiber_f; initial_func = f; finished = false }

(*let reset_processes () =
  Hashtbl.iter (fun _ v ->
      let fiber_f h =
        continue_with (fiber v.initial_func) () h in
      v.resume_func <- fiber_f;
      v.finished <- false;
    ) processes*)

let reconcile_trace_state () =
  Printf.printf "Reconciling trace with state\n";
  (* Here we have a trace in [current_trace] and a state stack in [state_stack].
     What we need to ensure is that if the trace is longer than the state stack
     we create new state stack entries for each of the new states and populate
     their [enabled] and [dones]. Then we need to make sure that the last
     element in the state stack's [dones] contains the last process we ran. If
     [dones] is equal to [enabled] then we remove the last element from the state
     stack, snip the last element from the trace and repeat the whole process. *)
  let trace_len = CCVector.length current_trace in
  Printf.printf "Current trace len: %d\n" trace_len;
  Printf.printf "Current state length: %d\n" (CCVector.length state_stack);
  while trace_len > CCVector.length state_stack do
    let nth_trace = CCVector.get current_trace (CCVector.length state_stack) in
    let enabled = nth_trace.enabled in
    let dones = IntSet.empty in
    Printf.printf "Adding dones: "; print_set dones ; Printf.printf " enabled: "; print_set enabled;
    Printf.printf " at state_stack: %d\n" (CCVector.length state_stack);
    CCVector.push state_stack { enabled; dones };
  done;
  Printf.printf "New state length: %d\n" (CCVector.length state_stack);
  let rec update_state current_pos =
    Printf.printf "Updating pos: %d\n" current_pos;
    let trace_ele = CCVector.get current_trace current_pos in
    let state_ele = CCVector.get state_stack current_pos in
    state_ele.dones <- IntSet.add (trace_ele.process_id) state_ele.dones;
    if IntSet.cardinal state_ele.enabled == IntSet.cardinal state_ele.dones then
      (* we've completed everything at this state. Now we need to nuke this node
         and update the next element in the state stack *)
      begin
        CCVector.remove_and_shift state_stack current_pos;
        update_state (current_pos-1)
      end
    else
      ()
  in update_state ((CCVector.length current_trace)-1)

let trace_start_func = ref (fun () -> ())

let trace f =
  trace_start_func := f;
  f (); (* initial start *)
  tracing := true;
  (* cache the number of processes in case it's expensive*)
  let num_processes = Hashtbl.length processes in
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
        let (process_id_to_run, process_to_run) = if CCVector.length current_trace >= CCVector.length state_stack then
            (* We need to essentially continue exploring the state so we can
               add to the trace and reconcile later on. *)
            let enabled = Hashtbl.to_seq processes
                          |> Seq.filter (fun (_,proc) -> not proc.finished)
                          |> Seq.map (fun (id,_) -> id)
                          |> IntSet.of_seq in
            let process_id_to_run = IntSet.min_elt enabled in
            let process_to_run = Hashtbl.find processes process_id_to_run in
            (process_id_to_run, process_to_run)
          else
            (* We've already been in a state this deep (or just created one). There
               should always be a state remaining in the [enabled] - [dones] set.
               Pick the smallest state in that set to run. *)
            begin
              Printf.printf "State stack len: %d, current trace len: %d\n" (CCVector.length state_stack) (CCVector.length current_trace);
              let last_state = CCVector.get state_stack (CCVector.length current_trace) in
              let process_id_to_run = IntSet.diff last_state.enabled last_state.dones
                                      |> IntSet.min_elt in
              let process_to_run = Hashtbl.find processes process_id_to_run in
              (process_id_to_run, process_to_run)
            end
        in
        current_process := process_id_to_run;
        (Printf.printf "Switching to process %d\n" !current_process;
         process_to_run.resume_func (handler run_trace))
      end
  in
  while true do
    Printf.printf "\n";
    run_trace ();
    reconcile_trace_state ();
    CCVector.clear current_trace;
    Hashtbl.clear processes;
    finished_processes := 0;
    tracing := false;
    !trace_start_func ();
    tracing := true
  done;
  tracing := false

let clear () = ()
