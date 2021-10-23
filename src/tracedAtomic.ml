open EffectHandlers
open EffectHandlers.Deep

type 'a t = 'a Atomic.t

type _ eff +=
  | Make : 'a -> 'a Atomic.t eff
  | Get : 'a Atomic.t -> 'a eff
  | Set : ('a Atomic.t * 'a) -> unit eff
  | Exchange : ('a Atomic.t * 'a) -> 'a eff
  | CompareAndSwap : ('a Atomic.t * 'a * 'a) -> bool eff
  | FetchAndAdd : (int Atomic.t * int) -> int eff

type atomic_op = Make | Get | Set | Exchange | CompareAndSwap | FetchAndAdd

type trace_cell = { op : atomic_op; process_id : int; repr : Obj.t }

type process_data = {
  id : int;
  mutable event_counter : int;
  mutable resume_func : unit -> unit;
  mutable finished : bool;
}

let processes = Hashtbl.create 10

let current_process = ref 0

let trace_list = ref []

let add_trace op repr =
  trace_list := { op; process_id = !current_process; repr } :: !trace_list

let update_process_data f =
  let process_rec = Hashtbl.find processes !current_process in
  process_rec.resume_func <- f;
  process_rec.event_counter <- process_rec.event_counter + 1

let finish_process () =
  let process_rec = Hashtbl.find processes !current_process in
  process_rec.finished <- true

let handler runner =
  {
    retc =
      (fun _ ->
        (
          Printf.printf "Finished process %d\n" !current_process;
        finish_process ();
        runner ()));
    exnc = (fun _ -> ());
    effc =
      (fun (type a) (e : a eff) ->
        match e with
        | Make v ->
            Some
              (fun (k : (a, _) continuation) ->
                Printf.printf "Process %d: Make\n" !current_process;
                let m = Atomic.make v in
                add_trace Make (Obj.repr m);
                update_process_data (fun () -> continue k m);
                current_process :=
                  (!current_process + 1) mod Hashtbl.length processes;
                runner ())
        | Get v ->
            Some
              (fun (k : (a, _) continuation) ->
                Printf.printf "Process %d: Get\n" !current_process;
                add_trace Get (Obj.repr v);
                update_process_data (fun () -> continue k (Atomic.get v));
                current_process :=
                  (!current_process + 1) mod Hashtbl.length processes;
                runner ())
        | Set (r, v) ->
            Some
              (fun (k : (a, _) continuation) ->
                Printf.printf "Process %d: Set\n" !current_process;
                add_trace Set (Obj.repr r);
                update_process_data (fun () -> continue k (Atomic.set r v));
                current_process :=
                  (!current_process + 1) mod Hashtbl.length processes;
                runner ())
        | Exchange (a, b) ->
            Some
              (fun (k : (a, _) continuation) ->
                Printf.printf "Process %d: Exchange\n" !current_process;
                add_trace Exchange (Obj.repr a);
                update_process_data (fun () -> continue k (Atomic.exchange a b));
                current_process :=
                  (!current_process + 1) mod Hashtbl.length processes;
                runner ())
        | CompareAndSwap (x, s, v) ->
            Some
              (fun (k : (a, _) continuation) ->
                Printf.printf "Process %d: CAS\n" !current_process;
                add_trace CompareAndSwap (Obj.repr x);
                update_process_data (fun () ->
                    continue k (Atomic.compare_and_set x s v));
                current_process :=
                  (!current_process + 1) mod Hashtbl.length processes;
                runner ())
        | FetchAndAdd (v, x) ->
            Some
              (fun (k : (a, _) continuation) ->
                Printf.printf "Process %d: FetchAndAdd\n" !current_process;
                add_trace FetchAndAdd (Obj.repr v);
                update_process_data (fun () ->
                    continue k (Atomic.fetch_and_add v x));
                current_process :=
                  (!current_process + 1) mod Hashtbl.length processes;
                runner ())
        | _ ->
            Printf.printf "Process %d finished\n" !current_process;
            None);
  }

let tracing = ref false

let trace () =
  tracing := true;
  let num_processes = Hashtbl.length processes in
  (* do our first trace in a haphazard fashion *)
  let rec round_robin_run finished_counter () =
    if finished_counter >= num_processes then (* We are done *)
      ()
    else
      let process_to_run = Hashtbl.find processes !current_process in
      if process_to_run.finished then (
        current_process := (!current_process + 1) mod num_processes;
        round_robin_run (finished_counter + 1) ())
      else
        (* process not finished, need to run it*)
        (Printf.printf "Switching to process %d\n" !current_process;
        match_with process_to_run.resume_func () (handler (round_robin_run 0)))
  in
  round_robin_run 0 ();
  tracing := false

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

let spawn _f =
  let new_id = Hashtbl.length processes in
  Hashtbl.add processes new_id
    { id = new_id; event_counter = 0; resume_func = _f; finished = false }

let clear () = ()
