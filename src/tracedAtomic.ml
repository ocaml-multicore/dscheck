open Effect
open Effect.Shallow

type 'a t = 'a Atomic.t * int

type _ Effect.t +=
  | Make : 'a -> 'a t Effect.t
  | Get : 'a t -> 'a Effect.t
  | Set : ('a t * 'a) -> unit Effect.t
  | Exchange : ('a t * 'a) -> 'a Effect.t
  | CompareAndSwap : ('a t * 'a * 'a) -> bool Effect.t
  | FetchAndAdd : (int t * int) -> int Effect.t

module IntSet = Set.Make (Int)
module IntMap = Map.Make (Int)

let _string_of_set s = IntSet.fold (fun y x -> string_of_int y ^ "," ^ x) s ""

type atomic_op =
  | Start
  | Make
  | Get
  | Set
  | Exchange
  | CompareAndSwap
  | FetchAndAdd

let atomic_op_str x =
  match x with
  | Start -> "start"
  | Make -> "make"
  | Get -> "get"
  | Set -> "set"
  | Exchange -> "exchange"
  | CompareAndSwap -> "compare_and_swap"
  | FetchAndAdd -> "fetch_and_add"

let tracing = ref false
let finished_processes = ref 0

type process_data = {
  mutable next_op : atomic_op;
  mutable next_repr : int option;
  mutable resume_func : (unit, unit) handler -> unit;
  mutable finished : bool;
  mutable discontinue_f : unit -> unit;
}

let every_func = ref (fun () -> ())
let final_func = ref (fun () -> ())

(* Atomics implementation *)
let atomics_counter = ref 1

let make v =
  if !tracing then perform (Make v)
  else
    let i = !atomics_counter in
    atomics_counter := !atomics_counter + 1;
    (Atomic.make v, i)

let get r =
  if !tracing then perform (Get r) else match r with v, _ -> Atomic.get v

let set r v =
  if !tracing then perform (Set (r, v))
  else match r with x, _ -> Atomic.set x v

let exchange r v =
  if !tracing then perform (Exchange (r, v))
  else match r with x, _ -> Atomic.exchange x v

let compare_and_set r seen v =
  if !tracing then perform (CompareAndSwap (r, seen, v))
  else match r with x, _ -> Atomic.compare_and_set x seen v

let fetch_and_add r n =
  if !tracing then perform (FetchAndAdd (r, n))
  else match r with x, _ -> Atomic.fetch_and_add x n

let incr r = ignore (fetch_and_add r 1)
let decr r = ignore (fetch_and_add r (-1))

(* Tracing infrastructure *)

exception Terminated_early

let discontinue k () =
  discontinue_with k Terminated_early
    {
      retc = (fun _ -> ());
      exnc = (function Terminated_early -> () | e -> raise e);
      effc = (fun (type a) (_ : a Effect.t) -> None);
    }

let processes = CCVector.create ()

let update_process_data process_id f op repr k =
  let process_rec = CCVector.get processes process_id in
  process_rec.resume_func <- f;
  process_rec.next_repr <- repr;
  process_rec.next_op <- op;
  process_rec.discontinue_f <- discontinue k

let finish_process process_id =
  let process_rec = CCVector.get processes process_id in
  process_rec.finished <- true;
  process_rec.discontinue_f <- (fun () -> ());
  finished_processes := !finished_processes + 1

let handler current_process_id runner =
  {
    retc =
      (fun _ ->
        finish_process current_process_id;
        runner ());
    exnc = (fun s -> raise s);
    effc =
      (fun (type a) (e : a Effect.t) ->
        match e with
        | Make v ->
            Some
              (fun (k : (a, _) continuation) ->
                let i = !atomics_counter in
                let m = (Atomic.make v, i) in
                atomics_counter := !atomics_counter + 1;
                update_process_data current_process_id
                  (fun h -> continue_with k m h)
                  Make (Some i) k;
                runner ())
        | Get (v, i) ->
            Some
              (fun (k : (a, _) continuation) ->
                update_process_data current_process_id
                  (fun h -> continue_with k (Atomic.get v) h)
                  Get (Some i) k;
                runner ())
        | Set ((r, i), v) ->
            Some
              (fun (k : (a, _) continuation) ->
                update_process_data current_process_id
                  (fun h -> continue_with k (Atomic.set r v) h)
                  Set (Some i) k;
                runner ())
        | Exchange ((a, i), b) ->
            Some
              (fun (k : (a, _) continuation) ->
                update_process_data current_process_id
                  (fun h -> continue_with k (Atomic.exchange a b) h)
                  Exchange (Some i) k;
                runner ())
        | CompareAndSwap ((x, i), s, v) ->
            Some
              (fun (k : (a, _) continuation) ->
                update_process_data current_process_id
                  (fun h -> continue_with k (Atomic.compare_and_set x s v) h)
                  CompareAndSwap (Some i) k;
                runner ())
        | FetchAndAdd ((v, i), x) ->
            Some
              (fun (k : (a, _) continuation) ->
                update_process_data current_process_id
                  (fun h -> continue_with k (Atomic.fetch_and_add v x) h)
                  FetchAndAdd (Some i) k;
                runner ())
        | _ -> None);
  }

let spawn f =
  let fiber_f = fiber f in
  let resume_func = continue_with fiber_f () in
  CCVector.push processes
    {
      next_op = Start;
      next_repr = None;
      resume_func;
      finished = false;
      discontinue_f = discontinue fiber_f;
    }

let rec last_element l =
  match l with h :: [] -> h | [] -> assert false | _ :: tl -> last_element tl

type proc_rec = { proc_id : int; op : atomic_op; obj_ptr : int option }

type state_cell = {
  procs : proc_rec list;
  run_proc : int;
  run_op : atomic_op;
  run_ptr : int option;
  enabled : IntSet.t;
  mutable backtrack : IntSet.t;
}

let num_runs = ref 0
let num_traces = ref 0

(* we stash the current state in case a check fails and we need to log it *)
let schedule_for_checks = ref []

let do_run init_func init_schedule =
  init_func ();
  (*set up run *)
  tracing := true;
  schedule_for_checks := init_schedule;
  (* cache the number of processes in case it's expensive*)
  let num_processes = CCVector.length processes in
  (* current number of ops we are through the current run *)
  let rec run_trace s () =
    tracing := false;
    !every_func ();
    tracing := true;
    match s with
    | [] ->
        if !finished_processes == num_processes then (
          tracing := false;
          !final_func ();
          num_traces := !num_traces + 1;
          tracing := true)
    | (process_id_to_run, next_op, next_ptr) :: schedule ->
        if !finished_processes == num_processes then
          (* this should never happen *)
          failwith "no enabled processes"
        else
          let process_to_run = CCVector.get processes process_id_to_run in
          assert (process_to_run.next_op = next_op);
          assert (process_to_run.next_repr = next_ptr);
          process_to_run.resume_func
            (handler process_id_to_run (run_trace schedule))
  in
  tracing := true;
  run_trace init_schedule ();
  finished_processes := 0;
  tracing := false;
  num_runs := !num_runs + 1;
  if !num_runs mod 1000 == 0 then Printf.printf "run: %d\n%!" !num_runs;
  let procs =
    CCVector.mapi
      (fun i p -> { proc_id = i; op = p.next_op; obj_ptr = p.next_repr })
      processes
    |> CCVector.to_list
  in
  let current_enabled =
    CCVector.to_seq processes |> OSeq.zip_index
    |> Seq.filter (fun (_, proc) -> not proc.finished)
    |> Seq.map (fun (id, _) -> id)
    |> IntSet.of_seq
  in
  CCVector.iter (fun proc -> proc.discontinue_f ()) processes;
  CCVector.clear processes;
  atomics_counter := 1;
  match last_element init_schedule with
  | run_proc, run_op, run_ptr ->
      {
        procs;
        enabled = current_enabled;
        run_proc;
        run_op;
        run_ptr;
        backtrack = IntSet.empty;
      }

module Clock_vector = struct
  type t = (int, CCVector.ro) CCVector.t

  let make size : t = CCVector.init size (fun _ -> 0)

  let extend desired_size t : t =
    let diff = desired_size - CCVector.length t in
    if diff <= 0 then t
    else
      let mut = CCVector.copy t in
      for _ = 1 to diff do
        CCVector.push mut 0
      done;
      CCVector.freeze mut

  let update (t : t) ~proc_id ~state_time : t =
    let t = CCVector.copy t in
    CCVector.set t proc_id state_time;
    CCVector.freeze t

  let get t ~proc_id = CCVector.get t proc_id

  let max (t1 : t) (t2 : t) : t =
    let rec f = function
      | [], [] -> []
      | [], _ | _, [] ->
          failwith
            (Printf.sprintf "max: vector clocks have different lengths (%d, %d)"
               (CCVector.length t1) (CCVector.length t2))
      | item1 :: tail1, item2 :: tail2 -> max item1 item2 :: f (tail1, tail2)
    in
    CCVector.of_list (f (CCVector.to_list t1, CCVector.to_list t2))
    |> CCVector.freeze
end

type proc_obj_key = Processor of int | Object of int

module Proc_obj_map = Map.Make (struct
  type t = proc_obj_key

  let compare t1 t2 =
    match (t1, t2) with
    | Processor v1, Processor v2 | Object v1, Object v2 -> Int.compare v1 v2
    | Processor _, Object _ -> 1
    | Object _, Processor _ -> -1
end)

let extend_all desired_size =
  Proc_obj_map.map (Clock_vector.extend desired_size)

let rec explore func state clock_vectors last_access =
  let s = last_element state in
  List.iter
    (fun proc ->
      match Option.bind proc.obj_ptr (fun ptr -> IntMap.find_opt ptr last_access) with 
      | None -> () 
      | Some i ->
        assert (i > 0);
        let pre_s = List.nth state (i - 1) in
        let happens_before =
          let cv = Proc_obj_map.find (Processor proc.proc_id) clock_vectors in
          Clock_vector.get cv ~proc_id:pre_s.run_proc
        in
        if i > happens_before then
          let j = proc.proc_id in
          if IntSet.mem j pre_s.enabled then
            pre_s.backtrack <- IntSet.add j pre_s.backtrack
          else pre_s.backtrack <- IntSet.union pre_s.backtrack pre_s.enabled)
    s.procs;
  let state_time = List.length state in
  if IntSet.cardinal s.enabled > 0 then (
    let p = IntSet.min_elt s.enabled in
    let dones = ref IntSet.empty in
    s.backtrack <- IntSet.singleton p;
    while IntSet.(cardinal (diff s.backtrack !dones)) > 0 do
      let j = IntSet.min_elt (IntSet.diff s.backtrack !dones) in
      dones := IntSet.add j !dones;
      let j_proc = List.nth s.procs j in
      let schedule =
        List.map (fun s -> (s.run_proc, s.run_op, s.run_ptr)) state
        @ [ (j, j_proc.op, j_proc.obj_ptr) ]
      in
      let current_state = do_run func schedule in
      let statedash = state @ [ current_state ] in

      assert (Option.is_some j_proc.obj_ptr || j_proc.op == Start);

      (* Start is a no-op for practical reasons. *)
      let new_last_access =
        match j_proc.obj_ptr with
        | Some ptr -> IntMap.add ptr state_time last_access
        | None -> last_access
      in
      (* Update clock vectors *)
      let cv_length = List.length current_state.procs in
      let new_clock_vectors =
        let clock_vectors = extend_all cv_length clock_vectors in
        match j_proc.op with
        | Make -> clock_vectors
        | Start ->
            Proc_obj_map.add (Processor j)
              (Clock_vector.make cv_length)
              clock_vectors
        | Get | Set | Exchange | CompareAndSwap | FetchAndAdd ->
            let obj_key = Object (Option.get j_proc.obj_ptr) in
            let proc_key = Processor j in
            let cv =
              let proc_cv = Proc_obj_map.find proc_key clock_vectors in
              let cv =
                let obj_cv_opt = Proc_obj_map.find_opt obj_key clock_vectors in
                match obj_cv_opt with
                | Some obj_cv ->
                    assert (CCVector.length proc_cv = CCVector.length obj_cv);
                    Clock_vector.max obj_cv proc_cv
                | None -> proc_cv
              in
              Clock_vector.update cv ~proc_id:j ~state_time
            in
            Proc_obj_map.update obj_key (fun _ -> Some cv) clock_vectors
            |> Proc_obj_map.update proc_key (fun _ -> Some cv)
      in
      explore func statedash new_clock_vectors new_last_access
    done)

let every f = every_func := f
let final f = final_func := f

let check f =
  let tracing_at_start = !tracing in
  tracing := false;
  if not (f ()) then (
    Printf.printf "Found assertion violation at run %d:\n" !num_runs;
    List.iter
      (fun s ->
        match s with
        | last_run_proc, last_run_op, last_run_ptr ->
            let last_run_ptr =
              Option.map string_of_int last_run_ptr |> Option.value ~default:""
            in
            Printf.printf "Process %d: %s %s\n" last_run_proc
              (atomic_op_str last_run_op)
              last_run_ptr)
      !schedule_for_checks;
    assert false);
  tracing := tracing_at_start

let reset_state () =
  finished_processes := 0;
  atomics_counter := 1;
  num_runs := 0;
  num_traces := 0;
  schedule_for_checks := [];
  CCVector.clear processes

let trace func =
  reset_state ();
  let empty_state = do_run func [ (0, Start, None) ] :: [] in
  let clock_vectors =
    Proc_obj_map.add (Processor 0) (Clock_vector.make 1) Proc_obj_map.empty
  in
  let empty_last_access = IntMap.empty in
  explore func empty_state clock_vectors empty_last_access

let num_runs () = !num_runs
let num_traces () = !num_traces
