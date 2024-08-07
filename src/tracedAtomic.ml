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
let tracing = ref false
let finished_processes = ref 0

type process_data = {
  mutable next_op : Atomic_op.t;
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

let make_contended = make

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
      exnc = (function Terminated_early -> () | _e -> ());
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
                let rec status =
                  ref
                    (`Unknown
                      (fun () ->
                        let result = Atomic.get x == s in
                        status := if result then `Success else `Fail;
                        if result then `Success else `Fail))
                in
                update_process_data current_process_id
                  (fun h ->
                    continue_with k
                      ((match !status with
                       | `Success | `Fail ->
                           failwith "this result has been predicted"
                       | `Unknown _ -> ());
                       let result = Atomic.compare_and_set x s v in
                       status := if result then `Success else `Fail;
                       result)
                      h)
                  (Atomic_op.CompareAndSwap status) (Some i) k;
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

type proc_rec = { proc_id : int; op : Atomic_op.t; obj_ptr : int option }

type state_cell = {
  procs : proc_rec list;
  run_proc : int;
  run_op : Atomic_op.t;
  run_ptr : int option;
  enabled : IntSet.t;
  mutable backtrack : IntSet.t;
}

let sleep_set_blocked = ref 0
let num_states = ref 0
let num_interleavings = ref 0

(* we stash the current state in case a check fails and we need to log it *)
let schedule_for_checks = ref []

let var_name i =
  match i with
  | None -> ""
  | Some i ->
      let c = Char.chr (i + 96) in
      Printf.sprintf "%c" c

let print_execution_sequence chan =
  let highest_proc =
    List.fold_left
      (fun highest (curr_proc, _, _) ->
        if curr_proc > highest then curr_proc else highest)
      (-1) !schedule_for_checks
  in

  let bar =
    List.init ((highest_proc * 20) + 20) (fun _ -> "-") |> String.concat ""
  in
  Printf.fprintf chan "\nsequence %d\n" !num_interleavings;
  Printf.fprintf chan "%s\n" bar;
  List.init (highest_proc + 1) (fun proc ->
      Printf.fprintf chan "P%d\t\t\t" proc)
  |> ignore;
  Printf.fprintf chan "\n%s\n" bar;

  List.iter
    (fun s ->
      match s with
      | last_run_proc, last_run_op, last_run_ptr ->
          let last_run_ptr = var_name last_run_ptr in
          let tabs =
            List.init last_run_proc (fun _ -> "\t\t\t") |> String.concat ""
          in
          Printf.fprintf chan "%s%s %s\n" tabs
            (Atomic_op.to_str last_run_op)
            last_run_ptr)
    !schedule_for_checks;
  Printf.fprintf chan "%s\n%!" bar

let interleavings_chan = (ref None : out_channel option ref)
let record_traces_flag = ref false

let do_run init_func init_schedule =
  init_func ();
  (*set up run *)
  tracing := true;
  schedule_for_checks := init_schedule;
  (* cache the number of processes in case it's expensive*)
  let num_processes = CCVector.length processes in
  (* current number of ops we are through the current run *)
  finished_processes := 0;
  let rec run_trace s true_schedule_rev () =
    tracing := false;
    !every_func ();
    tracing := true;
    match s with
    | [] ->
        if !finished_processes == num_processes then (
          tracing := false;

          num_interleavings := !num_interleavings + 1;

          if !record_traces_flag then
            Trace_tracker.add_trace (List.rev true_schedule_rev);

          (match !interleavings_chan with
          | None -> ()
          | Some chan -> print_execution_sequence chan);

          !final_func ();
          tracing := true)
    | (process_id_to_run, next_op, next_ptr) :: schedule -> (
        if !finished_processes == num_processes then
          (* this should never happen *)
          failwith "no enabled processes"
        else
          let process_to_run = CCVector.get processes process_id_to_run in
          let at = process_to_run.next_op in
          assert (Atomic_op.weak_cmp process_to_run.next_op next_op);
          assert (process_to_run.next_repr = next_ptr);

          let true_schedule_rev =
            (process_id_to_run, process_to_run.next_op, process_to_run.next_repr)
            :: true_schedule_rev
          in

          process_to_run.resume_func
            (handler process_id_to_run (run_trace schedule true_schedule_rev));
          match at with
          | CompareAndSwap cas -> (
              match !cas with `Unknown _ -> assert false | _ -> ())
          | _ -> ())
  in
  tracing := true;
  run_trace init_schedule [] ();

  finished_processes := 0;
  tracing := false;
  num_states := !num_states + 1;
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

let rec explore_random func state =
  let s = last_element state in
  let enabled = IntSet.to_seq s.enabled |> List.of_seq in
  let len = List.length enabled in
  if len == 0 then ()
  else
    let random_index = Random.int len in
    let j = List.nth enabled random_index in
    let j_proc = List.nth s.procs j in
    let schedule =
      List.map (fun s -> (s.run_proc, s.run_op, s.run_ptr)) state
      @ [ (j, j_proc.op, j_proc.obj_ptr) ]
    in
    let statedash = state @ [ do_run func schedule ] in
    explore_random func statedash

let same_proc state_cell1 state_cell2 =
  state_cell1.run_proc = state_cell2.run_proc

module Causality = struct
  let hb ((proc1 : int), (ptr1 : int option), op1)
      ((proc2 : int), (ptr2 : int option), op2) =
    (* assumes the two ops are adjacent *)
    let same_proc = proc1 = proc2 in
    let same_var =
      match (ptr1, ptr2) with
      | Some ptr1, Some ptr2 -> ptr1 = ptr2
      | Some _, None | None, Some _ | None, None -> false
    in
    let conflicting =
      let is_write = Atomic_op.is_write ~allow_unknown:true in
      Lazy.from_val (is_write op1 || is_write op2)
    in
    same_proc || (same_var && Lazy.force conflicting)

  let happens_before = function
    | `State (state_cell1, state_cell2) ->
        hb
          (state_cell1.run_proc, state_cell1.run_ptr, state_cell1.run_op)
          (state_cell2.run_proc, state_cell2.run_ptr, state_cell2.run_op)
    | `Proc (proc1, proc2) ->
        hb
          (proc1.proc_id, proc1.obj_ptr, proc1.op)
          (proc2.proc_id, proc2.obj_ptr, proc2.op)

  let mark_happen_before operation (sequence : state_cell list) =
    let sequence = List.map (fun v -> (v, ref false)) sequence in
    let sequence = (operation, ref true) :: sequence in
    let mark_intransitive hd_sequence tl_sequence =
      List.iter
        (fun (state_cell, hb) ->
          hb := !hb || happens_before (`State (hd_sequence, state_cell)))
        tl_sequence
    in
    let rec mark_all = function
      | [] | _ :: [] -> ()
      | (op, hb) :: tl ->
          if !hb then mark_intransitive op tl;
          mark_all tl
    in
    mark_all sequence;
    match sequence with
    | [] -> assert false
    | (operation', _) :: sequence ->
        assert (operation == operation');
        sequence
end

let is_reversible_race (op1 : state_cell) (between : state_cell list)
    (op2 : state_cell) =
  let hb_intransitively =
    (* Two ops have to be causally related for us to want to reverse them. *)
    Causality.happens_before (`State (op1, op2))
  in
  let diff_proc =
    (* If two ops belong two the same proc, they cannot be reversed. *)
    not (same_proc op1 op2)
  in
  if hb_intransitively && diff_proc then
    let not_transitively_related =
      (* If two ops are related transitively, technically not a race (see paper). *)
      let between = Causality.mark_happen_before op1 between in
      let between_hb =
        List.filter_map (fun (op, hb) -> if !hb then Some op else None) between
      in
      let op2_not_transitively_related =
        List.for_all
          (fun op -> not (Causality.happens_before (`State (op2, op))))
          between_hb
      in
      op2_not_transitively_related
    in
    not_transitively_related
  else false

let filter_out_happen_after operation sequence =
  Causality.mark_happen_before operation sequence
  |> List.filter_map (fun (op, hb) -> if !hb then None else Some op)

let rec explore_source func state sleep_sets =
  (* The code here closely follows the Algorithm 1 outlined in [Source Sets:
      A Foundation for Optimal Dynamic Partial Order Reduction]. Likewise
      variable names (e.g. reversible race, indep_and_p, initials) etc.
      reference constructs introduced in the paper.
  *)
  let sleep = ref (last_element sleep_sets) in
  let s = last_element state in
  let p_maybe = IntSet.min_elt_opt (IntSet.diff s.enabled !sleep) in
  match p_maybe with
  | None ->
      if not (IntSet.is_empty s.enabled) then
        sleep_set_blocked := !sleep_set_blocked + 1
  | Some p ->
      s.backtrack <- IntSet.singleton p;

      while IntSet.(cardinal (diff s.backtrack !sleep)) > 0 do
        let p = IntSet.min_elt (IntSet.diff s.backtrack !sleep) in
        let proc = List.nth s.procs p in

        let state_top =
          let schedule =
            List.map (fun s -> (s.run_proc, s.run_op, s.run_ptr)) state
            @ [ (p, proc.op, proc.obj_ptr) ]
          in
          do_run func schedule
        in
        assert (state_top.run_proc = p);
        let new_state = state @ [ state_top ] in

        (* Find the races (transitions dependent directly, without a transitive dependency).
        *)
        let reversible_races =
          List.fold_right
            (fun op1 ((between, reversible_races, skip_rest) as acc) ->
              if skip_rest then acc
              else if is_reversible_race op1 between state_top then
                ( op1 :: between,
                  op1 :: reversible_races,
                  Atomic_op.is_write op1.run_op )
              else (op1 :: between, reversible_races, false))
            state ([], [], false)
          |> function
          | _, l, _ -> l
        in
        List.iter
          (fun e ->
            let prefix, suffix =
              (* We need the last operation before the first operation of the race
                  occured because that's where alternative (reversal) is scheduled.
                  We need the suffix to compute how to schedule the reversal. *)
              let found_e, prefix_rev, suffix_rev =
                List.fold_left
                  (fun (seen_e, prefix, suffix) proc' ->
                    if seen_e then (seen_e, prefix, proc' :: suffix)
                    else if proc' == e then (true, prefix, suffix)
                    else (false, proc' :: prefix, suffix))
                  (false, [], []) state
              in

              assert found_e;
              (* Out first operation is always a spawn, which cannot
                 race with anything. Thus, any race has a prefix.
              *)
              assert (List.length prefix_rev > 0);
              assert (
                List.length suffix_rev
                = List.length state - List.length prefix_rev - 1);
              (List.rev prefix_rev, List.rev suffix_rev)
            in

            (* Filter out operations that are dependent on the first operation
               of the race (e.g. successive operations of e.run_proc). We definitely
               don't want to schedule them.
            *)
            let indep_and_p =
              let indep = filter_out_happen_after e suffix in
              indep @ [ state_top ]
            in
            (* Compute the set of operations, that lead to reversal of the race.
               The main premise here is that there may be a number of independent
               sequences that lead to reversal.

               For example, suppose two racing operations t, t' and some sequences
               E, w, u. Suppose the current sequence is E.t.w.u.t', t' happens
               after u and w is independent of everything.

               There's at least two ways to reverse t and t':
               - E.u.t'.(t,w in any order)
               - E.w.u.t'.t

               Thus, initials consist of the first operations of u and w, since
               these are the operations that lead to exploration of the above
               sequences from E.
            *)
            let initials =
              let rec loop = function
                | [] -> []
                | initial :: sequence ->
                    initial.run_proc
                    :: loop (filter_out_happen_after initial sequence)
              in
              loop indep_and_p
            in

            (* Exploring one of the initials guarantees that reversal has been
               visited. Thus, schedule one of the initials only if none of them
                is in backtrack. *)
            let prefix_top = last_element prefix in
            if
              IntSet.(cardinal (inter prefix_top.backtrack (of_list initials)))
              = 0
              &&
              let slp = List.nth sleep_sets (List.length prefix - 1) in
              IntSet.(cardinal (inter slp (of_list initials))) = 0
            then
              (* We can add any initial *)
              let initial = last_element initials in
              prefix_top.backtrack <- IntSet.add initial prefix_top.backtrack)
          reversible_races;

        let sleep' =
          (* Keep q that are independent with p only. Must be other thread of execution and act on a different object (or none).

             The key idea here is as follows. Suppose we have processed some execution sequence E and there are
             just two enabled transitions left. Namely, t=(read a), t'=(read b). Crucially, they are independent.
             We begin the exploration from E with E.t and descend into E.t.t' afterwards. Since no more transitions
             are enabled, we return back to E and execute E.t'. But there's no point in executing E.t'.t. Since t and
             t' are independent, there's a guarantee that E.t.t' and E.t'.t belong to the same trace.

             Therefore, at E, t is put into sleep set, and we explore with E.t' with sleep=[t]. Then E.t'.t gets
             sleep-set blocked and we save an execution sequence. Naturally, if there's some w such that it's dependent
             with t, then before we explore E.t'.w, we have to "wake" t up.
          *)
          IntSet.filter
            (fun q ->
              let proc' = List.nth s.procs q in
              not (Causality.happens_before (`Proc (proc, proc'))))
            !sleep
        in
        explore_source func new_state (sleep_sets @ [ sleep' ]);
        sleep := IntSet.add p !sleep
      done

let rec explore func state clock last_access =
  let s = last_element state in
  List.iter
    (fun proc ->
      let j = proc.proc_id in
      let i =
        Option.bind proc.obj_ptr (fun ptr -> IntMap.find_opt ptr last_access)
        |> Option.value ~default:0
      in
      if i != 0 then
        let pre_s = List.nth state (i - 1) in
        if IntSet.mem j pre_s.enabled then
          pre_s.backtrack <- IntSet.add j pre_s.backtrack
        else pre_s.backtrack <- IntSet.union pre_s.backtrack pre_s.enabled)
    s.procs;
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
      let statedash = state @ [ do_run func schedule ] in
      let state_time = List.length statedash - 1 in
      let new_last_access =
        match j_proc.obj_ptr with
        | Some ptr -> IntMap.add ptr state_time last_access
        | None -> last_access
      in
      let new_clock = IntMap.add j state_time clock in
      explore func statedash new_clock new_last_access
    done)

let every f = every_func := f
let final f = final_func := f

let check f =
  let tracing_at_start = !tracing in
  tracing := false;
  if not (f ()) then (
    Printf.printf "Found assertion violation at run %d:\n" !num_interleavings;
    print_execution_sequence stdout;
    assert false);
  tracing := tracing_at_start

let reset_state () =
  finished_processes := 0;
  atomics_counter := 1;
  num_states := 0;
  sleep_set_blocked := 0;
  num_interleavings := 0;
  schedule_for_checks := [];
  Trace_tracker.clear_traces ();
  CCVector.clear processes

let dscheck_trace_file_env = Sys.getenv_opt "dscheck_trace_file"

let random func iters =
  reset_state ();
  let empty_state = do_run func [ (0, Start, None) ] :: [] in
  for _ = 1 to iters do
    explore_random func empty_state
  done

let dpor func =
  reset_state ();
  let empty_state = do_run func [ (0, Start, None) ] :: [] in
  let empty_clock = IntMap.empty in
  let empty_last_access = IntMap.empty in
  explore func empty_state empty_clock empty_last_access

let dpor_source func =
  reset_state ();
  let empty_state = do_run func [ (0, Start, None) ] in
  explore_source func [ empty_state ] [ IntSet.empty ]

let trace ?(impl = `Dpor_source) ?interleavings ?(record_traces = false) func =
  record_traces_flag := record_traces || Option.is_some dscheck_trace_file_env;
  interleavings_chan := interleavings;

  (match impl with
  | `Dpor -> dpor func
  | `Random iters -> random func iters
  | `Dpor_source -> dpor_source func);

  (* print reports *)
  if record_traces && Option.is_none !interleavings_chan then
    interleavings_chan := Some stdout;

  (match !interleavings_chan with
  | None -> ()
  | Some chan ->
      Printf.fprintf chan "\nexplored %d interleavings and %d states\n"
        !num_interleavings !num_states);

  match dscheck_trace_file_env with
  | None -> ()
  | Some path ->
      let chan = open_out path in
      Trace_tracker.print_traces chan;
      close_out chan
