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
            (atomic_op_str last_run_op)
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
  let rec run_trace s () =
    tracing := false;
    !every_func ();
    tracing := true;
    match s with
    | [] ->
        if !finished_processes == num_processes then (
          tracing := false;

          num_interleavings := !num_interleavings + 1;
          if !record_traces_flag then
            Trace_tracker.add_trace !schedule_for_checks;

          (match !interleavings_chan with
          | None -> ()
          | Some chan -> print_execution_sequence chan);

          !final_func ();
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
  num_states := !num_states + 1;
  (* if !num_states mod 1000 == 0 then Printf.printf "run: %d\n%!" !num_states; *)
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

let filter_out_happen_after operation sequence =
  let dependent_proc = ref (IntSet.singleton operation.run_proc) in
  let dependent_vars =
    ref
      (Option.map IntSet.singleton operation.run_ptr
      |> Option.value ~default:IntSet.empty)
  in
  List.filter_map
    (fun (state_cell : state_cell) ->
      let happen_after =
        IntSet.mem state_cell.run_proc !dependent_proc
        ||
        match state_cell.run_ptr with
        | None -> false
        | Some run_ptr -> IntSet.mem run_ptr !dependent_vars
      in
      if happen_after then (
        dependent_proc := IntSet.add state_cell.run_proc !dependent_proc;
        match state_cell.run_ptr with
        | None -> ()
        | Some run_ptr -> dependent_vars := IntSet.add run_ptr !dependent_vars);

      if happen_after then None else Some state_cell)
    sequence

let rec explore_source func state sleep_sets =
  let sleep = ref (last_element sleep_sets) in
  let s = last_element state in
  let p_maybe = IntSet.min_elt_opt (IntSet.diff s.enabled !sleep) in
  match p_maybe with
  | None -> ()
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

        (* Find the most recent race. Technically, this is the only one
           that fullfils the definition of race as defined per source set
           paper (as long as our atomic operations access one variable at a time).
        *)
        let reversible_race =
          Option.bind proc.obj_ptr (fun obj_ptr ->
              let dependent_ops =
                List.filter
                  (fun proc' ->
                    match proc'.run_ptr with
                    | None -> false
                    | Some run_ptr -> obj_ptr = run_ptr && proc'.run_proc <> p)
                  new_state
              in
              match List.rev dependent_ops with [] -> None | v :: _ -> Some v)
        in

        (match reversible_race with
        | None -> ()
        | Some e ->
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
              let rec f = function
                | [] -> []
                | initial :: sequence ->
                    initial.run_proc
                    :: f (filter_out_happen_after initial sequence)
              in
              f indep_and_p
            in

            (* Exploring one of the initials guarantees that reversal has been
               visited. Thus, schedule one of the initials only if none of them
                is in backtrack. *)
            let prefix_top = last_element prefix in
            if
              IntSet.(cardinal (inter prefix_top.backtrack (of_list initials)))
              = 0
            then
              (* We can add any initial*)
              let initial = last_element initials in
              prefix_top.backtrack <- IntSet.add initial prefix_top.backtrack);

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
              if q == p then false
              else
                let proc' = List.nth s.procs q in
                match proc'.obj_ptr with
                | None -> true
                | Some obj_ptr' ->
                    Option.map (fun obj_ptr -> obj_ptr <> obj_ptr') proc.obj_ptr
                    |> Option.value ~default:true)
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
