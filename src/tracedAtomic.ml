open Effect
open Effect.Shallow

module Uid = struct
  type t = int list
  let compare = List.compare Int.compare
  let to_string t = String.concat "," (List.map string_of_int t)
end

module IdSet = Set.Make (Uid)
module IdMap = Map.Make (Uid)

type 'a t = 'a Atomic.t * Uid.t

type _ Effect.t +=
  | Make : 'a -> 'a t Effect.t
  | Get : 'a t -> 'a Effect.t
  | Set : ('a t * 'a) -> unit Effect.t
  | Exchange : ('a t * 'a) -> 'a Effect.t
  | CompareAndSwap : ('a t * 'a * 'a) -> bool Effect.t
  | FetchAndAdd : (int t * int) -> int Effect.t
  | Spawn : (unit -> unit) -> unit Effect.t

type cas_result = Unknown | Success | Failed

type atomic_op =
  | Start | Make | Get | Set | Exchange | FetchAndAdd | Spawn
  | CompareAndSwap of cas_result ref

let atomic_op_equal a b = match a, b with
  | CompareAndSwap _, CompareAndSwap _ -> true
  | _ -> a = b

let atomic_op_str x =
  match x with
  | Start -> "start"
  | Make -> "make"
  | Get -> "get"
  | Set -> "set"
  | Exchange -> "exchange"
  | FetchAndAdd -> "fetch_and_add"
  | Spawn -> "spawn"
  | CompareAndSwap cas ->
      begin match !cas with
      | Unknown -> "compare_and_swap?"
      | Success -> "compare_and_swap"
      | Failed -> "compare_and_no_swap"
      end

let tracing = ref false

let finished_processes = ref 0

type process_data = {
  uid : Uid.t;
  mutable domain_generator : int;
  mutable atomic_generator : int;
  mutable next_op: atomic_op;
  mutable next_repr: Uid.t option;
  mutable resume_func : (unit, unit) handler -> unit;
  mutable finished : bool;
}

let every_func = ref (fun () -> ())
let final_func = ref (fun () -> ())

(* Atomics implementation *)
let atomics_counter = ref (-1)

let make v = if !tracing then perform (Make v) else
    begin
      let i = !atomics_counter in
      atomics_counter := !atomics_counter - 1;
      (Atomic.make v, [i])
    end

let get r = if !tracing then perform (Get r) else match r with | (v,_) -> Atomic.get v

let set r v = if !tracing then perform (Set (r, v)) else match r with | (x,_) -> Atomic.set x v

let exchange r v =
  if !tracing then perform (Exchange (r, v)) else match r with | (x,_) -> Atomic.exchange x v

let compare_and_set r seen v =
  if !tracing then perform (CompareAndSwap (r, seen, v))
  else match r with | (x,_) -> Atomic.compare_and_set x seen v

let fetch_and_add r n =
  if !tracing then perform (FetchAndAdd (r, n)) else match r with | (x,_) -> Atomic.fetch_and_add x n

let incr r = ignore (fetch_and_add r 1)

let decr r = ignore (fetch_and_add r (-1))

(* Tracing infrastructure *)
let processes = ref IdMap.empty
let push_process p =
  assert (not (IdMap.mem p.uid !processes)) ;
  processes := IdMap.add p.uid p !processes

let get_process id = IdMap.find id !processes

let update_process_data process_id f op repr =
  let process_rec = get_process process_id in
  process_rec.resume_func <- f;
  process_rec.next_repr <- repr;
  process_rec.next_op <- op

let finish_process process_id =
  let process_rec = get_process process_id in
  process_rec.finished <- true;
  finished_processes := !finished_processes + 1

let handler current_process runner =
  {
    retc =
      (fun _ ->
         (
           finish_process current_process.uid;
           runner ()));
    exnc = (fun s -> raise s);
    effc =
      (fun (type a) (e : a Effect.t) ->
         match e with
         | Make v ->
           Some
             (fun (k : (a, _) continuation) ->
                let i = current_process.atomic_generator :: current_process.uid in
                current_process.atomic_generator <- current_process.atomic_generator + 1 ;
                let m = (Atomic.make v, i) in
                update_process_data current_process.uid (fun h -> continue_with k m h) Make (Some i);
                runner ())
         | Get (v,i) ->
           Some
             (fun (k : (a, _) continuation) ->
                update_process_data current_process.uid (fun h -> continue_with k (Atomic.get v) h) Get (Some i);
                runner ())
         | Set ((r,i), v) ->
           Some
             (fun (k : (a, _) continuation) ->
                update_process_data current_process.uid (fun h -> continue_with k (Atomic.set r v) h) Set (Some i);
                runner ())
         | Exchange ((a,i), b) ->
           Some
             (fun (k : (a, _) continuation) ->
                update_process_data current_process.uid (fun h -> continue_with k (Atomic.exchange a b) h) Exchange (Some i);
                runner ())
         | CompareAndSwap ((x,i), s, v) ->
           Some
             (fun (k : (a, _) continuation) ->
                let res = ref Unknown in
                update_process_data current_process.uid (fun h ->
                  let ok = Atomic.compare_and_set x s v in
                  res := if ok then Success else Failed ;
                  continue_with k ok h) (CompareAndSwap res) (Some i);
                runner ())
         | FetchAndAdd ((v,i), x) ->
           Some
             (fun (k : (a, _) continuation) ->
                update_process_data current_process.uid (fun h ->
                    continue_with k (Atomic.fetch_and_add v x) h) FetchAndAdd (Some i);
                runner ())
         | Spawn f ->
            Some
              (fun (k : (a, _) continuation) ->
                 let fiber_f h = continue_with (fiber f) () h in
                 let uid = current_process.domain_generator :: current_process.uid in
                 current_process.domain_generator <- current_process.domain_generator + 1 ;
                 push_process
                   { uid; domain_generator = 0; atomic_generator = 0; next_op = Start; next_repr = None; resume_func = fiber_f; finished = false } ;
                 update_process_data current_process.uid (fun h ->
                     continue_with k () h) Spawn (Some uid);
                 runner ())
         | _ ->
           None);
  }

let spawn f =
  if !tracing then perform (Spawn f) else failwith "spawn outside tracing"

type proc_rec = { proc_id: Uid.t; op: atomic_op; obj_ptr : Uid.t option }
type state_cell = {
  procs : proc_rec IdMap.t;
  run : proc_rec;
  enabled : IdSet.t;
  mutable backtrack : proc_rec list IdMap.t;
}

let num_runs = ref 0

(* we stash the current state in case a check fails and we need to log it *)
let schedule_for_checks = ref []

let setup_run func init_schedule =
  processes := IdMap.empty ;
  finished_processes := 0;
  schedule_for_checks := init_schedule;
  tracing := true;
  let uid = [] in
  let fiber_f h = continue_with (fiber func) () h in
  push_process
    { uid; domain_generator = 0; atomic_generator = 0; next_op = Start; next_repr = None; resume_func = fiber_f; finished = false } ;
  tracing := false;
  num_runs := !num_runs + 1;
  if !num_runs mod 1000 == 0 then
    Format.printf "run: %d@." !num_runs

let do_run init_schedule =
  let rec run_trace s () =
    tracing := false;
    !every_func ();
    tracing := true;
    match s with
    | [] -> if !finished_processes == IdMap.cardinal !processes then begin
        tracing := false;
        !final_func ();
        tracing := true
      end
    | { proc_id = process_id_to_run ; op = next_op ; obj_ptr = next_ptr } :: schedule ->
      let process_to_run = get_process process_id_to_run in
      assert(not process_to_run.finished);
      assert(atomic_op_equal process_to_run.next_op next_op);
      assert(process_to_run.next_repr = next_ptr);
      process_to_run.resume_func (handler process_to_run (run_trace schedule))
  in
  tracing := true;
  run_trace (List.rev init_schedule) ();
  tracing := false;
  let procs =
    IdMap.mapi (fun i p -> { proc_id = i; op = p.next_op; obj_ptr = p.next_repr }) !processes
  in
  let current_enabled =
    IdMap.to_seq !processes
    |> Seq.filter (fun (_, p) -> not p.finished)
    |> Seq.map (fun (id, _) -> id)
    |> IdSet.of_seq
  in
  let last_run = List.hd init_schedule in
  { procs; enabled = current_enabled; run = last_run; backtrack = IdMap.empty }

type category =
  | Ignore
  | Read
  | Write
  | Read_write

let categorize = function
  | Spawn | Start | Make -> Ignore
  | Get -> Read
  | Set -> Write
  | Exchange | FetchAndAdd -> Read_write
  | CompareAndSwap res ->
      begin match !res with
      | Unknown -> failwith "CAS unknown outcome" (* should not happen *)
      | Success -> Read_write
      | Failed -> Read_write
      end

let rec list_findi predicate lst i = match lst with
  | [] -> None
  | x::_ when predicate i x -> Some (i, x)
  | _::xs -> list_findi predicate xs (i + 1)

let list_findi predicate lst = list_findi predicate lst 0

let mark_backtrack proc time state (last_read, last_write) =
  let j = proc.proc_id in
  let find ptr map = match IdMap.find_opt ptr map with
    | None -> None
    | Some lst ->
        List.find_opt (fun (_, proc_id) -> proc_id <> j) lst
  in
  let find_loc proc =
    match categorize proc.op, proc.obj_ptr with
    | Ignore, _ -> None
    | Read, Some ptr -> find ptr last_write
    | (Write | Read_write), Some ptr -> max (find ptr last_read) (find ptr last_write)
    | _ -> assert false
  in
  let rec find_replay_trace ~lower ~upper proc_id =
    let pre_s = List.nth state (time - upper + 1) in
    let replay_steps = List.filteri (fun k s -> k >= lower && k <= time - upper && s.run.proc_id = proc_id) state in
    let replay_steps = List.rev_map (fun s -> s.run) replay_steps in
    let causal p = match find_loc p with
      | None -> true
      | Some (k, _) -> k <= upper in
    if List.for_all causal replay_steps
    then if IdSet.mem proc_id pre_s.enabled
         then Some replay_steps
         else let is_parent k s = k > lower && k < time - upper && s.run.op = Spawn && s.run.obj_ptr = Some proc_id in
              match list_findi is_parent state with
              | None -> None
              | Some (parent_i, spawn) ->
                  assert (parent_i > lower) ;
                  begin match find_replay_trace ~lower:parent_i ~upper spawn.run.proc_id with
                  | None -> None
                  | Some spawn_steps -> Some (spawn_steps @ replay_steps)
                  end
    else None
  in
  match find_loc proc with
  | None -> ()
  | Some (i, _) ->
    assert (List.length state = time) ;
    let pre_s = List.nth state (time - i + 1) in
    match find_replay_trace ~lower:0 ~upper:i proc.proc_id with
    | None -> ()
    | Some replay_steps ->
        if match IdMap.find_opt j pre_s.backtrack with
        | None -> true
        | Some lst -> List.length lst > List.length replay_steps
        then pre_s.backtrack <- IdMap.add j replay_steps pre_s.backtrack

let map_diff_set map set =
  IdMap.filter (fun key _ -> not (IdSet.mem key set)) map

let rec explore func time state (explored, state_planned) current_schedule clock (last_read, last_write) =
  let s = List.hd state in
  assert (IdMap.is_empty s.backtrack) ;
  let dones = ref IdSet.empty in
  begin match state_planned with
  | [] ->
    if IdSet.cardinal s.enabled > 0 then begin
      let p = IdSet.min_elt s.enabled in
      let init_step = IdMap.find p s.procs in
      s.backtrack <- IdMap.singleton p [init_step] ;
    end
  | { proc_id ; _ } :: _ ->
      dones := explored ;
      s.backtrack <- IdMap.singleton proc_id state_planned
  end ;
  let is_backtracking = ref false in
  while IdMap.(cardinal (map_diff_set s.backtrack !dones)) > 0 do
    let j, new_steps = IdMap.min_binding (map_diff_set s.backtrack !dones) in
    let new_explored =
      if !is_backtracking || state_planned <> [] then !dones else IdSet.empty in
    dones := IdSet.add j !dones;
    let new_step, new_state_planned =
      match new_steps with
      | [] -> assert false
      | next :: future -> next, future
    in
    let new_schedule = new_step :: current_schedule in
    let schedule =
      if !is_backtracking
      then begin
        setup_run func new_schedule ;
        new_schedule
      end
      else begin
        is_backtracking := true ;
        schedule_for_checks := new_schedule;
        [new_step]
      end
    in
    let step = do_run schedule in
    let new_state =
      { step with run = new_step ; backtrack = IdMap.empty }
      :: state
    in
    let new_time = time + 1 in
    mark_backtrack step.run new_time new_state (last_read, last_write);
    let add ptr map =
      IdMap.update
        ptr
        (function None -> Some [new_time, step.run.proc_id]
         | Some steps -> Some ((new_time, step.run.proc_id) :: steps))
        map
    in
    let new_last_access =
      match categorize step.run.op, step.run.obj_ptr with
      | Ignore, _ -> last_read, last_write
      | Read, Some ptr -> add ptr last_read, last_write
      | Write, Some ptr -> last_read, add ptr last_write
      | Read_write, Some ptr -> add ptr last_read, add ptr last_write
      | _ -> assert false
    in
    let new_clock = IdMap.add j new_time clock in
    explore func new_time new_state (new_explored, new_state_planned) new_schedule new_clock new_last_access
  done

let every f =
  every_func := f

let final f =
  final_func := f

let print_trace () =
  List.iter (fun { proc_id ; op ; obj_ptr } ->
    let last_run_ptr = Option.map Uid.to_string obj_ptr |> Option.value ~default:"" in
    Format.printf "  Process %s: %s %s@." (Uid.to_string proc_id) (atomic_op_str op) last_run_ptr
  ) (List.rev !schedule_for_checks)

let check f =
  let tracing_at_start = !tracing in
  tracing := false;
  if not (f ()) then begin
    Format.printf "Check: found assertion violation!@." ;
    assert false
  end;
  tracing := tracing_at_start


let trace func =
  num_runs := 0 ;
  let empty_schedule = [{ proc_id = [] ; op = Start ; obj_ptr = None }] in
  setup_run func empty_schedule ;
  let empty_state = do_run empty_schedule :: [] in
  let empty_state_planned = (IdSet.empty, []) in
  let empty_clock = IdMap.empty in
  let empty_last_access = IdMap.empty, IdMap.empty in
  try explore func 1 empty_state empty_state_planned empty_schedule empty_clock empty_last_access
  with exn ->
    Format.printf "Found error at run %d:@." !num_runs;
    print_trace () ;
    Format.printf "Unhandled exception: %s@." (Printexc.to_string exn) ;
    Printexc.print_backtrace stdout

let trace func =
  Fun.protect
    (fun () -> trace func)
    ~finally:(fun () -> Format.printf "@.Finished after %i runs.@." !num_runs)
