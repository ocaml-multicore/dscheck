open Effect
open Effect.Shallow

module Uid = struct
  type t = int list
  let compare = List.compare Int.compare
  let to_string t = String.concat "," (List.map string_of_int t)

  let rec alpha i =
    let head = Char.chr (Char.code 'A' + i mod 26) in
    let i = i / 26 in
    head :: if i > 0 then alpha i else []

  let pretty lst =
    match List.concat (List.map alpha lst) with
    | [] -> "_"
    | [x] -> String.make 1 x
    | lst ->
      let arr = Array.of_list lst in
      "(" ^ String.init (Array.length arr) (Array.get arr) ^ ")"
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
let atomics_counter = Atomic.make (-1)

let make v = if !tracing then perform (Make v) else
    begin
      let i = Atomic.fetch_and_add atomics_counter (- 1) in
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
            let uid = current_process.domain_generator :: current_process.uid in
            current_process.domain_generator <- current_process.domain_generator + 1 ;
            Some
              (fun (k : (a, _) continuation) ->
                 update_process_data current_process.uid (fun h ->
                     let fiber_f h = continue_with (fiber f) () h in
                     push_process
                       { uid; domain_generator = 0; atomic_generator = 0; next_op = Start; next_repr = None; resume_func = fiber_f; finished = false } ;
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

module Step = struct
  type t = proc_rec
  let compare a b = Uid.compare a.proc_id b.proc_id

  let atomic_op_str op arg =
    let arg = match op, arg with
      | _, None -> "?"
      | Spawn, Some domain_id -> Uid.pretty domain_id
      | _, Some ptr -> Uid.to_string ptr
    in
    match op with
    | Start -> "start"
    | Spawn -> "spawn() = " ^ arg
    | Make -> "make() = " ^ arg
    | Get -> "get(" ^ arg ^ ")"
    | Set -> "set(" ^ arg ^ ")"
    | Exchange -> "exchange(" ^ arg ^ ")"
    | FetchAndAdd -> "fetch_and_add(" ^ arg ^ ")"
    | CompareAndSwap cas ->
        begin match !cas with
        | Unknown -> "compare_and_swap(" ^ arg ^ ")"
        | Success -> "compare_and_swap(" ^ arg ^ ")"
        | Failed -> "compare_and_no_swap(" ^ arg ^ ")"
        end

  let to_string t =
    Printf.sprintf "%s %s"
      (Uid.pretty t.proc_id)
      (atomic_op_str t.op t.obj_ptr)
end

module T = Trie.Make (Step)

let num_runs = ref 0

(* we stash the current state in case a check fails and we need to log it *)
let schedule_for_checks = ref []

let max_depth = ref 0
let depth = ref 0
let incr_depth () =
  depth := !depth + 1 ;
  if !depth > !max_depth then max_depth := !depth

let group_by fn = function
  | [] -> []
  | first :: rest ->
      let rec go previous previouses = function
        | [] -> [previous::previouses]
        | x::xs when fn x previous -> go x (previous::previouses) xs
        | x::xs -> (previous::previouses) :: go x [] xs
      in
      List.rev (go first [] rest)

let print_header () =
  Format.printf "%5s %6s %5s/%-5s %s@."
    "run" "todo" "depth" "max" "latest trace"

let print_stats trie =
  Format.printf "%#4dk %#6d %#5d/%-#5d "
    (!num_runs / 1000)
    (T.nb_todos trie)
    (Option.value (T.min_depth trie) ~default:0)
    !max_depth ;
  List.iter
    (function
      | steps when List.compare_length_with steps 3 <= 0 ->
          List.iter (fun step -> Format.printf "%s" (Uid.pretty step.proc_id)) steps
      | (step :: _) as steps ->
          Format.printf "(%s%i)" (Uid.pretty step.proc_id) (List.length steps)
      | _ -> assert false)
    (group_by (fun a b -> a.proc_id = b.proc_id) !schedule_for_checks) ;
  Format.printf "%!"

let print_trace () =
  List.iter
    (fun step -> Format.printf "  %s@." (Step.to_string step))
    (List.rev !schedule_for_checks)


let setup_run func init_schedule trie =
  processes := IdMap.empty ;
  finished_processes := 0;
  num_runs := !num_runs + 1;
  if !num_runs mod 1000 == 0 then begin
    Format.printf "%c[2K\r%!" (Char.chr 0x1b) ;
    print_stats trie
  end ;
  schedule_for_checks := init_schedule;
  depth := 0 ;
  tracing := true;
  let uid = [] in
  let fiber_f h = continue_with (fiber func) () h in
  push_process
    { uid; domain_generator = 0; atomic_generator = 0; next_op = Start; next_repr = None; resume_func = fiber_f; finished = false } ;
  tracing := false

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
      incr_depth () ;
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
  let rec find_replay_trace ~pre_s ~lower ~upper proc_id =
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
                  if pre_s.run.proc_id = spawn.run.proc_id
                  then None
                  else begin match find_replay_trace ~pre_s ~lower:parent_i ~upper spawn.run.proc_id with
                  | None -> None
                  | Some spawn_steps -> Some (spawn_steps @ replay_steps)
                  end
    else None
  in
  match find_loc proc with
  | None -> ()
  | Some (i, _) ->
    assert (i < time) ;
    let pre_s = List.nth state (time - i + 1) in
    match find_replay_trace ~pre_s ~lower:0 ~upper:i proc.proc_id with
    | None -> ()
    | Some [] -> failwith "empty replay steps"
    | Some ((first_step :: _) as replay_steps) ->
        let j = first_step.proc_id in
        if match IdMap.find_opt j pre_s.backtrack with
        | None -> true
        | Some lst ->
            assert (List.length lst <= List.length replay_steps) ;
            false
        then pre_s.backtrack <- IdMap.add j replay_steps pre_s.backtrack

let round_robin previous =
  let urgent =
    IdMap.fold
      (fun proc_id step acc ->
        assert (proc_id = step.proc_id) ;
        if IdSet.mem proc_id previous.enabled
        then match step.op with
        | Start | Spawn | Make -> proc_id :: acc
        | _ -> acc
        else acc)
    previous.procs
    []
  in
  match urgent with
  | [] ->
      let id = previous.run.proc_id in
      begin match IdSet.find_first_opt (fun j -> j > id) previous.enabled with
      | None -> IdSet.min_elt_opt previous.enabled
      | found -> found
      end
  | [single] -> Some single
  | first :: _ -> Some first (* which one is best? *)

let rec explore func time state trie current_schedule (last_read, last_write) =
  let s = List.hd state in
  assert (IdMap.is_empty s.backtrack) ;
  let todo_next =
    match T.next trie with
    | None ->
        begin match round_robin s with
        | None ->
            assert (IdSet.is_empty s.enabled) ;
            None
        | Some p ->
            let init_step = IdMap.find p s.procs in
            Some (init_step, T.todo)
        end
    | Some (step, new_trie) ->
        Some (step, new_trie)
  in
  match todo_next with
  | None -> false, T.ok 1
  | Some (new_step, new_trie) ->
    begin try
      let new_schedule = new_step :: current_schedule in
      let schedule =
        schedule_for_checks := new_schedule;
        [new_step]
      in
      let step = do_run schedule in
      let new_state =
        { step with run = new_step ; backtrack = IdMap.empty }
        :: state
      in
      let new_time = time + 1 in
      if T.nb_oks new_trie = 0 && not (T.has_error new_trie)
      then mark_backtrack step.run new_time new_state (last_read, last_write);
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
      let has_error, child =
        explore func new_time new_state new_trie new_schedule new_last_access
      in
      let trie =
        IdMap.fold
          (fun _ backstep trie ->
            T.insert_todos ~skip_edge:new_step backstep trie)
          s.backtrack
          trie
      in
      let trie = T.add new_step child trie in
      let trie = T.simplify trie in
      has_error, trie
  with exn ->
    let msg = Printexc.to_string exn in
    let trie = T.add new_step (T.error msg) trie in
    print_header () ;
    print_stats trie ;

    Format.printf "@.@.ERROR: %s@." msg ;
    Printexc.print_backtrace stdout ;
    print_trace () ;

    true, trie
  end

let every f =
  every_func := f

let final f =
  final_func := f

let check f =
  let tracing_at_start = !tracing in
  tracing := false;
  if not (f ()) then begin
    Format.printf "Check: found assertion violation!@." ;
    failwith "invalid check"
  end;
  tracing := tracing_at_start

let error_count = ref 0

let explore_one func trie =
  let empty_schedule = [{ proc_id = [] ; op = Start ; obj_ptr = None }] in
  setup_run func empty_schedule trie ;
  let empty_state = do_run empty_schedule :: [] in
  let empty_last_access = IdMap.empty, IdMap.empty in
  explore func 1 empty_state trie empty_schedule empty_last_access

let rec explore_all ~count ~errors func trie =
  if !error_count >= errors
  || !num_runs >= count
  || T.nb_todos trie = 0
  then trie (* graphviz_output ?graphviz trie *)
  else begin
    let has_error, trie = explore_one func trie in
    if has_error then error_count := !error_count + 1 ;
    explore_all ~count ~errors func trie
  end

let graphviz_output ?graphviz trie =
  match graphviz with
  | None -> ()
  | Some filename -> T.graphviz ~filename trie

let trace ?(count = max_int) ?(errors = 1) ?graphviz func =
  print_header () ;
  num_runs := 0 ;
  let trie = explore_all ~count ~errors func T.todo in
  graphviz_output ?graphviz trie ;
  Format.printf "@.Found %#i errors after %#i runs.@." !error_count !num_runs ;
  ()
