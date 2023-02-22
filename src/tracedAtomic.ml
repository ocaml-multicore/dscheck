open Effect
open Effect.Shallow

module Uid = struct
  type t = int list
  let compare = List.compare Int.compare
  let to_string t = String.concat "," (List.map string_of_int t)
end

module IdSet = Set.Make (Uid)
module IdMap = Map.Make (Uid)

module Uid_pretty = struct
  let gen = ref 0
  let cache = ref IdMap.empty

  let find t =
    match IdMap.find t !cache with
    | i -> i
    | exception Not_found ->
        let i = !gen in
        incr gen ;
        cache := IdMap.add t i !cache ;
        i

  let to_string = function
    | [] -> "_"
    | t ->
        let i = find t in
        if i >= 0 && i < 26
        then String.make 1 (Char.chr (Char.code 'A' + i))
        else string_of_int i
end

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
  mutable generator : int;
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
                let i = current_process.generator :: current_process.uid in
                current_process.generator <- current_process.generator + 1 ;
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
                 let uid = current_process.generator :: current_process.uid in
                 current_process.generator <- current_process.generator + 1 ;
                 let new_process =
                   { uid; generator = 0; next_op = Start; next_repr = None; resume_func = fiber_f; finished = false }
                 in
                 update_process_data
                   current_process.uid
                   (fun h ->
                     push_process new_process ;
                     continue_with k () h)
                   Spawn
                   (Some uid);
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

let group_by fn = function
  | [] -> []
  | first :: rest ->
      let rec go previous previouses = function
        | [] -> [previous::previouses]
        | x::xs when fn x previous -> go x (previous::previouses) xs
        | x::xs -> (previous::previouses) :: go x [] xs
      in
      List.rev (go first [] rest)

let pretty_print h lst =
  List.iter
    (function
      | steps when List.compare_length_with steps 3 <= 0 ->
          List.iter (fun step -> Format.fprintf h "%s" (Uid_pretty.to_string step.proc_id)) steps
      | (step :: _) as steps ->
          Format.fprintf h "(%s%i)" (Uid_pretty.to_string step.proc_id) (List.length steps)
      | _ -> assert false)
    (group_by (fun a b -> a.proc_id = b.proc_id) lst)

let clear_line = "\027[2K\r"

let num_runs = ref 0

(* we stash the current state in case a check fails and we need to log it *)
let schedule_for_checks = ref []

let setup_run func init_schedule =
  num_runs := !num_runs + 1;
  if !num_runs mod 1000 == 0 then
    Format.printf "%srun: %#i %a %!" clear_line !num_runs pretty_print !schedule_for_checks;
  processes := IdMap.empty ;
  finished_processes := 0;
  schedule_for_checks := init_schedule;
  tracing := true;
  let uid = [] in
  let fiber_f h = continue_with (fiber func) () h in
  push_process
    { uid; generator = 0; next_op = Start; next_repr = None; resume_func = fiber_f; finished = false } ;
  tracing := false

let do_run init_schedule =
  let trace = ref [] in
  let rec run_trace s () =
    tracing := false;
    !every_func ();
    tracing := true;
    match s with
    | [] -> if !finished_processes == IdMap.cardinal !processes then begin
        tracing := false;
        !final_func ();
        tracing := true;
      end
    | { proc_id = process_id_to_run ; op = next_op ; obj_ptr = next_ptr } :: schedule ->
      let process_to_run = get_process process_id_to_run in
      assert(not process_to_run.finished);
      assert(atomic_op_equal process_to_run.next_op next_op);
      assert(process_to_run.next_repr = next_ptr);
      let run = { proc_id = process_id_to_run ; op = process_to_run.next_op ; obj_ptr = process_to_run.next_repr } in
      trace := run :: !trace ;
      process_to_run.resume_func (handler process_to_run (run_trace schedule))
  in
  tracing := true;
  run_trace (List.rev init_schedule) () ;
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
  let last_run = List.hd !trace in
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
      | Failed -> Read
      end

let mark_backtrack proc state =
  match proc.op, proc.obj_ptr, state with
  | (Spawn | Start | Make), _, _
  | _, None, _
  | _, _, [] -> ()
  | _, Some proc_ptr, _ :: state ->
      let rec go active_uids timeline acc =
        match timeline with
        | [] -> ()
        | pre :: timeline ->
            let is_required = IdSet.mem pre.run.proc_id active_uids in
            begin match is_required, pre.run.obj_ptr with
            | true, Some ptr when ptr = proc_ptr && categorize pre.run.op <> Read ->
                ()
            | false, Some ptr when ptr = proc_ptr ->
                begin match categorize proc.op, categorize pre.run.op with
                | _, Ignore -> failwith "how?"
                | Read, Read -> go active_uids timeline acc
                | _ ->
                    begin match acc, timeline with
                    | [], _ | _, [] -> assert false
                    | last :: _, pre :: _ ->
                        let j = last.proc_id in
                        if match IdMap.find_opt j pre.backtrack with
                           | None -> true
                           | Some lst -> List.length lst > List.length acc
                        then (
                          pre.backtrack <- IdMap.add j acc pre.backtrack ;
                        )
                    end
                end
            | true, None ->
                go active_uids timeline (pre.run :: acc)
            | true, Some ptr ->
                let active_uids = IdSet.add ptr active_uids in
                go active_uids timeline (pre.run :: acc)
            | false, Some ptr when IdSet.mem ptr active_uids ->
                begin match categorize pre.run.op with
                | Read -> go active_uids timeline acc
                | _ ->
                    let active_uids = IdSet.add pre.run.proc_id active_uids in
                    go active_uids timeline (pre.run :: acc)
                end
            | false, _ ->
                go active_uids timeline acc
            end
      in
      let active_uids = IdSet.add proc_ptr (IdSet.singleton proc.proc_id) in
      go active_uids state [proc]

let map_subtract_set map set =
  IdMap.filter (fun key _ -> not (IdSet.mem key set)) map

let rec explore func time state (explored, state_planned) current_schedule =
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
  while IdMap.(cardinal (map_subtract_set s.backtrack !dones)) > 0 do
    let j, new_steps = IdMap.min_binding (map_subtract_set s.backtrack !dones) in
    let new_explored = !dones in
    dones := IdSet.add j new_explored;
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
    let new_state = { step with backtrack = IdMap.empty } :: state in
    let new_time = time + 1 in
    mark_backtrack step.run new_state;
    explore func new_time new_state (new_explored, new_state_planned) new_schedule
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

let reset_state () = 
  finished_processes := 0; 
  Atomic.set atomics_counter 1;
  num_runs := 0;
  schedule_for_checks := [];
  processes := IdMap.empty

let trace func =
  reset_state ();
  let empty_schedule = [{ proc_id = [] ; op = Start ; obj_ptr = None }] in
  setup_run func empty_schedule ;
  let empty_state = do_run empty_schedule :: [] in
  let empty_state_planned = (IdSet.empty, []) in
  try explore func 1 empty_state empty_state_planned empty_schedule
  with exn ->
    Format.printf "Found error at run %d:@." !num_runs;
    print_trace () ;
    Format.printf "Unhandled exception: %s@." (Printexc.to_string exn) ;
    Printexc.print_backtrace stdout ;
    raise exn

let trace func =
  Fun.protect
    (fun () -> trace func)
    ~finally:(fun () -> Format.printf "@.Finished after %#i runs.@." !num_runs)
