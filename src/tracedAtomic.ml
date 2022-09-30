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

module IntSet = Set.Make(
  struct
    let compare = Stdlib.compare
    type t = int
  end )

module IntMap = Map.Make(
  struct
    type t = int
    let compare = Int.compare
  end
  )

let _string_of_set s =
  IntSet.fold (fun y x -> (string_of_int y) ^ "," ^ x) s ""

type atomic_op = Start | Make | Get | Set | Exchange | CompareAndSwap | FetchAndAdd

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
  mutable next_op: atomic_op;
  mutable next_repr: int option;
  mutable resume_func : (unit, unit) handler -> unit;
  mutable finished : bool;
}

let every_func = ref (fun () -> ())
let final_func = ref (fun () -> ())

(* Atomics implementation *)
let atomics_counter = ref 1

let make v = if !tracing then perform (Make v) else
    begin
      let i = !atomics_counter in
      atomics_counter := !atomics_counter + 1;
      (Atomic.make v, i)
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
let processes = CCVector.create ()

let update_process_data process_id f op repr =
  let process_rec = CCVector.get processes process_id in
  process_rec.resume_func <- f;
  process_rec.next_repr <- repr;
  process_rec.next_op <- op

let finish_process process_id =
  let process_rec = CCVector.get processes process_id in
  process_rec.finished <- true;
  finished_processes := !finished_processes + 1

let handler current_process_id runner =
  {
    retc =
      (fun _ ->
         (
           finish_process current_process_id;
           runner ()));
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
                update_process_data current_process_id (fun h -> continue_with k m h) Make (Some i);
                runner ())
         | Get (v,i) ->
           Some
             (fun (k : (a, _) continuation) ->
                update_process_data current_process_id (fun h -> continue_with k (Atomic.get v) h) Get (Some i);
                runner ())
         | Set ((r,i), v) ->
           Some
             (fun (k : (a, _) continuation) ->
                update_process_data current_process_id (fun h -> continue_with k (Atomic.set r v) h) Set (Some i);
                runner ())
         | Exchange ((a,i), b) ->
           Some
             (fun (k : (a, _) continuation) ->
                update_process_data current_process_id (fun h -> continue_with k (Atomic.exchange a b) h) Exchange (Some i);
                runner ())
         | CompareAndSwap ((x,i), s, v) ->
           Some
             (fun (k : (a, _) continuation) ->
                update_process_data current_process_id (fun h ->
                    continue_with k (Atomic.compare_and_set x s v) h) CompareAndSwap (Some i);
                runner ())
         | FetchAndAdd ((v,i), x) ->
           Some
             (fun (k : (a, _) continuation) ->
                update_process_data current_process_id (fun h ->
                    continue_with k (Atomic.fetch_and_add v x) h) FetchAndAdd (Some i);
                runner ())
         | _ ->
           None);
  }

let spawn f =
  let fiber_f h =
    continue_with (fiber f) () h in
  CCVector.push processes
    { next_op = Start; next_repr = None; resume_func = fiber_f; finished = false }

type proc_rec = { proc_id: int; op: atomic_op; obj_ptr : int option }
type state_cell = { procs: proc_rec list; run: proc_rec; enabled : IntSet.t; mutable backtrack : IntSet.t }

let num_runs = ref 0

(* we stash the current state in case a check fails and we need to log it *)
let schedule_for_checks = ref []

let setup_run func init_schedule =
  atomics_counter := 1;
  CCVector.clear processes;
  schedule_for_checks := init_schedule;
  func () ;
  finished_processes := 0;
  num_runs := !num_runs + 1;
  if !num_runs mod 1000 == 0 then
    Printf.printf "run: %d\n" !num_runs

let do_run init_schedule =
  (* cache the number of processes in case it's expensive*)
  let num_processes = CCVector.length processes in
  (* current number of ops we are through the current run *)
  let rec run_trace s () =
    tracing := false;
    !every_func ();
    tracing := true;
    match s with
    | [] -> if !finished_processes == num_processes then begin
        tracing := false;
        !final_func ();
        tracing := true
      end
    | { proc_id = process_id_to_run ; op = next_op ; obj_ptr = next_ptr } :: schedule -> begin
        if !finished_processes == num_processes then
          (* this should never happen *)
          failwith("no enabled processes")
        else
          begin
            let process_to_run = CCVector.get processes process_id_to_run in
            assert(process_to_run.next_op = next_op);
            assert(process_to_run.next_repr = next_ptr);
            process_to_run.resume_func (handler process_id_to_run (run_trace schedule))
          end
      end
  in
  tracing := true;
  run_trace (List.rev init_schedule) ();
  tracing := false;
  let procs = CCVector.mapi (fun i p -> { proc_id = i; op = p.next_op; obj_ptr = p.next_repr }) processes |> CCVector.to_list in
  let current_enabled = CCVector.to_seq processes
                        |> OSeq.zip_index
                        |> Seq.filter (fun (_,proc) -> not proc.finished)
                        |> Seq.map (fun (id,_) -> id)
                        |> IntSet.of_seq in
  let last_run = List.hd init_schedule in
  { procs; enabled = current_enabled; run = last_run ; backtrack = IntSet.empty }

type category =
  | Ignore
  | Read
  | Write
  | Read_write

let categorize = function
  | Start | Make -> Ignore
  | Get -> Read
  | Set | Exchange -> Write
  | CompareAndSwap | FetchAndAdd -> Read_write

let mark_backtrack proc time state (last_read, last_write) =
  let j = proc.proc_id in
  let find ptr map = match IntMap.find_opt ptr map with
    | None -> None
    | Some lst ->
        List.find_opt (fun (_, proc_id) -> proc_id <> j) lst
  in
  let i = match categorize proc.op, proc.obj_ptr with
    | Ignore, _ -> None
    | Read, Some ptr -> find ptr last_write
    | Write, Some ptr -> find ptr last_read
    | Read_write, Some ptr -> max (find ptr last_read) (find ptr last_write)
    | _ -> assert false
  in
  match i with
  | None -> ()
  | Some (i, _) ->
    assert (List.length state = time) ;
    let pre_s = List.nth state (time - i) in
    if IntSet.mem j pre_s.enabled then
      pre_s.backtrack <- IntSet.add j pre_s.backtrack
    else
      pre_s.backtrack <- IntSet.union pre_s.backtrack pre_s.enabled

let rec explore func time state current_schedule clock (last_read, last_write) =
  let s = List.hd state in
  if IntSet.cardinal s.enabled > 0 then begin
    let p = IntSet.min_elt s.enabled in
    let dones = ref IntSet.empty in
    s.backtrack <- IntSet.singleton p;
    let is_backtracking = ref false in
    while IntSet.(cardinal (diff s.backtrack !dones)) > 0 do
      let j = IntSet.min_elt (IntSet.diff s.backtrack !dones) in
      dones := IntSet.add j !dones;
      let j_proc = List.nth s.procs j in
      let new_step = j_proc in
      let full_schedule = new_step :: current_schedule in
      let schedule =
        if !is_backtracking
        then begin
          setup_run func full_schedule ;
          full_schedule
        end
        else begin
          is_backtracking := true ;
          schedule_for_checks := full_schedule;
          [new_step]
        end
      in
      let step = do_run schedule in
      mark_backtrack step.run time state (last_read, last_write);
      let new_state = step :: state in
      let new_schedule = step.run :: current_schedule in
      let new_time = time + 1 in
      let add ptr map =
        IntMap.update
          ptr
          (function None -> Some [time, step.run.proc_id]
           | Some steps -> Some ((time, step.run.proc_id) :: steps))
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
      let new_clock = IntMap.add j new_time clock in
      explore func new_time new_state new_schedule new_clock new_last_access
    done
  end

let every f =
  every_func := f

let final f =
  final_func := f

let check f =
  let tracing_at_start = !tracing in
  tracing := false;
  if not (f ()) then begin
    Printf.printf "Found assertion violation at run %d:\n" !num_runs;
    List.iter (fun { proc_id ; op ; obj_ptr } ->
      let last_run_ptr = Option.map string_of_int obj_ptr |> Option.value ~default:"" in
      Printf.printf "Process %d: %s %s\n" proc_id (atomic_op_str op) last_run_ptr
    ) (List.rev !schedule_for_checks) ;
      assert(false)
  end;
  tracing := tracing_at_start


let trace func =
  let empty_schedule = [{ proc_id = 0 ; op = Start ; obj_ptr = None }] in
  setup_run func empty_schedule ;
  let empty_state = do_run empty_schedule :: [] in
  let empty_clock = IntMap.empty in
  let empty_last_access = IntMap.empty, IntMap.empty in
  explore func 1 empty_state empty_schedule empty_clock empty_last_access

let trace func =
  Fun.protect
    (fun () -> trace func)
    ~finally:(fun () -> Printf.printf "Finished after %i runs.\n%!" !num_runs)
