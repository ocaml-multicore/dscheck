open EffectHandlers
open EffectHandlers.Shallow

type 'a t = 'a Atomic.t * int

type _ eff +=
  | Make : 'a -> 'a t eff
  | Get : 'a t -> 'a eff
  | Set : ('a t * 'a) -> unit eff
  | Exchange : ('a t * 'a) -> 'a eff
  | CompareAndSwap : ('a t * 'a * 'a) -> bool eff
  | FetchAndAdd : (int t * int) -> int eff

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
  id : int;
  mutable next_op: atomic_op;
  mutable next_repr: int option;
  mutable resume_func : (unit, unit) handler -> unit;
  initial_func : (unit -> unit);
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
      (fun (type a) (e : a eff) ->
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
  let new_id = CCVector.length processes in
  let fiber_f h =
    continue_with (fiber f) () h in
  CCVector.push processes
    { id = new_id; next_op = Start; next_repr = None; resume_func = fiber_f; initial_func = f; finished = false }

let rec last_element l =
  match l with
  | h :: [] -> h
  | [] -> assert(false)
  | _ :: tl -> last_element tl

type proc_rec = { proc_id: int; op: atomic_op; obj_ptr : int option }
type state_cell = { procs: proc_rec list; run_proc: int; enabled : IntSet.t; mutable backtrack : IntSet.t }

let num_runs = ref 0

(* we stash the current state in case a check fails and we need to log it *)
let state_for_checks = ref []

let do_run init_func init_schedule =
  init_func (); (*set up run *)
  tracing := true;
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
        Printf.printf "Running finish\n";
        !final_func ();
        let last_state = ref None in
        List.iter (fun s ->
            begin match !last_state with
            | None -> Printf.printf "Process %d: start\n" s.run_proc
            | Some(x) -> begin
                let last_run_proc = List.nth x.procs s.run_proc in
                let last_run_ptr = Option.map string_of_int last_run_proc.obj_ptr |> Option.value ~default:"" in
                  Printf.printf "Process %d: %s %s\n" s.run_proc (atomic_op_str last_run_proc.op) last_run_ptr
              end;
            end;
            last_state := (Some s)
          ) !state_for_checks;
        tracing := true
      end
    | process_id_to_run :: schedule -> begin
        if !finished_processes == num_processes then
          (* this should never happen *)
          failwith("no enabled processes")
        else
          begin
            let process_to_run = CCVector.get processes process_id_to_run in
            process_to_run.resume_func (handler process_id_to_run (run_trace schedule))
          end
      end
  in
  tracing := true;
  run_trace init_schedule ();
  finished_processes := 0;
  tracing := false;
  num_runs := !num_runs + 1;
  if !num_runs mod 1 == 0 then
    Printf.printf "run: %d\n" !num_runs;
  let procs = CCVector.mapi (fun i p -> { proc_id = i; op = p.next_op; obj_ptr = p.next_repr }) processes |> CCVector.to_list in
  let current_enabled = CCVector.to_seq processes
                        |> OSeq.zip_index
                        |> Seq.filter (fun (_,proc) -> not proc.finished)
                        |> Seq.map (fun (id,_) -> id)
                        |> IntSet.of_seq in
  CCVector.clear processes;
  atomics_counter := 1;
  { procs; enabled = current_enabled; run_proc = last_element init_schedule; backtrack = IntSet.empty }

let rec explore depth func state clock last_access =
  let s = last_element state in
  List.iter (fun proc ->
      let j = proc.proc_id in
      let i = Option.bind proc.obj_ptr (fun ptr -> IntMap.find_opt ptr last_access) |> Option.value ~default:0 in
      let last_hb_p = IntMap.find_opt j clock |> Option.value ~default:0 in
      if i != 0 && i < last_hb_p then begin
        let pre_s = List.nth state (i-1) in
        if IntSet.mem j pre_s.enabled then
          pre_s.backtrack <- IntSet.add j pre_s.backtrack
        else
          pre_s.backtrack <- IntSet.union pre_s.backtrack pre_s.enabled
      end
    ) s.procs;
  if IntSet.cardinal s.enabled > 0 then begin
    let p = IntSet.min_elt s.enabled in
    let dones = ref IntSet.empty in
    s.backtrack <- IntSet.singleton p;
    while IntSet.(cardinal (diff s.backtrack !dones)) > 0 do
      let j = IntSet.min_elt (IntSet.diff s.backtrack !dones) in
      dones := IntSet.add j !dones;
      let schedule = (List.map (fun s -> s.run_proc) state) @ [j] in
      state_for_checks := state;
      let statedash = state @ [do_run func schedule] in
      let state_time = (List.length statedash)-1 in
      let j_proc = List.nth s.procs j in
      let new_last_access = match j_proc.obj_ptr with Some(ptr) -> IntMap.add ptr state_time last_access | None -> last_access in
      let new_clock = IntMap.add j state_time clock in
      explore (depth+1) func statedash new_clock new_last_access
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
    let state = !state_for_checks in
    Printf.printf "Found assertion violation at run %d:\n" !num_runs;
    let last_state = ref None in
    List.iter (fun s ->
        begin match !last_state with
        | None -> Printf.printf "Process %d: start\n" s.run_proc
        | Some(x) -> begin
            let last_run_proc = List.nth x.procs s.run_proc in
            let last_run_ptr = Option.map string_of_int last_run_proc.obj_ptr |> Option.value ~default:"" in
              Printf.printf "Process %d: %s %s\n" s.run_proc (atomic_op_str last_run_proc.op) last_run_ptr
          end;
        end;
        last_state := (Some s)
      ) state;
      assert(false)
  end;
  tracing := tracing_at_start


let trace func =
  let empty_state = do_run func [0] :: [] in
  let empty_clock = IntMap.empty in
  let empty_last_access = IntMap.empty in
  explore 0 func empty_state empty_clock empty_last_access
