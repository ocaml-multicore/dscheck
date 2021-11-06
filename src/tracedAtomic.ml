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

module PtrMap = Map.Make(
  struct
    type t = Obj.t
    let compare = Stdlib.compare
  end
  )

module IntMap = Map.Make(
  struct
    type t = int
    let compare = Int.compare
  end
  )

let _string_of_set s =
  IntSet.fold (fun y x -> (string_of_int y) ^ "," ^ x) s ""

type atomic_op = Start | Make | Get | Set | Exchange | CompareAndSwap | FetchAndAdd

let tracing = ref false

let finished_processes = ref 0

type process_data = {
  id : int;
  mutable next_op: atomic_op;
  mutable next_repr: Obj.t option;
  mutable resume_func : (unit, unit) handler -> unit;
  initial_func : (unit -> unit);
  mutable finished : bool;
}

(* Atomics implementation *)
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
                let m = Atomic.make v in
                update_process_data current_process_id (fun h -> continue_with k m h) Make (Some (Obj.repr m));
                runner ())
         | Get v ->
           Some
             (fun (k : (a, _) continuation) ->
                update_process_data current_process_id (fun h -> continue_with k (Atomic.get v) h) Get (Some (Obj.repr v));
                runner ())
         | Set (r, v) ->
           Some
             (fun (k : (a, _) continuation) ->
                update_process_data current_process_id (fun h -> continue_with k (Atomic.set r v) h) Set (Some (Obj.repr r));
                runner ())
         | Exchange (a, b) ->
           Some
             (fun (k : (a, _) continuation) ->
                update_process_data current_process_id (fun h -> continue_with k (Atomic.exchange a b) h) Exchange (Some (Obj.repr a));
                runner ())
         | CompareAndSwap (x, s, v) ->
           Some
             (fun (k : (a, _) continuation) ->
                update_process_data current_process_id (fun h ->
                    continue_with k (Atomic.compare_and_set x s v) h) CompareAndSwap (Some (Obj.repr x));
                runner ())
         | FetchAndAdd (v, x) ->
           Some
             (fun (k : (a, _) continuation) ->
                update_process_data current_process_id (fun h ->
                    continue_with k (Atomic.fetch_and_add v x) h) FetchAndAdd (Some (Obj.repr v));
                runner ())
         | _ ->
           Printf.printf "Unknown on %d\n" current_process_id;
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

type proc_rec = { proc_id: int; op: atomic_op; obj_ptr : Obj.t option }
type state_cell = { procs: proc_rec list; run_proc: int; enabled : IntSet.t; mutable backtrack : IntSet.t }

let do_run init_func init_schedule =
  init_func (); (*set up run *)
  tracing := true;
  (* cache the number of processes in case it's expensive*)
  let num_processes = CCVector.length processes in
  (* current number of ops we are through the current run *)
  let rec run_trace s () =
    match s with
    | [] -> ()
    | process_id_to_run :: schedule -> begin
        if !finished_processes == num_processes then
          (* this should never happen *)
          failwith("no enabled processes")
        else
          begin
            Printf.printf "sched: %d\n" process_id_to_run;
            let process_to_run = CCVector.get processes process_id_to_run in
            process_to_run.resume_func (handler process_id_to_run (run_trace schedule))
          end
      end
  in
  tracing := true;
  run_trace init_schedule ();
  finished_processes := 0;
  tracing := false;
  Printf.printf "sched len: %d\n" (List.length init_schedule);
  let procs = CCVector.mapi (fun i p -> { proc_id = i; op = p.next_op; obj_ptr = p.next_repr }) processes |> CCVector.to_list in
  let current_enabled = CCVector.to_seq processes
                        |> OSeq.zip_index
                        |> Seq.filter (fun (_,proc) -> not proc.finished)
                        |> Seq.map (fun (id,_) -> id)
                        |> IntSet.of_seq in
  CCVector.clear processes;
  { procs; enabled = current_enabled; run_proc = last_element init_schedule; backtrack = IntSet.empty }

let rec explore func state clock last_access =
  let s = last_element state in
  List.iter (fun proc ->
      let j = proc.proc_id in
      let i = Option.bind proc.obj_ptr (fun ptr -> PtrMap.find_opt ptr last_access) |> Option.value ~default:0 in
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
      let schedule = j :: (List.map (fun s -> s.run_proc) state) in
      let statedash = (do_run func schedule) :: state in
      let statedash_len = List.length statedash in
      let j_proc = List.nth s.procs j in
      let new_last_access = match j_proc.obj_ptr with Some(ptr) -> PtrMap.add ptr statedash_len last_access | None -> last_access in
      let new_clock = IntMap.add j statedash_len clock in
      Printf.printf "dashlen: %d\n" statedash_len;
      explore func statedash new_clock new_last_access
    done
  end

let trace func =
  let empty_state = do_run func [0] :: [] in
  let empty_clock = IntMap.empty in
  let empty_last_access = PtrMap.empty in
  explore func empty_state empty_clock empty_last_access
