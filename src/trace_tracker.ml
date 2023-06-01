module Op = struct
  type t = { proc : int; variable : int; step : int }

  let is_dependent t1 t2 = t1.variable == t2.variable

  let compare_proc_step t1 t2 =
    let c1 = Int.compare t1.proc t2.proc in
    if c1 <> 0 then c1 else Int.compare t1.step t2.step

  let to_str t = Printf.sprintf "(%d,%c)" t.proc (Char.chr (t.variable + 96))
end

module Trace = struct
  module Key = struct
    type t = (Op.t * Op.t option) list

    let compare t1 t2 =
      List.compare
        (fun (op1, dep1) (op2, dep2) ->
          let c1 = Op.compare_proc_step op1 op2 in
          if c1 <> 0 then c1 else Option.compare Op.compare_proc_step dep1 dep2)
        t1 t2
  end

  type t = Op.t List.t

  let of_schedule_for_checks schedule_for_checks : t =
    let steps = Hashtbl.create 10 in
    List.map
      (fun (proc, _, variable) ->
        Option.map
          (fun variable : Op.t ->
            let current =
              Hashtbl.find_opt steps proc |> Option.value ~default:0
            in
            Hashtbl.replace steps proc (current + 1);
            let step = Hashtbl.find steps proc in

            { proc; variable; step })
          variable)
      schedule_for_checks
    |> List.filter_map Fun.id

  let to_string t = List.map Op.to_str t |> String.concat ","

  let tag_with_deps (t : t) : Key.t =
    let next_dep op t = List.find_opt (Op.is_dependent op) t in
    let rec attach_deps = function
      | [] -> []
      | hd :: [] -> [ (hd, None) ]
      | hd :: tl -> (hd, next_dep hd tl) :: attach_deps tl
    in
    let tagged = attach_deps t in
    List.sort (fun (op1, _) (op2, _) -> Op.compare_proc_step op1 op2) tagged

  let deps_to_str (key : Key.t) : string =
    List.map
      (fun (op, dep) ->
        Op.to_str op ^ "-"
        ^ (Option.map Op.to_str dep |> Option.value ~default:"none"))
      key
    |> String.concat ","
end

module TraceMap = Map.Make (Trace.Key)

type t = Trace.t TraceMap.t

let traces = ref TraceMap.empty

let add_trace trace =
  let trace = Trace.of_schedule_for_checks trace in
  let key = Trace.tag_with_deps trace in
  traces :=
    TraceMap.update key
      (function Some v -> Some v | None -> Some trace)
      !traces

let print traces channel =
  Printf.fprintf channel "----\n";
  TraceMap.iter
    (fun _ trace -> Printf.fprintf channel "%s\n" (Trace.to_string trace))
    traces;
  Printf.fprintf channel "----\n";
  flush channel

let print_traces chan = print !traces chan
let clear_traces () = traces := TraceMap.empty
let get_traces () = !traces

let get_deps_str traces =
  TraceMap.to_seq traces |> List.of_seq
  |> List.map (fun (_, value) -> value)
  |> List.map Trace.tag_with_deps
  |> List.map Trace.deps_to_str |> String.concat "\n"

let equal t1 t2 =
  TraceMap.compare
    (fun _ _ ->
      0
      (* any values under the same key are known to be equivalent, even if the exact sequence is not identical *))
    t1 t2
  == 0

let subset t1 t2 =
  TraceMap.fold (fun key _ seen_all -> TraceMap.mem key t2 && seen_all) t1 true
