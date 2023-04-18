module Atomic = Dscheck.TracedAtomic
module IntSet = Set.Make (Int)

type config = {
  globals_count : int;
  value_limit : int;
  operations_count : int;
  thread_count : int;
  generate_conditionals : bool;
  print_tests : bool;
  seed : int option;
}

let print_config t =
  Printf.printf "CONFIG\n";
  Printf.printf "globals_count: %d\n" t.globals_count;
  Printf.printf "value_limit: %d\n" t.value_limit;
  Printf.printf "operations_count: %d\n" t.operations_count;
  Printf.printf "thread_count: %d\n" t.thread_count;
  Printf.printf "generate_conditionals: %b\n%!" t.generate_conditionals;
  Printf.printf "seed: %s\n%!"
    (Option.map Int.to_string t.seed |> Option.value ~default:"<random>")

let var_name i = Char.chr (i + 97)

module Function = struct
  type t =
    | Get of { true_on : IntSet.t }
    | CompareAndSet of { old_value : int; new_value : int }
    | FetchAndAdd of { delta : int; true_on : IntSet.t }

  let gen value_limit =
    let rec set_f () =
      let set =
        List.init value_limit (fun i -> if Random.bool () then Some i else None)
        |> List.filter_map Fun.id |> IntSet.of_list
      in
      let size = IntSet.cardinal set in
      if 0 < size && size < value_limit then set else set_f ()
    in
    match Random.int 6 with
    | 0 | 1 | 2 -> Get { true_on = set_f () }
    | 3 | 4 ->
        let old_value = Random.int value_limit in
        let new_value = Random.int value_limit in
        CompareAndSet { old_value; new_value }
    | 5 ->
        let delta = Random.int value_limit in
        FetchAndAdd { delta; true_on = set_f () }
    | _ -> assert false

  let eval variable = function
    | Get { true_on } -> IntSet.mem (Atomic.get variable) true_on
    | FetchAndAdd { delta; true_on } ->
        IntSet.mem (Atomic.fetch_and_add variable delta) true_on
    | CompareAndSet { old_value; new_value } ->
        Atomic.compare_and_set variable old_value new_value

  let to_string var_id = function
    | Get { true_on } ->
        Printf.sprintf "IntSet.mem (Atomic.get %c) (IntSet.of_list [%s])"
          (var_name var_id)
          (IntSet.to_seq true_on |> List.of_seq |> List.map Int.to_string
         |> String.concat "; ")
    | FetchAndAdd { delta; true_on } ->
        Printf.sprintf
          "IntSet.mem (Atomic.fetch_and_add %c %d) (IntSet.of_list [%s])"
          (var_name var_id) delta
          (IntSet.to_seq true_on |> List.of_seq |> List.map Int.to_string
         |> String.concat "; ")
    | CompareAndSet { old_value; new_value } ->
        Printf.sprintf "Atomic.compare_and_set %c %d %d" (var_name var_id)
          old_value new_value
end

module Conditional = struct
  type t = { functions : Function.t list; operator : [ `And | `Or ] }

  let gen function_count ~value_limit =
    let functions =
      List.init function_count (fun _ -> Function.gen value_limit)
    in
    let operator = if Random.bool () then `And else `Or in
    { functions; operator }

  let eval t variables =
    let functions = t.functions in
    let rec f operator vars funcs =
      match (vars, funcs) with
      | [], [] -> ( match operator with `And -> true | `Or -> false)
      | var :: vars_tl, func :: funcs_tl -> (
          let output = Function.eval var func in
          match (operator, output) with
          | `Or, true -> true
          | `And, false -> false
          | `Or, false | `And, true -> f operator vars_tl funcs_tl)
      | _, [] | [], _ ->
          failwith
            "gen_program: lists of variables and functions have different \
             lengths"
    in
    f t.operator variables functions

  let to_string t ~var_ids =
    let operator_str = match t.operator with `Or -> " || " | `And -> " && " in
    List.combine t.functions var_ids
    |> List.map (fun (func, var_id) -> Function.to_string var_id func)
    |> String.concat operator_str
end

module Step = struct
  type t =
    | Write of { var_id : int; new_value : int; next : t }
    | Read of { var_id : int; next : t }
    | CompareAndSet of {
        var_id : int;
        old_value : int;
        new_value : int;
        next : t;
      }
    | FetchAndAdd of { var_id : int; delta : int; next : t }
    | Conditional of {
        var_ids : int list;
        conditional : Conditional.t;
        on_true : t;
        next : t;
      }
    | Noop

  let rec run ~globals = function
    | Write { var_id; new_value; next } ->
        Atomic.set (CCVector.get globals var_id) new_value;
        run ~globals next
    | Read { var_id; next } ->
        ignore (Atomic.get (CCVector.get globals var_id));
        run ~globals next
    | CompareAndSet { var_id; old_value; new_value; next } ->
        ignore
          (Atomic.compare_and_set
             (CCVector.get globals var_id)
             old_value new_value);
        run ~globals next
    | FetchAndAdd { var_id; delta; next } ->
        ignore (Atomic.fetch_and_add (CCVector.get globals var_id) delta);
        run ~globals next
    | Conditional { var_ids; conditional; on_true; next } ->
        let variables =
          List.map (fun var_id -> CCVector.get globals var_id) var_ids
        in
        if Conditional.eval conditional variables then run ~globals on_true;
        run ~globals next
    | Noop -> ()

  let rec print t ~depth =
    let indent = List.init depth (fun _ -> "\t") |> String.concat "" in
    match t with
    | Write { var_id; new_value; next } ->
        Printf.printf "%sAtomic.set %c %d;\n" indent (var_name var_id) new_value;
        print ~depth next
    | Read { var_id; next } ->
        Printf.printf "%sAtomic.get %c |> ignore;\n" indent (var_name var_id);
        print ~depth next
    | CompareAndSet { var_id; old_value; new_value; next } ->
        Printf.printf "%sAtomic.compare_and_set %c %d %d |> ignore;\n" indent
          (var_name var_id) old_value new_value;
        print ~depth next
    | FetchAndAdd { var_id; delta; next } ->
        Printf.printf "%sAtomic.fetch_and_add %c %d |> ignore;\n" indent
          (var_name var_id) delta;
        print ~depth next
    | Conditional { var_ids; conditional; on_true; next } ->
        let s = Conditional.to_string conditional ~var_ids in
        Printf.printf "%sif (%s) then (\n" indent s;
        print ~depth:(depth + 1) on_true;
        Printf.printf "%s);\n" indent;
        print ~depth next
    | Noop -> ()

  let rec gen ~config ~fuel () =
    let var_id = Random.int config.globals_count in
    let next fuel =
      if fuel > 1 then gen ~config ~fuel:(fuel - 1) () else Noop
    in
    let maybe_conditionals = if config.generate_conditionals then 1 else 0 in
    match Random.int (6 + maybe_conditionals) with
    | 0 | 1 ->
        let new_value = Random.int config.value_limit in
        Write { var_id; new_value; next = next fuel }
    | 2 | 3 -> Read { var_id; next = next fuel }
    | 4 ->
        let old_value = Random.int config.value_limit in
        let new_value = Random.int config.value_limit in
        CompareAndSet { var_id; old_value; new_value; next = next fuel }
    | 5 ->
        let delta = Random.int config.value_limit - (config.value_limit / 2) in
        FetchAndAdd { var_id; delta; next = next fuel }
    | 6 ->
        let func_count =
          min (max 1 fuel) (min config.globals_count (1 + Random.int 2))
        in
        let var_ids =
          List.init func_count (fun _ -> Random.int config.globals_count)
        in
        let conditional =
          Conditional.gen func_count ~value_limit:config.value_limit
        in
        let fuel_a, fuel_b =
          let tmp = Random.int (max (fuel - func_count) 1) in
          (tmp / 2, tmp / 2)
        in

        let on_true = gen ~config ~fuel:fuel_a () in
        Conditional { var_ids; conditional; on_true; next = next fuel_b }
    | _ -> failwith "drew number without corresponding instruction"
end

module Program = struct
  type thread = Step.t
  type t = { globals : (int, CCVector.ro) CCVector.t; threads : thread list }

  let run ~impl { globals; threads } =
    Atomic.trace ~impl ~record_traces:true (fun () ->
        let globals = CCVector.map Atomic.make globals |> CCVector.freeze in
        List.iter
          (fun thread -> Atomic.spawn (fun () -> Step.run ~globals thread))
          threads;
        ());
    Dscheck.Trace_tracker.get_traces ()

  let print { globals; threads } =
    CCVector.iteri
      (fun var_id value ->
        Printf.printf "let %c = Atomic.make %d in\n" (var_name var_id) value)
      globals;
    List.iter
      (fun thread ->
        Printf.printf "\nDomain.spawn (fun () -> \n";
        Step.print thread ~depth:1;
        Printf.printf ")\n%!")
      threads
end

let run_random config () =
  (match config.seed with
  | None -> Random.self_init ()
  | Some seed -> Random.init seed);
  let globals = CCVector.of_list (List.init config.globals_count Fun.id) in
  let thread_f = Step.gen ~config ~fuel:config.operations_count in
  let threads = List.init config.thread_count (fun _ -> thread_f ()) in
  let program = ({ globals; threads } : Program.t) in
  if config.print_tests then Program.print program;

  let dpor_source = Program.run ~impl:`Dpor_source program in
  let dpor = Program.run ~impl:`Dpor program in
  if not (Dscheck.Trace_tracker.equal dpor_source dpor) then (
    Printf.printf "found mismatch\n\n%!";
    Program.print program;
    Dscheck.Trace_tracker.print dpor stdout;
    Dscheck.Trace_tracker.print dpor_source stdout;
    assert false)

let run config test_count =
  Printf.printf "\n\n";
  for i = 0 to test_count do
    Printf.printf "----run: %d/%d\r%!" i test_count;
    run_random config ()
  done;
  Printf.printf "\nall generated programs passed\n%!"

(* cmd *)
let _ =
  let test_count = ref 100 in
  let globals_count = ref 3 in
  let value_limit = ref 3 in
  let operations_count = ref 3 in
  let thread_count = ref 3 in
  let generate_conditionals = ref true in
  let print_tests = ref false in
  let seed = ref 0 in
  let speclist =
    [
      ( "-test-count",
        Arg.Set_int test_count,
        "number of programs to generate and test" );
      ("-print-tests", Arg.Set print_tests, "print all tests");
      ( "-global-vars-count",
        Arg.Set_int globals_count,
        "number of shared atomic variables (the more, the higher the reduction)"
      );
      ( "-value-limit",
        Arg.Set_int value_limit,
        "range of values used by generated operations" );
      ( "-operations-count",
        Arg.Set_int operations_count,
        "number of operations per thread" );
      ("-thread-count", Arg.Set_int thread_count, "number of threads");
      ( "-generate-conditionals",
        Arg.Set generate_conditionals,
        "enable/disable generation of conditional statements" );
      ("-seed", Arg.Set_int seed, "random seed for generation");
    ]
  in
  Arg.parse speclist
    (fun _ -> ())
    "gen_program.exe [-test-count INT] [-global-vars-count INT] [-value-limit \
     INT] [-operations-count INT] [-thread-count INT] [-generate-conditionals \
     BOOL]";
  let config =
    ({
       globals_count = !globals_count;
       value_limit = !value_limit;
       operations_count = !operations_count;
       thread_count = !thread_count;
       generate_conditionals = !generate_conditionals;
       print_tests = !print_tests;
       seed = (if !seed > 0 then Some !seed else None);
     }
      : config)
  in
  print_config config;
  run config !test_count
