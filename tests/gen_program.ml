module Atomic = Dscheck.TracedAtomic
module IntSet = Set.Make (Int)

type config = {
  global_count : int;
  value_limit : int;
  operation_count : int;
  domain_count : int;
  generate_conditionals : bool;
  print_tests : bool;
  seed : int;
}

let print_config t =
  Printf.printf "CONFIG\n";
  Printf.printf "global_count: %d\n" t.global_count;
  Printf.printf "value_limit: %d\n" t.value_limit;
  Printf.printf "operations_count: %d\n" t.operation_count;
  Printf.printf "domain_count: %d\n" t.domain_count;
  Printf.printf "generate_conditionals: %b\n%!" t.generate_conditionals;
  Printf.printf "seed: %d\n%!" t.seed

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
    let var_id = Random.int config.global_count in
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
          min (max 1 fuel) (min config.global_count (1 + Random.int 2))
        in
        let var_ids =
          List.init func_count (fun _ -> Random.int config.global_count)
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
  Random.init config.seed;
  let globals = CCVector.of_list (List.init config.global_count Fun.id) in
  let thread_f = Step.gen ~config ~fuel:config.operation_count in
  let threads = List.init config.domain_count (fun _ -> thread_f ()) in
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
  for i = 1 to test_count do
    Printf.printf "----run: %d/%d\r%!" i test_count;
    run_random config ()
  done;
  Printf.printf "\nall generated programs passed\n%!"

(* Command line interface *)
open Cmdliner

let test_count =
  let default = 100 in
  let info =
    Arg.info [ "t"; "test-count" ] ~docv:"INT"
      ~doc:"Number of programs to generate and test."
  in
  Arg.value (Arg.opt Arg.int default info)

let global_count =
  let default = 3 in
  let info =
    Arg.info [ "g"; "global-count" ] ~docv:"INT"
      ~doc:"Number of global atomic variables in generated programs."
  in
  Arg.value (Arg.opt Arg.int default info)

let print_tests =
  let info = Arg.info [ "p"; "print-tests" ] ~doc:"Print generated tests." in
  Arg.value (Arg.flag info)

let value_limit =
  let default = 3 in
  let info =
    Arg.info [ "l"; "value-limit" ] ~docv:"INT"
      ~doc:
        "Values of atomic operations stay (mostly) between zero and this value."
  in
  Arg.value (Arg.opt Arg.int default info)

let operation_count =
  let default = 3 in
  let info =
    Arg.info [ "o"; "operation-count" ] ~docv:"INT"
      ~doc:"Number of operations generated for every domain."
  in
  Arg.value (Arg.opt Arg.int default info)

let domain_count =
  let default = 3 in
  let info =
    Arg.info [ "d"; "domain-count" ] ~docv:"INT"
      ~doc:"Number of domains in generated tests."
  in
  Arg.value (Arg.opt Arg.int default info)

let generate_conditionals =
  let info =
    Arg.info
      [ "c"; "generate-conditionals" ]
      ~doc:"Generate tests with conditional statements."
  in
  Arg.value (Arg.flag info)

let seed_opt =
  let info = Arg.info [ "s"; "random-seed" ] ~docv:"INT" ~doc:"Random seed" in
  Arg.value (Arg.opt (Arg.some Arg.int) None info)

let cmd =
  let open Term in
  const
    (fun
      test_count
      global_count
      print_tests
      value_limit
      operation_count
      domain_count
      generate_conditionals
      seed_opt
    ->
      let seed =
        match seed_opt with
        | Some seed -> seed
        | None ->
            Random.self_init ();
            Random.bits ()
      in
      let config =
        ({
           global_count;
           value_limit;
           operation_count;
           domain_count;
           generate_conditionals;
           print_tests;
           seed;
         }
          : config)
      in
      print_config config;
      run config test_count)
  $ test_count $ global_count $ print_tests $ value_limit $ operation_count
  $ domain_count $ generate_conditionals $ seed_opt

let () =
  exit @@ Cmd.eval
  @@ Cmd.v (Cmd.info ~doc:"Test generator for DSCheck" "gen_program") cmd
