type 'a atomic = { uid : int ; value : 'a }

type ast =
  | Make of int atomic
  | Get of int atomic
  | Set of int atomic * int
  | Compare_and_set of int atomic * int * int
  | Exchange of int atomic * int
  | Fetch_and_add of int atomic * int
  | Spawn of ast
  | Seq of ast * ast
  | Unit

let rec pp h = function
  | Unit ->
      Format.fprintf h "()"
  | Make t ->
      Format.fprintf h "@[let t%i = Atomic.make %i in@]" t.uid t.value
  | Get t ->
      Format.fprintf h "@[let _ : int = Atomic.get t%i in@]" t.uid
  | Set (t, v) ->
      Format.fprintf h "@[Atomic.set t%i %i ;@]" t.uid v
  | Compare_and_set (t, v, w) ->
      Format.fprintf h "@[let _ : bool = Atomic.compare_and_set t%i %i %i in@]" t.uid v w
  | Exchange (t, v) ->
      Format.fprintf h "@[let _ : int = Atomic.exchange t%i %i in@]" t.uid v
  | Fetch_and_add (t, v) ->
      Format.fprintf h "@[let _ : int = Atomic.fetch_and_add t%i %i in@]" t.uid v
  | Spawn domain ->
      Format.fprintf h "@[<v 2>Domain.spawn (fun () ->@ %a) ;@]"
        pp domain
  | Seq (a, b) ->
      Format.fprintf h "%a@;%a" pp a pp b

module Env = Map.Make (Int)

let gen_atomic env =
  let nth = Random.int (Env.cardinal env) in
  let i = ref 0 in
  let exception Found of int atomic in
  try Env.iter (fun _ t -> if !i = nth then raise (Found t) else incr i) env ;
      assert false
  with Found v -> v

let gen_int () = Random.int 1000

let rec gen ~uid ~fuel env =
  if fuel <= 0
  then Unit, uid
  else let expr, uid, fuel, env = gen_expr ~uid ~fuel env in
       let rest, uid = gen ~uid ~fuel env in
       Seq (expr, rest), uid

and gen_expr ~uid ~fuel env =
  match if Env.is_empty env then 6 else Random.int 7 with
  | 0 ->
      let fuel0 = Random.int fuel in
      let domain, uid = gen ~uid ~fuel:fuel0 env in
      Spawn domain, uid, fuel - fuel0, env
  | 1 ->
      let at = gen_atomic env in
      let new_value = gen_int () in
      let env = Env.add at.uid { at with value = new_value } env in
      Set (at, new_value), uid, fuel - 1, env
  | 2 ->
      let at = gen_atomic env in
      let new_value = gen_int () in
      let env = Env.add at.uid { at with value = new_value } env in
      Compare_and_set (at, at.value, new_value), uid, fuel - 1, env
  | 3 ->
      let at = gen_atomic env in
      let new_value = gen_int () in
      let env = Env.add at.uid { at with value = new_value } env in
      Exchange (at, new_value), uid, fuel - 1, env
  | 4 ->
      let at = gen_atomic env in
      let new_value = gen_int () in
      let env = Env.add at.uid { at with value = at.value + new_value } env in
      Fetch_and_add (at, new_value), uid, fuel - 1, env
  | 5 ->
      let at = gen_atomic env in
      Get at, uid, fuel - 1, env
  | _ ->
      let at = { uid ; value = gen_int () } in
      let env = Env.add at.uid at env in
      Make at, uid + 1, fuel, env

let rec pp_result i h = function
  | [] -> Format.fprintf h ""
  | x::xs -> Format.fprintf h "t%i=%i %a" i x (pp_result (i + 1)) xs

let pp_result = pp_result 0

module Outcomes : sig
  type elt = int list
  type t
  val make : unit -> t
  val length : t -> int
  val total : t -> int
  val mem : t -> elt -> bool
  val find : t -> elt -> int
  val record : t -> elt -> unit
  val iter : (elt -> int -> unit) -> t -> unit
end = struct
  type elt = int list

  module H = Hashtbl.Make (struct
    type t = elt
    let equal = List.equal Int.equal
    let hash = Hashtbl.hash
  end)

  type t = int H.t
  let make () = H.create 16
  let length = H.length
  let mem = H.mem
  let iter = H.iter
  let find t result = try H.find t result with Not_found -> 0
  let total t = H.fold (fun _ x y -> x + y) t 0
  let record t result = H.replace t result (find t result + 1)
end

module Sample = struct

  let rec eval outcomes env = function
    | [] ->
        let env = Env.bindings env |> List.map snd in
        Outcomes.record outcomes env
    | domains ->
        let rec go i rest = function
          | [] -> assert false
          | next :: domains when i = 0 ->
              eval_step outcomes env (List.rev_append rest domains) next
          | next :: domains ->
              go (i - 1) (next :: rest) domains
        in
        go (Random.int (List.length domains)) [] domains

  and eval_step outcomes env domains = function
    | Make at ->
        let env = Env.add at.uid at.value env in
        eval outcomes env domains
    | Unit ->
        eval outcomes env domains
    | Get _ ->
        eval outcomes env domains
    | Set (at, v) ->
        let env = Env.add at.uid v env in
        eval outcomes env domains
    | Compare_and_set (at, v, w) ->
        let env =
          if Env.find at.uid env = v
          then Env.add at.uid w env
          else env
        in
        eval outcomes env domains
    | Exchange (at, v) ->
        let env = Env.add at.uid v env in
        eval outcomes env domains
    | Fetch_and_add (at, dv) ->
        let v = Env.find at.uid env in
        let env = Env.add at.uid (v + dv) env in
        eval outcomes env domains
    | Spawn d ->
        eval outcomes env (d::domains)
    | Seq (Seq _, _) -> assert false
    | Seq (a, b) -> (* not actually recursive *)
        eval_step outcomes env (b::domains) a

  let eval outcomes ast = eval outcomes Env.empty [ast]
end

module Check = struct
  module A = Dscheck.TracedAtomic

  let rec eval env = function
    | Unit -> ()
    | Make at ->
        let t = A.make at.value in
        env.(at.uid) <- t
    | Get at ->
        let _ : int = A.get env.(at.uid) in
        ()
    | Set (at, v) ->
        A.set env.(at.uid) v
    | Compare_and_set (at, v, w) ->
        let _ : bool = A.compare_and_set env.(at.uid) v w in
        ()
    | Exchange (at, v) ->
        let _ : int = A.exchange env.(at.uid) v in
        ()
    | Fetch_and_add (at, v) ->
        let _ : int = A.fetch_and_add env.(at.uid) v in
        ()
    | Spawn domain ->
        A.spawn (fun () -> ignore (eval env domain))
    | Seq (a, b) ->
        eval env a ;
        eval env b

  let eval nb_atomics expected_outcomes ast =
    let found = ref 0 in
    let trace_outcomes = Outcomes.make () in
    let dummy = A.make (-1) in
    A.trace (fun () ->
      let env = Array.make nb_atomics dummy in
      eval env ast ;
      A.final (fun () ->
          if Array.exists (( == ) dummy) env
          then failwith "final was called before the end!" ;
          let env = Array.to_list (Array.map A.get env) in
          let is_new = not (Outcomes.mem trace_outcomes env) in
          Outcomes.record trace_outcomes env ;
          if is_new && Outcomes.mem expected_outcomes env
          then begin
            incr found;
            Format.printf "@.Found %i/%i: %a@."
              !found
              (Outcomes.length expected_outcomes)
              pp_result env ;
            (* TODO: quit tracing if everything was found? *)
          end
    )) ;
    trace_outcomes
end

let run ~seed ~fuel ~samples:nb_samples =
  Format.printf "@.#################### --seed=%i ##################################@." seed ;
  Random.init seed ;
  let ast, nb_atomics = gen ~uid:0 ~fuel Env.empty in
  Format.printf "@.let test () =@   @[<v>%a@]@.@." pp ast ;
  let samples = Outcomes.make () in
  for _ = 0 to nb_samples do
    Sample.eval samples ast
  done ;
  Format.printf "Sampling discovered %i possible outcomes:@." (Outcomes.length samples) ;
  let total_samples = Outcomes.total samples in
  Outcomes.iter
    (fun key count_samples ->
      Format.printf " %5.2f%% : %a@."
        (100.0 *. float count_samples /. float total_samples)
        pp_result key)
    samples ;
  Format.printf "@.Run dscheck:@." ;
  let traces = Check.eval nb_atomics samples ast in
  let total_traces = Outcomes.total traces in
  Format.printf "@.Dscheck ran %i traces and discovered %i outcomes:@."
    total_traces
    (Outcomes.length traces) ;
  Outcomes.iter
    (fun key count_dscheck ->
      let count_samples = Outcomes.find samples key in
      Format.printf "   OK: %5.2f%% (dscheck)    %5.2f%% (samples)@."
        (100.0 *. float count_dscheck /. float total_traces)
        (100.0 *. float count_samples /. float total_samples)
    ) traces ;
  let not_found = ref 0 in
  Outcomes.iter
    (fun key _ ->
      if not (Outcomes.mem traces key)
      then begin
        incr not_found;
        Format.printf "ERROR: not found %a@." pp_result key
      end)
    samples ;
  if !not_found > 0
  then failwith (Printf.sprintf "dscheck missed some traces! --seed=%i" seed)

let rec test_loop ?(seed = Random.bits ()) ~fuel ~samples nb =
  if nb > 0
  then begin
    run ~seed ~fuel ~samples ;
    test_loop ~fuel ~samples (nb - 1)
  end

open Cmdliner

let seed =
  Arg.value
  @@ Arg.opt Arg.(some int) None
  @@ Arg.info ~doc:"Random seed" ~docv:"SEED" [ "seed" ]

let fuel =
  Arg.required
  @@ Arg.opt Arg.(some int) (Some 16)
  @@ Arg.info ~doc:"Size of generated programs" ~docv:"FUEL" [ "fuel" ]

let samples =
  Arg.required
  @@ Arg.opt Arg.(some int) (Some 100_000)
  @@ Arg.info ~doc:"Number of random samples" ~docv:"SAMPLES" [ "samples" ]

let nb =
  Arg.required
  @@ Arg.opt Arg.(some int) (Some 100)
  @@ Arg.info ~doc:"Number of tests iteration" ~docv:"NB" [ "tests" ]

let cmd =
  let open Term in
  const (fun seed fuel samples nb -> test_loop ?seed ~fuel ~samples nb) $ seed $ fuel $ samples $ nb

let () =
  Random.self_init () ;
  Stdlib.exit @@ Cmd.eval @@ Cmd.v (Cmd.info ~doc:"Random test generator" "test") cmd
