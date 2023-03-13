module Atomic = Dscheck.TracedAtomic

let two_vars () =
  let a = Atomic.make 0 in
  let b = Atomic.make 0 in
  let c = Atomic.make 0 in
  let d = Atomic.make 0 in

  Atomic.trace (fun () ->
      Atomic.spawn (fun () ->
          Atomic.set a 1;
          Atomic.set b 1;
          Atomic.set c 1;
          Atomic.set d 1)
      |> ignore;

      Atomic.spawn (fun () ->
          Atomic.set d 2;
          Atomic.set c 2;
          Atomic.set b 2;
          Atomic.set a 2)
      |> ignore;

      Atomic.final (fun () -> ()));

  assert (Atomic.num_runs () = 33);
  assert (Atomic.num_traces () = 5)

let () =
  let open Alcotest in
  run "disjoint_test" [ ("basic", [ test_case "two-vars" `Quick two_vars ]) ]
