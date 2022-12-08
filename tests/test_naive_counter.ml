module Atomic = Dscheck.TracedAtomic

let counter incr () =
  let counter = Atomic.make 0 in
  Atomic.spawn (fun () -> incr counter);
  Atomic.spawn (fun () -> incr counter);
  Atomic.final (fun () -> Atomic.check (fun () -> Atomic.get counter == 2))

let test_naive_counter () =
  let naive_incr counter = Atomic.set counter (Atomic.get counter + 1) in
  match Atomic.trace (counter naive_incr) with
  | exception _ -> ()
  | _ -> failwith "expected failure"

let test_safe_counter () = Atomic.trace (counter Atomic.incr)

let () =
  let open Alcotest in
  run "dscheck"
    [
      ( "counter",
        [
          test_case "naive" `Quick test_naive_counter;
          test_case "safe" `Quick test_safe_counter;
        ] );
    ]
