module Atomic = Dscheck.TracedAtomic

(* Example from ocaml-parrafuzz presentation at the OCaml Workshop 2021:
   https://www.youtube.com/watch?v=GZsUoSaIpIs *)

let test i =
  let x = Atomic.make i in
  let y = Atomic.make 0 in
  Atomic.spawn (fun () ->
    Atomic.spawn (fun () -> if Atomic.get x = 10 then Atomic.set y 2)) ;
  Atomic.set x 0 ;
  Atomic.set y 1 ;
  Atomic.final (fun () -> Atomic.check (fun () -> Atomic.get y <> 2))

let test_0 () = Atomic.trace (fun () -> test 0)

let test_10 () =
  match Atomic.trace (fun () -> test 10) with
  | exception _ -> ()
  | _ -> failwith "expected failure"

let () =
  let open Alcotest in
  run "dscheck"
    [
      ( "simple",
        [
          test_case "test-0" `Quick test_0;
          test_case "test-10" `Quick test_10;
        ] );
    ]
