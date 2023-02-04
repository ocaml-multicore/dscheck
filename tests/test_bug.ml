(** https://github.com/ocaml-multicore/dscheck/pull/3#issuecomment-1366878914 *)

module Atomic = Dscheck.TracedAtomic

let test () =
  let cancelled = Atomic.make false in
  let max_requests = Atomic.make 0 in
  Atomic.spawn (fun () ->
      Atomic.set cancelled true;
      Atomic.decr max_requests;
    );
  Atomic.spawn (fun () ->
      ignore (Atomic.get max_requests);
      assert (Atomic.get cancelled)     (* This bug should be detected *)
    )

let test () =
  match Atomic.trace test with
  | exception _ -> ()
  | _ -> failwith "expected failure"

let () =
  let open Alcotest in
  run "dscheck"
    [
      ( "bug",
        [
          test_case "test" `Quick test;
        ] );
    ]
