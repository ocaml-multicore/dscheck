module Atomic = Dscheck.TracedAtomic

(* a simple concurrent list *)

type conc_list = { value : int; next : conc_list option }

let rec add_node_naive list_head n =
  (* try to add a new node to head *)
  let old_head = Atomic.get list_head in
  let new_node = { value = n; next = Some old_head } in
  (* introduce bug *)
  if Atomic.get list_head = old_head then (
    Atomic.set list_head new_node;
    true)
  else add_node_naive list_head n

let rec add_node_safe list_head n =
  let old_head = Atomic.get list_head in
  let new_node = { value = n; next = Some old_head } in
  if Atomic.compare_and_set list_head old_head new_node then true
  else add_node_safe list_head n

let check_node list_head n =
  let rec check_from_node node =
    match (node.value, node.next) with
    | v, _ when v = n -> true
    | _, None -> false
    | _, Some next_node -> check_from_node next_node
  in
  (* try to find the node *)
  check_from_node (Atomic.get list_head)

let create_test add_node_f upto () =
  let list_head = Atomic.make { value = 0; next = None } in
  for x = 1 to upto do
    Atomic.spawn (fun () ->
        assert (add_node_f list_head x);
        assert (check_node list_head x))
  done;
  Atomic.final (fun () ->
      for x = 1 to upto do
        Atomic.check (fun () -> check_node list_head x)
      done)

let test_list_naive_single_domain () =
  Atomic.trace (create_test add_node_naive 1)

let test_list_naive domains () =
  match Atomic.trace (create_test add_node_naive domains) with
  | exception _ -> ()
  | _ -> failwith "expected failure"

let test_list_safe () = 
  Atomic.trace (create_test add_node_safe 3)

let () =
  let open Alcotest in
  run "dscheck"
    [
      ( "list",
        [
          test_case "naive-1-domain" `Quick (test_list_naive_single_domain);
          test_case "naive-2-domains" `Quick (test_list_naive 2);
          test_case "naive-8-domains" `Quick (test_list_naive 8);
          test_case "safe" `Quick test_list_safe;
        ] );
    ]
