module Atomic = Dscheck.TracedAtomic

(* a simple concurrent list *)

type conc_list = { value: int; next: conc_list option }

let rec add_node ~bug list_head n =
  (* try to add a new node to head *)
  let old_head = Atomic.get list_head in
  let new_node = { value = n ; next = (Some old_head) } in
    (* introduce bug *)
    if bug && Atomic.get list_head = old_head then begin
      Atomic.set list_head new_node;
      true
    end
    else if Atomic.compare_and_set list_head old_head new_node
    then true
    else
      add_node ~bug list_head n

let check_node list_head n =
  let rec check_from_node node =
    match (node.value, node.next) with
    | (v, _) when v = n -> true
    | (_, None) -> false
    | (_ , Some(next_node)) -> begin
      check_from_node next_node
    end
  in
  (* try to find the node *)
    check_from_node (Atomic.get list_head)

let add_and_check ~bug list_head n () =
  assert(add_node ~bug list_head n);
  assert(check_node list_head n)

let create_test ~buggy upto () =
  let list_head = Atomic.make { value = 0 ; next = None } in
  for x = 1 to upto do
    Atomic.spawn (add_and_check ~bug:(x = buggy) list_head x);
  done;
  Atomic.final (fun () ->
    for x = 1 to upto do
      Atomic.check(fun () -> check_node list_head x)
    done
  )

let () =
  Atomic.trace (create_test ~buggy:2 4)
