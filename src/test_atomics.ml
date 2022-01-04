module Lfsm = Lf_skipmap.Make(struct type t = int
    let compare = Int.compare
    let to_string = string_of_int
  end)

let insert_and_check sm n () =
  if not(Lfsm.insert sm n "Nothing") then
    failwith (Printf.sprintf "Could not insert %d" n);
  match Lfsm.find_opt sm n with
  | None -> failwith (Printf.sprintf "Could not find %d after insertion" n)
  | _ -> ();
  if not(Lfsm.remove sm n) then
    failwith (Printf.sprintf "Could not remove %d, already removed" n)

let create_test upto () =
  let lfsm = Lfsm.make 1 in
    for x = 0 to (upto-1) do
      TracedAtomic.spawn(insert_and_check lfsm x)
    done

let () =
  TracedAtomic.trace(create_test 2)