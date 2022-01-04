module Lfsm = Lf_skipmap.Make(struct type t = int
    let compare = Int.compare
    let to_string = string_of_int
  end)

let insert_and_check sm n () =
  assert(Lfsm.insert sm n "Nothing");
  assert(Lfsm.remove sm n)

let create_test upto () =
  let lfsm = Lfsm.make 2 in
    for x = 0 to (upto-1) do
      TracedAtomic.spawn(insert_and_check lfsm x)
    done

let () =
  TracedAtomic.trace(create_test 2)