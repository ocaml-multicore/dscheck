module Lfsm = Lf_skipmap.Make(struct type t = int
let compare = compare
let to_string = string_of_int
end)

let () =
  let sm = Lfsm.make 2 in
    Lfsm.insert sm 5 "hello" |> ignore;
    Printf.printf "Found: %s\n" (Lfsm.lookup sm 5 |> Option.get)