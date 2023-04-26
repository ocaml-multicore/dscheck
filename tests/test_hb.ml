module Atomic = Dscheck.TracedAtomic
module IntSet = Set.Make (Int)

(* get + get *)
let test1 () =
  let a = Atomic.make 0 in

  Atomic.spawn (fun () -> Atomic.set a 1);

  Atomic.spawn (fun () -> Atomic.compare_and_set a 0 0 |> ignore);

  Atomic.spawn (fun () ->
      if IntSet.mem (Atomic.get a) (IntSet.of_list [ 0; 2 ]) then
        Atomic.get a |> ignore)

let () = Atomic.trace ~interleavings:stdout test1
