module Atomic = Dscheck.TracedAtomic

(* get + get *)
let test1 () =
  let a = Atomic.make 0 in
  Atomic.spawn (fun () -> Atomic.set a 2);
  Atomic.spawn (fun () -> Atomic.get a |> ignore);
  (* Atomic.spawn (fun () -> Atomic.set a 2); *)
  ()

let () = Atomic.trace ~interleavings:stdout test1