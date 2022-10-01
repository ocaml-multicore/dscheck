module Atomic = Dscheck.TracedAtomic

(* Example from ocaml-parrafuzz presentation at the OCaml Workshop 2021:
   https://www.youtube.com/watch?v=GZsUoSaIpIs *)

let test i =
  let x = Atomic.make i in
  let y = Atomic.make 0 in
  Atomic.spawn (fun () -> if Atomic.get x = 10 then Atomic.set y 2) ;
  Atomic.spawn (fun () -> Atomic.set x 0 ; Atomic.set y 1) ;
  Atomic.final (fun () -> Atomic.check (fun () -> Atomic.get y <> 2))

let () =
  Atomic.trace (fun () -> test 0) ;
  Printf.printf "\n-----------------------------\n\n%!" ;
  Atomic.trace (fun () -> test 10)
