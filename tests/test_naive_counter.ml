module Atomic = Dscheck.TracedAtomic

let test_counter () =
  let counter = Atomic.make 0 in
  let incr () = Atomic.set counter (Atomic.get counter + 1) in 
  Atomic.spawn incr;
  Atomic.spawn incr;
  Atomic.final (fun () -> Atomic.check (fun () -> Atomic.get counter == 2))

let () = Atomic.trace test_counter
