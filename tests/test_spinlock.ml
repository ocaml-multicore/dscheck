module Atomic = Dscheck.TracedAtomic

let test () =
  let lock = Atomic.make true in
  let result = Atomic.make 0 in
  Atomic.spawn (fun () -> while Atomic.get lock do Atomic.incr result done) ;
  Atomic.spawn (fun () -> Atomic.set lock false) ;
  Atomic.final (fun () -> Atomic.check (fun () -> Atomic.get result < 10))

let () = Atomic.trace test
