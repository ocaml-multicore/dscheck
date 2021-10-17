module Atomic = TracedAtomic

let do_test () =
  let a = Atomic.make 0 in
  let b = Atomic.get a in
  let c = Atomic.set a b in
  ignore(c)

let () =
  Atomic.trace do_test