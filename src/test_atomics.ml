module Atomic = TracedAtomic

let () =
  let a = Atomic.make 0 in
  let b = Atomic.get a in
  let c = Atomic.set a b in
  ignore(c)