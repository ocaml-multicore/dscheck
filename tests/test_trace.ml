module Atomic = Dscheck.TracedAtomic

let counter incr () =
  let c1 = Atomic.make 0 in
  let c2 = Atomic.make 0 in
  Atomic.spawn (fun () -> incr c1);
  Atomic.spawn (fun () ->
      incr c1;
      incr c2);

  Atomic.final (fun () ->
      Atomic.check (fun () -> Atomic.get c1 == 2 && Atomic.get c2 == 1))

let test_safe_counter () =
  Atomic.trace ~interleavings:stdout ~record_traces:true (counter Atomic.incr);
  Dscheck.Trace_tracker.print_traces stdout

let _ = test_safe_counter ()
