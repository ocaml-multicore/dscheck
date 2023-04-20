module Atomic = Dscheck.TracedAtomic

(* get + get *)
let test1 () =
  let a = Atomic.make 0 in
  Atomic.spawn (fun () -> Atomic.compare_and_set a 0 1 |> ignore);
  Atomic.spawn (fun () -> Atomic.compare_and_set a 1 2 |> ignore);
  Atomic.spawn (fun () -> Atomic.compare_and_set a 1 2 |> ignore);
  ()

let () =
  Atomic.trace ~record_traces:true test1;
  Printf.printf "traces: %d\n%!"
    (Dscheck.Trace_tracker.get_traces () |> Dscheck.Trace_tracker.count)
