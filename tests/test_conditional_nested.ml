module Atomic = Dscheck.TracedAtomic

let test () =
  let b = Atomic.make 0 in
  let c = Atomic.make 0 in
  let ok = Atomic.make false in
  let seen_b = ref (-1) in

  Atomic.spawn (fun () -> Atomic.set b 1);
  Atomic.spawn (fun () ->
      Atomic.set c 1;
      Atomic.set b 2);
  Atomic.spawn (fun () ->
      if Atomic.get c = 0 then (
        seen_b := Atomic.get b;
        if !seen_b = 0 then Atomic.set ok true))

(* Atomic.final (fun () ->
    Format.printf "seen_b=%i b=%i c=%i ok=%b@." (!seen_b) (Atomic.get b) (Atomic.get c)
      (Atomic.get ok)) *)

let () = Atomic.trace ~record_traces:true test
