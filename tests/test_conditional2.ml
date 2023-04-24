module Atomic = Dscheck.TracedAtomic

let test () =
  let x = Atomic.make 0 in
  let y = Atomic.make 0 in
  let z = Atomic.make 0 in

  let tmp = ref (-1) in
  Atomic.spawn (fun () -> tmp := Atomic.get x);
  Atomic.spawn (fun () -> Atomic.set y 1);

  Atomic.spawn (fun () ->
      let m = Atomic.get y in
      if m = 0 then Atomic.set z 1);

  Atomic.spawn (fun () ->
      let n = Atomic.get z in
      let l = Atomic.get y in
      if n = 1 then if l = 0 then Atomic.set x 1)

(*
   Atomic.final (fun () ->
       Format.printf "tmp=%d x=%d y=%d z=%d\n%!" !tmp (Atomic.get x)
         (Atomic.get y) (Atomic.get z)) *)

let () = Atomic.trace ~record_traces:true test
