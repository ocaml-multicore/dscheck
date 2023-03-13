module Atomic = Dscheck.TracedAtomic

let drain queue =
  let remaining = ref 0 in
  while not (Michael_scott_queue.is_empty queue) do
    remaining := !remaining + 1;
    assert (Option.is_some (Michael_scott_queue.pop queue))
  done;
  !remaining

let producer_consumer () =
  Atomic.trace (fun () ->
      let queue = Michael_scott_queue.create () in
      let items_total = 4 in

      Atomic.spawn (fun () ->
          for _ = 1 to items_total do
            Michael_scott_queue.push queue 0
          done);

      (* consumer *)
      let popped = ref 0 in
      Atomic.spawn (fun () ->
          for _ = 1 to items_total do
            match Michael_scott_queue.pop queue with
            | None -> ()
            | Some _ -> popped := !popped + 1
          done);

      (* checks*)
      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let remaining = drain queue in
              !popped + remaining = items_total)))

let () =
  let open Alcotest in
  run "michael_scott_queue_dscheck"
    [ ("basic", [ test_case "1-producer-1-consumer" `Slow producer_consumer ]) ]
