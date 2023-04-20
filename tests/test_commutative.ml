module Atomic = Dscheck.TracedAtomic

(* get + get *)
let test1 () =
  let a = Atomic.make 0 in
  Atomic.spawn (fun () -> Atomic.get a |> ignore);
  Atomic.spawn (fun () -> Atomic.get a |> ignore);
  Atomic.final (fun () -> Printf.printf ".%!")

(* get + cas fail *)
let test2 () =
  let a = Atomic.make 0 in
  let b = Atomic.make 0 in
  Atomic.spawn (fun () -> Atomic.get a |> ignore);
  
  Atomic.spawn (fun () -> Atomic.compare_and_set a 1 2 |> ignore;
  Atomic.get b |> ignore);

  Atomic.final (fun () -> Printf.printf ".%!")

(* get + cas success *)
let test3 () =
  let a = Atomic.make 0 in
  Atomic.spawn (fun () -> Atomic.get a |> ignore);
  Atomic.spawn (fun () -> Atomic.compare_and_set a 0 1 |> ignore
  
  );
  Atomic.final (fun () -> Printf.printf ".%!")

(* get + cas fail + set  *)
let test4 () =
  let a = Atomic.make 0 in
  Atomic.spawn (fun () -> Atomic.compare_and_set a 2 3 |> ignore);
  Atomic.spawn (fun () -> Atomic.get a |> ignore);
  Atomic.spawn (fun () -> Atomic.set a 1 |> ignore);
  Atomic.final (fun () -> Printf.printf ".%!")

let test5 () =
  let a = Atomic.make 0 in
  Atomic.spawn (fun () -> Atomic.get a |> ignore);
  Atomic.spawn (fun () -> Atomic.set a 2 |> ignore);
  Atomic.spawn (fun () -> Atomic.compare_and_set a 2 3 |> ignore);
  Atomic.final (fun () -> Printf.printf ".%!")

let () =
  let tests = [ test1; test2; test3; test4; test5 ] in
  List.iter
    (fun test ->
      print_string "\n-----test-----\n";
      Atomic.trace ~impl:`Dpor_source test;
      print_string "\n")
    tests
