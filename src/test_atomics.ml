module Lfsm = Lf_skipmap.Make(struct type t = int
    let compare = Int.compare
    let to_string = string_of_int
  end)

let () =
  for _ = 0 to 99 do
    let num_nodes = 1000 in
    let sm = Lfsm.make 12 in
    Printf.printf "inserting..\n";
    for i = 0 to num_nodes do
      begin
        assert(Lfsm.insert sm i (string_of_int i));
      end
    done;
    Printf.printf "finding..\n";
    for _ = 1 to 200 do
      for i = num_nodes downto 0 do
        Lfsm.find sm i |> ignore
      done
    done;
    Printf.printf "removing..\n";
    for i = 0 to num_nodes do
      assert(Lfsm.remove sm i)
    done;
  done