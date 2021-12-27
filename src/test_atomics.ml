module Lfsm = Lf_skipmap.Make(struct type t = int
let compare = Int.compare
let to_string = string_of_int
end)

let () =
  let sm = Lfsm.make 12 in
    for i = 0 to 1000 do
      begin
        assert(Lfsm.insert sm i (string_of_int i));
      end
    done;
    for _ = 1 to 100000 do
      for i = 1000 downto 0 do
        Lfsm.find sm i |> ignore
      done
    done