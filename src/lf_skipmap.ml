module type OrderedType =
sig
  type t
  val compare: t -> t -> int
  val to_string: t -> string
end

module Make(Ord: OrderedType) = struct
  type 'a ele =
      Min
    | Max
    | Element of { key: Ord.t; data: 'a }

  type 'a marked_ref = { r: 'a ; mark : bool }

  type 'a cell = { ele:'a ele; top_level: int; mutable forward: 'a cell marked_ref Atomic.t array }

  type 'a t = { head: 'a cell; tail: 'a cell; levels: int }

  let make levels =
    let tail = { ele = Max; top_level = levels; forward = [||] } in
    let tail_forward = Array.make levels (Atomic.make ({ r = tail; mark = false })) in
    tail.forward <- tail_forward;
    let head_forward = Array.make levels (Atomic.make ({ r = tail; mark = false })) in
    let head = { ele = Min; top_level = levels; forward = head_forward } in
    { head = head; tail = tail; levels }

  let find sk key preds succs =
    let rec sk_find ~level ~pred ~curr ~succ =
      begin
        let curr_mref = Array.get !pred.forward level |> Atomic.get in
        curr := curr_mref.r;
        let succ_mref = Array.get !curr.forward level |> Atomic.get in
        let is_marked = succ_mref.mark in
        succ := succ_mref.r;
        begin
          if is_marked then begin
            let curr_mref = if curr_mref.mark then { r = curr_mref.r; mark = false } else curr_mref in
            Atomic.compare_and_set (Array.get !pred.forward level) curr_mref { r = !succ; mark = false } |> ignore;
            sk_find ~level ~pred ~curr ~succ
          end else begin
            let curr_ele = !curr.ele in
            match curr_ele with
            | Element({ key = curr_key; _ }) -> begin
                if (Ord.compare curr_key key) < 0 then begin
                  pred := !curr;
                  curr := !succ;
                  sk_find ~level ~pred ~curr ~succ
                end else begin
                  Array.set preds level (Some !pred);
                  Array.set succs level (Some !curr);
                  if level == 0 then
                    curr_key == key
                  else
                    sk_find ~level:(level-1) ~pred ~curr ~succ
                end
              end
            | Max -> begin
                Array.set preds level (Some !pred);
                Array.set succs level (Some !curr);
                if level == 0 then
                  false
                else
                  sk_find ~level:(level-1) ~pred ~curr ~succ
              end
            | Min -> assert(false)
          end
        end
      end
    in sk_find ~level:(sk.levels-1) ~pred:(ref sk.head) ~curr:(ref sk.head) ~succ:(ref sk.head)

  let print_cell cell =
    match cell.ele with
    | Element({ key; _ }) ->
      Printf.printf "Element: %s\n" (Ord.to_string key)
    | Min -> Printf.printf "Min\n"
    | Max -> Printf.printf "Max\n"

  let rec insert sk key data =
    let preds = Array.make sk.levels None in
    let succs = Array.make sk.levels None in
    let found = find sk key preds succs in
    for l = 0 to (sk.levels-1) do
      Printf.printf "%d [\n" l;
      print_cell (Array.get preds l |> Option.get);
      print_cell (Array.get succs l |> Option.get);
      Printf.printf "]\n"
    done;
    Printf.printf "\n";
    if found then
      false
    else begin
      let top_level = Random.int sk.levels in
      Printf.printf "Inserting at %d\n" top_level;
      let forward = Array.init (top_level+1) (fun i -> Atomic.make { r = Array.get succs i |> Option.get; mark = false }) in
      let new_cell = { ele = Element({ key; data }); top_level; forward } in
      let pred = Array.get preds 0 |> Option.get in
      let pred_forward = Array.get pred.forward 0 in
      let pref_forward_mref = Atomic.get pred_forward in
      if not(Atomic.compare_and_set pred_forward pref_forward_mref { r = new_cell; mark = false }) then
        insert sk key data
      else begin
        let rec try_insert level =
          let pred = Array.get preds level |> Option.get in
          let pred_forward = Array.get pred.forward level in
          let pref_forward_mref = Atomic.get pred_forward in
          if not(Atomic.compare_and_set pred_forward pref_forward_mref { r = new_cell; mark = false }) then begin
            find sk key preds succs |> ignore;
            try_insert level
          end in
        for level = 1 to top_level-1 do
          try_insert level
        done;
        true
      end
    end

  let lookup sk lookup_key =
    Printf.printf "Lookup\n";
    let pred = ref sk.head in
    let curr = ref sk.head in
    let succ = ref sk.head in
    let level = ref (sk.levels-1) in
    while !level >= 0 do
      Printf.printf "Lookup at level %d\n" !level;
      let curr_mref = Array.get !pred.forward !level |> Atomic.get in
      curr := curr_mref.r;
      print_cell !curr;
      Printf.printf "Getting level: %d. Len: %d\n" !level (Array.length !curr.forward);
      let succ_mref = Array.get !curr.forward !level |> Atomic.get in
      let is_marked = ref succ_mref.mark in
      succ := succ_mref.r;
      while !is_marked do
        curr := !succ;
        let succ_mref = Array.get !curr.forward !level |> Atomic.get in
        is_marked := succ_mref.mark;
        succ := succ_mref.r;
      done;
      let curr_ele = !curr.ele in
            match curr_ele with
            | Element({ key = curr_key; _ }) -> begin
                if (Ord.compare curr_key lookup_key) < 0 then begin
                  pred := !curr;
                  curr := !succ
                end else
                  decr level
                end
            | Max -> begin
                pred := !curr;
                curr := !succ;
                decr level
            end
            | Min -> assert(false)
    done;

    let curr_ele = !curr.ele in
    match curr_ele with
    | Element({ key; data }) -> begin
      if (Ord.compare key lookup_key) == 0 then
        Some(data)
      else
        None
    end
    | Max | Min -> None
end