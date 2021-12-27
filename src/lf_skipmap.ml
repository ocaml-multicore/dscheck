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
    let head_forward = Array.init levels (fun _ -> Atomic.make ({ r = tail; mark = false })) in
    let head = { ele = Min; top_level = levels; forward = head_forward } in
    { head = head; tail = tail; levels }

  let internal_find sk key preds succs =
    let rec sk_find ~level ~pred ~curr ~succ =
      Printf.printf "sk_find key:%s level:%d\n" (Ord.to_string key) level;
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

  let rec insert sk key data =
    let preds = Array.make sk.levels None in
    let succs = Array.make sk.levels None in
    let found = internal_find sk key preds succs in
    if found then
      begin
        false
      end
    else begin
      let top_level = Random.int sk.levels in
      let forward = Array.init (top_level+1)  (fun i -> Atomic.make { r = Array.get succs i |> Option.get; mark = false }) in
      let new_cell = { ele = Element({ key; data }); top_level; forward } in
      let pred = Array.get preds 0 |> Option.get in
      let pred_forward = Array.get pred.forward 0 in
      let pred_forward_mref = Atomic.get pred_forward in
      if not(Atomic.compare_and_set pred_forward pred_forward_mref { r = new_cell; mark = false }) then
        insert sk key data
      else begin
        let rec try_insert level =
          let pred = Array.get preds level |> Option.get in
          let pred_forward = Array.get pred.forward level in
          let pred_forward_mref = Atomic.get pred_forward in
          if not(Atomic.compare_and_set pred_forward pred_forward_mref { r = new_cell; mark = false }) then begin
            internal_find sk key preds succs |> ignore;
            try_insert level
          end in
        for level = 1 to top_level-1 do
          try_insert level
        done;
        true
      end
    end

  let find sk lookup_key =
    let pred = ref sk.head in
    let curr = ref sk.head in
    let succ = ref sk.head in
    let level = ref (sk.levels-1) in
    while !level >= 0 do
      let curr_mref = Array.get !pred.forward !level |> Atomic.get in
      curr := curr_mref.r;
      let succ_mref = Array.get !curr.forward !level |> Atomic.get in
      let is_marked = ref succ_mref.mark in
      succ := succ_mref.r;
      while !is_marked do
        curr := !succ;
        let succ_mref = Array.get !curr.forward !level |> Atomic.get in
        is_marked := succ_mref.mark;
        succ := succ_mref.r;
      done;
      match !curr.ele with
      | Element({ key = curr_key; _ }) -> begin
          let comp = (Ord.compare curr_key lookup_key) in
          if comp < 0 then begin
            pred := !curr;
            curr := !succ
          end else decr level end
      | Max -> decr level
      | Min -> assert(false)
    done;

    let curr_ele = !curr.ele in
    match curr_ele with
    | Element({ key; data }) -> begin
        if (Ord.compare key lookup_key) == 0 then
          data
        else
          raise Not_found
      end
    | Max | Min -> raise Not_found

  let find_opt sk lookup_key =
    try
      Some(find sk lookup_key)
    with
      Not_found -> None

  let remove sk key =
    let preds = Array.make sk.levels None in
    let succs = Array.make sk.levels None in
    let found = internal_find sk key preds succs in
    if not found then
      false
    else begin
      let to_remove = Array.get succs 0 |> Option.get in
      for level = to_remove.top_level-1 downto 1 do
        let rec try_mark () =
          let atomic_mref = Array.get to_remove.forward level in
          let mref = Atomic.get atomic_mref in
          let succ = mref.r in
          let marked = mref.mark in
          if marked then begin
            if not(Atomic.compare_and_set atomic_mref mref { r = succ ; mark = true }) then
              try_mark ()
            else
              ()
          end
        in try_mark ()
      done;
      let rec try_lowest () =
        let atomic_mref = Array.get to_remove.forward 0 in
        let mref = Atomic.get atomic_mref in
        let succ = mref.r in
        let marked = mref.mark in
        let mark_success = Atomic.compare_and_set atomic_mref mref { r = succ ; mark = true } in
        if mark_success then
          begin
            internal_find sk key preds succs |> ignore;
            true
          end
        else if marked then
          false
        else
          try_lowest ()
      in
      try_lowest ()
    end
end