module type EDGE = sig
  type t
  val compare : t -> t -> int
  val to_string : t -> string
end

module Make (Edge : EDGE) = struct

  module M = Map.Make (Edge)

  type edge = Edge.t

  type t =
    { error : bool
    ; is_todo : bool
    ; todo : int option
    ; nb : int
    ; nb_ok : int
    ; s : s
    }

  and s =
    | Ok
    | Skip
    | Todo
    | Error of string
    | Branch of t M.t

  let min_depth t = t.todo
  let nb_todos t = t.nb
  let nb_oks t = t.nb_ok
  let has_error t = t.error

  let min_todo a b = match a, b with
    | None, t | t, None -> t
    | Some a, Some b -> Some (min a b)

  let incr_todo = function None -> None | Some t -> Some (t + 1)

  let ok nb_ok = { error = false ; is_todo = false ; todo = None ; nb = 0 ; nb_ok ; s = Ok }
  let skip = { (ok 0) with s = Skip }
  let error msg = { (ok 0) with error = true ; s = Error msg }
  let todo = { error = false ; is_todo = true ; todo = Some 0 ; nb = 1 ; nb_ok = 0 ; s = Todo }

  let branch children =
    let error, is_todo, todo, nb, nb_ok =
      M.fold
        (fun _ child (acc_error, acc_is_todo, acc_todo, acc_nb, acc_nb_ok) ->
          child.error || acc_error,
          (child.s = Skip || child.is_todo) && acc_is_todo,
          min_todo child.todo acc_todo,
          acc_nb + child.nb,
          acc_nb_ok + child.nb_ok)
        children
        (false, true, None, 0, 0)
    in
    { error ; is_todo ; todo = incr_todo todo ; nb ; nb_ok ; s = Branch children }

  let simplify t =
    match t.s with
    | _ when t.error || t.todo <> None -> t
    | Branch children ->
        let ok_children, has_skip = ref 0, ref false in
        M.iter
          (fun _ c -> match c.s with
            | Ok -> incr ok_children
            | Skip -> has_skip := true
            | Branch _ ->
                assert (c.todo = None) ;
                assert (c.error = false) ;
                assert (c.nb_ok > 0)
            | _ -> assert false)
          children ;
        if !ok_children = 1 && !has_skip
        then t
        else ok t.nb_ok
    | _ -> t

  let add edge child t =
    match t.s with
    | Ok | Skip | Error _ -> assert false
    | Todo -> branch (M.singleton edge child)
    | Branch m -> branch (M.add edge child m)

  let todolist ~skip_edge edges =
    List.fold_right
      (fun edge t -> branch (M.add skip_edge skip (M.singleton edge t)))
      edges
      todo

  let insert_todos ~skip_edge todos t =
    match t.s, todos with
    | Todo, []
    | Skip, _
    | (Ok | Error _), _
    | Branch _, [] ->
        t
    | Todo, todos -> todolist ~skip_edge todos
    | Branch m, edge::todos ->
        if M.mem edge m
        then t
        else begin
          let t = todolist ~skip_edge todos in
          let m = M.add edge t m in
          assert (M.mem skip_edge m) ;
          branch m
        end

  let next t = match t.todo, t.s with
    | None, _ -> None
    | Some _, Todo -> None
    | Some todo_distance, Branch m ->
        let candidates =
          M.fold
            (fun edge child acc ->
              match child.todo with
              | Some distance (* when distance = todo_distance - 1 *) ->
                  let weight = 1.0 /. float (100 * child.nb + distance - todo_distance + 1) in
                  (weight, edge, child) :: acc
              | _ -> acc)
            m
            []
        in
        let candidates =
          if not t.error
          then candidates
          else match List.filter (fun (_, _, child) -> child.error || child.is_todo) candidates with
          | [] -> candidates
          | cs -> cs
        in
        begin match candidates with
        | [(_, edge, child)] -> Some (edge, child)
        | _ ->
          let total = List.fold_left (fun acc (w, _, _) -> acc +. w) 0.0 candidates in
          let selection = Random.float total in
          let rec go selection = function
            | [] -> failwith "Trie: candidate not found"
            | [(_, edge, child)] -> Some (edge, child)
            | (weight, edge, child) :: _ when selection <= weight -> Some (edge, child)
            | (weight, _, _) :: rest -> go (selection -. weight) rest
          in
          go selection candidates
        end
    | _ -> assert false


  let graphviz ~filename t =
    let h = open_out filename in
    let gen = ref 0 in
    let fresh () =
      let u = !gen in
      gen := u + 1 ;
      u
    in
    let refcounts = Hashtbl.create 16 in
    let refcount uid =
      match Hashtbl.find_opt refcounts uid with
      | None -> 0
      | Some count -> count
    in
    let incr_refcount uid =
      let rc = refcount uid + 1 in
      assert (rc >= 1) ;
      Hashtbl.replace refcounts uid rc
    in
    let decr_refcount uid =
      let rc = refcount uid - 1 in
      assert (rc >= 1) ;
      Hashtbl.replace refcounts uid rc
    in
    let cache = Hashtbl.create 16 in
    let id_magic = Char.chr 0 in
    let rec go ~print ~rc t =
      let s = t.s in
      let label =
        match s with
        | Ok -> "Ok"
        | Skip -> "Skip"
        | Todo -> "TODO"
        | Error msg -> "ERROR: " ^ msg
        | Branch _ -> ""
      in
      let has_error =
        if t.error
        then "style=filled fillcolor=black fontcolor=white"
        else if t.todo = None
        then "style=filled fillcolor=green"
        else if t.is_todo
        then "style=filled fillcolor=grey"
        else ""
      in
      let style = if label = "" then "shape=point" else Printf.sprintf "label=%S" label in
      let buf = Buffer.create 16 in
      Printf.bprintf buf "%c [%s %s] ;\n" id_magic style has_error ;
      let ids_children = ref [] in
      begin match s with
      | _ when not t.error -> ()
      | Branch children ->
          M.iter
            (fun edge child ->
              if child.s = Skip
              then ()
              else begin
                let id_edge = edge_to_string ~print ~rc edge child in
                let arrow_weight =
                  if t.error && child.error
                  then "[weight=1000 penwidth=4] "
                  else ""
                in
                Printf.bprintf buf "%c -> e%i %s ;\n" id_magic id_edge arrow_weight ;
                ids_children := id_edge :: !ids_children ;
              end
            )
            children
      | _ -> ()
      end ;
      let self = Buffer.contents buf in
      match Hashtbl.find cache self with
      | (uid, printed) ->
          if not print
          then (if rc then (incr_refcount uid ; List.iter decr_refcount !ids_children))
          else if not printed
          then begin
            Hashtbl.replace cache self (uid, true) ;
            let self = String.concat ("n" ^ string_of_int uid) (String.split_on_char id_magic self) in
            Printf.fprintf h "%s" self ;
          end ;
          uid
      | exception Not_found ->
          let uid = fresh () in
          Hashtbl.add cache self (uid, print) ;
          if not print
          then (if rc then incr_refcount uid)
          else begin
            let self = String.concat ("n" ^ string_of_int uid) (String.split_on_char id_magic self) in
            Printf.fprintf h "%s" self ;
          end ;
          uid

    and skip_forward acc t =
      if refcount (go ~print:false ~rc:false t) > 1
      then List.rev acc, t
      else
      let skip = match t.s with
        | Branch children ->
            M.fold
              (fun edge child acc -> if child.s = Skip then acc else (edge, child) :: acc)
              children
              []
        | _ -> []
      in
      match skip with
      | [edge, single] -> skip_forward (edge :: acc) single
      | _ -> (List.rev acc, t)

    and edge_to_string ~print ~rc edge child =
      let edges, child =
        if print
        then skip_forward [edge] child
        else [edge], child
      in
      let buf = Buffer.create 16 in
      let label = String.concat "\\l" (List.map Edge.to_string edges) in
      let has_error =
        if child.error
        then "style=filled fillcolor=black fontcolor=white"
        else if child.todo = None
        then "style=filled fillcolor=lightgreen"
        else if child.is_todo
        then "style=filled fillcolor=grey"
        else ""
      in
      let arrow_weight =
        if child.error
        then "[weight=1000 penwidth=4] "
        else ""
      in
      Printf.bprintf buf "%c [shape=rectangle label=\"%s\\l\" %s] ;\n" id_magic label has_error ;
      let id_child =
        if not child.error
        then begin
          if child.nb > 1 then begin
            Printf.bprintf buf "%c_todo [label=%S fillcolor=grey style=filled];\n"
              id_magic
              (Printf.sprintf "TODO %#i" child.nb) ;
            Printf.bprintf buf "%c -> %c_todo;\n" id_magic id_magic ;
          end ;
          if child.nb_ok > 0 then begin
            Printf.bprintf buf "%c_ok [label=%S fillcolor=lightgreen style=filled];\n"
              id_magic
              (Printf.sprintf "OK %#i" child.nb_ok) ;
            Printf.bprintf buf "%c -> %c_ok;\n" id_magic id_magic ;
          end ;
          None
        end
        else begin
          let id_child = go ~print ~rc child in
          Printf.bprintf buf "%c -> n%i %s;\n" id_magic id_child arrow_weight ;
          Some id_child
        end
      in
      let self = Buffer.contents buf in
      match Hashtbl.find cache self with
      | (uid, printed) ->
          if not print
          then (if rc then ( incr_refcount uid ; Option.iter decr_refcount id_child ) )
          else if not printed
          then begin
            Hashtbl.replace cache self (uid, true) ;
            let self = String.concat ("e" ^ string_of_int uid) (String.split_on_char id_magic self) in
            Printf.fprintf h "%s" self ;
          end ;
          uid
      | exception Not_found ->
          let uid = fresh () in
          Hashtbl.add cache self (uid, print) ;
          if not print
          then (if rc then incr_refcount uid)
          else begin
            let self = String.concat ("e" ^ string_of_int uid) (String.split_on_char id_magic self) in
            Printf.fprintf h "%s" self ;
          end ;
          uid
    in
    Printf.fprintf h "digraph dscheck {\n" ;
    Printf.fprintf h "node [fontname=%S] ;\n" "Courier New" ;
    let _ : int = go ~print:false ~rc:true t in
    let _ : int = go ~print:true ~rc:false t in
    Printf.fprintf h "}\n" ;
    close_out h
end
