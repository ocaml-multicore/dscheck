type t =
  | Start
  | Make
  | Get
  | Set
  | Exchange
  | CompareAndSwap of
      [ `Success | `Fail | `Unknown of unit -> [ `Success | `Fail ] ] ref
  | FetchAndAdd

let to_str x =
  match x with
  | Start -> "start"
  | Make -> "make"
  | Get -> "get"
  | Set -> "set"
  | Exchange -> "exchange"
  | CompareAndSwap _ -> "compare_and_swap"
  | FetchAndAdd -> "fetch_and_add"

let is_write : t -> bool = function
  | Get -> false
  | CompareAndSwap outcome -> (
      match !outcome with
      | `Success -> true 
      | `Fail -> false
      | `Unknown currently_f -> (
          match currently_f () with `Success -> true | `Fail -> false))
  | _ -> true

let weak_cmp t1 t2 = 
  match (t1, t2) with 
  | CompareAndSwap _, CompareAndSwap _ -> true 
  | (CompareAndSwap _, _) | (_, CompareAndSwap _) -> false 
  | _, _ -> t1 = t2 
  
