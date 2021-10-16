open EffectHandlers
open EffectHandlers.Deep

type 'a t = 'a Atomic.t

type _ eff +=
    Make : 'a -> 'a Atomic.t eff
  | Get : 'a Atomic.t -> 'a eff
  | Set : ('a Atomic.t * 'a) -> unit eff
  | Exchange : ('a Atomic.t * 'a) -> 'a eff
  | CompareAndSwap : ('a Atomic.t * 'a * 'a) -> bool eff
  | FetchAndAdd : (int Atomic.t * int) -> int eff


let handler = {
  effc = fun (type a) (e : a eff) ->
    match e with
    | Make v -> Some(fun (k : (a, _) continuation) -> continue k (Atomic.make v))
    | Get v -> Some(fun (k : (a, _) continuation) -> continue k (Atomic.get v))
    | Set (r, v) -> Some(fun (k : (a, _) continuation) -> continue k (Atomic.set r v))
    | Exchange (a, b) -> Some(fun (k : (a, _) continuation) -> continue k (Atomic.exchange a b))
    | CompareAndSwap (x, s, v) -> Some(fun (k : (a, _) continuation) -> continue k (Atomic.compare_and_set x s v))
    | FetchAndAdd (v, x) -> Some(fun (k : (a, _) continuation) -> continue k (Atomic.fetch_and_add v x))
    | _ -> None
}

let trace f =
  try_with f () handler

let make v =
  perform (Make v)

let get r =
  perform (Get r)

let set r v =
  perform (Set (r, v))

let[@inline never] exchange r v =
  perform (Exchange (r, v))

let[@inline never] compare_and_set r seen v =
  perform (CompareAndSwap (r, seen, v))

let[@inline never] fetch_and_add r n =
  perform (FetchAndAdd (r, n))

let incr r = ignore (fetch_and_add r 1)
let decr r = ignore (fetch_and_add r (-1))