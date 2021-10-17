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

type atomic_op = Make | Get | Set | Exchange | CompareAndSwap | FetchAndAdd

type trace_record = { op: atomic_op; repr: Obj.t; prev: trace_record option }

let trace_head = Atomic.make None

let rec add_trace op repr =
  let curr_head = Atomic.get trace_head in
    if not(Atomic.compare_and_set trace_head curr_head (Some { op; repr; prev = curr_head })) then
      add_trace op repr
    else
      ()

let handler = {
  effc = fun (type a) (e : a eff) ->
    match e with
    | Make v -> Some(
      fun (k : (a, _) continuation) ->
        let m = Atomic.make v in
        add_trace Make (Obj.repr m);
        continue k m
      )
    | Get v -> Some(
        fun (k : (a, _) continuation) ->
          add_trace Get (Obj.repr v);
          continue k (Atomic.get v)
        )
    | Set (r, v) -> Some(
        fun (k : (a, _) continuation) ->
          add_trace Set (Obj.repr r);
          continue k (Atomic.set r v)
        )
    | Exchange (a, b) -> Some(
        fun (k : (a, _) continuation) ->
          add_trace Exchange (Obj.repr a);
          continue k (Atomic.exchange a b)
        )
    | CompareAndSwap (x, s, v) -> Some(
        fun (k : (a, _) continuation) ->
          add_trace CompareAndSwap (Obj.repr x);
          continue k (Atomic.compare_and_set x s v)
        )
    | FetchAndAdd (v, x) -> Some(
        fun (k : (a, _) continuation) ->
          add_trace FetchAndAdd (Obj.repr v);
          continue k (Atomic.fetch_and_add v x)
        )
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