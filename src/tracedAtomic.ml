type !'a t

external make : 'a -> 'a t = "trace_make"
external get : 'a t -> 'a = "trace_get"
external set : 'a t -> 'a -> unit = "trace_set"
external exchange : 'a t -> 'a -> 'a = "trace_exchange"
external compare_and_set : 'a t -> 'a -> 'a -> bool = "trace_cas"
external fetch_and_add : 'a t -> 'a -> 'a = "trace_fadd"

let incr r = ignore (fetch_and_add r 1)
let decr r = ignore (fetch_and_add r (-1))