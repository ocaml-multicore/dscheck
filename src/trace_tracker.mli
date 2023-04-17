type t

val add_trace : (int * 'a * int option) list -> unit
val clear_traces : unit -> unit
val get_traces : unit -> t
val print_traces : out_channel -> unit
val print : t -> out_channel -> unit
val equal : t -> t -> bool
val get_deps_str : t -> string
val subset : t -> t -> bool
