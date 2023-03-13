(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Stephen Dolan, University of Cambridge                     *)
(*             Gabriel Scherer, projet Partout, INRIA Paris-Saclay        *)
(*                                                                        *)
(*   Copyright 2020 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type !'a t
(** An atomic (mutable) reference to a value of type ['a]. *)

val make : 'a -> 'a t
(** Create an atomic reference. *)

val get : 'a t -> 'a
(** Get the current value of the atomic reference. *)

val set : 'a t -> 'a -> unit
(** Set a new value for the atomic reference. *)

val exchange : 'a t -> 'a -> 'a
(** Set a new value for the atomic reference, and return the current value. *)

val compare_and_set : 'a t -> 'a -> 'a -> bool
(** [compare_and_set r seen v] sets the new value of [r] to [v] only
    if its current value is physically equal to [seen] -- the
    comparison and the set occur atomically. Returns [true] if the
    comparison succeeded (so the set happened) and [false]
    otherwise. *)

val fetch_and_add : int t -> int -> int
(** [fetch_and_add r n] atomically increments the value of [r] by [n],
    and returns the current value (before the increment). *)

val incr : int t -> unit
(** [incr r] atomically increments the value of [r] by [1]. *)

val decr : int t -> unit
(** [decr r] atomically decrements the value of [r] by [1]. *)

val trace : (unit -> unit) -> unit
(** start the simulation trace *)

val spawn : (unit -> unit) -> unit
(** spawn [f] as a new 'thread' *)

val check : (unit -> bool) -> unit
(** if [f] returns false then an assertion has failed *)

val final : (unit -> unit) -> unit
(** run [f] after all processes complete *)

val every : (unit -> unit) -> unit
(** run [f] between every possible interleaving *)

val num_runs : unit -> int
(** [num_runs ()] returns number of states explored in the last run *)

val num_traces : unit -> int
(** [num_traces ()] returns number of traces validated in the last run *)
