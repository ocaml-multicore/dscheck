(*
 * Copyright (c) 2015, Théo Laurent <theo.laurent@ens.fr>
 * Copyright (c) 2015, KC Sivaramakrishnan <sk826@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(* Michael-Scott queue copied over from [lockfree](github.com/ocaml-multicore/lockfree) *)
module Atomic = Dscheck.TracedAtomic

type 'a node = Nil | Next of 'a * 'a node Atomic.t
type 'a t = { head : 'a node Atomic.t; tail : 'a node Atomic.t }

let create () =
  let head = Next (Obj.magic (), Atomic.make Nil) in
  { head = Atomic.make head; tail = Atomic.make head }

let is_empty q =
  match Atomic.get q.head with
  | Nil -> failwith "MSQueue.is_empty: impossible"
  | Next (_, x) -> ( match Atomic.get x with Nil -> true | _ -> false)

let pop q =
  let rec loop () =
    let s = Atomic.get q.head in
    let nhead =
      match s with
      | Nil -> failwith "MSQueue.pop: impossible"
      | Next (_, x) -> Atomic.get x
    in
    match nhead with
    | Nil -> None
    | Next (v, _) when Atomic.compare_and_set q.head s nhead -> Some v
    | _ -> loop ()
  in
  loop ()

let push q v =
  let rec find_tail_and_enq curr_end node =
    if Atomic.compare_and_set curr_end Nil node then ()
    else
      match Atomic.get curr_end with
      | Nil -> find_tail_and_enq curr_end node
      | Next (_, n) -> find_tail_and_enq n node
  in
  let newnode = Next (v, Atomic.make Nil) in
  let tail = Atomic.get q.tail in
  match tail with
  | Nil -> failwith "HW_MSQueue.push: impossible"
  | Next (_, n) ->
      find_tail_and_enq n newnode;
      ignore (Atomic.compare_and_set q.tail tail newnode)
