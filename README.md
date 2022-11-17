# `dscheck` — tool for testing concurrent OCaml programs 

Experimental model checker for testing concurrent algorithms based on [Dynamic Partial-Order Reduction for Model Checking Software](https://users.soe.ucsc.edu/~cormac/papers/popl05.pdf). At the core, `dscheck` runs each test numerous times, exhaustively trying every single interleaving of atomic operations. User-provided assertions are run at the end of every execution. 


# Usage

Lockfree can be installed from `opam`: `opam install dscheck`. Sample usage on a [naive counter](tests/test_naive_counter.ml) is shown below. 

```ocaml
module Atomic = Dscheck.TracedAtomic
(* tested implementation needs to use dscheck's atomic module *)

let test_counter () =
  let counter = Atomic.make 0 in
  let incr () = Atomic.set counter (Atomic.get counter + 1) in 
  Atomic.spawn incr;
  Atomic.spawn incr;
  Atomic.final (fun () -> Atomic.check (fun () -> Atomic.get counter == 2))
```
 
This is a classic example of a race condition with two threads trying to increment a counter without synchronisation. When the race occurs, one of the updates is lost. `dscheck` finds the interleaving that leads to that: 

```
Found assertion violation at run 12:
Process 0: start 
Process 0: get 1
Process 1: start 
Process 1: get 1
Process 0: set 1
Process 1: set 1
Fatal error: exception File "src/tracedAtomic.ml", line 266, characters 6-12: Assertion failed
```


# Contributions

Contributions are appreciated! Please create issues/PRs to this repo.

