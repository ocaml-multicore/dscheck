# DSCheck — tool for testing concurrent OCaml programs

Experimental model checker for testing concurrent programs. DSCheck explores
interleavings of a user-provided program and helps ensure that its invariants
are maintained regardless of scheduling decisions.

# Contents

1. [Motivation](#motivation)
2. [Get DSCheck](#get-dscheck)
3. [Usage](#usage)
4. [Development](#development)
5. [Contributions](#contributions)
6. [References](#references)

# Motivation

*Note: This README assumes the optimizations in [Granular Dependency
relation](https://github.com/ocaml-multicore/dscheck/pull/22) are in. It's in
the process of being reviwed soon. Until then you can pin your installation to
`granular-dependency-relation` to use it.*

As experience shows, fine-grained concurrency is notoriously challenging to get
right.

- As the program grows, the number of possible interleavings increases
  exponentially and quickly becomes too large for a human to reasonably
  validate. That's exacerbated by the fact that different interleavings often
  lead to the right outcome for different reasons.

- Certain concurrency bugs manifest rarely and are borderline impossible to
  reproduce. They may occur under specific system conditions only and disappear
  when a debugging system is attached.

DSCheck helps manage this complexity by letting us instrument a multicore test
to explore relevant interleavings. Thus ensuring that all terminal states are
valid and no edge cases have been missed.

# Get DSCheck

Dscheck can be installed from `opam`: `opam install dscheck`.

# Usage

Sample usage on [naive counter](tests/test_naive_counter.ml) is shown below.

```ocaml
module Atomic = Dscheck.TracedAtomic
(* the test needs to use DSCheck's atomic module *)

let test_counter () =
  let counter = Atomic.make 0 in
  let incr () = Atomic.set counter (Atomic.get counter + 1) in
  Atomic.spawn incr;
  Atomic.spawn incr;
  Atomic.final (fun () -> Atomic.check (fun () -> Atomic.get counter == 2))
```

The test spawns two domains (`Atomic.spawn`), each trying to increase the
counter. The assertion at the end validates that counter has the expected value
(`Atomic.final`). This is a classic example of a race condition with two threads
trying to perform read-modify-write operation without synchronisation. In
effect, there is a risk of losing one of the updates. DSCheck finds and reports
the offending interleaving to the user:

```
Found assertion violation at run 2:

sequence 2
----------------------------------------
P0                      P1
----------------------------------------
start
get a
                        start
                        get a
set a
                        set a
----------------------------------------
```

## Validation Soundness

For model-checking to be sound, tested program must meet the following
conditions:

- Determinism. Otherwise DSCheck may encounter errors or (more dangerously)
  terminate successfuly without exploring all traces.
- Tested programs cannot have races between non-atomic variables. DSCheck does
  not explore different _behaviours_, (e.g. a non-atomic read may see the most
  recently written value or a number of stale ones).
- Domains can communicate through atomic variables only. Validation including
  higher-level synchronisation primitives is possible but constitutes future
  work.
- Tested programs have to be at least lock-free. If any thread cannot finish on
  its own, DSCheck will explore its transitions ad infinitum. As some remedy,
  the space of traces can be covered partially by forcing the test to be
  lock-free. For example, spinlock can be modified to fail explicitely once some
  artifical limit is reached.

## Validation Logic

As highlighted in the [Motivation](#motivation), the number of interleavings
tends to grow exponentialy with size of the program and the number of threads.
It follows that the interleavings of even small programs are not just impossible
for humans to walk through but also incomputable in reasonable time.

The key advance that made DSCheck-like model-checkers possible is the emergence
of dynamic partial-order reduction (DPOR) methods. The application to
model-checking stems from the observation that in real-world programs many
interleavings are equivalent and if at least one is covered, so is the entire
equivalence class. More formally, a particular interleaving is a total order
induced by a causal relation between events of different domains
(partial-order). DSCheck aims to cover exactly one interleaving per trace.

While the DPOR algorithms and formalism tend to be quite involved, the emergent
behavior is intuitive. Consider the following program:

```ocaml
let a = Atomic.make 0 in
let b = Atomic.make 0 in

(* Domain P *)
Domain.spawn (fun () ->
    Atomic.set a 1;
    Atomic.set b 1;
    ) |> ignore;

(* Domain Q *)
Domain.spawn (fun () ->
    Atomic.set a 2;
    ) |> ignore
```

There are three possible interleavings: `P.P.Q`, `P.Q.P`, `Q.P.P`. Clearly, the
ordering between _Q_ and the second step of _P_ does not matter. Thus the
execution sequences `P.P.Q` and `P.Q.P` are different realizations of the same
trace.

DPOR skips the redundant execution sequences and provides an exponential
improvement over the naive search. That in turn significantly expands the
universe of checkable programs and makes this enumeration useful.

### Reads

The leading example showcases reduction of search space based on accesses to
disjoint locations. A similar approach can be taken for accesses on overlapping
locations that do not conflict. If _P_ and _Q_ had only read memory, there would
have been no race between them, in turn requiring DSCheck to explore only a
single interleaving.

```ocaml
let a = Atomic.make 0 in

(* P *)
Atomic.spawn (fun () ->
    ignore (Atomic.compare_and_set a 1 2));

(* Q *)
Atomic.spawn (fun () ->
    ignore (Atomic.compare_and_set a 2 3));
```

Compare and set is a read-write operation. In this particular case, however,
both CASes fail and leave the initial value untouched. DSCheck recognizes such
special cases and avoids exploration of redundant interleavings.

### Causal Ordering

```ocaml
let a = Atomic.make 0 in
let b = Atomic.make 0 in

Atomic.spawn (fun () ->
    Atomic.set a 1; (* P *)
    Atomic.set b 1; (* Q *));

Atomic.spawn (fun () ->
    Atomic.set a 2;
    Atomic.set b 2);
```

In more general sense, DSCheck tracks causal order between events of different
domains and tries to schedule sequences reversing it, where possible. At times,
it may be counterintuitive. In the example above DSCheck explores 4
interleavings. What if we swap the lines `P` and `Q`?

# Development

## Design Notes

DSCheck sees test as a graph, where edge is a step in execution of one of the
active domains. The validation involves traversing the graph in a depth-first
fashion and running user assertions in the leaf states. For example, the graph
for [Example Reads](#reads) looks as follows:

```
Start (a=0) ---> P: CompareAndSet a 1 2 ---> Q: CompareAndSet a 2 3
|                                                          |
\/                                                         \/
Q: CompareAndSet a 2 3 ---> P: CompareAndSet a 1 2 ---> Termination (a=0)
```

DSCheck begins by running the main function of the test, which registers domains
P and Q. Then, the exploration starts by taking a step into execution of either
domains and follows with one step of the other, thus arriving at the terminal
state. In the case above both paths are equivalent (hence shared leaf node), but
it naturally does not have to be the case, e.g. variable `a` could be
initialized with `1` making both paths unique traces. Consider the following.

```
Start (a=1) --> P: CompareAndSet a 1 2 --> Q: CompareAndSet a 2 3
|                                                       |
|                                                       \/
|                                                       Termination (a=3)
\/
Q: CompareAndSet a 2 3 ---> P: CompareAndSet a 1 2 ---> Termination (a=2)
```

_P_ and _Q_ are dependent operations and the two interleavings lead to different
outcomes. Thus DSCheck has to explore both. Skimming over the details, DSCheck
begins by exploring the first branch, `P.Q`, to the end and schedules new
transitions on the way there. Here, it will notice that _P_ and _Q_ are
potentially racing operations (since it's a read-write pair on the same
location) and schedule transition _Q_ after start. We call that a _race
reversal_.

The DFS exploration may look tricky at first. The key idea to realize is that at
any step in the sequence, model-checker aims to explore all the traces produced
by remaining events. For some events _c_-_z_ and execution sequence `x.c.w`,
DSCheck has to explore all traces produced by the remaining events. If _g_ and
_h_ are dependent and _A_, _B_ some sequences, it has to explore at least
`x.c.w.A.g.h.B` and `x.c.w.A.h.g.B` (or equivalent). It does so by choosing a
random path and continuously scheduling sequences reversing encountered races.

The key optimization techniques identify transitions leading to sequences, which
are equal to some already explored ones.

- Persistent/source sets. A DPOR algorithm has to execute at least all the
  transition in the source set at a particular state to ensure that all revelant
  interleavings are explored. Once such a set has been explored, there's no need
  to explore any other transitions from that state. See
  [Comparing Source Sets and Persistent Sets for Partial Order Reduction](https://user.it.uu.se/~bengt/Papers/Full/kimfest17.pdf).
- Sleep sets. At times, we can suspend exploration of a transition until a
  relevant event occurs. For example, if _x_ and _c_ are independent and we have
  explored `x.c`, there's no need to explore `c.x` unless some other event
  (dependent with _x_) occurs.

## Testing

The formalism underpinning DPOR leads to a fairly straightforward testing setup.
For any two execution sentences, they belong to the same trace if reordering of
commutative operations leads from one to the other. For example, operations
`P:(read a), Q:(read b)` clearly commute (since their reordering leads to the
same outcome), while `P:(write a), Q:(write a)` may not. Thus, since one
sequence can be transformed into another, they are realizations of the same
trace.

Whenever DSCheck explores multiple sequences for a single trace, it constitutes
an inefficiency. Conversely, if a change to the DPOR logic leaves some traces
without unexplored, it is incorrect. Note, the assignment of execution sequences
to traces uses the definition of dependency relation. If the change improves
dependency relation rather than DPOR, we would expect to see new pairs of
equivalent execution sequences and thus groups of multiple traces collapsing
into one.

To facilitate the testing, DSCheck includes a random test generator and a trace
deduplication mechanism. For any proposed change, we can generate a large number
of tests and ensure that the same traces have been explored. Furthermore, if
reference implementation is suspicious itself or too inefficient, the proposed
change can be asserted to explore a superset of traces explored by a random
search.

The trace deduplication mechanism took a few iterations to get right. Generally,
the approach involving extracting traces (happens-before) from sequences and
comparing those turned out to be more robust than the attempts to bring the
execution sequences into some normal form and compare directly.

## Literature Glossary

Literature defines a lot of new term. While the rigour is important for
implementation, here's brief explanation in the context of DSCheck.

- Event. Modification of shared state or communication between threads.
- Transition. One step forward into execution of a particular domain. That
  includes the atomic operation it suspended on and all non-atomic operation
  precedent the next atomic call. In the case of DSCheck one transition is a
  single event.
- Execution sequence. A particular interleaving of a program.
- Trace. A class of equivalent execution sequences. An optimal DPOR explores
  exactly one execution sequence per trace.
- Dependency relationship. A binary relation from two transitions to boolean
  indicating whether events are dependent. If two adjacent events are
  independent, then they commute, i.e. swapping two adjacent independent events
  produces a new execution sequence that constitutes the same trace. Thus, DPOR
  focuses on reordering pairs of dependent events.
- Happens-before relationship. A superset of dependency relationship, which
  includes program order.
- Reversible race. Two events executed by different domains, which are
  hb-related directly and not transitively. The latter lets us avoid some
  redundant exploration.
- Maximal trace. Trace that terminates all domains.
- Enabling/disabling transitions. Some transitions may enable or disable
  transitions in other domains, e.g. domain A taking a lock renders any other
  acquisition attempts disabled. Currently not implemented but worth mentioning
  as it is often used in the literature.

## Future Work

- DSCheck was written with validation of lock-free structures in mind and
  handles single-word atomic operations only. There is a wealth of other
  thread-safe communication and synchroniation methods and, in principle, we
  should be able to validate all of them.
  - Non-atomic accesses. OCaml's memory model gives a precise semantics to
    concurrent non-atomic accesses. These could be verified with DSCheck as
    well. The key part seems to be the possibility of reading a stale value.
    Thus, DSCheck should maintain the list of values that may be read from any
    non-atomic location and ensure that program works in all cases. See
    [CDSCHECKER: Checking Concurrent Data Structures Written with C/C++ Atomics](http://plrg.eecs.uci.edu/publications/c11modelcheck.pdf)
    for more details.
  - High-level primitives, e.g. lock, channel, join. Currently, DSCheck cannot
    terminate on any program weaker than lock-free. Blocking primitives need
    explicit support. Section 5
    [Source Sets: A Foundation for Optimal Dynamic Partial Order Reduction](https://user.it.uu.se/~parosh/publications/papers/jacm17.pdf)
    includes a modification of Source- and Optimal-DPOR allowing blocking
    operations.
  - Kcas, to validate programs using
    [kcas](https://github.com/ocaml-multicore/kcas) efficiently. That fits into
    Source-DPOR and the existing implementation quite naturally as operations
    with multiple happens-before and happens-after dependencies.
- Further performance improvements. In particular implementation of wake-up
  trees to eliminate sleep-set blocking or a leap towards
  [TruST](https://plv.mpi-sws.org/genmc/popl2022-trust.pdf).
- Support nested domain spawns and concurrent logic in the main test function.
  DSCheck lets us spawn n domains in the main test function and validate their
  interleavings, which is enough to test lock-free algorithms, but many
  real-world programs are more complicated.

## Reference Implementations

- [CDSChecker](https://github.com/computersforpeace/model-checker) for the
  original DPOR implementation.
- [Nidhugg](https://github.com/nidhugg/nidhugg/) for Source-DPOR.

### Nidhugg

Nidhugg may come helpful for troubleshooting DSCheck. It's based on the same
publication and, although aimed at C/C++ programs, it does have sequential
consistency mode as well. Install it as per instructions in the repository.

Consider the following program. It spawns two threads, each trying to increment
`a` with CAS. Note making the variable `zero` local reduces accesses to shared
memory and lowers the amount of noise.

```C
#include <pthread.h>
#include <stdatomic.h>

atomic_int a;

static void *t(void *arg)
{
  int zero = 0;
  atomic_compare_exchange_strong(&a, &zero, 1);
  return NULL;
}

int main()
{
  pthread_t tid[2];
  atomic_init(&a, 0);

  pthread_create(&tid[0], NULL, t, (void *)(uintptr_t)0);
  pthread_create(&tid[1], NULL, t, (void *)(uintptr_t)0);

  pthread_join(tid[0], NULL);
  pthread_join(tid[1], NULL);

  return 0;
}
```

Save the program as `test.c` and run using the following command:
`nidhuggc -- --debug-print-on-reset --sc --source test.c 2>&1 | rg "(Cmp|=)"`.

```
 === TSOTraceBuilder reset ===
      (<0.0>,1-4)     CmpXhg(Global(1)(4),0x0,0x1)     SLP:{} - (<0.1>,1)
          (<0.1>,1-5) CmpXhgFail(Global(1)(4),0x0,0x1) SLP:{}
 =============================
 === TSOTraceBuilder reset ===
          (<0.1>,1)   CmpXhg(Global(1)(4),0x0,0x1)     SLP:{<0.0>}
      (<0.0>,1-5)     CmpXhgFail(Global(1)(4),0x0,0x1) SLP:{}
 =============================
```

The output shows visited interleavings. It also displays contents of the `sleep`
and `backtrack` sets at any stage. To get a better understanding of how nidhugg
takes particular decision consider adding log statements to
`TSOTraceBuilder::race_detect`.

# Contributions

Contributions are appreciated! Please create issues/PRs to this repo.

# References

- [Source Sets: A Foundation for Optimal Dynamic Partial Order Reduction](https://user.it.uu.se/~parosh/publications/papers/jacm17.pdf)
- [CDSCHECKER: Checking Concurrent Data Structures Written with C/C++ Atomics](http://plrg.eecs.uci.edu/publications/c11modelcheck.pdf)
- [Dynamic Partial-Order Reduction for Model Checking Software](https://users.soe.ucsc.edu/~cormac/papers/popl05.pdf)
- [Comparing Source Sets and Persistent Sets for Partial Order Reduction](https://user.it.uu.se/~bengt/Papers/Full/kimfest17.pdf)
- [Truly Stateless, Optimal Dynamic Partial Order Reduction](https://plv.mpi-sws.org/genmc/popl2022-trust.pdf)
