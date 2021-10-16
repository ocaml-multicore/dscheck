/*
external make : 'a -> 'a t = "trace_make"
external get : 'a t -> 'a = "trace_get"
external set : 'a t -> 'a -> unit = "trace_set"
external exchange : 'a t -> 'a -> 'a = "trace_exchange"
external compare_and_set : 'a t -> 'a -> 'a -> bool = "trace_cas"
external fetch_and_add : 'a t -> 'a -> 'a = "trace_fadd"
*/

#include "caml/memory.h"
#include "caml/mlvalues.h"

enum AtomicOp {
    MAKE,
    GET,
    SET,
    EXCHANGE,
    CAS,
    FADD
};

typedef enum AtomicOp atomic_op;

struct AtomicOpAddr {
    atomic_op op;
    char* addr;
    struct AtomicOpAddr* next;
};

typedef struct AtomicOpAddr atomic_op_addr;

static __thread atomic_op_addr* op_list;

void add_op(atomic_op op, value addr) {
    atomic_op_addr* op_addr = (atomic_op_addr*)caml_stat_alloc(sizeof(atomic_op_addr));
    op_addr->op = op;
    op_addr->addr = (char*)addr;
    op_addr->next = op_list;
    op_list = op_addr;
}

value trace_make(value i) {
    add_op(MAKE, i);

}

value trace_get(value i) {
    add_op(GET, i);
}

value trace_set(value i) {
    add_op(SET, i);
}

value trace_exchange(value i, value v) {
    add_op(EXCHANGE, i);
}

value trace_cas(value r, value s, value v) {
    add_op(CAS, r);
}

value trace_fadd(value r, value i) {
    add_op(FADD, r);
}