set -eux pipefail

dscheck_trace_file="tests/traces/ms_queue" dune exec tests/test_michael_scott_queue.exe
dscheck_trace_file="tests/traces/naive_counter" dune exec tests/test_naive_counter.exe
dscheck_trace_file="tests/traces/list" dune exec tests/test_list.exe
dscheck_trace_file="tests/traces/conditional_nested" dune exec tests/test_conditional_nested.exe
dscheck_trace_file="tests/traces/conditional_ssb" dune exec tests/test_conditional_ssb.exe