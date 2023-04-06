set -eux pipefail

dscheck_trace_file="tests/traces/ms_queue" dune exec tests/test_michael_scott_queue.exe
dscheck_trace_file="tests/traces/naive_counter" dune exec tests/test_naive_counter.exe
dscheck_trace_file="tests/traces/list" dune exec tests/test_list.exe
dscheck_trace_file="tests/traces/conditional1" dune exec tests/test_conditional1.exe
dscheck_trace_file="tests/traces/conditional2" dune exec tests/test_conditional2.exe