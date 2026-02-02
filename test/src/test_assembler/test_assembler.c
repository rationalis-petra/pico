#include "test_assembler/test_assembler.h"

void run_assembler_tests(TestLog *log, Allocator *a) {
    if (suite_start(log, mv_string("nullary-op"))) {
        run_nullary_op_assembler_tests(log, a);
        suite_end(log);
    }

    if (suite_start(log, mv_string("unary-op"))) {
        run_unary_op_assembler_tests(log, a);
        suite_end(log);
    }

    if (suite_start(log, mv_string("binary-op"))) {
        run_binary_op_assembler_tests(log, a);
        suite_end(log);
    }

    if (suite_start(log, mv_string("sse"))) {
        run_sse_assembler_tests(log, a);
        suite_end(log);
    }
}
