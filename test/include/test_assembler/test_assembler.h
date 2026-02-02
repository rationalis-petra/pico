#ifndef __TEST_ASSEMBLER_ASSEMBLER_H
#define __TEST_ASSEMBLER_ASSEMBLER_H

#include "test/test_log.h"

void run_assembler_tests(TestLog* log, Allocator* a);

void run_nullary_op_assembler_tests(TestLog* log, Allocator* a);
void run_unary_op_assembler_tests(TestLog* log, Allocator* a);
void run_binary_op_assembler_tests(TestLog* log, Allocator* a);
void run_sse_assembler_tests(TestLog* log, Allocator* a);

#endif
