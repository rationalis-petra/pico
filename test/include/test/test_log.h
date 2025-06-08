#ifndef __TEST_TEST_LOG_H
#define __TEST_TEST_LOG_H

#include "platform/memory/allocator.h"
#include "platform/io/terminal.h"

typedef struct {
    FormattedOStream* stream;
} TestLog;

TestLog* mk_test_log(FormattedOStream* stream, Allocator* a);
void delete_test_log(TestLog* log, Allocator* a);

void test_log_fail(TestLog* log, String message);
void test_log_error(TestLog* log, String message);
void test_log_info(TestLog* log, String message);
void test_log_pass(TestLog* log, String message);

#endif
