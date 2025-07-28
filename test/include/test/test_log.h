#ifndef __TEST_TEST_LOG_H
#define __TEST_TEST_LOG_H

#include <time.h>

#include "platform/memory/allocator.h"
#include "platform/profiling/profiling.h"
#include "platform/io/terminal.h"

typedef struct {
    bool show_fails;
    bool show_passes;
    bool show_errors;
    bool show_info;
} Verbosity;

typedef struct {
    FormattedOStream* stream;
    Verbosity verbosity;

    PerfTime start_time;
    PerfTime setup_time;

    bool in_test;
    String current_test;

    size_t test_count;
    size_t passed_tests;
    size_t failed_tests;
} TestLog;

TestLog* mk_test_log(FormattedOStream* stream, Verbosity v, Allocator* a);
void finish_setup(TestLog* log);
void delete_test_log(TestLog* log, Allocator* a);

bool suite_start(TestLog* log, String name);
void suite_end(TestLog* log);

bool test_start(TestLog* log, String name);

void test_pass(TestLog* log);
void test_fail(TestLog* log);

void test_log_error(TestLog* log, String message);
void test_log_info(TestLog* log, String message);

FormattedOStream* get_fstream(TestLog* log);

int summarize_tests(TestLog* log, Allocator* a);

#endif
