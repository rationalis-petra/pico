#ifndef __TEST_TEST_LOG_H
#define __TEST_TEST_LOG_H

#include <time.h>

#include "platform/memory/allocator.h"
#include "platform/time/time.h"
#include "platform/terminal/terminal.h"

#include "components/logging/structured_logging.h"

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
    PtrArray current_suites;

    size_t test_count;
    size_t skipped_tests;
    size_t passed_tests;
    size_t failed_tests;
    Logger* slogger;
    Allocator* gpa;
} TestLog;

TestLog* mk_test_log(FormattedOStream* stream, Verbosity v, Allocator* a);
void finish_setup(TestLog* log);
void delete_test_log(TestLog* log, Allocator* a);

bool suite_start(TestLog* log, String name);
void suite_end(TestLog* log);

bool test_start(TestLog* log, String name);

void test_pass(TestLog* log);
void test_skip(TestLog* log);
void test_fail(TestLog* log);

Logger* get_structured_logger(TestLog* log);

void test_log_error(TestLog* log, String message);
void test_log_info(TestLog* log, String message);

FormattedOStream* get_fstream(TestLog* log);

int summarize_tests(TestLog* log, Allocator* a);

#endif
