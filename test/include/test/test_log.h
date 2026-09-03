#ifndef __TEST_TEST_LOG_H
#define __TEST_TEST_LOG_H

#include <time.h>

#include "platform/memory/allocator.h"
#include "platform/time/time.h"
#include "platform/terminal/terminal.h"

#include "components/logging/structured_logging.h"


/**
 *  Test Log
 * -----------
 * The test log manages test state for the entire application.
 * Functions such as suite_start and test_start return booleans 
 * which are true if the test/test-suite should be run, and false
 * otherwise.
 * 
 * TestLog log = mk_test_log(...);
 * // Any setup of global state that is necessary
 * finish_setup(...);
 * if (suite_start(log, mv_string("my suit name")) {
 *   if (suite_setup(log)) {
 *      // Suite setup (optional)
 *   }
 *   if (test_start(log, mv_string("my test name")) {
 *      // Test code
 *      if (result == expected_value) {
 *        test_pass(log);
 *      } else {
 *        test_fail(log);
 *      }
 *   }
 * 
 *   if (suite_teardown(log)) {
 *      // Suite teardown (optional)
 *   }
 * }
 * 
 */

typedef struct {
    bool show_fails;
    bool show_passes;
    bool show_errors;
    bool show_info;
    bool log_extra_info;
} Verbosity;

typedef struct {
    FormattedOStream* stream;
    Verbosity verbosity;

    PerfTime start_time;
    PerfTime setup_time;

    bool in_test;
    String current_test;
    PtrArray current_suites;

    bool barrier_check_failed;

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

/**
 * If any tests before the barrier have failed, then assume
 * that all future tests should be skipped. Note that suites
 * still run (so we can count skipped tests), it's just the 
 * tests that don't.
 */
void test_barrier(TestLog* log);  

// Return true if all tests run thus far have passed, and false otherwise.
bool all_passed(TestLog* log);

bool suite_start(TestLog* log, String name);
void suite_end(TestLog* log);
bool suite_setup(TestLog* log);
bool suite_teardown(TestLog* log);

bool test_start(TestLog* log, String name);

void test_pass(TestLog* log);
void test_skip(TestLog* log);
void test_fail(TestLog* log);

Logger* get_structured_logger(TestLog* log);
void clear_logger(TestLog* log);

void test_log_error(TestLog* log, String message);
void test_log_info(TestLog* log, String message);

FormattedOStream* get_fstream(TestLog* log);

int summarize_tests(TestLog* log, Allocator* a);

#endif
