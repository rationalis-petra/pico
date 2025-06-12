#include "data/stringify.h"

#include "test/test_log.h"

const Colour err_colour = (Colour){.r = 220, .g = 180, .b = 50};
const Colour fail_colour = (Colour){.r = 200, .g = 50, .b = 50};
const Colour info_colour = (Colour){.r = 100, .g = 210, .b = 230};
const Colour pass_colour = (Colour){.r = 30, .g = 200, .b = 30};

TestLog* mk_test_log(FormattedOStream* stream, Allocator* a) {
  TestLog* out = mem_alloc(sizeof(TestLog), a);
  *out = (TestLog) {
      .stream = stream,
      .test_count = 0,
      .passed_tests = 0,
      .failed_tests = 0,
  };
  return out;
}

void test_start(TestLog* log) {
    log->test_count++;
}

void delete_test_log(TestLog* log, Allocator* a) {
    mem_free(log, a);
}

void test_log_fail(TestLog* log, String message) {
    log->failed_tests++;

    start_coloured_text(fail_colour, log->stream);
    write_fstring(mv_string("[Fail] "), log->stream);
    write_fstring(message, log->stream);
    write_fstring(mv_string("\n"), log->stream);
    end_coloured_text(log->stream);
}

void test_log_error(TestLog* log, String message) {
    log->failed_tests++;

    start_coloured_text(err_colour, log->stream);
    write_fstring(mv_string("[Error] "), log->stream);
    write_fstring(message, log->stream);
    write_fstring(mv_string("\n"), log->stream);
    end_coloured_text(log->stream);
}

void test_log_info(TestLog* log, String message) {
    start_coloured_text(info_colour, log->stream);
    write_fstring(mv_string("[Info] "), log->stream);
    write_fstring(message, log->stream);
    write_fstring(mv_string("\n"), log->stream);
    end_coloured_text(log->stream);
}

void test_log_pass(TestLog* log, String message) {
    log->passed_tests++;

    start_coloured_text(pass_colour, log->stream);
    write_fstring(mv_string("[Pass] "), log->stream);
    write_fstring(message, log->stream);
    write_fstring(mv_string("\n"), log->stream);
    end_coloured_text(log->stream);
}

FormattedOStream *get_fstream(TestLog *log) {
    return log->stream;
}

void summarize_tests(TestLog *log, Allocator* a) {
    write_fstring(mv_string("\nTest Suite Completed"), log->stream);
    write_fstring(mv_string("\n──────────────────────────────────────────────────────────\n"), log->stream);
    if (log->passed_tests + log->failed_tests != log->test_count) {
        start_coloured_text(fail_colour, log->stream);
        write_fstring(mv_string("Failure of test suite: total tests != passed + failed"), log->stream);
        end_coloured_text(log->stream);
    }
    String str;

    write_fstring(mv_string("\n  Passed: "), log->stream);
    str = string_u64(log->passed_tests, a);
    write_fstring(str, log->stream);
    delete_string(str, a);
    write_fstring(mv_string("\n  Failed: "), log->stream);
    str = string_u64(log->failed_tests, a);
    write_fstring(str, log->stream);
    delete_string(str, a);
    write_fstring(mv_string("\n  Total : "), log->stream);
    str = string_u64(log->test_count, a);
    write_fstring(str, log->stream);
    delete_string(str, a);
    write_fstring(mv_string("\n"), log->stream);
    write_fstring(mv_string("\n──────────────────────────────────────────────────────────\n"), log->stream);
}
