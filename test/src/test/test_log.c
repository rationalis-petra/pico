#include "test/test_log.h"

const Colour err_colour = (Colour){.r = 220, .g = 180, .b = 50};
const Colour fail_colour = (Colour){.r = 200, .g = 50, .b = 50};
const Colour info_colour = (Colour){.r = 100, .g = 210, .b = 230};
const Colour pass_colour = (Colour){.r = 30, .g = 200, .b = 30};

TestLog* mk_test_log(FormattedOStream* stream, Allocator* a) {
  TestLog* out = mem_alloc(sizeof(TestLog), a);
  *out = (TestLog) {.stream = stream};
  return out;
}

void delete_test_log(TestLog* log, Allocator* a) {
  mem_free(log, a);
}

void test_log_fail(TestLog* log, String message) {
    start_coloured_text(fail_colour, log->stream);
    write_fstring(mv_string("[Fail]"), log->stream);
    write_fstring(message, log->stream);
    write_fstring(mv_string("\n"), log->stream);
    end_coloured_text(log->stream);
}

void test_log_error(TestLog* log, String message) {
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
    start_coloured_text(pass_colour, log->stream);
    write_fstring(mv_string("[Pass] "), log->stream);
    write_fstring(message, log->stream);
    write_fstring(mv_string("\n"), log->stream);
    end_coloured_text(log->stream);
}
