#include "test/test_log.h"

TestLog* mk_test_log(OStream* stream, Allocator* a) {
  TestLog* out = mem_alloc(sizeof(TestLog), a);
  *out = (TestLog) {.stream = stream};
  return out;
}

void delete_test_log(TestLog* log, Allocator* a) {
  mem_free(log, a);
}

void test_log_fail(TestLog* log, String message) {
  write_string(mv_string("[Fail]"), log->stream);
  write_string(message, log->stream);
  write_string(mv_string("\n"), log->stream);
}

void test_log_error(TestLog* log, String message) {
  write_string(mv_string("[Error] "), log->stream);
  write_string(message, log->stream);
  write_string(mv_string("\n"), log->stream);
}

void test_log_info(TestLog* log, String message) {
  write_string(mv_string("[Info] "), log->stream);
  write_string(message, log->stream);
  write_string(mv_string("\n"), log->stream);
}
