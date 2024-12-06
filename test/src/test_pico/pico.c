#include "data/amap.h"

#include "test_pico/parse/parse.h"

#include "test_pico/pico.h"


void run_pico_tests(RunDescriptor to_run, TestLog* log, Allocator* a) {
  StrPtrAMap tests = mk_str_ptr_amap(16, a);
  str_ptr_insert(mv_string("pico"), run_pico_parse_tests, &tests);

  test_log_info(log, mv_string("Beginning Pico Tests!"));
  run_tests(tests, to_run, log, a);
  test_log_info(log, mv_string("Ending Pico tests!"));

  sdelete_str_ptr_amap(tests);
}
