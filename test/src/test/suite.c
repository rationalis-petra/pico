#include "test/suite.h"

void run_tests(StrPtrAMap tests, RunDescriptor to_run, TestLog* log, Allocator* a) {
  switch (to_run.type) {
  case RunAll:
    for (size_t i = 0; i < tests.len; i++) {
      TestSuite* test = tests.data[i].val;
      test(to_run, log, a);
    }
    break;
  case RunOnly:
    for (size_t i = 0; i < to_run.tests.len; i++) {
      TestPath* child = to_run.tests.data[i];
      TestSuite** test = (TestSuite**)str_ptr_lookup(child->name, tests);
      if (test) {
        (*test)((RunDescriptor){.type = to_run.type, .tests = child->children}, log, a);
      } else {
        test_log_fail(log, mv_string("Test Does Not Exist"));
      }
    }
    break;

  case RunExcept:
    test_log_fail(log, mv_string("Run Except not implemented"));
    break;
  }
}


