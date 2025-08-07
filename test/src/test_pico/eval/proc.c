#include "test_pico/eval/components.h"
#include "test_pico/helper.h"

#define RUN(str) run_toplevel(str, module, env, log, a); refresh_env(env, a)
#define TEST_EQ(str) test_toplevel_eq(str, &expected, module, env, log, a);
#define TEST_STDOUT(str) test_toplevel_stdout(str, expected, module, env, log, a);

void run_pico_eval_proc_tests(TestLog *log, Module* module, Environment* env, Allocator *a) {
    // check evaluation of procedure with 
    if (test_start(log, mv_string("int-literal"))) {
        int64_t expected = -10;
        TEST_EQ("-10");
    }
}
