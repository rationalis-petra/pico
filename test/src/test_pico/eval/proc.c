#include "test_pico/eval/components.h"
#include "test_pico/helper.h"

#define TEST_EQ(str) test_toplevel_eq(str, &expected, module, context)

void run_pico_eval_proc_tests(TestLog *log, Module* module, Environment* env, Target target, Allocator *a) {
    TestContext context = (TestContext) {
        .env = env,
        .a = a,
        .log = log,
        .target = target,
    };

    // check evaluation of procedure with 
    if (test_start(log, mv_string("int-literal"))) {
        int64_t expected = -10;
        TEST_EQ("-10");
    }
}
