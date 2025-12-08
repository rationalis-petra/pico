#include "test_pico/stdlib/components.h"
#include "test_pico/helper.h"

#define RUN(str) run_toplevel(str, module, context); refresh_env(env, a)
#define TEST_EQ(str) test_toplevel_eq(str, &expected, module, context)
#define TEST_STDOUT(str) test_toplevel_stdout(str, expected, module, context)

void run_pico_stdlib_abs_numeric_tests(TestLog *log, Module* module, Environment* env, Target target, RegionAllocator* region) {
    TestContext context = (TestContext) {
        .env = env,
        .region = region,
        .log = log,
        .target = target,
    };

    if (test_start(log, mv_string("numeric-add"))) {
        int64_t expected = 55;
        TEST_EQ("(+ 10 45)");
    }

    if (test_start(log, mv_string("numeric-add-instance=f32"))) {
        float expected = 3.4 + 678.0;
        TEST_EQ("(is (+ 3.4 678.0) F32)");
    }

    if (test_start(log, mv_string("numeric-div-instance=f64"))) {
        double expected = 3.0 / 4.5;
        TEST_EQ("(/ 3.0 4.5)");
    }

    if (test_start(log, mv_string("numeric-div-instance=f32"))) {
        float expected = 3.0 / 4.5;
        TEST_EQ("(is (/ 3.0 4.5) F32)");
    }

    if (test_start(log, mv_string("numeric-mul"))) {
        double expected = 3.0 * 4.5;
        TEST_EQ("(* 3.0 4.5)");
    }
}
