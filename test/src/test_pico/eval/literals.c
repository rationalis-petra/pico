#include "pico/values/array.h"

#include "test_pico/eval/components.h"
#include "test_pico/helper.h"

#define TEST_EQ(str) test_toplevel_eq(str, &expected, module, context)

void run_pico_eval_literals_tests(TestLog *log, Module* module, Environment* env, Target target, Allocator *a) {
    TestContext context = (TestContext) {
        .env = env,
        .a = a,
        .log = log,
        .target = target,
    };

    if (test_start(log, mv_string("function-large-body"))) {
        int64_t expected = 10;
        TEST_EQ("10");
    }

    if (test_start(log, mv_string("negative-int-literal"))) {
        int64_t expected = -10;
        TEST_EQ("-10");
    }

    if (test_start(log, mv_string("positive-float-literal"))) {
        double expected = 10.5f;
        TEST_EQ("10.5");
    }

    if (test_start(log, mv_string("positive-small-float-literal"))) {
        double expected = 0.5f;
        TEST_EQ("0.5");
    }

    if (test_start(log, mv_string("negative-float-literal"))) {
        double expected = -10.5f;
        TEST_EQ("-10.5");
    }

    if (test_start(log, mv_string("negative-small-float-literal"))) {
        double expected = -0.5f;
        TEST_EQ("-0.5");
    }

    if (test_start(log, mv_string("complicated-float-literal"))) {
        double expected = 1342.734375;
        TEST_EQ("1342.734375");
    }

    if (test_start(log, mv_string("1-dimensional-array-literal"))) {
        int64_t expected_data[] = {1, 2, 3, 4, 5};
        uint64_t expected_shape[] = {5};
        Array expected = (Array) {
            .shape.len = 1,
            .shape.data = expected_shape,
            .data = expected_data,
        };// = mk_int_array(1, 5, {1, 2, 3, 4, 5});
        TEST_EQ("⟨1 2 3 4 5⟩");
    }
}
