#include "test_pico/eval/components.h"
#include "test_pico/helper.h"

#define RUN(str) run_toplevel(str, module, env, log, a); refresh_env(env, a)
#define TEST_EQ(str) test_toplevel_eq(str, &expected, module, env, log, a);
#define TEST_STDOUT(str) test_toplevel_stdout(str, expected, module, env, log, a);

void run_pico_eval_polymorphic_tests(TestLog *log, Module* module, Environment* env, Allocator *a) {
    // -------------------------------------------------------------------------
    //
    //     Static control and binding - seq/let
    //
    // -------------------------------------------------------------------------

    if (test_start(log, mv_string("simple-let"))) {
        int64_t expected = 3;
        TEST_EQ("((all [A] (let [x (is 3 I64)] x)) {Unit})");
    }

    if (test_start(log, mv_string("simple-let"))) {
        int32_t expected = -3;
        TEST_EQ("((all [A] (let [x (is -3 I32)] x)) {Unit})");
    }

    RUN("(def Point Struct [.x I64] [.y I64])");
    if (test_start(log, mv_string("large-let"))) {
        int64_t expected[2] = {3, -10};
        TEST_EQ("((all [A] (let [x (struct Point [.x 3] [.y -10])] x)) {Unit})");
    }

    // -------------------------------------------------------------------------
    //
    //     Dynamic binding - dynamic/use/bind/set
    //
    // -------------------------------------------------------------------------

    RUN("(def dvar dynamic -10)");
    if (test_start(log, mv_string("dynamic-use"))) {
        int64_t expected = -10;
        TEST_EQ("((all [A] (use dvar)) {Unit})");
    }

    if (test_start(log, mv_string("dynamic-set"))) {
        int64_t expected = 3;
        RUN("(set dvar 3)");
        TEST_EQ("((all [A] (use dvar)) {Unit})");
    }

    RUN("(def ldvar dynamic struct [.x -10] [.y 10])");
    if (test_start(log, mv_string("large-dynamic-use"))) {
        int64_t expected[2] = {-10, 10};
        TEST_EQ("((all [A] (use ldvar)) {Unit})");
    }

    if (test_start(log, mv_string("large-dynamic-set"))) {
        int64_t expected[2] = {100, -100};
        RUN("((all [A] (set ldvar struct [.x 100] [.y -100])) {Unit})");
        TEST_EQ("(use ldvar)");
    }
}
