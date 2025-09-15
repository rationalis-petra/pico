#include "test_pico/eval/components.h"
#include "test_pico/helper.h"

#define RUN(str) run_toplevel(str, module, context); refresh_env(env, a)
#define TEST_EQ(str) test_toplevel_eq(str, &expected, module, context)
#define TEST_STDOUT(str) test_toplevel_stdout(str, expected, module, context)

void run_pico_eval_polymorphic_tests(TestLog *log, Module* module, Environment* env, Target target, Allocator *a) {
    TestContext context = (TestContext) {
        .env = env,
        .a = a,
        .log = log,
        .target = target,

    };

    // Literals
    if (test_start(log, mv_string("const-return"))) {
        int64_t expected = -28;
        // TODO (BUG): We should be able to remove the 'is' and still 
        //             typecheck
        TEST_EQ("((is (all [A] -28) (All [A] I64)) {Unit})");
    }

    // -------------------------------------------------------------------------
    //
    //     Static control and binding - seq/let/if/
    //
    // -------------------------------------------------------------------------

    if (test_start(log, mv_string("seq-simple"))) {
        int64_t expected = 17;
        TEST_EQ("((all [A] (is (seq 3 4 10 17) I64)) {Unit})");
    }

    if (test_start(log, mv_string("seq-fvar"))) {
        int64_t expected = -10;
        TEST_EQ("((all [A] proc [(x A)] (seq x 3 10 x)) -10)");
    }

    if (test_start(log, mv_string("seq-bind-lit"))) {
        int64_t expected = 6;
        TEST_EQ("((all [A] proc [(x A)] (is (seq [let! y 6] 10 y) I64)) -10)");
    }

    if (test_start(log, mv_string("seq-bind-lit-multi"))) {
        int64_t expected = 6;
        TEST_EQ("((all [A] proc [(x A)] (is (seq [let! y 6] [let! z 3] [let! p 2] 10 y) I64)) -10)");
    }

    if (test_start(log, mv_string("seq-bind-fvar"))) {
        int64_t expected = -10;
        TEST_EQ("((all [A] proc [(x A)] (seq x [let! y x] 10 y)) -10)");
    }

    if (test_start(log, mv_string("seq-bind-fvar-multi"))) {
        int64_t expected = -10;
        TEST_EQ("((all [A] proc [(x A)] (seq x [let! y x] [let! z 3] [let! p -2] 10 y)) -10)");
    }

    if (test_start(log, mv_string("simple-let"))) {
        int32_t expected = -3;
        TEST_EQ("((all [A] (let [x (is -3 I32)] x)) {Unit})");
    }

    /*
    if (test_start(log, mv_string("let-var"))) {
        int32_t expected = -27;
        TEST_EQ("((all [A] proc [(x A)] (let [y x] y)) -27)");
    }
    */

    /*
    RUN("(def Point Struct [.x I64] [.y I64])");
    if (test_start(log, mv_string("large-let"))) {
        int64_t expected[2] = {3, -10};
        TEST_EQ("((all [A] (let [x (struct Point [.x 3] [.y -10])] x)) {Unit})");
    }

    RUN("(def choose all [A] proc [(b Bool) (x A) (y A)] (if b x y))");
    if (test_start(log, mv_string("simple-if-true"))) {
        int64_t expected = 3;
        TEST_EQ("(choose :true 3 4)");
    }

    if (test_start(log, mv_string("simple-if-false"))) {
        int64_t expected = 4;
        TEST_EQ("(choose :false 3 4)");
    }

    // -----------------------------------------------------
    // 
    //      Struct
    // 
    // -----------------------------------------------------

    RUN("(def FourElt Struct [.x I64] [.y I64] [.z I64] [.p I64])");
    if (test_start(log, mv_string("struct-simple"))) {
        int64_t expected[] = {1, -2, 3, -4};
        TEST_EQ("((all [A] struct FourElt [.x 1] [.y -2] [.z 3] [.p -4]) {Unit})");
    }

    RUN("(def FourElt Struct [.x I64] [.y I64] [.z I64] [.p I64])");
    if (test_start(log, mv_string("struct-simple-alter"))) {
        RUN("(def ss struct FourElt [.x 1] [.y -2] [.z 3] [.p -4])");
        int64_t expected[] = {100, -2, 3, -27};
        TEST_EQ("((all [A] struct ss [.x 100] [.p -27]) {Unit})");
    }

    RUN("(def NonAligned Struct [.x I32] [.y I64] [.z I8] [.p I16])");
    if (test_start(log, mv_string("struct-nonaligned"))) {
        typedef struct {int32_t x; int64_t y; int8_t z; int16_t p;} NonAligned;
        NonAligned expected = (NonAligned) {.x = 1, .y = -2, .z = 3, .p = -4};
        TEST_EQ("((all [A] struct NonAligned [.x 1] [.y -2] [.z 3] [.p -4]) {Unit})");
    }

    if (test_start(log, mv_string("struct-nonaligned-alter"))) {
        RUN("(def nas struct NonAligned [.x 1] [.y -2] [.z 3] [.p -4])");
        typedef struct {int32_t x; int64_t y; int8_t z; int16_t p;} NonAligned;
        NonAligned expected = (NonAligned) {.x = 100, .y = -2, .z = 3, .p = -27};
        TEST_EQ("((all [A] struct nas [.x 100] [.p -27]) {Unit})");
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

    /* if (test_start(log, mv_string("large-dynamic-set"))) { */
    /*     int64_t expected[2] = {100, -100}; */
    /*     RUN("((all [A] (set ldvar struct [.x 100] [.y -100])) {Unit})"); */
    /*     TEST_EQ("(use ldvar)"); */
    /* } */
}
