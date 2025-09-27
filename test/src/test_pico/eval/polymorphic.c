#include <string.h>
#include "platform/memory/static.h"

#include "pico/stdlib/extra.h"

#include "test_pico/eval/components.h"
#include "test_pico/helper.h"

#define RUN(str) run_toplevel(str, module, context); refresh_env(env, a)
#define TEST_EQ(str) test_toplevel_eq(str, &expected, module, context)
#define TEST_STDOUT(str) test_toplevel_stdout(str, expected, module, context)
#define TEST_MEM(str) test_toplevel_mem(str, expected, start, sizeof(expected), module, context)

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
        TEST_EQ("((all [A] -28) {Unit})");
    }

    if (test_start(log, mv_string("static-return"))) {
        int64_t expected = 39;
        TEST_EQ("((all [A] proc [(x I64)] x) {Unit} 39)");
    }

    if (test_start(log, mv_string("mix-arg-static-return"))) {
        int64_t expected = 39;
        TEST_EQ("((all [A] proc [(a A) (x I64)] x) -3 39)");
    }

    if (test_start(log, mv_string("static-return"))) {
        RUN("(def int-id proc [(x I64)] x)");

        int64_t expected = 89;
        TEST_EQ("((all [A] proc [(x I64)] (int-id x)) {Unit} 89)");
    }

    if (test_start(log, mv_string("poly-return-large"))) {
        RUN("(def id all [A] proc [(x A)] x)");
        typedef struct {int64_t x; int64_t y;} Pr;

        Pr expected = (Pr) {.x=127, .y=-75};
        TEST_EQ("(id (struct [.x 127] [.y -75]))");
    }

    if (test_start(log, mv_string("poly-internal-return-large"))) {
        RUN("(def id all [A] proc [(x A)] x)");
        RUN("(def id-2 all [A] proc [(x A)] (id x))");
        typedef struct {int64_t x; int64_t y;} Pr;

        Pr expected = (Pr) {.x=127, .y=-75};
        TEST_EQ("(id-2 (struct [.x 127] [.y -75]))");
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

    // TODO: polymorphic let-in-seq of large values

    if (test_start(log, mv_string("simple-let"))) {
        int32_t expected = -3;
        TEST_EQ("((all [A] (let [x (is -3 I32)] x)) {Unit})");
    }

    if (test_start(log, mv_string("simple-let-multi"))) {
        int32_t expected = -3;
        TEST_EQ("((all [A] (let [x (is -3 I32)] [y (is 2 I32)] x)) {Unit})");
    }

    // TODO: polymorphic let of large values

    if (test_start(log, mv_string("let-var"))) {
        int64_t expected = -27;
        TEST_EQ("((all [A] proc [(x A)] (let [y x] y)) -27)");
    }

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

    // ---------------------------------------------------------------------
    //
    //     Labels 
    //
    // -------------------------------------------------------------------------

    if (test_start(log, mv_string("labels-simple"))) {
        int64_t expected = 27;
        TEST_EQ("((all [A] (labels 27)) {Unit})");
    }

    if (test_start(log, mv_string("labels-single-goto"))) {
        int64_t expected = -8;
        TEST_EQ("((all [A] labels (go-to start) [start -8]) {Unit})");
    }

    if (test_start(log, mv_string("labels-pass-var"))) {
        int64_t expected = 3;
        TEST_EQ("((all [A] labels (go-to start 3) [start [x] x]) {Unit})");
    }

    if (test_start(log, mv_string("labels-loop"))) {
        int64_t expected = 10;
        TEST_EQ("((all [A] labels (go-to loop 0) [loop [x] (if (i64.< x 10) (go-to loop (i64.+ x 1)) x)]) {Unit})");
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

    if (test_start(log, mv_string("struct-nonaligned-project"))) {
        RUN("(def nas struct NonAligned [.x 1] [.y -2] [.z 3] [.p -4])");
        int64_t expected = 3;
        TEST_EQ("((all [A] nas.z) {Unit})");
    }

    // -------------------------------------------------------------------------
    //
    //     Labels
    //
    // -------------------------------------------------------------------------



    /* if (test_start(log, mv_string("labels-simple"))) { */
    /*     int8_t expected = 27; */
    /*     TEST_EQ("(labels 27)"); */
    /* } */

    // -------------------------------------------------------------------------
    //
    //     Funcall - calling functions from within polymorphic code
    //
    // -------------------------------------------------------------------------


    if (test_start(log, mv_string("apply"))) {
        int64_t expected = 5;
        TEST_EQ("((all [A] proc [(fn (Proc [A A] A)) (x A) (y A)] (fn x y)) i64.+ -5 10)");
    }

    if (test_start(log, mv_string("apply-poly-poly"))) {
        RUN("(def id all [A] proc [(x A)] x)");
        RUN("(def id2 all [A] proc [(x A)] (id x))");
        int64_t expected = 77;
        TEST_EQ("(id 77)");
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

    // -------------------------------------------------------------------------
    //
    //     Combinations known to be odd / problematic in the past
    //
    // -------------------------------------------------------------------------

    void* mem = mem_alloc(128, a);
    Allocator old = get_std_current_allocator();
    void* start;
    {
        Allocator sta = mk_static_allocator(mem, 128);
        start = mem_alloc(8, &sta);
    }

    /* if (test_start(log, mv_string("test-poly-call-in-loop"))) { */
    /*     Allocator sta = mk_static_allocator(mem, 128); */
    /*     uint64_t expected[] = {0, 1 }; */

    /*     set_std_current_allocator(sta); */
    /*     TEST_MEM("(let [addr malloc (size-of I64)] (loop [for i from 0 upto 5] (store {U64} addr i)))"); */
    /* } */

    set_std_current_allocator(old);
    mem_free(mem, a);
}
