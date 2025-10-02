#include "platform/memory/arena.h"

#include "pico/stdlib/extra.h"

#include "test_pico/stdlib/components.h"
#include "test_pico/helper.h"

#define RUN(str) run_toplevel(str, module, context); refresh_env(env, a)
#define TEST_EQ(str) test_toplevel_eq(str, &expected, module, context)

void run_pico_stdlib_data_either_tests(TestLog *log, Module* module, Environment* env, Target target, Allocator *a) {
    Allocator arena = mk_arena_allocator(16384, a);
    TestContext context = (TestContext) {
        .env = env,
        .a = a,
        .log = log,
        .target = target,
    };

    typedef struct {
        uint64_t tag;
        int64_t data;
    } Either;

    if (test_start(log, mv_string("either-lhs-simple"))) {
        Either expected = (Either) {.tag = 0, .data = -35};
        TEST_EQ("((either.Either I64 I64):left -35)");
    }

    if (test_start(log, mv_string("either-lhs-simple"))) {
        Either expected = (Either) {.tag = 1, .data = 8796};
        TEST_EQ("((either.Either I64 I64):right 8796)");
    }

    if (test_start(log, mv_string("left-fn"))) {
        Either expected = (Either) {.tag = 0, .data = -987};
        TEST_EQ("(either.left {I64 U64} -987)");
    }

    /* if (test_start(log, mv_string("pair-poly-fn"))) { */
    /*     Point expected = (Point) {.x = 1432, .y = -120938}; */
    /*     TEST_EQ("(pair.pair 1432 -120938)"); */
    /* } */

    /* typedef struct { */
    /*     int32_t x; */
    /*     int32_t y; */
    /* } Point32; */
    /* if (test_start(log, mv_string("pair-small-size"))) { */
    /*     Point32 expected = (Point32) {.x = 1432, .y = -960}; */
    /*     TEST_EQ("(pair.pair (is 1432 I32) (is -960 I32))"); */
    /* } */

    /* typedef struct { */
    /*     uint64_t tag; */
    /*     int32_t x; */
    /*     int32_t y; */
    /* } EnumPoint; */

    /* if (test_start(log, mv_string("pair-in-enum"))) { */
    /*     EnumPoint expected = (EnumPoint) {.tag = 0, .x = 1432, .y = -120938}; */
    /*     TEST_EQ("(:some (pair.pair {I32 I32} 1432 -120938))"); */
    /* } */

    release_arena_allocator(arena);
}
