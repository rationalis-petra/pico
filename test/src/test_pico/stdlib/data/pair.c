#include "platform/memory/arena.h"

#include "pico/stdlib/extra.h"

#include "test_pico/stdlib/components.h"
#include "test_pico/helper.h"

#define RUN(str) run_toplevel(str, module, env, log, a); refresh_env(env, a)
#define TEST_EQ(str) test_toplevel_eq(str, &expected, module, env, log, a)

void run_pico_stdlib_data_pair_tests(TestLog *log, Module* module, Environment* env, Allocator *a) {
    Allocator arena = mk_arena_allocator(16384, a);

    typedef struct {
        int64_t x;
        int64_t y;
    } Point;

    if (test_start(log, mv_string("pair-simple"))) {
        Point expected = (Point) {.x = 12397, .y = -35};
        TEST_EQ("(struct (Pair I64 I64) [._1 12397] [._2 -35])");
    }

    if (test_start(log, mv_string("pair-fn"))) {
        RUN("(def pair-fn proc [x y] (struct (Pair I64 I64) [._1 x] [._2 y]))");
        Point expected = (Point) {.x = -987, .y = 935};
        TEST_EQ("(pair-fn -987 935)");
    }

    if (test_start(log, mv_string("pair-poly-fn"))) {
        Point expected = (Point) {.x = 1432, .y = -120938};
        TEST_EQ("(pair.pair 1432 -120938)");
    }

    typedef struct {
        int32_t x;
        int32_t y;
    } Point32;
    if (test_start(log, mv_string("pair-small-size"))) {
        Point32 expected = (Point32) {.x = 1432, .y = -960};
        TEST_EQ("(pair.pair (is 1432 I32) (is -960 I32))");
    }

    typedef struct {
        uint64_t tag;
        int32_t x;
        int32_t y;
    } EnumPoint;

    if (test_start(log, mv_string("pair-in-enum"))) {
        EnumPoint expected = (EnumPoint) {.tag = 0, .x = 1432, .y = -120938};
        TEST_EQ("(:some (pair.pair {I32 I32} 1432 -120938))");
    }

    release_arena_allocator(arena);
}
