#include "platform/memory/arena.h"

#include "pico/stdlib/extra.h"

#include "test_pico/stdlib/components.h"
#include "test_pico/helper.h"

void run_pico_stdlib_data_pair_tests(TestLog *log, Module* module, Allocator *a) {
    Allocator arena = mk_arena_allocator(4096, a);

    typedef struct {
        int64_t x;
        int64_t y;
    } Point;

    if (test_start(log, mv_string("pair-simple"))) {
        Point expected = (Point) {.x = 12397, .y = -35};
        test_toplevel_eq("(struct (Pair I64 I64) [._1 12397] [._2 -35])", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("pair-fn"))) {
        run_toplevel("(def pair-fn proc [x y] (struct (Pair I64 I64) [._1 x] [._2 y]))", module, log, a) ;
        Point expected = (Point) {.x = -987, .y = 935};
        test_toplevel_eq("(pair-fn -987 935)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("pair-poly-fn"))) {
        Point expected = (Point) {.x = 1432, .y = -120938};
        test_toplevel_eq("(pair.pair 1432 -120938)", &expected, module, log, a) ;
    }

    typedef struct {
        int32_t x;
        int32_t y;
    } Point32;
    /* if (test_start(log, mv_string("pair-small-size"))) { */
    /*     Point32 expected = (Point32) {.x = 1432, .y = -960}; */
    /*     test_toplevel_eq("(pair.pair (is 1432 I32) (is -960 I32))", &expected, module, log, a) ; */
    /* } */

    typedef struct {
        uint64_t tag;
        int32_t x;
        int32_t y;
    } EnumPoint;

    if (test_start(log, mv_string("pair-in-enum"))) {
        EnumPoint expected = (EnumPoint) {.tag = 0, .x = 1432, .y = -120938};
        test_toplevel_eq("(:some (pair.pair {I32 I32} 1432 -120938))", &expected, module, log, a) ;
    }

    release_arena_allocator(arena);
}
