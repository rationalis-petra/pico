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

    release_arena_allocator(arena);
}
