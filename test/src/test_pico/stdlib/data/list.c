#include "platform/memory/arena.h"

#include "pico/stdlib/extra.h"

#include "test_pico/stdlib/components.h"
#include "test_pico/stdlib/helper.h"

void run_pico_stdlib_data_list_tests(TestLog *log, Module* module, Allocator *a) {
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

    if (test_start(log, mv_string("Instnatiate Implicit with Default UVar"))) {
        // TODO (BUG): this leaks - set current allocator?
        Allocator current_old = get_std_current_allocator();
        set_std_current_allocator(arena);
        uint64_t expected = 10;
        test_toplevel_eq("(seq [let! arr (mk-array 1 1)] (aset 0 10 arr) (elt 0 arr))",
            &expected, module, log, a) ;
        set_std_current_allocator(current_old);
    }

    release_arena_allocator(arena);
}
