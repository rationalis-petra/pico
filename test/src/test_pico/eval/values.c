#include "test_pico/eval/components.h"
#include "test_pico/helper.h"

#include "pico/stdlib/core/kernel.h"

#define TEST_EQ(str) test_toplevel_eq(str, &expected, module, context)

void run_pico_eval_values_tests(TestLog *log, Module* module, Environment* env, Target target, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    PiAllocator pico_region = convert_to_pallocator(&ra);
    PiAllocator* pia = &pico_region;

    TestContext context = (TestContext) {
        .env = env,
        .region = region,
        .pia = pia,
        .log = log,
        .target = target,
    };

    // -----------------------------------------------------
    // 
    //      Tiles & Tile-Loops
    // 
    // -----------------------------------------------------
    if (test_start(log, mv_string("1d-tile-literal"))) {
        int64_t expected[] = {1, 2, 3, 4};
        TEST_EQ("(tile {4} [1 2 3 4])");
    }

    if (test_start(log, mv_string("1d-tile-literal-inferred-size"))) {
        int64_t expected[] = {2, 4, 6, 8};
        TEST_EQ("(tile [2 4 6 8])");
    }

    if (test_start(log, mv_string("2d-tile-literal"))) {
        int64_t expected[] = {1, 2, 3, 4, 5, 6, 7, 8};
        TEST_EQ("(tile {2 4} [[1 2 3 4] [5 6 7 8]])");
    }

    if (test_start(log, mv_string("2d-tile-literal-inferred-size"))) {
        int64_t expected[] = {2, 4, 6, 8, 10, 12, 14, 16};
        TEST_EQ("(tile [[2 4 6 8] [10 12 14 16]])");
    }

    if (test_start(log, mv_string("elt-of-tile"))) {
        int64_t expected = 9;
        TEST_EQ("(telt 2 (tile [3 7 9 12]))");
    }

    if (test_start(log, mv_string("elt-of-tile-2d"))) {
        int64_t expected = 3;
        TEST_EQ("(telt [1 0] (tile [[2 4 6 8] [3 7 9 12]]))");
    }

    if (test_start(log, mv_string("with-gen-tiles-inner"))) {
        uint64_t expected[8] = {0, 1, 2, 3, 0, 1, 2, 3};
        TEST_EQ("(with [i j] [2 4] j)");
    }

    if (test_start(log, mv_string("with-gen-tiles-outer"))) {
        uint64_t expected[8] = {0, 0, 0, 0, 1, 1, 1, 1};
        TEST_EQ("(with [i j] [2 4] i)");
    }

    if (test_start(log, mv_string("with-fold"))) {
        int64_t expected = 12;
        TEST_EQ("(with [i j] [2 4] {fold prim.u64.+ 0} j)");
    }

    // -----------------------------------------------------
    // 
    //      Polymorphic Tiles & Tile-Loops
    // 
    // -----------------------------------------------------
    if (test_start(log, mv_string("poly-1d-tile-literal"))) {
        int64_t expected[] = {1, 2, 3, 4};
        TEST_EQ("((all [A] proc [(x A) y z w] tile {4} [x y z w]) 1 2 3 4)");
    }

    if (test_start(log, mv_string("poly-1d-tile-literal-inferred-size"))) {
        int64_t expected[] = {2, 4, 6, 8};
        TEST_EQ("((all [A] proc [(x A) y z w] tile [x y z w]) 2 4 6 8)");
    }

    if (test_start(log, mv_string("poly-2d-tile-literal"))) {
        int64_t expected[] = {1, 2, 3, 4, 5, 6, 7, 8};
        TEST_EQ("((all [A] proc [(x A) y z w a b c d] "
                "  tile {2 4} [[x y z w] [a b c d]])"
                "  1 2 3 4 5 6 7 8)");
    }

    if (test_start(log, mv_string("poly-2d-tile-literal-inferred-size"))) {
        int64_t expected[] = {2, 4, 6, 8, 10, 12, 14, 16};
        TEST_EQ("((all [A] proc [(x A) y z w a b c d] "
                "  tile [[x y z w] [a b c d]])"
                "  2 4 6 8 10 12 14 16)");
    }

    if (test_start(log, mv_string("poly-elt-of-tile"))) {
        int64_t expected = 9;
        TEST_EQ("((all [A] proc [(arr (Tile [4] A))] telt 2 arr) (tile [3 7 9 12]))");
    }

    if (test_start(log, mv_string("poly-elt-of-tile-2d"))) {
        int64_t expected = 3;
        TEST_EQ("((all [A] proc [(arr (Tile [2 4] A))] telt [1 0] arr) (tile [[2 4 6 8] [3 7 9 12]]))");
    }

    if (test_start(log, mv_string("poly-with-gen-tiles"))) {
        int64_t expected[8] = {7, 7, 7, 7, 7, 7, 7, 7};
        TEST_EQ("((all [A] proc [(v A)] with [i j] [2 4] v) 7)");
    }

    if (test_start(log, mv_string("poly-with-gen-tiles-outer"))) {
        uint64_t expected[8] = {0, 0, 0, 0, 1, 1, 1, 1};
        TEST_EQ("(with [i j] [2 4] i)");
    }

    if (test_start(log, mv_string("poly-with-fold"))) {
        int64_t expected = 24;
        TEST_EQ("((all [A] proc [(v A) (f Proc [A A] A) (n A)] with [i j] [2 4] {fold f n} v) 3 prim.i64.+ 0)");
    }

}
