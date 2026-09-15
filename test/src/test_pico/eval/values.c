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

    // -----------------------------------------------------
    // 
    // Enumeration
    // 
    // -----------------------------------------------------

    if (test_start(log, mv_string("enum-simple"))) {
        RUN("(def TagOnly Enum :tag-1 :tag-2)");
        uint64_t expected = 1;
        TEST_EQ("TagOnly:tag-2");
    }

    if (test_start(log, mv_string("enum-simple-2"))) {
        RUN("(def TagOnly Enum :tag-1 :tag-2)");
        uint64_t expected = 1;
        TEST_EQ("((proc [] TagOnly:tag-2))");
    }

    typedef struct {
        uint64_t tag;
        int32_t x;
        int32_t y;
    } SimpleEnum;

    if (test_start(log, mv_string("enum-simple"))) {
        RUN("(def SE Enum [:simple I32 I32])");
        SimpleEnum expected = (SimpleEnum) {.tag = 0, .x = 1086, .y = -200};
        TEST_EQ("(SE:simple 1086 -200)");
    }

    if (test_start(log, mv_string("match-simple"))) {
        RUN("(def SE Enum [:simple I32 I32])");
        int32_t expected = 886;
        TEST_EQ("(match (SE:simple 1086 -200) [[:simple x y] (prim.i32.+ x y)])");
    }

    if (test_start(log, mv_string("match-proc-simple"))) {
        RUN("(def add proc [val] match val [[:simple x y] (prim.i32.+ x y)])");
        RUN("(def SE Enum [:simple I32 I32])");
        int32_t expected = 886;
        TEST_EQ("(add (SE:simple 1086 -200))");
    }

    if (test_start(log, mv_string("match-struct-inner"))) {
        int32_t expected = 900;
        TEST_EQ("(match (:some (struct [.x (is I32 1100)] [.y (is I32 -200)]))\n"
                 "  [[:some pr] (prim.i32.+ pr.x pr.y)])");
    }

    typedef struct {
        uint64_t tag;
        union {
            struct {
                int32_t sml;
                int64_t big;
            } large_enum;
            uint64_t us;
        };
    } MixedEnum;

    if (test_start(log, mv_string("enum-mixed-sml"))) {
        RUN("(def Mixed Enum [:large I32 I64] [:sml U64])");
        MixedEnum expected = (MixedEnum) {.tag = 1, .us = 1029731092};
        TEST_EQ("(Mixed:sml 1029731092)");
    }
    
    if (test_start(log, mv_string("enum-mixed-large"))) {
        RUN("(def Mixed Enum [:large I32 I64] [:sml U64])");
        MixedEnum expected = (MixedEnum) {.tag = 0, .large_enum.sml = -1086, .large_enum.big = 1937987};
        TEST_EQ("(Mixed:large -1086 1937987)");
    }

    typedef struct {
        uint32_t tag;
        uint32_t payload;
    } SmallTagEnum;

    if (test_start(log, mv_string("enum-small-tag"))) {
        RUN("(def SmallTag Enum 32 [:left U32] [:right U32])");
        SmallTagEnum expected = {.tag = 1, .payload = 2938};
        TEST_EQ("(SmallTag:right 2938)");
    }

    // This test was added due to experiencinng issues with the typechecking
    // stage not inserting correct tag values when the (value) type of a match
    // had not yet been inferred.
    if (test_start(log, mv_string("enum-type-inferred"))) {
        int64_t expected = 127;
        TEST_EQ("(match :bar\n"
                "  [:foo 1]"
                "  [:bar 127])");
    }


    if (test_start(log, mv_string("enum-match-branch"))) {
        RUN("(def HasBool Enum [:just U16 Bool])");
        int64_t expected = 127;
        TEST_EQ("(match (HasBool:just 12 :false)\n"
                "  [[:just x t] (if t -91723 127)])");
    }

    if (test_start(log, mv_string("enum-match-wildcard"))) {
        RUN("(def ManyVars Enum :a :b :c :d :e)");
        int64_t expected = 127;
        TEST_EQ("(match ManyVars:c\n"
                "  [[:a] 12]"
                "  [[:e] 90873]"
                "  [_ 127])");
    }

    if (test_start(log, mv_string("enum-match-wildcard-alt"))) {
        RUN("(def ManyVars Enum :a :b :c :d :e)");
        int64_t expected = 90873;
        TEST_EQ("(match ManyVars:e\n"
                "  [[:a] 12]"
                "  [[:e] 90873]"
                "  [_ 127])");
    }

    if (test_start(log, mv_string("enum-match-smaller-tag"))) {
        RUN("(def SmallTag Enum 32 [:left U32] [:right U32])");
        int32_t expected = 12389;
        TEST_EQ("(match (SmallTag:right 12389)\n"
                "  [[:left l] 9723]"
                "  [[:right e] e])");
    }

    // If we instantiate a Family (like Either or Result) with Unit, then we
    // need to know that the match can handle 0-size values correctly
    if (test_start(log, mv_string("if-match-proc-some"))) {
        RUN("(def if-make-maybe proc [b] if b :none (:some 64))");
        RUN("(def SmallTag Enum [:left Unit] [:right U32])");
        int32_t expected = 9723;
        TEST_EQ("(match (SmallTag:left :unit)\n"
                "  [[:left l] 9723]"
                "  [[:right e] e])");
    }

    // -----------------------------------------------------
    // 
    //   Flags
    // 
    // -----------------------------------------------------

    if (test_start(log, mv_string("flag"))) {
        int64_t expected = 2;
        RUN("(def Perms Flags :read :write :execute)");
        TEST_EQ("(Perms:write)");
    }
}
