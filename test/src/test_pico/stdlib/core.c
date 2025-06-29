#include "test_pico/stdlib/components.h"
#include "test_pico/helper.h"

void run_pico_stdlib_core_tests(TestLog *log, Module* module, Allocator *a) {
    // -------------------------------------------------------------------------
    //
    // Sequence testing
    //
    // -------------------------------------------------------------------------

    if (test_start(log, mv_string("simple-let"))) {
        int64_t expected = 3;
        test_toplevel_eq("(let [x 3] x)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("simple-sequence"))) {
        int64_t expected = 3;
        test_toplevel_eq("(seq 1 2 3)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("let-in-sequence"))) {
        int64_t expected = 2;
        test_toplevel_eq("(seq [let! x 2] x)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("let-many-in-sequence"))) {
        int64_t expected = 5;
        test_toplevel_eq("(seq [let! x 2 y 3] (u32.+ x y))", &expected, module, log, a) ;
    }

    // -------------------------------------------------------------------------
    //
    // Conditional testing
    //
    // -------------------------------------------------------------------------

    if (test_start(log, mv_string("if-true"))) {
        int64_t expected = 2;
        test_toplevel_eq("(if :true 2 3)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("if-false"))) {
        int64_t expected = 3;
        test_toplevel_eq("(if :false 2 3)", &expected, module, log, a) ;
    }

    run_toplevel("(def if-proc proc [(b Bool) (x I32) (y I32)] if b x y)", module, log, a) ;
    if (test_start(log, mv_string("if-proc-true"))) {
        int64_t expected = 1;
        test_toplevel_eq("(if-proc :true 1 2)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("if-proc-false"))) {
        int64_t expected = -12;
        test_toplevel_eq("(if-proc :false 1 -12)", &expected, module, log, a) ;
    }

    run_toplevel("(def if-proc-inv proc [(b Bool) (x I32) (y I32)] if b y x)", module, log, a) ;
    if (test_start(log, mv_string("if-proc-inv-true"))) {
        int64_t expected = -45;
        test_toplevel_eq("(if-proc-inv :true 1 -45)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("if-proc-inv-false"))) {
        int64_t expected = 720;
        test_toplevel_eq("(if-proc-inv :false 720 -12)", &expected, module, log, a) ;
    }

    // -----------------------------------------------------
    // 
    // Struct
    // 
    // -----------------------------------------------------

    typedef struct {
        int64_t x;
        int64_t y;
    } Point;

    typedef struct {
        int8_t x;
        int16_t y;
        int32_t z;
    } MisalignedStruct;
    run_toplevel("(def MAS Struct [.x I8] [.y I16] [.z I32])", module, log, a) ;

    typedef struct {
        int32_t x;
        int16_t y;
        int8_t z;
    } AlignedStruct;
    run_toplevel("(def AS Struct [.x I32] [.y I16] [.z I8])", module, log, a) ;

    if (test_start(log, mv_string("struct"))) {
        Point expected = (Point) {.x = 3, .y = -5};
        test_toplevel_eq("(struct [.x 3] [.y -5])", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("struct-nested"))) {
        typedef struct {
            uint64_t val;
            Point p2;
        } Nest;
        Nest expected = (Nest) {.val = -8765, .p2.x = -57, .p2.y = 127};
        test_toplevel_eq("(struct [.v1 -8765] [.p2 (struct [.x -57] [.y 127])])", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("struct-refersed"))) {
        Point expected = (Point) {.x = 3, .y = -5};
        test_toplevel_eq("(struct (Struct [.x I64] [.y I64]) [.y -5] [.x 3])", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("struct-alignment"))) {
        Point expected = (Point) {.x = 3, .y = -5};
        test_toplevel_eq("(struct [.x 3] [.y -5])", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("struct-space-misaligned"))) {
        MisalignedStruct expected = (MisalignedStruct) {.x = 3, .y = -5, .z = 4};
        test_toplevel_eq("(struct MAS [.x 3] [.y -5] [.z 4])", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("struct-packed-aligned"))) {
        AlignedStruct expected = (AlignedStruct) {.x = 1527, .y = -5, .z = 2};
        test_toplevel_eq("(struct AS [.x 1527] [.y -5] [.z 2])", &expected, module, log, a) ;
    }

    // -----------------------------------------------------
    // 
    // Enumeration
    // 
    // -----------------------------------------------------

    typedef struct {
        uint64_t tag;
        int32_t x;
        int32_t y;
    } SimpleEnum;
    run_toplevel("(def SE Enum [:simple I32 I32])", module, log, a) ;

    if (test_start(log, mv_string("enum-simple"))) {
        SimpleEnum expected = (SimpleEnum) {.tag = 0, .x = 1086, .y = -200};
        test_toplevel_eq("(SE:simple 1086 -200)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("match-simple"))) {
        int32_t expected = 886;
        test_toplevel_eq("(match (SE:simple 1086 -200) [[:simple x y] (i32.+ x y)])", &expected, module, log, a) ;
    }

    run_toplevel("(def add proc [val] match val [[:simple x y] (i32.+ x y)])", module, log, a) ;
    if (test_start(log, mv_string("match-proc-simple"))) {
        int32_t expected = 886;
        test_toplevel_eq("(add (SE:simple 1086 -200))", &expected, module, log, a) ;
    }
}
