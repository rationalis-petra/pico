#include "test_pico/stdlib/components.h"
#include "test_pico/stdlib/helper.h"

void run_pico_stdlib_core_tests(TestLog *log, Module* module, Allocator *a) {
    // -------------------------------------------------------------------------
    //
    // Types: types used in testing
    //
    // -------------------------------------------------------------------------

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

}
