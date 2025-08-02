#include "pico/stdlib/core.h"

#include "test_pico/stdlib/components.h"
#include "test_pico/helper.h"


void run_pico_stdlib_core_tests(TestLog *log, Module* module, Allocator *a) {
    // -----------------------------------------------------
    // 
    //  Widen/Narrow
    // 
    // -----------------------------------------------------
    if (test_start(log, mv_string("widen-u32-u64"))) {
        int64_t expected = 678;
        test_toplevel_eq("(widen (is 678 U32) U64)", &expected, module, log, a) ;
    }


    // -------------------------------------------------------------------------
    //
    // Static control and binding - seq/let
    //
    // -------------------------------------------------------------------------

    if (test_start(log, mv_string("simple-let"))) {
        int64_t expected = 3;
        test_toplevel_eq("(let [x 3] x)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("simple-let"))) {
        int32_t expected = -3;
        test_toplevel_eq("(let [x (is -3 I32)] x)", &expected, module, log, a) ;
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


    // -----------------------------------------------------
    // 
    //      Procedures, Application and all
    // 
    // -----------------------------------------------------
    if (test_start(log, mv_string("proc-const"))) {
        int64_t expected = -985;
        test_toplevel_eq("((proc [(x U64)] -985) 127)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("proc-id"))) {
        int64_t expected = 127;
        test_toplevel_eq("((proc [x] x) 127)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("proc-add"))) {
        int64_t expected = 5;
        test_toplevel_eq("((proc [x y] (u64.+ x y)) 2 3)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("proc-higher-order"))) {
        int64_t expected = -128;
        test_toplevel_eq("((proc [(f (Proc [I64 I64] I64)) x y] f x y) i64.+ -256 128)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("proc-all-id"))) {
        int64_t expected = -75;
        test_toplevel_eq("((all [A] proc [(x A)] x) -75)", &expected, module, log, a) ;
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

    typedef struct {
        uint64_t tag;
        int64_t num;
    } MaybeI64;

    run_toplevel("(def if-make-maybe proc [b] if b :none (:some 64))", module, log, a) ;
    if (test_start(log, mv_string("if-match-proc-some"))) {
        MaybeI64 expected = (MaybeI64){.tag = 0, .num = 64};
        test_toplevel_eq("(if-make-maybe :false)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("if-match-proc-none"))) {
        MaybeI64 expected = (MaybeI64){.tag = 1};
        test_toplevel_eq("(if-make-maybe :true)", &expected, module, log, a) ;
    }

    // -----------------------------------------------------
    // 
    //      Struct
    // 
    // -----------------------------------------------------

    typedef struct {
        int64_t x;
        int64_t y;
    } Point;

    typedef struct {
        int32_t x;
        int32_t y;
        int32_t z;
    } NestInner;
    run_toplevel("(def NestInner Struct [.x I32] [.y I32] [.z I32])", module, log, a) ;

    typedef struct {
        NestInner n1;
        NestInner n2;
    } NestOuter;
    run_toplevel("(def NestOuter Struct [.n1 NestInner] [.n2 NestInner])", module, log, a) ;

    typedef struct {
        int8_t x;
        int16_t y;
        int32_t z;
    } MisalignedStruct;
    run_toplevel("(def MAS Struct [.x I8] [.y I16] [.z I32])", module, log, a) ;

    typedef struct {
        int8_t x;
        int8_t y;
    } SmallStruct;
    run_toplevel("(def SML Struct [.x I8] [.y I8])", module, log, a) ;

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

    if (test_start(log, mv_string("struct-with-base"))) {
        Point expected = (Point) {.x = 10, .y = -15};
        test_toplevel_eq("(struct (struct [.x 3] [.y -15]) [.x 10])", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("struct-nested-v1"))) {
        typedef struct {
            uint64_t val;
            Point p2;
        } Nest;
        Nest expected = (Nest) {.val = -8765, .p2.x = -57, .p2.y = 127};
        test_toplevel_eq("(struct [.v1 -8765] [.p2 (struct [.x -57] [.y 127])])", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("struct-nested-v2"))) {
        NestOuter expected = (NestOuter) {.n1.x = -8765, .n1.y = -57, .n1.z = 127, .n2.x = 875, .n2.y = 52, .n2.z = -122};
        test_toplevel_eq("(struct NestOuter [.n1 (struct [.x -8765] [.y -57] [.z 127])] [.n2 (struct [.x 875] [.y 52] [.z -122])])", &expected, module, log, a) ;
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

    if (test_start(log, mv_string("struct-small"))) {
        SmallStruct expected = (SmallStruct) {.x = 3, .y = -8};
        test_toplevel_eq("(struct SML [.x 3] [.y -8])", &expected, module, log, a) ;
    }

    // Projection
    if (test_start(log, mv_string("project-sturct-misaligned"))) {
        int32_t expected = 4;
        test_toplevel_eq("(seq [let! st (struct MAS [.x 3] [.y -5] [.z 4])] st.z)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("project-struct-small"))) {
        int8_t expected = -8;
        test_toplevel_eq("(seq [let! st (struct SML [.x 3] [.y -8])] st.y)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("project-struct-small"))) {
        int8_t expected = -8;
        test_toplevel_eq("(seq [let! st (struct SML [.x 3] [.y -8])] st.y)", &expected, module, log, a) ;
    }

    run_toplevel("(def thrice struct NestInner [.x -12] [.y 3] [.z 1])", module, log, a) ;
    if (test_start(log, mv_string("project-point3-x"))) {
        int32_t expected = -12;
        test_toplevel_eq("(seq thrice.x)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("project-point3-y"))) {
        int32_t expected = 3;
        test_toplevel_eq("(seq thrice.y)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("project-point3-z"))) {
        int32_t expected = 1;
        test_toplevel_eq("(seq thrice.z)", &expected, module, log, a) ;
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
    run_toplevel("(def PSE Enum [:simple I32 I32])", module, log, a) ;

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
        test_toplevel_eq("(add (SE:simple 1086 -200))", &expected, module, log, a);
    }

    if (test_start(log, mv_string("match-struct-inner"))) {
        int32_t expected = 900;
        test_toplevel_eq("(match (:some (struct [.x (is 1100 I32)] [.y (is -200 I32)]))\n"
                 "  [[:some pr] (i32.+ pr.x pr.y)])", &expected, module, log, a) ;
    }
    // -----------------------------------------------------
    // 
    //      Type Metadata (size, align, offset)
    // 
    // -----------------------------------------------------
    if (test_start(log, mv_string("test-size-of-I64"))) {
        uint64_t expected = 8;
        test_toplevel_eq("(size-of I64)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("test-size-of-structs"))) {
        uint64_t expected = 12;
        test_toplevel_eq("(size-of (Struct [.x U8] [.y I32] [.z U16]))", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("test-align-of-I64"))) {
        uint64_t expected = 8;
        test_toplevel_eq("(align-of I64)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("test-align-of-structs"))) {
        uint64_t expected = 4;
        test_toplevel_eq("(align-of (Struct [.x U8] [.y I32] [.z U16]))", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("test-offset-point"))) {
        uint64_t expected = 8;
        test_toplevel_eq("(offset-of y (Struct [.x I64] [.y I64]))", &expected, module, log, a) ;
    }

    /* if (test_start(log, mv_string("proc-const"))) { */
    /*     int64_t expected = -985; */
    /*     test_toplevel_eq("(Proc [U64 U64] U64)", &expected, module, log, a) ; */
    /*     run_toplevel("(Proc [U64 U64] U64)", module, log, a) ; */
    /* } */

    // -----------------------------------------------------
    // 
    //      Type Constructors
    // 
    // -----------------------------------------------------
    if (test_start(log, mv_string("I64"))) {
        PiType* expected = mk_prim_type(a, Int_64);
        test_toplevel_eq("I64", &expected, module, log, a) ;
        delete_pi_type_p(expected, a);
    }

    if (test_start(log, mv_string("proc-const"))) {
        PiType* expected = mk_proc_type(a, 2, mk_prim_type(a, Int_64), mk_prim_type(a, Int_64), mk_prim_type(a, Int_64));
        test_toplevel_eq("(Proc [I64 I64] I64)", &expected, module, log, a) ;
        delete_pi_type_p(expected, a);
    }

    if (test_start(log, mv_string("recursive-named"))) {
        PiType* vty = mk_var_type(a, "Element");
        PiType* lty = mk_app_type(a, get_list_type(), vty);
        PiType* expected = mk_named_type(a, "Element",
                                         mk_struct_type(a, 1, "children", lty));
        test_toplevel_eq("(Named Element Struct [.chidren (List Element)])", &expected, module, log, a) ;
        delete_pi_type_p(expected, a);
        delete_pi_type_p(vty, a);
    }
}
