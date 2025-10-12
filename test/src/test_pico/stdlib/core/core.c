#include <string.h>
#include "platform/memory/static.h"

#include "pico/stdlib/core.h"
#include "pico/stdlib/extra.h"

#include "test_pico/stdlib/components.h"
#include "test_pico/helper.h"

#define RUN(str) run_toplevel(str, module, context); refresh_env(env, a)
#define TEST_EQ(str) test_toplevel_eq(str, &expected, module, context)
#define TEST_MEM(str) test_toplevel_mem(str, &expected, start, sizeof(expected), module, context)

void run_pico_stdlib_core_tests(TestLog *log, Module* module, Environment* env, Target target, Allocator *a) {
    TestContext context = (TestContext) {
        .env = env,
        .a = a,
        .log = log,
        .target = target,
    };

    // -----------------------------------------------------
    // 
    //  Widen/Narrow
    // 
    // -----------------------------------------------------

    if (test_start(log, mv_string("widen-u32-u64"))) {
        int64_t expected = 678;
        TEST_EQ("(widen (is 678 U32) U64)");
    }

    // -------------------------------------------------------------------------
    //
    //     Static control and binding - seq/let
    //
    // -------------------------------------------------------------------------

    if (test_start(log, mv_string("simple-let"))) {
        int64_t expected = 3;
        TEST_EQ("(let [x 3] x)");
    }

    if (test_start(log, mv_string("simple-let"))) {
        int32_t expected = -3;
        TEST_EQ("(let [x (is -3 I32)] x)");
    }

    if (test_start(log, mv_string("let-large-value"))) {
        // Test that when a value is moved up the stack, if it is larger than the 
        // total binding size, it does not overwrite itself.
        typedef struct {int64_t x; int64_t y;} Pr;
        Pr expected = (Pr){ .x = 5, .y = 2 };
        TEST_EQ("(let [x 2] (struct [.x 5] [.y x]))");
    }

    if (test_start(log, mv_string("simple-sequence"))) {
        int64_t expected = 3;
        TEST_EQ("(seq 1 2 3)");
    }

    if (test_start(log, mv_string("let-in-sequence"))) {
        int64_t expected = 2;
        TEST_EQ("(seq [let! x 2] x)");
    }

    if (test_start(log, mv_string("let-many-in-sequence"))) {
        int64_t expected = 5;
        TEST_EQ("(seq [let! x 2] [let! y 3] (u32.+ x y))");
    }

    if (test_start(log, mv_string("let-large-in-sequence"))) {
        typedef struct {int64_t x; int64_t y;} Pr;
        Pr expected = (Pr){ .x = 122, .y = -79 };
        TEST_EQ("(seq [let! x 122] (struct [.x 122] [.y -79]))");
    }

    // -------------------------------------------------------------------------
    //
    //     Labels 
    //
    // -------------------------------------------------------------------------

    if (test_start(log, mv_string("labels-simple"))) {
        int64_t expected = 27;
        TEST_EQ("(labels 27)");
    }

    if (test_start(log, mv_string("labels-single-goto"))) {
        int64_t expected = -8;
        TEST_EQ("(labels (go-to start) [start -8])");
    }

    if (test_start(log, mv_string("labels-pass-var"))) {
        int64_t expected = 3;
        TEST_EQ("(labels (go-to start 3) [start [x] x])");
    }

    if (test_start(log, mv_string("labels-loop"))) {
        int64_t expected = 10;
        TEST_EQ("(labels (go-to loop 0) [loop [x] (if (i64.< x 10) (go-to loop (i64.+ x 1)) x)])");
    }

    // -------------------------------------------------------------------------
    //
    //     Dynamic binding - dynamic/use/bind/set
    //
    // -------------------------------------------------------------------------

    RUN("(def dvar dynamic -10)");
    if (test_start(log, mv_string("dynamic-use"))) {
        int64_t expected = -10;
        TEST_EQ("(use dvar)");
    }

    if (test_start(log, mv_string("dynamic-set"))) {
        int64_t expected = 3;
        RUN("(set dvar 3)");
        TEST_EQ("(use dvar)");
    }

    RUN("(def ldvar dynamic struct [.x -10] [.y 10])");
    if (test_start(log, mv_string("large-dynamic-use"))) {
        int64_t expected[2] = {-10, 10};
        TEST_EQ("(use ldvar)");
    }

    // -----------------------------------------------------
    // 
    //      Procedures, Application and all
    // 
    // -----------------------------------------------------
    if (test_start(log, mv_string("proc-const"))) {
        int64_t expected = -985;
        TEST_EQ("((proc [(x U64)] -985) 127)");
    }

    if (test_start(log, mv_string("proc-id"))) {
        int64_t expected = 127;
        TEST_EQ("((proc [x] x) 127)");
    }

    if (test_start(log, mv_string("proc-add"))) {
        int64_t expected = 5;
        TEST_EQ("((proc [x y] (u64.+ x y)) 2 3)");
    }

    if (test_start(log, mv_string("proc-higher-order"))) {
        int64_t expected = -128;
        TEST_EQ("((proc [(f (Proc [I64 I64] I64)) x y] f x y) i64.+ -256 128)");
    }

    if (test_start(log, mv_string("proc-all-id"))) {
        int64_t expected = -75;
        TEST_EQ("((all [A] proc [(x A)] x) -75)");
    }


    // -------------------------------------------------------------------------
    //
    // Conditional testing
    //
    // -------------------------------------------------------------------------

    if (test_start(log, mv_string("if-true"))) {
        int64_t expected = 2;
        TEST_EQ("(if :true 2 3)");
    }

    if (test_start(log, mv_string("if-false"))) {
        int64_t expected = 3;
        TEST_EQ("(if :false 2 3)");
    }

    RUN("(def if-proc proc [(b Bool) (x I32) (y I32)] if b x y)");
    if (test_start(log, mv_string("if-proc-true"))) {
        int64_t expected = 1;
        TEST_EQ("(if-proc :true 1 2)");
    }

    if (test_start(log, mv_string("if-proc-false"))) {
        int64_t expected = -12;
        TEST_EQ("(if-proc :false 1 -12)");
    }

    RUN("(def if-proc-inv proc [(b Bool) (x I32) (y I32)] if b y x)");
    if (test_start(log, mv_string("if-proc-inv-true"))) {
        int64_t expected = -45;
        TEST_EQ("(if-proc-inv :true 1 -45)");
    }

    if (test_start(log, mv_string("if-proc-inv-false"))) {
        int64_t expected = 720;
        TEST_EQ("(if-proc-inv :false 720 -12)");
    }

    typedef struct {
        uint64_t tag;
        int64_t num;
    } MaybeI64;

    RUN("(def if-make-maybe proc [b] if b :none (:some 64))");
    if (test_start(log, mv_string("if-match-proc-some"))) {
        MaybeI64 expected = (MaybeI64){.tag = 0, .num = 64};
        TEST_EQ("(if-make-maybe :false)");
    }

    if (test_start(log, mv_string("if-match-proc-none"))) {
        MaybeI64 expected = (MaybeI64){.tag = 1};
        TEST_EQ("(if-make-maybe :true)");
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
    RUN("(def NestInner Struct [.x I32] [.y I32] [.z I32])") ;

    typedef struct {
        NestInner n1;
        NestInner n2;
    } NestOuter;
    RUN("(def NestOuter Struct [.n1 NestInner] [.n2 NestInner])") ;

    typedef struct {
        int8_t x;
        int16_t y;
        int32_t z;
    } MisalignedStruct;
    RUN("(def MAS Struct [.x I8] [.y I16] [.z I32])") ;

    typedef struct {
        int8_t x;
        int8_t y;
    } SmallStruct;
    RUN("(def SML Struct [.x I8] [.y I8])") ;

    typedef struct {
        int32_t x;
        int16_t y;
        int8_t z;
    } AlignedStruct;
    RUN("(def AS Struct [.x I32] [.y I16] [.z I8])") ;

    if (test_start(log, mv_string("struct"))) {
        Point expected = (Point) {.x = 3, .y = -5};
        TEST_EQ("(struct [.x 3] [.y -5])");
    }

    if (test_start(log, mv_string("struct-with-base"))) {
        Point expected = (Point) {.x = 10, .y = -15};
        TEST_EQ("(struct (struct [.x 3] [.y -15]) [.x 10])");
    }

    if (test_start(log, mv_string("struct-nested-v1"))) {
        typedef struct {
            uint64_t val;
            Point p2;
        } Nest;
        Nest expected = (Nest) {.val = -8765, .p2.x = -57, .p2.y = 127};
        TEST_EQ("(struct [.v1 -8765] [.p2 (struct [.x -57] [.y 127])])");
    }

    if (test_start(log, mv_string("struct-nested-v2"))) {
        NestOuter expected = (NestOuter) {.n1.x = -8765, .n1.y = -57, .n1.z = 127, .n2.x = 875, .n2.y = 52, .n2.z = -122};
        TEST_EQ("(struct NestOuter [.n1 (struct [.x -8765] [.y -57] [.z 127])] [.n2 (struct [.x 875] [.y 52] [.z -122])])");
    }

    if (test_start(log, mv_string("struct-reversed"))) {
        Point expected = (Point) {.x = 3, .y = -5};
        TEST_EQ("(struct (Struct [.x I64] [.y I64]) [.y -5] [.x 3])");
    }

    if (test_start(log, mv_string("struct-alignment"))) {
        Point expected = (Point) {.x = 3, .y = -5};
        TEST_EQ("(struct [.x 3] [.y -5])");
    }

    if (test_start(log, mv_string("struct-space-misaligned"))) {
        MisalignedStruct expected = (MisalignedStruct) {.x = 3, .y = -5, .z = 4};
        TEST_EQ("(struct MAS [.x 3] [.y -5] [.z 4])");
    }

    if (test_start(log, mv_string("struct-packed-aligned"))) {
        AlignedStruct expected = (AlignedStruct) {.x = 1527, .y = -5, .z = 2};
        TEST_EQ("(struct AS [.x 1527] [.y -5] [.z 2])");
    }

    if (test_start(log, mv_string("struct-small"))) {
        SmallStruct expected = (SmallStruct) {.x = 3, .y = -8};
        TEST_EQ("(struct SML [.x 3] [.y -8])");
    }

    if (test_start(log, mv_string("struct-alter"))) {
        SmallStruct expected = (SmallStruct) {.x = 3, .y = 12};
        TEST_EQ("(struct (struct SML [.x 3] [.y -8]) [.y 12])");
    }

    // Projection
    // ----------

    if (test_start(log, mv_string("project-sturct-misaligned"))) {
        int32_t expected = 4;
        TEST_EQ("(seq [let! st (struct MAS [.x 3] [.y -5] [.z 4])] st.z)");
    }

    if (test_start(log, mv_string("project-struct-small"))) {
        int8_t expected = -8;
        TEST_EQ("(seq [let! st (struct SML [.x 3] [.y -8])] st.y)");
    }

    RUN("(def thrice struct NestInner [.x -12] [.y 3] [.z 1])") ;
    if (test_start(log, mv_string("project-point3-x"))) {
        int32_t expected = -12;
        TEST_EQ("(seq thrice.x)");
    }

    if (test_start(log, mv_string("project-point3-y"))) {
        int32_t expected = 3;
        TEST_EQ("(seq thrice.y)");
    }

    if (test_start(log, mv_string("project-point3-z"))) {
        int32_t expected = 1;
        TEST_EQ("(seq thrice.z)");
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
    RUN("(def SE Enum [:simple I32 I32])");
    RUN("(def PSE Enum [:simple I32 I32])");

    if (test_start(log, mv_string("enum-simple"))) {
        SimpleEnum expected = (SimpleEnum) {.tag = 0, .x = 1086, .y = -200};
        TEST_EQ("(SE:simple 1086 -200)");
    }

    if (test_start(log, mv_string("match-simple"))) {
        int32_t expected = 886;
        TEST_EQ("(match (SE:simple 1086 -200) [[:simple x y] (i32.+ x y)])");
    }

    RUN("(def add proc [val] match val [[:simple x y] (i32.+ x y)])");
    if (test_start(log, mv_string("match-proc-simple"))) {
        int32_t expected = 886;
        TEST_EQ("(add (SE:simple 1086 -200))");
    }

    if (test_start(log, mv_string("match-struct-inner"))) {
        int32_t expected = 900;
        TEST_EQ("(match (:some (struct [.x (is 1100 I32)] [.y (is -200 I32)]))\n"
                 "  [[:some pr] (i32.+ pr.x pr.y)])");
    }

    // -----------------------------------------------------
    // 
    //      Type Metadata (size, align, offset)
    // 
    // -----------------------------------------------------

    if (test_start(log, mv_string("test-size-of-I64"))) {
        uint64_t expected = 8;
        TEST_EQ("(size-of I64)");
    }

    if (test_start(log, mv_string("test-size-of-structs"))) {
        uint64_t expected = 12;
        TEST_EQ("(size-of (Struct [.x U8] [.y I32] [.z U16]))");
    }

    if (test_start(log, mv_string("test-align-of-I64"))) {
        uint64_t expected = 8;
        TEST_EQ("(align-of I64)");
    }

    if (test_start(log, mv_string("test-align-of-structs"))) {
        uint64_t expected = 4;
        TEST_EQ("(align-of (Struct [.x U8] [.y I32] [.z U16]))");
    }

    if (test_start(log, mv_string("test-offset-point"))) {
        uint64_t expected = 8;
        TEST_EQ("(offset-of y (Struct [.x I64] [.y I64]))");
    }

    // -----------------------------------------------------
    // 
    //  Instances
    // 
    // -----------------------------------------------------

    if (test_start(log, mv_string("instance-const"))) {
        int64_t expected = 77;
        RUN("(def Inhabited Trait [A] [.value A])");
        // TODO (BUG)
        // swapping the order of below statements gives an 'ambiguous instance' error?
        RUN("(def get-value all [A] proc {(in (Inhabited A))} [(x A)] in.value)");
        RUN("(def i64-inhabited instance (Inhabited I64) [.value 77])");

        TEST_EQ("(get-value {I64} 5)");
    }

    if (test_start(log, mv_string("instance-mval"))) {
        int64_t expected = -77;
        RUN("(def MultiInhabited Trait [A] [.val-1 A] [.val-2 A])");
        // TODO (BUG)
        // swapping the order of below statements gives an 'ambiguous instance' error?
        RUN("(def get-second-value all [A] proc {(in (MultiInhabited A))} [(x A)] in.val-2)");
        RUN("(def i64-multi-inhabited instance (MultiInhabited I64) [.val-1 77] [.val-2 -77])");

        TEST_EQ("(get-second-value {I64} 5)");
    }

    if (test_start(log, mv_string("instance-mval"))) {
        uint64_t expected = 43;
        RUN("(def MultiConstInhabited Trait [A] [.val-1 A] [.val-2 U64])");
        RUN("(def get-snd-const-value all [A] proc {(in (MultiConstInhabited A))} [(x A)] in.val-2)");
        RUN("(def i64-multi-const-inhabited instance (MultiConstInhabited I64) [.val-1 77] [.val-2 43])");

        TEST_EQ("(get-snd-const-value {I64} 5)");
    }

    // TODO: enable me!
    /* if (test_start(log, mv_string("instance-const-unaligned"))) { */
    /*     int64_t expected = -98; */
    /*     RUN("(def Inhabited Trait [A] [.value A])"); */
    /*     // TODO (BUG) */
    /*     // swapping the order of below statements gives an 'ambiguous instance' error? */
    /*     RUN("(def get-value all [A] proc {(in (Inhabited A))} [(x A)] in.value)"); */
    /*     RUN("(def i8-inhabited instance (Inhabited I8) [.value -98])"); */

    /*     TEST_EQ("(get-value {I8} 5)"); */
    /* } */

    // -----------------------------------------------------
    // 
    //      Miscellaneous Bits and Bobs
    // 
    // -----------------------------------------------------

    if (test_start(log, mv_string("size-of-I64"))) {
        uint64_t expected = 8;
        TEST_EQ("(size-of I64)");
    }

    if (test_start(log, mv_string("size-of-I8"))) {
        uint64_t expected = 1;
        TEST_EQ("(size-of I8)");
    }

    if (test_start(log, mv_string("align-of-I64"))) {
        uint64_t expected = 8;
        TEST_EQ("(size-of I64)");
    }

    if (test_start(log, mv_string("align-of-I8"))) {
        uint64_t expected = 1;
        TEST_EQ("(align-of I8)");
    }

    if (test_start(log, mv_string("offset-of-simple-0"))) {
        uint64_t expected = 0;
        TEST_EQ("(offset-of x (Struct [.x I8] [.y I16]))");
    }

    if (test_start(log, mv_string("offset-of-simple-nonzero"))) {
        uint64_t expected = 1;
        TEST_EQ("(offset-of y (Struct [.x I8] [.y I8]))");
    }

    if (test_start(log, mv_string("offset-of-complex-nonzero"))) {
        uint64_t expected = 2;
        TEST_EQ("(offset-of y (Struct [.x I8] [.y I16]))");
    }

    // -----------------------------------------------------
    // 
    //      Core functions (notably, load/store)
    // 
    // -----------------------------------------------------
    
    void* mem = mem_alloc(128, a);
    Allocator old = get_std_current_allocator();
    void* start;
    {
        Allocator sta = mk_static_allocator(mem, 128);
        start = mem_alloc(8, &sta);
    }

    if (test_start(log, mv_string("test-load-i64"))) {
        Allocator sta = mk_static_allocator(mem, 128);
        *(int64_t*)start = 8;

        set_std_current_allocator(sta);
        uint64_t expected = 8;
        TEST_EQ("(let [addr malloc (size-of I64)] (load {I64} addr))");
    }

    if (test_start(log, mv_string("test-load-i8"))) {
        Allocator sta = mk_static_allocator(mem, 128);
        *(int8_t*)start = -5;

        set_std_current_allocator(sta);
        int8_t expected = -5;
        TEST_EQ("(let [addr malloc (size-of I8)] (load {I8} addr))");
    }

    if (test_start(log, mv_string("test-load-struct"))) {
        Allocator sta = mk_static_allocator(mem, 128);
        typedef struct { int64_t x; int64_t y; } IPr;
        set_std_current_allocator(sta);
        IPr expected = {.x = 25, .y = -5};
        *(IPr*)(start) = expected;
        TEST_EQ("(let [addr malloc 16] (load {(Struct [.x I64] [.y I64])} addr))");
    }

    if (test_start(log, mv_string("test-store-i8"))) {
        Allocator sta = mk_static_allocator(mem, 128);
        int8_t expected = -5;

        set_std_current_allocator(sta);
        RUN("(let [addr malloc (size-of I8)] (store {I8} addr -5))");
        TEST_MEM("(let [addr malloc (size-of I8)] (store {I8} addr -5))");
    }

    if (test_start(log, mv_string("test-store-i64"))) {
        Allocator sta = mk_static_allocator(mem, 128);
        int64_t expected = 197231987;

        set_std_current_allocator(sta);
        TEST_MEM("(let [addr malloc (size-of I64)] (store {I64} addr 197231987))");
    }

    if (test_start(log, mv_string("test-store-i64"))) {
        Allocator sta = mk_static_allocator(mem, 128);
        typedef struct { int64_t x; int64_t y; } IPr;
        IPr expected = {.x = -123, .y = 9713};

        set_std_current_allocator(sta);
        TEST_MEM("(let [addr malloc 16] (store addr (struct [.x -123] [.y 9713])))");
    }

    set_std_current_allocator(old);
    mem_free(mem, a);
}
