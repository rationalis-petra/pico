#include "platform/memory/executable.h"
#include "platform/memory/arena.h"

#include "pico/stdlib/stdlib.h"
#include "pico/stdlib/extra.h"

#include "test_pico/pipeline/pipeline.h"
#include "test_pico/pipeline/helper.h"

void run_pico_pipeline_tests(RunDescriptor to_run, TestLog* log, Allocator* a) {
    // Setup
    Allocator exalloc = mk_executable_allocator(a);
    Allocator arena = mk_arena_allocator(4096, a);
    Assembler* ass = mk_assembler(&exalloc);
    Package* base = get_base_package();

    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(3, a),
    };
    add_import_all(&imports.clauses, a, 1, "core");
    add_import_all(&imports.clauses, a, 1, "num");
    add_import_all(&imports.clauses, a, 1, "extra");
    add_import_all(&imports.clauses, a, 2, "data", "array");
    add_import_all(&imports.clauses, a, 1, "data");

    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("pipeline-test-module")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, base, NULL, a);
    delete_module_header(header);


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

    {
        test_start(log, mv_string("int-literal"));
        int64_t expected = -10;
        test_toplevel_eq("-10", &expected, module, log, a) ;
    }

    // TODO (FEAT): move libraries out into their own section
    {
        test_start(log, mv_string("unsigned-add"));
        uint64_t expected = 3;
        test_toplevel_eq("(u64.+ 1 2)", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("signed-add"));
        int64_t expected = 6;
        test_toplevel_eq("(i64.+ 4 2)", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("signed-sub"));
        int64_t expected = -1;
        test_toplevel_eq("(i64.- 1 2)", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("unsigned-mul"));
        int64_t expected = 24;
        test_toplevel_eq("(i64.* 4 6)", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("and-ff"));
        uint8_t expected = 0;
        test_toplevel_eq("(bool.and :false :true)", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("and-ft"));
        uint8_t expected = 0;
        test_toplevel_eq("(bool.and :false :true)", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("and-tt"));
        uint8_t expected = 1;
        test_toplevel_eq("(bool.and :true :true)", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("or-ff"));
        uint8_t expected = 0;
        test_toplevel_eq("(bool.or :false :false)", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("or-ft"));
        uint8_t expected = 1;
        test_toplevel_eq("(bool.or :false :true)", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("or-tt"));
        uint8_t expected = 1;
        test_toplevel_eq("(bool.or :true :true)", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("not-t"));
        bool expected = false;
        test_toplevel_eq("(bool.not :true)", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("not-f"));
        bool expected = true;
        test_toplevel_eq("(bool.not :false)", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("simple-let"));
        int64_t expected = 3;
        test_toplevel_eq("(let [x 3] x)", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("simple-sequence"));
        int64_t expected = 3;
        test_toplevel_eq("(seq 1 2 3)", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("let-in-sequence"));
        int64_t expected = 2;
        test_toplevel_eq("(seq [let! x 2] x)", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("let-many-in-sequence"));
        int64_t expected = 5;
        test_toplevel_eq("(seq [let! x 2 y 3] (u32.+ x y))", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("struct"));
        Point expected = (Point) {.x = 3, .y = -5};
        test_toplevel_eq("(struct [.x 3] [.y -5])", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("struct-nested"));
        typedef struct {
            uint64_t val;
            Point p2;
        } Nest;
        Nest expected = (Nest) {.val = -8765, .p2.x = -57, .p2.y = 127};
        test_toplevel_eq("(struct [.v1 -8765] [.p2 (struct [.x -57] [.y 127])])", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("struct-refersed"));
        Point expected = (Point) {.x = 3, .y = -5};
        test_toplevel_eq("(struct (Struct [.x I64] [.y I64]) [.y -5] [.x 3])", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("struct-alignment"));
        Point expected = (Point) {.x = 3, .y = -5};
        test_toplevel_eq("(struct [.x 3] [.y -5])", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("struct-space-misaligned"));
        MisalignedStruct expected = (MisalignedStruct) {.x = 3, .y = -5, .z = 4};
        test_toplevel_eq("(struct MAS [.x 3] [.y -5] [.z 4])", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("struct-packed-aligned"));
        AlignedStruct expected = (AlignedStruct) {.x = 1527, .y = -5, .z = 2};
        test_toplevel_eq("(struct AS [.x 1527] [.y -5] [.z 2])", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("pair-simple"));
        Point expected = (Point) {.x = 12397, .y = -35};
        test_toplevel_eq("(struct (Pair I64 I64) [._1 12397] [._2 -35])", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("pair-fn"));
        run_toplevel("(def pair-fn proc [x y] (struct (Pair I64 I64) [._1 x] [._2 y]))", module, log, a) ;
        Point expected = (Point) {.x = -987, .y = 935};
        test_toplevel_eq("(pair-fn -987 935)", &expected, module, log, a) ;
    }

    {
        typedef struct I64Pair {int64_t x, y;} I64Pair;
        test_start(log, mv_string("pair-poly"));
        Point expected = (Point) {.x = 1432, .y = -120938};
        test_toplevel_eq("(pair.pair 1432 -120938)", &expected, module, log, a) ;
    }

    {
        // TODO (BUG): this leaks - set current allocator?
        Allocator current_old = get_std_current_allocator();
        set_std_current_allocator(arena);
        test_start(log, mv_string("Instnatiate Implicit with Default UVar"));
        uint64_t expected = 10;
        test_toplevel_eq("(seq [let! arr (mk-array 1 1)] (aset 0 10 arr) (elt 0 arr))",
            &expected, module, log, a) ;
        set_std_current_allocator(current_old);
    }

    {
        test_start(log, mv_string("print"));
        test_toplevel_stdout("(print \"test\")", "test", module, log, a) ;
    }

    {
        Allocator current_old = get_std_current_allocator();
        set_std_current_allocator(arena);
        test_start(log, mv_string("single-for-upto"));
        test_toplevel_stdout("(loop [for i from 1 upto 10] (print (u64.to-string i)))", "12345678910", module, log, a) ;
        set_std_current_allocator(current_old);
    }

    {
        Allocator current_old = get_std_current_allocator();
        set_std_current_allocator(arena);
        test_start(log, mv_string("single-for-below"));
        test_toplevel_stdout("(loop [for i from 1 below 10] (print (u64.to-string i)))", "123456789", module, log, a) ;
        set_std_current_allocator(current_old);
    }

    {
        Allocator current_old = get_std_current_allocator();
        set_std_current_allocator(arena);
        test_start(log, mv_string("single-for-downto"));
        test_toplevel_stdout("(loop [for i from 10 downto 1] (print (u64.to-string i)))", "10987654321", module, log, a) ;
        set_std_current_allocator(current_old);
    }

    {
        Allocator current_old = get_std_current_allocator();
        set_std_current_allocator(arena);
        test_start(log, mv_string("single-for-below"));
        test_toplevel_stdout("(loop [for i from 10 above 1] (print (u64.to-string i)))", "1098765432", module, log, a) ;
        set_std_current_allocator(current_old);
    }

    delete_module(module);
    delete_assembler(ass);
    release_executable_allocator(exalloc);
    release_arena_allocator(arena);
}
