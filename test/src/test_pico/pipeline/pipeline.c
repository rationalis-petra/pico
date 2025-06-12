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
    Package* base = base_package(ass, a, a);

    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(3, a),
    };
    add_import_all(&imports.clauses, a, 1, "core");
    add_import_all(&imports.clauses, a, 1, "num");
    add_import_all(&imports.clauses, a, 1, "extra");
    add_import_all(&imports.clauses, a, 2, "data", "array");

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
        test_toplevel("-10", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("Addition"));
        uint64_t expected = 3;
        test_toplevel("(u64.+ 1 2)", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("Subtraction"));
        int64_t expected = -1;
        test_toplevel("(i64.- 1 2)", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("Subtraction"));
        int64_t expected = -1;
        test_toplevel("(i64.- 1 2)", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("simple-let"));
        int64_t expected = 3;
        test_toplevel("(let [x 3] x)", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("simple-sequence"));
        int64_t expected = 3;
        test_toplevel("(seq 1 2 3)", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("let-in-sequence"));
        int64_t expected = 2;
        test_toplevel("(seq [let! x 2] x)", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("let-many-in-sequence"));
        int64_t expected = 5;
        test_toplevel("(seq [let! x 2 y 3] (u32.+ x y))", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("struct"));
        Point expected = (Point) {.x = 3, .y = -5};
        test_toplevel("(struct [.x 3] [.y -5])", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("struct-alignment"));
        Point expected = (Point) {.x = 3, .y = -5};
        test_toplevel("(struct [.x 3] [.y -5])", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("struct-space-misaligned"));
        MisalignedStruct expected = (MisalignedStruct) {.x = 3, .y = -5, .z = 4};
        test_toplevel("(struct MAS [.x 3] [.y -5] [.z 4])", &expected, module, log, a) ;
    }

    {
        test_start(log, mv_string("struct-packed-aligned"));
        AlignedStruct expected = (AlignedStruct) {.x = 1527, .y = -5, .z = 2};
        test_toplevel("(struct AS [.x 1527] [.y -5] [.z 2])", &expected, module, log, a) ;
    }

    {
        // TODO (BUG): this leaks - set current allocator?
        Allocator* current_old = get_std_current_allocator();
        set_std_current_allocator(&arena);
        test_start(log, mv_string("Instnatiate Implicit with Default UVar"));
        uint64_t expected = 10;
        test_toplevel("(seq [let! arr (mk-array 1 1)] (aset 0 10 arr) (elt 0 arr))",
            &expected, module, log, a) ;
        set_std_current_allocator(current_old);
    }

    delete_module(module);
    delete_package(base);
    delete_assembler(ass);
    release_executable_allocator(exalloc);
    release_arena_allocator(arena);
}
