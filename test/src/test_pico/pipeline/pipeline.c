#include "platform/memory/executable.h"

#include "pico/stdlib/stdlib.h"

#include "test_pico/pipeline/pipeline.h"
#include "test_pico/pipeline/helper.h"

void run_pico_pipeline_tests(RunDescriptor to_run, TestLog* log, Allocator* a) {
    // Setup
    Allocator exalloc = mk_executable_allocator(a);
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

    {
        test_start(log);
        int64_t expected = -10;
        test_toplevel("int-literal",
            "-10",
            &expected, module, log, a) ;
    }

    {
        test_start(log);
        uint64_t expected = 3;
        test_toplevel("Addition",
            "(u64.+ 1 2)",
            &expected, module, log, a) ;
    }

    {
        test_start(log);
        int64_t expected = -1;
        test_toplevel("Subtraction", "(i64.- 1 2)", &expected, module, log, a) ;
    }

    {
        test_start(log);
        int64_t expected = -1;
        test_toplevel("Subtraction", "(i64.- 1 2)", &expected, module, log, a) ;
    }

    {
        test_start(log);
        int64_t expected = 3;
        test_toplevel("simple-let", "(let [x 3] x)", &expected, module, log, a) ;
    }

    {
        test_start(log);
        int64_t expected = 3;
        test_toplevel("simple-sequence", "(seq 1 2 3)", &expected, module, log, a) ;
    }

    {
        test_start(log);
        int64_t expected = 2;
        test_toplevel("let-in-sequence", "(seq [let! x 2] x)", &expected, module, log, a) ;
    }

    {
        test_start(log);
        int64_t expected = 5;
        test_toplevel("let-many-in-sequence", "(seq [let! x 2 y 3] (u32.+ x y))", &expected, module, log, a) ;
    }

    {
        test_start(log);
        typedef struct {
            int64_t x;
            int64_t y;
        } Point;
        Point expected = (Point) {.x = 3, .y = -5};
        test_toplevel("struct", "(struct [.x 3] [.y -5])", &expected, module, log, a) ;
    }

    {
        // TODO (BUG): this leaks - set current allocator?
        test_start(log);
        uint64_t expected = 10;
        test_toplevel("Instnatiate Implicit with Default UVar",
            "(seq [let! arr (mk-array 1 1)] (aset 0 10 arr) (elt 0 arr))",
            &expected, module, log, a) ;
    }

    delete_module(module);
    delete_package(base);
    delete_assembler(ass);
    release_executable_allocator(exalloc);
}
