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
        uint64_t expected = 10;
        test_toplevel("Instnatiate Implicit with Default UVar",
            "(seq [let! arr (mk-array 1 1)] (aset 0 10 arr) (elt 0 arr))",
            &expected, module, log, a)
            ;
    }

    delete_module(module);
    delete_package(base);
    delete_assembler(ass);
    release_executable_allocator(exalloc);
}
