#include "platform/memory/executable.h"
#include "platform/memory/arena.h"

#include "pico/stdlib/stdlib.h"
#include "pico/stdlib/extra.h"

#include "test_pico/stdlib/stdlib.h"
#include "test_pico/stdlib/components.h"
#include "test_pico/stdlib/helper.h"

void run_pico_stdlib_tests(TestLog* log, Allocator* a) {
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

    // TODO: what do with these bois?
    if (test_start(log, mv_string("int-literal"))) {
        int64_t expected = -10;
        test_toplevel_eq("-10", &expected, module, log, a) ;
    }

    if (suite_start(log, mv_string("core"))) {
        run_pico_stdlib_core_tests(log, module, a);
        suite_end(log);
    }

    if (suite_start(log, mv_string("num"))) {
        run_pico_stdlib_num_tests(log, module, a);
        suite_end(log);
    }

    if (suite_start(log, mv_string("extra"))) {
        run_pico_stdlib_extra_tests(log, module, a);
        suite_end(log);
    }

    if (suite_start(log, mv_string("data"))) {
        if (suite_start(log, mv_string("pair"))) {
            run_pico_stdlib_data_pair_tests(log, module, a);
        }
        suite_end(log);
    }


    if (test_start(log, mv_string("Instnatiate Implicit with Default UVar"))) {
        // TODO (BUG): this leaks - set current allocator?
        Allocator current_old = get_std_current_allocator();
        set_std_current_allocator(arena);
        uint64_t expected = 10;
        test_toplevel_eq("(seq [let! lst (list.mk-list 1 1)] (list.eset 0 10 lst) (list.elt 0 lst))",
            &expected, module, log, a) ;
        set_std_current_allocator(current_old);
    }

    delete_module(module);
    delete_assembler(ass);
    release_executable_allocator(exalloc);
    release_arena_allocator(arena);
}
