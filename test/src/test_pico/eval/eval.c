#include "platform/signals.h"
#include "platform/memory/executable.h"
#include "platform/memory/arena.h"

#include "pico/stdlib/stdlib.h"
#include "pico/stdlib/extra.h"

#include "test_pico/helper.h"
#include "test_pico/eval/eval.h"
#include "test_pico/eval/components.h"

void run_pico_eval_tests(TestLog* log, Allocator* a) {
    // Setup
    Allocator exalloc = mk_executable_allocator(a);
    Allocator arena = mk_arena_allocator(4096, a);
    Assembler* ass = mk_assembler(current_cpu_feature_flags(), &exalloc);
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

    ErrorPoint point;
    if (catch_error(point)) {
        panic(mv_string("Error in tests: test_pico/eval/eval.c"));
    }
    Module* module = mk_module(header, base, NULL, a);
    Environment* env = env_from_module(module, &point, a);
    delete_module_header(header);

    if (suite_start(log, mv_string("literals"))) {
        run_pico_eval_literals_tests(log, module, env, a);
        suite_end(log);
    }

    if (suite_start(log, mv_string("polymorphic"))) {
        run_pico_eval_polymorphic_tests(log, module, env, a);
        suite_end(log);
    }

    if (suite_start(log, mv_string("foreign-adapter"))) {
        run_pico_eval_foreign_adapter_tests(log, module, env, a);
        suite_end(log);
    }

    delete_env(env, a);
    delete_module(module);
    delete_assembler(ass);
    release_executable_allocator(exalloc);
    release_arena_allocator(arena);
}
