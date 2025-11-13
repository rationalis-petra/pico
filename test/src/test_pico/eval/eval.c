#include "platform/signals.h"
#include "platform/memory/arena.h"

#include "pico/stdlib/stdlib.h"

#include "test_pico/helper.h"
#include "test_pico/eval/eval.h"
#include "test_pico/eval/components.h"

void run_pico_eval_tests(TestLog* log, Target target, Allocator* a) {
    // Setup
    Allocator arena = mk_arena_allocator(4096, a);
    Package* base = get_base_package();

    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(8, a),
    };
    add_import_all(&imports.clauses, a, 1, "core");
    add_import_all(&imports.clauses, a, 1, "num");
    add_import_all(&imports.clauses, a, 1, "extra");
    add_import_all(&imports.clauses, a, 1, "data");
    add_import_all(&imports.clauses, a, 1, "platform");

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
    PiAllocator pia = convert_to_pallocator(a);
    Module* module = mk_module(header, base, NULL, pia);
    Environment* env = env_from_module(module, &point, a);
    delete_module_header(header);

    if (suite_start(log, mv_string("literals"))) {
        run_pico_eval_literals_tests(log, module, env, target, a);
        suite_end(log);
    }

    if (suite_start(log, mv_string("proc"))) {
        run_pico_eval_proc_tests(log, module, env, target, a);
        suite_end(log);
    }

    if (suite_start(log, mv_string("polymorphic"))) {
        run_pico_eval_polymorphic_tests(log, module, env, target, a);
        suite_end(log);
    }

    if (suite_start(log, mv_string("modular"))) {
        run_pico_eval_modular_tests(log, module, env, target, a);
        suite_end(log);
    }

    if (suite_start(log, mv_string("foreign-adapter"))) {
        run_pico_eval_foreign_adapter_tests(log, module, env, target, a);
        suite_end(log);
    }

    delete_env(env, a);
    delete_module(module);
    release_arena_allocator(arena);
}
