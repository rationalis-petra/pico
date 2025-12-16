#include "platform/signals.h"
#include "platform/memory/region.h"

#include "pico/stdlib/stdlib.h"

#include "test_pico/helper.h"
#include "test_pico/eval/eval.h"
#include "test_pico/eval/components.h"

void run_pico_eval_tests(TestLog* log, Target target, RegionAllocator* region) {
    // Setup
    Allocator gpa = ra_to_gpa(region);
    Allocator* a = &gpa;
    Package* base = get_base_package();

    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(8, a),
    };
    add_import_all(&imports.clauses, a, 1, "core");
    add_import_all(&imports.clauses, a, 1, "num");
    add_import_all(&imports.clauses, a, 1, "extra");
    add_import_all(&imports.clauses, a, 1, "data");
    add_import_all(&imports.clauses, a, 1, "platform");
    add_import_all(&imports.clauses, a, 2, "platform", "memory");

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
    Module* module = mk_module(header, base, NULL);
    Environment* env = env_from_module(module, &point, a);
    delete_module_header(header);

    if (suite_start(log, mv_string("literals"))) {
        RegionAllocator* subregion = make_subregion(region);
        run_pico_eval_literals_tests(log, module, env, target, subregion);
        release_subregion(subregion);
        suite_end(log);
    }

    if (suite_start(log, mv_string("proc"))) {
        RegionAllocator* subregion = make_subregion(region);
        run_pico_eval_proc_tests(log, module, env, target, subregion);
        release_subregion(subregion);
        suite_end(log);
    }

    if (suite_start(log, mv_string("polymorphic"))) {
        RegionAllocator* subregion = make_subregion(region);
        run_pico_eval_polymorphic_tests(log, module, env, target, subregion);
        release_subregion(subregion);
        suite_end(log);
    }

    if (suite_start(log, mv_string("modular"))) {
        RegionAllocator* subregion = make_subregion(region);
        run_pico_eval_modular_tests(log, module, env, target, subregion);
        release_subregion(subregion);
        suite_end(log);
    }

    if (suite_start(log, mv_string("foreign-adapter"))) {
        RegionAllocator* subregion = make_subregion(region);
        run_pico_eval_foreign_adapter_tests(log, module, env, target, subregion);
        release_subregion(subregion);
        suite_end(log);
    }

    delete_env(env, a);
    delete_module(module);
}
