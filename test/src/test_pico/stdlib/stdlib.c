#include "platform/signals.h"
#include "platform/memory/executable.h"
#include "platform/memory/arena.h"

#include "components/assembler/assembler.h"
#include "pico/stdlib/stdlib.h"
#include "pico/stdlib/meta/meta.h"

#include "test_pico/helper.h"
#include "test_pico/stdlib/stdlib.h"
#include "test_pico/stdlib/components.h"

void run_pico_stdlib_tests(TestLog* log, Target target, Allocator* a) {
    // Setup
    Allocator arena = mk_arena_allocator(16384, a);
    Package* base = get_base_package();

    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(3, a),
    };
    add_import_all(&imports.clauses, a, 1, "core");
    add_import_all(&imports.clauses, a, 1, "num");
    add_import_all(&imports.clauses, a, 1, "extra");
    add_import_all(&imports.clauses, a, 1, "data");
    add_import_all(&imports.clauses, a, 1, "meta");
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
        panic(mv_string("Error in tests: test_pico/stdlib/stdlib.c"));
    }

    Module* module = mk_module(header, base, NULL, a);
    Environment* env = env_from_module(module, &point, a);
    Module* old_current = get_std_current_module();
    set_std_current_module(module);
    delete_module_header(header);

    if (suite_start(log, mv_string("core"))) {
        run_pico_stdlib_core_type_tests(log, module, env, target, a);
        run_pico_stdlib_core_tests(log, module, env, target, a);
        suite_end(log);
    }

    if (suite_start(log, mv_string("num"))) {
        run_pico_stdlib_num_tests(log, module, env, target, a);
        suite_end(log);
    }

    if (suite_start(log, mv_string("meta"))) {
        if (suite_start(log, mv_string("refl"))) {
            run_pico_stdlib_meta_refl_tests(log, module, env, target, a);
            suite_end(log);
        }
        suite_end(log);
    }

    if (suite_start(log, mv_string("extra"))) {
        run_pico_stdlib_extra_tests(log, module, env, target, a);
        suite_end(log);
    }

    if (suite_start(log, mv_string("data"))) {
    /* TODO_NEW_CC: re-enable me!
        if (suite_start(log, mv_string("pair"))) {
            run_pico_stdlib_data_pair_tests(log, module, env, target, a);
            suite_end(log);
        }
    */
        if (suite_start(log, mv_string("list"))) {
            run_pico_stdlib_data_list_tests(log, module, env, target, a);
            suite_end(log);
        }
        suite_end(log);
    }

    set_std_current_module(old_current);
    delete_env(env, a);
    delete_module(module);
    release_arena_allocator(arena);
}
