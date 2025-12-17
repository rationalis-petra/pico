#include "platform/signals.h"

#include "pico/stdlib/stdlib.h"
#include "pico/stdlib/meta/meta.h"

#include "test_pico/helper.h"
#include "test_pico/stdlib/stdlib.h"
#include "test_pico/stdlib/components.h"

void run_pico_stdlib_tests(TestLog* log, Target target, Allocator* a) {
    // Setup
    Package* base = get_base_package();

    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(3, a),
    };
    add_import_all(&imports.clauses, a, 1, "core");
    add_import_all(&imports.clauses, a, 1, "num");
    add_import_all(&imports.clauses, a, 1, "extra");
    add_import_all(&imports.clauses, a, 1, "data");
    add_import_all(&imports.clauses, a, 2, "abs", "numeric");
    add_import_all(&imports.clauses, a, 1, "meta");
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
        panic(mv_string("Error in tests: test_pico/stdlib/stdlib.c"));
    }

    Module* module = mk_module(header, base, NULL);
    Environment* env = env_from_module(module, &point, a);
    Module* old_current = get_std_current_module();
    set_std_current_module(module);
    delete_module_header(header);

    RegionAllocator* region = make_region_allocator(16384, true, a);
    if (suite_start(log, mv_string("core"))) {
        RegionAllocator* subregion = make_subregion(region);
        run_pico_stdlib_core_type_tests(log, module, env, target, region);
        run_pico_stdlib_core_tests(log, module, env, target, subregion);
        suite_end(log);
        release_subregion(subregion);
    }

    if (suite_start(log, mv_string("num"))) {
        RegionAllocator* subregion = make_subregion(region);
        run_pico_stdlib_num_tests(log, module, env, target, subregion);
        suite_end(log);
        release_subregion(subregion);
    }

    if (suite_start(log, mv_string("meta"))) {
        if (suite_start(log, mv_string("gen"))) {
            RegionAllocator* subregion = make_subregion(region);
            run_pico_stdlib_meta_gen_tests(log, module, env, target, subregion);
            suite_end(log);
            release_subregion(subregion);
        }
        if (suite_start(log, mv_string("refl"))) {
            RegionAllocator* subregion = make_subregion(region);
            run_pico_stdlib_meta_refl_tests(log, module, env, target, subregion);
            suite_end(log);
            release_subregion(subregion);
        }
        suite_end(log);
    }

    if (suite_start(log, mv_string("extra"))) {
        RegionAllocator* subregion = make_subregion(region);
        run_pico_stdlib_extra_tests(log, module, env, target, subregion);
        suite_end(log);
        release_subregion(subregion);
    }

    if (suite_start(log, mv_string("abs"))) {
        if (suite_start(log, mv_string("numeric"))) {
            RegionAllocator* subregion = make_subregion(region);
            run_pico_stdlib_abs_numeric_tests(log, module, env, target, subregion);
            suite_end(log);
            release_subregion(subregion);
        }
        suite_end(log);
    }
    if (suite_start(log, mv_string("data"))) {
        if (suite_start(log, mv_string("pair"))) {
            RegionAllocator* subregion = make_subregion(region);
            run_pico_stdlib_data_pair_tests(log, module, env, target, subregion);
            suite_end(log);
            release_subregion(subregion);
        }
        if (suite_start(log, mv_string("either"))) {
            RegionAllocator* subregion = make_subregion(region);
            run_pico_stdlib_data_either_tests(log, module, env, target, subregion);
            suite_end(log);
            release_subregion(subregion);
        }
        if (suite_start(log, mv_string("list"))) {
            RegionAllocator* subregion = make_subregion(region);
            run_pico_stdlib_data_list_tests(log, module, env, target, subregion);
            suite_end(log);
            release_subregion(subregion);
        }
        suite_end(log);
    }

    set_std_current_module(old_current);
    delete_env(env, a);
    delete_module(module);
    delete_region_allocator(region);
}
