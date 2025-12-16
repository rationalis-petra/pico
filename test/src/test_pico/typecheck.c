#include "platform/signals.h"
#include "platform/memory/executable.h"
#include "platform/memory/region.h"

#include "pico/stdlib/stdlib.h"
#include "pico/stdlib/platform/submodules.h"
#include "pico/binding/environment.h"

#include "test_pico/helper.h"
#include "test_pico/typecheck.h"

#define RUN(str) run_toplevel(str, module, context); refresh_env(env)
#define TEST_TYPE(str) test_typecheck_eq(str, expected, env, context)

void run_pico_typecheck_tests(TestLog* log, Target target, RegionAllocator* region) {
    // Setup
    Allocator gpa = ra_to_gpa(region);
    Allocator* a = &gpa;

    Allocator exalloc = mk_executable_allocator(&gpa);

    PiAllocator pregion = convert_to_pallocator(&gpa);
    Assembler* ass = mk_assembler(current_cpu_feature_flags(), &exalloc);
    Package* base = get_base_package();

    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(8, a),
    };
    add_import_all(&imports.clauses, a, 1, "core");
    add_import_all(&imports.clauses, a, 1, "num");
    add_import_all(&imports.clauses, a, 1, "extra");
    add_import_all(&imports.clauses, a, 1, "data");
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
    Module* module = mk_module(header, base, NULL);

    ErrorPoint point;
    if (catch_error(point)) {
        panic(mv_string("Error in tests: test_pico/typecheck.c"));
    }
    Environment* env = env_from_module(module, &point, a);
    delete_module_header(header);


    TestContext context = (TestContext) {
        .env = env,
        .region = region,
        .log = log,
        .target = target,
    };

    if (test_start(log, mv_string("UVar through all"))) {
        PiType* expected = mk_prim_type(&pregion, Int_64);
        TEST_TYPE("((all [A] -28) {Unit})");
    }

    // TODO: fix this test!
    /* if (test_start(log, mv_string("Substitution through uvar"))) { */
    /*     RUN("(def id all [A] proc [(x A)] x)"); */
    /*     PiType* expected = mk_prim_type(&arena, Int_64); */
    /*     TEST_TYPE("((all [B] proc [(x B)] (id x)) 77)"); */
    /* } */

    if (test_start(log, mv_string("Instnatiate Implicit with Default UVar"))) {
        PiAllocator current_old = get_std_current_allocator();
        set_std_current_allocator(pregion);
        PiType* expected = mk_prim_type(&pregion, Int_64);
        TEST_TYPE("(seq [let! lst (list.mk-list 1 1)] (list.eset 0 10 lst) (list.elt 0 lst))");
        set_std_current_allocator(current_old);
    }

    if (test_start(log, mv_string("Default struct from field constraints"))) {
        PiAllocator current_old = get_std_current_allocator();
        set_std_current_allocator(pregion);
        PiType *expected =
            mk_proc_type(&pregion, 1,
                         mk_struct_type(&pregion, 2, "x", mk_prim_type(&pregion, Int_64),
                                        "y", mk_prim_type(&pregion, Int_64)), mk_prim_type(&pregion, Int_64));
        TEST_TYPE("(proc [point] (i64.+ point.x point.y))");
        set_std_current_allocator(current_old);
    }

    if (test_start(log, mv_string("Un-annotated variant in match"))) {
        PiAllocator current_old = get_std_current_allocator();
        set_std_current_allocator(pregion);
        PiType *expected =
            mk_proc_type(&pregion, 1,
                         mk_enum_type(&pregion, 2, "left", 1, mk_prim_type(&pregion, UInt_64),
                                      "right", 1, mk_prim_type(&pregion, Address)), mk_prim_type(&pregion, UInt_64));
        TEST_TYPE("(proc [either] match either [[:left v] v] [[:right x] (address-to-num x)])");
        set_std_current_allocator(current_old);
    }

    if (test_start(log, mv_string("enum from variant constraints"))) {
        PiAllocator current_old = get_std_current_allocator();
        set_std_current_allocator(pregion);
        PiType *expected = mk_proc_type(&pregion, 1, mk_prim_type(&pregion, Bool),
                                        mk_enum_type(&pregion, 2,
                                                     "left", 1, mk_prim_type(&pregion, Int_64),
                                                     "right", 1, mk_prim_type(&pregion, Address)));
        TEST_TYPE("(proc [which] if which (:left 10) (:right (alloc 8)))") ;
        set_std_current_allocator(current_old);
    }

    if (test_start(log, mv_string("declaration"))) {
        PiAllocator current_old = get_std_current_allocator();
        set_std_current_allocator(pregion);
        PiType *expected = mk_proc_type(&pregion, 1, mk_prim_type(&pregion, UInt_64),
                                        mk_prim_type(&pregion, UInt_64));
        RUN("(declare id [.type Proc [U64] U64])");
        RUN("(def id proc [x] x)");
        TEST_TYPE("id");
        set_std_current_allocator(current_old);
    }

    if (test_start(log, mv_string("kinds-1"))) {
        PiAllocator current_old = get_std_current_allocator();
        set_std_current_allocator(pregion);
        PiType ty = (PiType){.sort = TKind, .kind.nargs = 1};
        PiType* expected = &ty;
        TEST_TYPE("(Family [A] A)");
        set_std_current_allocator(current_old);
    }

    delete_env(env, a);
    delete_module(module);
    delete_assembler(ass);
    release_executable_allocator(exalloc);
}
