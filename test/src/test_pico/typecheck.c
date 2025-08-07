#include "platform/memory/executable.h"
#include "platform/memory/arena.h"

#include "pico/stdlib/stdlib.h"
#include "pico/stdlib/extra.h"
#include "pico/binding/environment.h"

#include "test_pico/helper.h"
#include "test_pico/typecheck.h"

#define RUN(str) run_toplevel(str, module, env, log, a); refresh_env(env, a); refresh_env(env, a)
#define TEST_TYPE(str) test_typecheck_eq(str, expected, env, log, a)

void run_pico_typecheck_tests(TestLog* log, Allocator* a) {
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
    Module* module = mk_module(header, base, NULL, a);
    Environment* env = env_from_module(module, a);
    delete_module_header(header);

    if (test_start(log, mv_string("Instnatiate Implicit with Default UVar"))) {
        // TODO (BUG): this leaks - set current allocator?
        Allocator current_old = get_std_current_allocator();
        set_std_current_allocator(arena);
        PiType* expected = mk_prim_type(&arena, Int_64);
        TEST_TYPE("(seq [let! lst (list.mk-list 1 1)] (list.eset 0 10 lst) (list.elt 0 lst))");
        set_std_current_allocator(current_old);
    }

    if (test_start(log, mv_string("Default struct from field constraints"))) {
        // TODO (BUG): this leaks - set current allocator?
        Allocator current_old = get_std_current_allocator();
        set_std_current_allocator(arena);
        PiType *expected =
            mk_proc_type(&arena, 1,
                         mk_struct_type(&arena, 2, "x", mk_prim_type(&arena, Int_64),
                                        "y", mk_prim_type(&arena, Int_64)), mk_prim_type(&arena, Int_64));
        TEST_TYPE("(proc [point] (i64.+ point.x point.y))");
        set_std_current_allocator(current_old);
    }

    if (test_start(log, mv_string("Un-annotated variant in match"))) {
        // TODO (BUG): this leaks - set current allocator?
        Allocator current_old = get_std_current_allocator();
        set_std_current_allocator(arena);
        PiType *expected =
            mk_proc_type(&arena, 1,
                         mk_enum_type(&arena, 2, "left", 1, mk_prim_type(&arena, UInt_64),
                                      "right", 1, mk_prim_type(&arena, Address)), mk_prim_type(&arena, UInt_64));
        TEST_TYPE("(proc [either] match either [[:left v] v] [[:right x] (address-to-num x)])");
        set_std_current_allocator(current_old);
    }

    if (test_start(log, mv_string("enum from variant constraints"))) {
        // TODO (BUG): this leaks - set current allocator?
        Allocator current_old = get_std_current_allocator();
        set_std_current_allocator(arena);
        PiType *expected = mk_proc_type(&arena, 1, mk_prim_type(&arena, Bool),
                                        mk_enum_type(&arena, 2,
                                                     "left", 1, mk_prim_type(&arena, Int_64),
                                                     "right", 1, mk_prim_type(&arena, Address)));
        TEST_TYPE("(proc [which] if which (:left 10) (:right (malloc 8)))") ;
        set_std_current_allocator(current_old);
    }

    if (test_start(log, mv_string("declaration"))) {
        // TODO (BUG): this leaks - set current allocator?
        Allocator current_old = get_std_current_allocator();
        set_std_current_allocator(arena);
        PiType *expected = mk_proc_type(&arena, 1, mk_prim_type(&arena, UInt_64),
                                        mk_prim_type(&arena, UInt_64));
        RUN("(declare id [.type Proc [U64] U64])");
        RUN("(def id proc [x] x)");
        TEST_TYPE("id");
        set_std_current_allocator(current_old);
    }

    if (test_start(log, mv_string("kinds-1"))) {
        // TODO (BUG): this leaks - set current allocator?
        Allocator current_old = get_std_current_allocator();
        set_std_current_allocator(arena);
        PiType ty = (PiType){.sort = TKind, .kind.nargs = 1};
        PiType* expected = &ty;
        TEST_TYPE("(Family [A] A)");
        set_std_current_allocator(current_old);
    }

    delete_env(env, a);
    delete_module(module);
    delete_assembler(ass);
    release_executable_allocator(exalloc);
    release_arena_allocator(arena);
}
