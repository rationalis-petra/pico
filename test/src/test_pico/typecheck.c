#include "platform/memory/executable.h"
#include "platform/memory/arena.h"

#include "pico/stdlib/stdlib.h"
#include "pico/stdlib/extra.h"

#include "test_pico/stdlib/stdlib.h"
#include "test_pico/stdlib/components.h"
#include "test_pico/helper.h"
#include "test_pico/typecheck.h"


void run_pico_typecheck_tests(TestLog* log, Allocator* a) {
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

    if (test_start(log, mv_string("Instnatiate Implicit with Default UVar"))) {
        // TODO (BUG): this leaks - set current allocator?
        Allocator current_old = get_std_current_allocator();
        set_std_current_allocator(arena);
        PiType* expected = mk_prim_type(&arena, Int_64);
        test_typecheck_eq("(seq [let! lst (list.mk-list 1 1)] (list.eset 0 10 lst) (list.elt 0 lst))",
            expected, module, log, a) ;
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
        test_typecheck_eq("(proc [point] (i64.+ point.x point.y))",
            expected, module, log, a) ;
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
        test_typecheck_eq("(proc [either] match either [[:left v] v] [[:right x] (address-to-num x)])",
            expected, module, log, a) ;
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
        test_typecheck_eq("(proc [which] if which (:left 10) (:right (malloc 8)))",
            expected, module, log, a) ;
        set_std_current_allocator(current_old);
    }

    delete_module(module);
    delete_assembler(ass);
    release_executable_allocator(exalloc);
    release_arena_allocator(arena);
}
