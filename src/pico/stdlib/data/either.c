#include "platform/signals.h"
#include "platform/memory/arena.h"

#include "pico/stdlib/helpers.h"
#include "pico/stdlib/data/submodules.h"

void add_either_module(Target target, Module *data, Allocator *alloc) {
    Allocator arena = mk_arena_allocator(16384, alloc);
    Allocator* a = &arena;

    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(4, a),
    };
    add_import_all(&imports.clauses, a, 1, "core");
    add_import_all(&imports.clauses, a, 1, "num");
    add_import_all(&imports.clauses, a, 1, "extra");
    add_import_all(&imports.clauses, a, 1, "meta");

    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("either")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(data), NULL, alloc);
    delete_module_header(header);

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) {
        //panic(doc_to_str(pi_point.error.message, 120, a));
        panic(mv_string("pico error in pair.c"));
    }

    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    const char *mk_either_type =
        "(def Either Family [A B] Enum [:left A] [:right B])";
    compile_toplevel(mk_either_type, module, target, &point, &pi_point, a);

    const char *mk_left_fn =
        "(def left all [A B] proc [(x A)] (Either A B):left x)";
    compile_toplevel(mk_left_fn, module, target, &point, &pi_point, a);

    const char *mk_right_fn =
        "(def right all [A B] proc [(y B)] (Either A B):right y)";
    compile_toplevel(mk_right_fn, module, target, &point, &pi_point, a);

    /* const char *mk_either_fn = */
    /*     "(def either all [A B] proc [(e (Either A B)) (f Proc [A] B) (g Proc [A] B)] (match e [:left x] (f x) [:right x] (g x)))"; */
    /* compile_toplevel(mk_either_fn, module, target, &point, &pi_point, a); */

    Result r = add_module_def(data, string_to_symbol(mv_string("either")), module);
    if (r.type == Err) panic(r.error_message);
    release_arena_allocator(arena);
}
