#include "platform/signals.h"
#include "platform/memory/arena.h"

#include "pico/stdlib/helpers.h"
#include "pico/stdlib/data/submodules.h"

void add_string_module(Target target, Module *data, Allocator *alloc) {
    Allocator arena = mk_arena_allocator(16384, alloc);
    Allocator* a = &arena;

    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(3, a),
    };
    add_import_all(&imports.clauses, a, 1, "core");
    add_import_all(&imports.clauses, a, 1, "num");
    add_import_all(&imports.clauses, a, 1, "extra");

    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("pointer")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(data), NULL, alloc);
    delete_module_header(header);

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) {
        panic(mv_string("pi error in ptr.c"));
    }

    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    const char* null_fn = "(def String Named String Struct [.memsize U64] [.bytes Address])";
    compile_toplevel(null_fn, module, target, &point, &pi_point, a);

    Result r = add_module_def(data, string_to_symbol(mv_string("string")), module);
    if (r.type == Err) panic(r.error_message);
    release_arena_allocator(arena);
}
