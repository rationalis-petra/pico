#include "platform/signals.h"
#include "platform/memory/arena.h"

#include "pico/stdlib/data/submodules.h"

void add_either_module(Target target, Module *data, Allocator *alloc) {
    Allocator arena = mk_arena_allocator(16384, alloc);
    Allocator* a = &arena;

    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, a),
    };
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

    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    Result r = add_module_def(data, string_to_symbol(mv_string("either")), module);
    if (r.type == Err) panic(r.error_message);
    release_arena_allocator(arena);
}
