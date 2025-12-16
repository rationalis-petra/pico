#include "platform/signals.h"

#include "pico/stdlib/helpers.h"
#include "pico/stdlib/core.h"
#include "pico/stdlib/data/submodules.h"

void add_memory_module(Target target, Module *data, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);

    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(3, &ra),
    };
    add_import_all(&imports.clauses, &ra, 1, "core");
    add_import_all(&imports.clauses, &ra, 1, "num");
    add_import_all(&imports.clauses, &ra, 1, "extra");
    add_import_all(&imports.clauses, &ra, 1, "data.pointer");

    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("memory")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(data), NULL);
    delete_module_header(header);

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) {
        //panic(doc_to_str(pi_point.error.message, 120, a));
        panic(mv_string("pi error in ptr.c"));
    }

    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    PiType type;
    Symbol sym;

    Segments null_segments = (Segments) {
        .code = mk_u8_array(0, &ra),
        .data = mk_u8_array(0, &ra),
    };

    type.kind.nargs = 1;
    sym = string_to_symbol(mv_string("AllocVTable"));
    add_def(module, sym, type, get_allocator_vtable_type(), null_segments, NULL);

    type.kind.nargs = 0;
    sym = string_to_symbol(mv_string("Allocator"));
    add_def(module, sym, type, get_allocator_type(), null_segments, NULL);

    Result r = add_module_def(data, string_to_symbol(mv_string("memory")), module);
    if (r.type == Err) panic(r.error_message);
}
