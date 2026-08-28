#include "platform/signals.h"

#include "components/pretty/string_printer.h"

#include "pico/stdlib/helpers.h"
#include "pico/stdlib/core/kernel.h"

void add_memory_module(Target target, Module *data, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    PiAllocator pico_region = convert_to_pallocator(&ra);
    PiAllocator* pia = &pico_region;

    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(3, &ra),
    };
    add_import_all(&imports.clauses, &ra, 1, "core");
    add_import_all(&imports.clauses, &ra, 1, "num");
    add_import_all(&imports.clauses, &ra, 1, "extra");
    add_import_all(&imports.clauses, &ra, 1, "data.pointer");

    ReExports re_exports = (ReExports) {
        .clauses = mk_import_clause_array(0, &ra),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_name(mv_string("memory")),
        .imports = imports,
        .re_exports = re_exports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(data), data);
    delete_module_header(header);

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) {
        //panic(doc_to_str(pi_point.error.message, 120, a));
        panic(mv_string("pi error in ptr.c"));
    }

    ErrorPoint point;
    if (catch_error(point)) {
        panic(doc_to_str(point.error_message, 120, &ra));
    }

    PiType type;
    Name name;

    Segments null_segments = (Segments) {
        .code = mk_u8_array(0, &ra),
        .data = mk_u8_array(0, &ra),
    };

    type = *mk_type_kind(pia, 1, mk_type_type(pia), mk_type_type(pia));
    name = string_to_name(mv_string("AllocVTable"));
    add_def(module, name, type, get_allocator_vtable_type(), null_segments, NULL);

    type = (PiType){.sort = TType};
    name = string_to_name(mv_string("Allocator"));
    add_def(module, name, type, get_allocator_type(), null_segments, NULL);
}
