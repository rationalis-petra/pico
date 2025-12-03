#include "platform/signals.h"

#include "pico/stdlib/user.h"
#include "pico/stdlib/helpers.h"

void add_user_module(Package* base, PiAllocator* module_allocator, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    Imports imports = (Imports) {.clauses = mk_import_clause_array(9, &ra),};

    add_import_all(&imports.clauses, &ra, 1, "core");
    add_import_all(&imports.clauses, &ra, 1, "extra");
    add_import_all(&imports.clauses, &ra, 1, "num");
    add_import_all(&imports.clauses, &ra, 2, "abs", "numeric");
    add_import_all(&imports.clauses, &ra, 2, "abs", "show");

    add_import(&imports.clauses, &ra, 1, "abs");
    add_import(&imports.clauses, &ra, 1, "data");
    add_import(&imports.clauses, &ra, 1, "platform");
    add_import(&imports.clauses, &ra, 1, "meta");
    add_import(&imports.clauses, &ra, 1, "foreign");
    add_import(&imports.clauses, &ra, 1, "debug");

    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };

    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("user")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, base, NULL, *module_allocator);

    Result r = add_module(string_to_symbol(mv_string("user")), module, base);
    if (r.type == Err) panic(r.error_message);
}
