#include "pico/stdlib/prelude.h"
#include "pico/stdlib/helpers.h"

void add_prelude_module(Package* base, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    Imports imports = (Imports) {.clauses = mk_import_clause_array(0, &ra),};

    ReExports re_exports = (ReExports) {
        .clauses = mk_import_clause_array(16, &ra),
    };
    add_import_all(&re_exports.clauses, &ra, 1, "core");
    add_import_all(&re_exports.clauses, &ra, 1, "extra");
    add_import_all(&re_exports.clauses, &ra, 1, "num");
    add_import_all(&re_exports.clauses, &ra, 2, "abs", "numeric");
    add_import_all(&re_exports.clauses, &ra, 2, "abs", "show");
    add_import_all(&re_exports.clauses, &ra, 2, "abs", "equality");

    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };

    ModuleHeader header = (ModuleHeader) {
        .name = string_to_name(mv_string("prelude")),
        .imports = imports,
        .re_exports = re_exports,
        .exports = exports,
    };
    mk_module(header, base, NULL);
}
