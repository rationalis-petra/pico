#include "pico/stdlib/prelude.h"
#include "pico/stdlib/helpers.h"

void add_prelude_module(Package* base, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    Imports imports = (Imports) {.clauses = mk_import_clause_array(0, &ra),};

    ReExports re_exports = (ReExports) {
        .clauses = mk_import_clause_array(16, &ra),
    };
    add_import_all(&re_exports.clauses, &ra, 2, "lang", "relic");
    add_import_all(&re_exports.clauses, &ra, 1, "num");
    /** TODO: just re-export and, or, not etc. */
    add_import_all(&re_exports.clauses, &ra, 2, "num", "bool");
    add_import_all(&re_exports.clauses, &ra, 2, "abs", "numeric");
    add_import_all(&re_exports.clauses, &ra, 2, "abs", "show");
    add_import_all(&re_exports.clauses, &ra, 2, "abs", "equality");
    add_import_all(&re_exports.clauses, &ra, 2, "abs", "order");

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
