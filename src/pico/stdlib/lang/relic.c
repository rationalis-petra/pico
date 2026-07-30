#include "pico/stdlib/lang/relic.h"
#include "pico/stdlib/helpers.h"

void add_relic_module(Module* lang, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    Imports imports = (Imports) {.clauses = mk_import_clause_array(0, &ra),};

    ReExports re_exports = (ReExports) {
        .clauses = mk_import_clause_array(16, &ra),
    };
    add_import_all(&re_exports.clauses, &ra, 1, "core");
    add_import_all(&re_exports.clauses, &ra, 1, "extra");

    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };

    ModuleHeader header = (ModuleHeader) {
        .name = string_to_name(mv_string("relic")),
        .imports = imports,
        .re_exports = re_exports,
        .exports = exports,
    };
    Package* base = get_package(lang);
    mk_module(header, base, lang);
}
