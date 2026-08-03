#include "pico/stdlib/user.h"
#include "pico/stdlib/helpers.h"

void add_user_module(Package* user, RegionAllocator* region) {

    Allocator ra = ra_to_gpa(region);
    Imports imports = (Imports) {.clauses = mk_import_clause_array(16, &ra),};

    add_import_all(&imports.clauses, &ra, 1, "prelude");

    add_import_all(&imports.clauses, &ra, 1, "num");
    add_import_all(&imports.clauses, &ra, 2, "platform", "terminal");

    add_import(&imports.clauses, &ra, 1, "abs");
    add_import(&imports.clauses, &ra, 1, "data");
    add_import(&imports.clauses, &ra, 1, "platform");
    add_import(&imports.clauses, &ra, 1, "meta");

    ReExports re_exports = (ReExports) {
        .clauses = mk_import_clause_array(0, &ra),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };

    ModuleHeader header = (ModuleHeader) {
        .name = string_to_name(mv_string("user")),
        .imports = imports,
        .re_exports = re_exports,
        .exports = exports,
    };
    mk_module(header, user, NULL);
}

Package* mk_user_package(Package* base, PiAllocator* module_allocator, RegionAllocator* region) {
    Package* user = mk_package(string_to_name(mv_string("base")), *module_allocator);
    add_dependency(user, base);

    RegionAllocator* subregion = make_subregion(region);
    add_user_module(user, subregion);

    return user;
}
