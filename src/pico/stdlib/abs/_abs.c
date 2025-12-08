#include "pico/stdlib/helpers.h"
#include "pico/stdlib/abs/submodules.h"

#include "pico/stdlib/abs/abs.h"

void add_abs_module(Target target, Package* base, PiAllocator* module_allocator, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, &ra),
    };

    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("numeric")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, base, NULL, *module_allocator);
    delete_module_header(header);

    RegionAllocator* subregion = make_subregion(region);
    add_numeric_module(target, module, module_allocator, subregion);
    reset_subregion(subregion);
    add_show_module(target, module, module_allocator, subregion);
    release_subregion(subregion);

    add_module(string_to_symbol(mv_string("abs")), module, base);
}
