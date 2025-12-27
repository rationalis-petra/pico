#include "pico/stdlib/data/submodules.h"
#include "pico/stdlib/data/data.h"

void add_data_module(Target target, Package* base, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, &ra),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("data")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, base, NULL);
    delete_module_header(header);

    RegionAllocator* subregion = make_subregion(region);
    add_allocators_module(target.target, module, subregion);
    reset_subregion(subregion);
    add_list_module(target, module, subregion);
    reset_subregion(subregion);
    add_either_module(target, module, subregion);
    reset_subregion(subregion);
    add_result_module(target, module, subregion);
    reset_subregion(subregion);
    add_maybe_module(target, module, subregion);
    reset_subregion(subregion);
    add_pair_module(target, module, subregion);
    reset_subregion(subregion);
    add_pointer_module(target, module, subregion);
    reset_subregion(subregion);
    add_string_module(target, module, subregion);
    release_subregion(subregion);

    add_module(string_to_symbol(mv_string("data")), module, base);
}
