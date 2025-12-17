#include "pico/values/modular.h"
#include "pico/stdlib/meta/meta.h"
#include "pico/stdlib/meta/submodules.h"

void add_meta_module(Assembler* ass, Package* base, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, &ra),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("meta")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, base, NULL);
    delete_module_header(header);

    RegionAllocator* subregion = make_subregion(region);
    add_gen_module(ass, module, subregion);
    reset_subregion(subregion);
    add_refl_module(ass, module, subregion);
    release_subregion(subregion);

    add_module(string_to_symbol(mv_string("meta")), module, base);

}

