#include <inttypes.h>

#include "pico/stdlib/core/prim/prim.h"
#include "pico/stdlib/core/prim/submodules.h"

void add_prim_module(Assembler* ass, Target target, Module* core, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, &ra),
    };
    ReExports re_exports = (ReExports) {
        .clauses = mk_import_clause_array(0, &ra),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_name(mv_string("prim")),
        .imports = imports,
        .re_exports = re_exports,
        .exports = exports,
    };
    Package* base = get_package(core);
    Module* module = mk_module(header, base, core);

    RegionAllocator* subregion = make_subregion(region);
    add_prim_bool_module(ass, target, module, subregion);

    add_prim_address_module(ass, module, subregion);
    reset_subregion(subregion);
    add_prim_integral_module(sz_8, false, ass, target, module, subregion);
    reset_subregion(subregion);
    add_prim_integral_module(sz_16, false, ass, target, module, subregion);
    reset_subregion(subregion);
    add_prim_integral_module(sz_32, false, ass, target, module, subregion);
    reset_subregion(subregion);
    add_prim_integral_module(sz_64, false, ass, target, module, subregion);
    reset_subregion(subregion);

    add_prim_integral_module(sz_8, true, ass, target, module, subregion);
    reset_subregion(subregion);
    add_prim_integral_module(sz_16, true, ass, target, module, subregion);
    reset_subregion(subregion);
    add_prim_integral_module(sz_32, true, ass, target, module, subregion);
    reset_subregion(subregion);
    add_prim_integral_module(sz_64, true, ass, target, module, subregion);
    reset_subregion(subregion);

    add_prim_float_module(Float_32, ass, target, module, subregion);
    reset_subregion(subregion);
    add_prim_float_module(Float_64, ass, target, module, subregion);
    reset_subregion(subregion);

}
