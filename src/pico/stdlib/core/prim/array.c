#include <string.h>

#include "platform/signals.h"
#include "platform/machine_info.h"

#include "components/pretty/string_printer.h"

#include "pico/codegen/backend-direct/internal.h"
#include "pico/stdlib/core/prim/submodules.h"

/**
 * Array primitive operations. While many could (in principle) be written by
 * hand, these are mostly included to assist in opimization/guarantee usage of
 * SIMD in some scenarios.
 *
 * TODO: 
 *  - reshape
 *  - map
 *  - zip
 *  - reduce
 *  - ravel
 *  - join
 * TODO: language feature implementation 
 *  - genarrays
 *  - iota as in SAC
 *  - array functions over values/pointers to arrays
 *  - Shape Polymorphism (dimension + entire shape)
 * 
 *  - matrix-multiply
 */

void add_prim_array_module(Assembler* ass, Module* prim, RegionAllocator* region) {
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
        .name = string_to_name(mv_string("array")),
        .imports = imports,
        .re_exports = re_exports,
        .exports = exports,
    };
    Package* base = get_package(prim);
    mk_module(header, base, prim);
    //Module* module = mk_module(header, base, prim);
    //Name name;

    //PiType type;
    //PiType type_val;
    //PiType* type_data = &type_val;
    ErrorPoint point;
    //PiAllocator pia = convert_to_pallocator(&ra);
    if (catch_error(point)) {
        panic(doc_to_str(point.error_message, 120, &ra));
    }

    /*
    Segments null_segments = (Segments) {
        .code = mk_u8_array(0, &ra),
        .data = mk_u8_array(0, &ra),
    };
    */

}
