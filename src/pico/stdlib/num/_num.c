#include <inttypes.h>
#include "platform/signals.h"

#include "pico/stdlib/num/num.h"
#include "pico/stdlib/num/submodules.h"

void add_num_module(Assembler* ass, Package* base, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, &ra),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("core")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, base, NULL);

    add_bool_module(ass, module, &ra);

    add_integral_module(mv_string("u8"), sz_8, false, ass, module, &ra);
    add_integral_module(mv_string("u16"), sz_16, false, ass, module, &ra);
    add_integral_module(mv_string("u32"), sz_32, false, ass, module, &ra);
    add_integral_module(mv_string("u64"), sz_64, false, ass, module, &ra);

    add_integral_module(mv_string("i8"), sz_8, true, ass, module, &ra);
    add_integral_module(mv_string("i16"), sz_16, true, ass, module, &ra);
    add_integral_module(mv_string("i32"), sz_32, true, ass, module, &ra);
    add_integral_module(mv_string("i64"), sz_64, true, ass, module, &ra);

    add_float_module(mv_string("f32"), Float_32, ass, module, &ra);
    add_float_module(mv_string("f64"), Float_64, ass, module, &ra);

    Result r =add_module(string_to_symbol(mv_string("num")), module, base);
    if (r.type == Err) panic(r.error_message);
}
