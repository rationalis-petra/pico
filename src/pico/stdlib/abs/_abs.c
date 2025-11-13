#include "pico/stdlib/helpers.h"
#include "pico/stdlib/abs/submodules.h"

#include "pico/stdlib/abs/abs.h"

void add_abs_module(Target target, Package* base, Allocator* a) {
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, a),
    };

    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("numeric")),
        .imports = imports,
        .exports = exports,
    };
    PiAllocator pico_module_allocator = convert_to_pallocator(a);
    Module* module = mk_module(header, base, NULL, pico_module_allocator);
    delete_module_header(header);

    add_numeric_module(target, module, a);
    add_show_module(target, module, a);

    add_module(string_to_symbol(mv_string("abs")), module, base);
}
