#include "pico/stdlib/helpers.h"
#include "pico/stdlib/abs/numeric.h"

#include "pico/stdlib/abs/abs.h"

void add_abs_module(Package* base, Allocator* a) {
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
    Module* module = mk_module(header, base, NULL, a);
    delete_module_header(header);

    add_numeric_module(module, base, a);

    add_module(string_to_symbol(mv_string("data")), module, base);
}
