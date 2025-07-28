#include "pico/stdlib/data/submodules.h"
#include "pico/stdlib/data/data.h"

void add_data_module(Assembler* ass, Package* base, Allocator* a) {
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, a),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("data")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, base, NULL, a);
    delete_module_header(header);

    add_list_module(module, a);
    add_either_module(ass, module, a);
    add_maybe_module(ass, module, a);
    add_pair_module(ass, module, a);
    add_pointer_module(ass, module, a);

    add_string_module(module, a);

    add_module(string_to_symbol(mv_string("data")), module, base);
}
