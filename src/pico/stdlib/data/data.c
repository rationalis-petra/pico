// #include "platform/signals.h"

#include "pico/stdlib/data/array.h"
#include "pico/stdlib/data/either.h"
#include "pico/stdlib/data/maybe.h"
#include "pico/stdlib/data/pair.h"
#include "pico/stdlib/data/ptr.h"

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

    add_array_module(module, a);
    add_either_module(ass, module, a);
    add_maybe_module(ass, module, a);
    add_pair_module(ass, module, a);
    add_ptr_module(ass, module, a);

    add_module(string_to_symbol(mv_string("data")), module, base);
}
