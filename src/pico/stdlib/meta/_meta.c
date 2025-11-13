#include "pico/values/modular.h"
#include "pico/stdlib/meta/meta.h"
#include "pico/stdlib/meta/submodules.h"

void add_meta_module(Assembler* ass, Package* base, Allocator* a) {
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, a),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("meta")),
        .imports = imports,
        .exports = exports,
    };
    PiAllocator pico_module_allocator = convert_to_pallocator(a);
    Module* module = mk_module(header, base, NULL, pico_module_allocator);
    delete_module_header(header);

    add_gen_module(ass, module, a);
    add_refl_module(ass, module, a);

    add_module(string_to_symbol(mv_string("meta")), module, base);

}

