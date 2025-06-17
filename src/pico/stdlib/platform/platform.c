#include "pico/stdlib/platform/window.h"
#include "pico/stdlib/platform/hedron.h"

#include "pico/stdlib/platform/platform.h"

void add_platform_module(Assembler* ass, Package* base, Allocator* a) {
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, a),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("platform")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, base, NULL, a);
    delete_module_header(header);

    add_window_module(ass, module, a);
    add_hedron_module(ass, module, a);

    add_module(string_to_symbol(mv_string("platform")), module, base);
}
