#include "pico/stdlib/user.h"
#include "pico/stdlib/helpers.h"

void add_user_module(Package* base, Allocator* a) {
    Imports imports = (Imports) {.clauses = mk_import_clause_array(9, a),};

    add_import_all(&imports.clauses, a, 1, "core");
    add_import_all(&imports.clauses, a, 1, "extra");
    add_import_all(&imports.clauses, a, 1, "num");
    add_import_all(&imports.clauses, a, 2, "abs", "numeric");
    add_import_all(&imports.clauses, a, 2, "abs", "show");

    add_import(&imports.clauses, a, 1, "abs");
    add_import(&imports.clauses, a, 1, "data");
    add_import(&imports.clauses, a, 1, "platform");
    add_import(&imports.clauses, a, 1, "meta");
    add_import(&imports.clauses, a, 1, "foreign");
    add_import(&imports.clauses, a, 1, "debug");

    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };

    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("user")),
        .imports = imports,
        .exports = exports,
    };
    PiAllocator pico_module_allocator = convert_to_pallocator(a);
    Module* module = mk_module(header, base, NULL, pico_module_allocator);
    delete_module_header(header);

    add_module(string_to_symbol(mv_string("user")), module, base);
}
