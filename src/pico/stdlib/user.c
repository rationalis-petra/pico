#include "pico/stdlib/user.h"

void add_user_module(Package* base, Allocator* a) {
    Imports imports = (Imports) {.clauses = mk_import_clause_array(3, a),};
    push_import_clause((ImportClause) {
            .type = ImportAll,
            .name = string_to_symbol(mv_string("core")),
        },
        &imports.clauses);
    push_import_clause((ImportClause) {
            .type = ImportAll,
            .name = string_to_symbol(mv_string("num")),
        },
        &imports.clauses);
    push_import_clause((ImportClause) {
            .type = ImportAll,
            .name = string_to_symbol(mv_string("extra")),
        },
        &imports.clauses);
    push_import_clause((ImportClause) {
            .type = Import,
            .name = string_to_symbol(mv_string("foreign")),
        },
        &imports.clauses);

    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };

    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("user")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, base, NULL, a);
    delete_module_header(header);

    add_module(string_to_symbol(mv_string("user")), module, base);
}
