#include "platform/signals.h"
#include "pico/stdlib/data/submodules.h"

void add_either_module(Assembler *ass, Module *data, Allocator *a) {
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, a),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("either")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(data), NULL, a);
    delete_module_header(header);
    //Symbol sym;

    //PiType* typep;
    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    Result r = add_module_def(data, string_to_symbol(mv_string("either")), module);
    if (r.type == Err) panic(r.error_message);
}
