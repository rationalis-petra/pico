#include "platform/signals.h"
#include "pico/stdlib/helpers.h"
#include "pico/stdlib/data/array.h"


// Interface
// elt :: All [A] Proc [U64 (Array A)] A
// map :: All [A B] Proc [(Proc [A] B) (Array A)] A
// 


void add_array_module(Assembler *ass, Module *data, Allocator *a) {
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, a),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("core")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(data), NULL, a);
    delete_module_header(header);
    Symbol sym;

    PiType* typep;
    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }


    Result r = add_module_def(data, string_to_symbol(mv_string("Array")), module);
    if (r.type == Err) panic(r.error_message);
}
