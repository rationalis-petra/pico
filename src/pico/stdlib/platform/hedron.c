#include "platform/signals.h"
#include "platform/window/window.h"

#include "pico/values/ctypes.h"
#include "pico/codegen/foreign_adapters.h"
#include "pico/stdlib/platform/hedron.h"

//static PiType* window_ty;

void add_hedron_module(Assembler *ass, Module *platform, Allocator *a) {
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, a),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("hedron")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(platform), NULL, a);
    delete_module_header(header);
    Symbol sym;

    ModuleEntry* e;
    PiType type;
    PiType* typep;
    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    Segments prepped;
    Segments fn_segments = {.data = mk_u8_array(0, a),};
    Segments null_segments = (Segments) {
        .code = mk_u8_array(0, a),
        .data = mk_u8_array(0, a),
    };


    sdelete_u8_array(fn_segments.data);
    sdelete_u8_array(null_segments.data);
    sdelete_u8_array(null_segments.code);

    Result r = add_module_def(platform, string_to_symbol(mv_string("hedron")), module);
    if (r.type == Err) panic(r.error_message);
}
