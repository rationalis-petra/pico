
#include "platform/signals.h"
#include "pico/stdlib/debug.h"


void add_debug_module(Target target, Package* base, Allocator* a) {
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, a),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("debug")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, base, NULL, a);
    delete_module_header(header);
    Symbol sym;

    PiType type;
    //PiType* typep;
    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    // TODO: we use int64_t as it has the requisite size (8 bytes)
    // for pico values: currently don't support non-64 bit values 
    TermFormer former;
    //TermFormer former;
    type.sort = TPrim;
    type.prim = TFormer;

    Segments null_segments = (Segments) {
        .code = mk_u8_array(0, a),
        .data = mk_u8_array(0, a),
    };

    // ------------------------------------------------------------------------
    // Term Formers
    // ------------------------------------------------------------------------
    former = FDescribe;
    sym = string_to_symbol(mv_string("describe"));
    add_def(module, sym, type, &former, null_segments, NULL);

    add_module(string_to_symbol(mv_string("debug")), module, base);

    sdelete_u8_array(null_segments.code);
    sdelete_u8_array(null_segments.data);
    // Note: do NOT delete the 'fn_segments.code' because it is the
    // assembler, and needs to be used later!
    //sdelete_u8_array(fn_segments.data);
}

