#include "platform/signals.h"

#include "pico/stdlib/foreign.h"

static PiType* exported_c_type;
PiType* get_c_type() {
    return exported_c_type;
}

void add_foreign_module(Package* base, Allocator* a) {
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, a),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("extra")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, base, NULL, a);
    delete_module_header(header);

    PiType type;
    Symbol sym;
    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    Segments null_segments = (Segments) {
        .code = mk_u8_array(0, a),
        .data = mk_u8_array(0, a),
    };


    type = (PiType) {.sort = TPrim, .prim = TFormer};
    TermFormer former = FReinterpretRelic;
    sym = string_to_symbol(mv_string("reinterpret-relic"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FReinterpretNative;
    sym = string_to_symbol(mv_string("reinterpret-native"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FConvertRelic;
    sym = string_to_symbol(mv_string("convert-relic"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FConvertNative;
    sym = string_to_symbol(mv_string("convert-native"));
    add_def(module, sym, type, &former, null_segments, NULL);

    // C Type Type
    {
        type = (PiType) {.sort = TKind, .kind.nargs= 0};
        PiType* type_data;
        PiType prim_sort_type = mk_enum_type(a, 5,
                                             "char", 0,
                                             "short", 0,
                                             "int", 0,
                                             "long", 0,
                                             "long-long", 0);
        type_data = &prim_sort_type;
        sym = string_to_symbol(mv_string("CPrimSort"));
        add_def(module, sym, type, &type_data, null_segments, NULL);

        PiType signed_type = mk_enum_type(a, 3,
                                          "signed", 0,
                                          "unsigned", 0,
                                          "unspecified", 0);
        type_data = &signed_type;
        sym = string_to_symbol(mv_string("Signed"));
        add_def(module, sym, type, &type_data, null_segments, NULL);

        PiType prim_type = mk_struct_type(a, 2,
                                   "sort", prim_sort_type,
                                   "signed", signed_type);
        type_data = &prim_type;
        sym = string_to_symbol(mv_string("CPrim"));
        add_def(module, sym, type, &type_data, null_segments, NULL);


        PiType c_type = mk_enum_type(a, 3,
                                     "void", 0,
                                     "prim", 1, prim_type,
                                     "unspecified", 0);
        type_data = &c_type;
        sym = string_to_symbol(mv_string("CType"));
        add_def(module, sym, type, &type_data, null_segments, NULL);

        ModuleEntry* e = get_def(sym, module);
        exported_c_type = e->value;

        //delete_pi_type(prim_type, a);
        delete_pi_type(c_type, a);
    }


    add_module(string_to_symbol(mv_string("foreign")), module, base);
    sdelete_u8_array(null_segments.code);
    sdelete_u8_array(null_segments.data);
}
