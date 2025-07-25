#include "platform/signals.h"
#include "platform/dynamic_library.h"
#include "platform/memory/arena.h"

#include "pico/codegen/foreign_adapters.h"
#include "pico/stdlib/core.h"
#include "pico/stdlib/extra.h"
#include "pico/stdlib/foreign.h"

static PiType* exported_c_type;
PiType* get_c_type() {
    return exported_c_type;
}

typedef struct {
    uint64_t tag;
    union {
        String error_message;
        DynLib* result;
    };
} DynLibResult;

DynLibResult wrap_dynlib_open(String str) {
    Allocator* a = get_std_allocator();
    DynLib* out;
    Result res = open_lib(&out, str, a);

    if (res.type == Err) {
      return (DynLibResult) {
          .tag = 0,
          .error_message = res.error_message,
      };
    } else {
      return (DynLibResult) {
          .tag = 1,
          .result = out,
      };
    }
}

void wrap_dynlib_close(DynLib* lib) {
    close_lib(lib);
}

typedef struct {
    uint64_t tag;
    union {
        String error_message;
        void* result;
    };
} SymbolResult;

SymbolResult wrap_dynlib_sym(DynLib* lib, String symbol) {
    void* out; 

    Result res = lib_sym(&out, lib, symbol);

    if (res.type == Err) {
        return (SymbolResult) {.tag = 0, .error_message = res.error_message};
    }
    else {
        return (SymbolResult) {.tag = 1, .result = out};
    }
}

void build_dynlib_open_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // here, we use void pointers because we don't need the  
    // C API to check everything for us.
    // DynLibResult
    CType dynlib_result = mk_struct_ctype(
        a, 2, "tag",
        mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}), "data",
        mk_union_ctype(a, 2,
                       "error_message", mk_string_ctype(a),
                       "dynlib", mk_voidptr_ctype(a)));

    // Proc type
    CType fn_ctype = mk_fn_ctype(a, 1, "path", mk_string_ctype(a), dynlib_result);

    convert_c_fn(wrap_dynlib_open, &fn_ctype, type, ass, a, point); 

    delete_c_type(fn_ctype, a);
}

void build_dynlib_close_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // Proc type
    CType fn_ctype = mk_fn_ctype(a, 1, "lib", copy_c_type(mk_voidptr_ctype(a), a), c_void);

    convert_c_fn(wrap_dynlib_close, &fn_ctype, type, ass, a, point); 

    delete_c_type(fn_ctype, a);
}

void build_dynlib_symbol_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // here, we use void pointers because we don't need the  
    // C API to check everything for us.
    // DynLibResult
    CType symbol_result = mk_struct_ctype(
        a, 2, "tag",
        mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}), "data",
        mk_union_ctype(a, 2,
                       "error_message", mk_string_ctype(a),
                       "value", mk_voidptr_ctype(a)));

    // Proc type
    CType fn_ctype = mk_fn_ctype(a, 2,
                                 "lib", mk_voidptr_ctype(a),
                                 "symbol", mk_string_ctype(a),
                                 symbol_result);

    convert_c_fn(wrap_dynlib_sym, &fn_ctype, type, ass, a, point); 

    delete_c_type(fn_ctype, a);

}

void add_foreign_module(Assembler* ass, Package *base, Allocator* a) {

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
    PiType* typep;
    Symbol sym;
    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    Segments null_segments = (Segments) {
        .code = mk_u8_array(0, a),
        .data = mk_u8_array(0, a),
    };

    // Now that we have setup appropriately, override the allocator
    Allocator arena = mk_arena_allocator(4096, a);
    a = &arena;

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
        PiType* prim_sort_type = mk_enum_type(a, 5,
                                             "char", 0,
                                             "short", 0,
                                             "int", 0,
                                             "long", 0,
                                             "long-long", 0);
        type_data = prim_sort_type;
        sym = string_to_symbol(mv_string("CPrimSort"));
        add_def(module, sym, type, &type_data, null_segments, NULL);

        PiType* signed_type = mk_enum_type(a, 3,
                                          "signed", 0,
                                          "unsigned", 0,
                                          "unspecified", 0);
        type_data = signed_type;
        sym = string_to_symbol(mv_string("Signed"));
        add_def(module, sym, type, &type_data, null_segments, NULL);

        PiType* prim_type = mk_struct_type(a, 2,
                                   "sort", prim_sort_type,
                                   "signed", signed_type);
        type_data = prim_type;
        sym = string_to_symbol(mv_string("CPrim"));
        add_def(module, sym, type, &type_data, null_segments, NULL);


        // TODO: replace u64 with symbol
        PiType *c_type =
            mk_named_type(a, "CType",
                          mk_enum_type(a, 7,
                                       "void", 0,
                                       "prim-int", 1, prim_type,
                                       "float", 0,
                                       "double", 0,
                                       "ptr", 1, mk_app_type(a, get_ptr_type(), mk_var_type(a, "CType")),
                                       "proc", 3,
                                         mk_app_type(a, get_maybe_type(), mk_prim_type(a, UInt_64)),
                                         mk_app_type(a, get_list_type(), mk_app_type(a, get_pair_type(), mk_prim_type(a, UInt_64), mk_var_type(a, "CType"))),
                                         mk_app_type(a, get_ptr_type(), mk_var_type(a, "CType")),
                                       "struct", 2,
                                         mk_app_type(a, get_maybe_type(), mk_prim_type(a, UInt_64)),
                                         mk_app_type(a, get_list_type(), mk_app_type(a, get_pair_type(), mk_prim_type(a, UInt_64), mk_var_type(a, "CType"))),
                                       "unspecified", 0));
        type_data = c_type;
        sym = string_to_symbol(mv_string("CType"));
        add_def(module, sym, type, &type_data, null_segments, NULL);

        ModuleEntry* e = get_def(sym, module);
        exported_c_type = e->value;
    }

    Segments fn_segments = {.data = mk_u8_array(0, a),};
    Segments prepped;
    PiType* type_data;

    // TODO: fill in correct type
    // dynlib-open : Proc [String] (Either String DynLib)
    PiType* dynlib_ty = mk_opaque_type(a, module, mk_prim_type(a, Address));
    type_data = dynlib_ty;
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("DynLib"));
    add_def(module, sym, type, &type_data, null_segments, NULL);
    clear_assembler(ass);

    PiType* str = mk_string_type(a);
    typep = mk_proc_type(a, 1, mk_string_type(a), mk_app_type(a, get_either_type(), str, copy_pi_type_p(dynlib_ty, a)));
    build_dynlib_open_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("dynlib-open"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(a, 1, copy_pi_type_p(dynlib_ty, a), mk_prim_type(a, Unit));
    build_dynlib_close_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("dynlib-close"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    str = mk_string_type(a);
    typep = mk_proc_type(a, 2, dynlib_ty, mk_string_type(a),
                         mk_app_type(a, get_either_type(), str, mk_prim_type(a, Address)));
    build_dynlib_symbol_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("dynlib-symbol"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    // Short-hand C types 
    CType c_type;
    CType* cdata = &c_type;

    // Int
    typep = get_c_type();
    sym = string_to_symbol(mv_string("c-long-long"));
    c_type = mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unspecified});
    add_def(module, sym, *typep, cdata, null_segments, NULL);

    typep = get_c_type();
    sym = string_to_symbol(mv_string("c-signed-long-long"));
    c_type = mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Signed});
    add_def(module, sym, *typep, cdata, null_segments, NULL);

    typep = get_c_type();
    sym = string_to_symbol(mv_string("c-unsigned-long-long"));
    c_type = mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned});
    add_def(module, sym, *typep, cdata, null_segments, NULL);

    typep = get_c_type();
    sym = string_to_symbol(mv_string("c-long"));
    c_type = mk_primint_ctype((CPrimInt){.prim = CLong, .is_signed = Unspecified});
    add_def(module, sym, *typep, cdata, null_segments, NULL);

    typep = get_c_type();
    sym = string_to_symbol(mv_string("c-signed-long"));
    c_type = mk_primint_ctype((CPrimInt){.prim = CLong, .is_signed = Signed});
    add_def(module, sym, *typep, cdata, null_segments, NULL);

    typep = get_c_type();
    sym = string_to_symbol(mv_string("c-unsigned-long"));
    c_type = mk_primint_ctype((CPrimInt){.prim = CLong, .is_signed = Unsigned});
    add_def(module, sym, *typep, cdata, null_segments, NULL);

    typep = get_c_type();
    sym = string_to_symbol(mv_string("c-int"));
    c_type = mk_primint_ctype((CPrimInt){.prim = CInt, .is_signed = Unspecified});
    add_def(module, sym, *typep, cdata, null_segments, NULL);

    typep = get_c_type();
    sym = string_to_symbol(mv_string("c-signed-int"));
    c_type = mk_primint_ctype((CPrimInt){.prim = CInt, .is_signed = Signed});
    add_def(module, sym, *typep, cdata, null_segments, NULL);

    typep = get_c_type();
    sym = string_to_symbol(mv_string("c-unsigned-int"));
    c_type = mk_primint_ctype((CPrimInt){.prim = CInt, .is_signed = Unsigned});
    add_def(module, sym, *typep, cdata, null_segments, NULL);

    typep = get_c_type();
    sym = string_to_symbol(mv_string("c-short"));
    c_type = mk_primint_ctype((CPrimInt){.prim = CShort, .is_signed = Unspecified});
    add_def(module, sym, *typep, cdata, null_segments, NULL);

    typep = get_c_type();
    sym = string_to_symbol(mv_string("c-signed-short"));
    c_type = mk_primint_ctype((CPrimInt){.prim = CShort, .is_signed = Signed});
    add_def(module, sym, *typep, cdata, null_segments, NULL);

    typep = get_c_type();
    sym = string_to_symbol(mv_string("c-unsigned-short"));
    c_type = mk_primint_ctype((CPrimInt){.prim = CShort, .is_signed = Unsigned});
    add_def(module, sym, *typep, cdata, null_segments, NULL);

    typep = get_c_type();
    sym = string_to_symbol(mv_string("c-char"));
    c_type = mk_primint_ctype((CPrimInt){.prim = CChar, .is_signed = Unspecified});
    add_def(module, sym, *typep, cdata, null_segments, NULL);

    typep = get_c_type();
    sym = string_to_symbol(mv_string("c-signed-char"));
    c_type = mk_primint_ctype((CPrimInt){.prim = CShort, .is_signed = Signed});
    add_def(module, sym, *typep, cdata, null_segments, NULL);

    typep = get_c_type();
    sym = string_to_symbol(mv_string("c-unsigned-char"));
    c_type = mk_primint_ctype((CPrimInt){.prim = CChar, .is_signed = Unsigned});
    add_def(module, sym, *typep, cdata, null_segments, NULL);

    add_module(string_to_symbol(mv_string("foreign")), module, base);
    sdelete_u8_array(null_segments.code);
    sdelete_u8_array(null_segments.data);

    sdelete_u8_array(fn_segments.data);
    release_arena_allocator(arena);
}
