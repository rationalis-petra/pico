#include "platform/signals.h"
#include "platform/memory/arena.h"

#include "pico/data/range.h"
#include "pico/syntax/concrete.h"
#include "pico/values/modular.h"
#include "pico/codegen/codegen.h"
#include "pico/stdlib/core.h"
#include "pico/stdlib/meta/submodules.h"

static PiType* symbol_type;
PiType* get_symbol_type() {
    return symbol_type;
}

static PiType* syntax_type;
PiType* get_syntax_type() {
    return syntax_type;
}

static PiType* range_type;
static PiType* macro_error_type;

static PiType* macro_result_type;
PiType* get_macro_result_type() {
    return macro_result_type;
}

CType mk_symbol_ctype(PiAllocator* pia) {
    return mk_struct_ctype(pia, 2,
                           "name", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                           "did", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}));
}

CType mk_range_ctype(PiAllocator* pia) {
    return mk_struct_ctype(pia, 2,
                           "start", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                           "end", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}));
}

CType mk_capture_ctype(PiAllocator* pia) {
    return mk_struct_ctype(pia, 2,
                           "type", mk_voidptr_ctype(pia),
                           "value", mk_voidptr_ctype(pia));
}

CType mk_atom_ctype(PiAllocator* pia) {
    CType atom_union = mk_union_ctype(pia, 6,
                                      "bool", mk_primint_ctype((CPrimInt){.prim = CChar, .is_signed = Unsigned}),
                                      "int_64", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Signed}),
                                      "double", (CType){.sort = CSDouble},
                                      "Symbol", mk_symbol_ctype(pia),
                                      "String", mk_string_ctype(pia),
                                      "Capture", mk_capture_ctype(pia));
    return mk_struct_ctype(pia, 2,
                           "tag", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                           "term", atom_union);
}

CType mk_branch_ctype(PiAllocator* pia) {
    return mk_struct_ctype(pia, 3,
                           "range", mk_range_ctype(pia),
                           "hint", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                           "branch", mk_list_ctype(pia));
}

CType mk_syntax_ctype(PiAllocator* pia) {
    CType atom_range = mk_struct_ctype(pia, 2,
                                       "range", mk_range_ctype(pia),
                                       "atom", mk_atom_ctype(pia));

    CType union_ctype = mk_union_ctype(pia, 2, "atom", atom_range, "node", mk_branch_ctype(pia));

    // Enum [:atom Range (Enum Atom)] [:branch Range Hint (Array Syntax)]

    return mk_struct_ctype(pia, 2,
                           "tag", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                           "term", union_ctype);
}

CType mk_macro_result_ctype(PiAllocator* pia) {
    CType macro_error = mk_struct_ctype(pia, 2, "message", mk_string_ctype(pia), "range", mk_range_ctype(pia));

    CType union_ctype = mk_union_ctype(pia, 2, "left", macro_error, "right", mk_syntax_ctype(pia));

    // Enum [:left (Pair message range)] [:right Syntax]

    return mk_struct_ctype(pia, 2,
                           "tag", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                           "term", union_ctype);
}

void build_mk_name_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a,ErrorPoint* point) {
    // Proc type
    CType name_ctype = mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned});

    CType fn_ctype = mk_fn_ctype(pia, 1, "name", mk_string_ctype(pia), name_ctype);

    convert_c_fn(string_to_name, &fn_ctype, type, ass, a, point); 
}

void build_mk_symbol_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    // Proc type
    CType fn_ctype = mk_fn_ctype(pia, 1, "symbol", mk_string_ctype(pia), mk_symbol_ctype(pia));

    convert_c_fn(string_to_symbol, &fn_ctype, type, ass, a, point); 
}

void build_mk_unique_symbol_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    // Proc type
    CType fn_ctype = mk_fn_ctype(pia, 1, "symbol", mk_string_ctype(pia), mk_symbol_ctype(pia));

    convert_c_fn(string_to_unique_symbol, &fn_ctype, type, ass, a, point); 
}

Range relic_range(uint64_t start, uint64_t end) {
  return (Range) {
      .start = start, .end = end
  };
}

void build_range_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    // Proc type
    CType fn_ctype = mk_fn_ctype(pia, 2,
                                 "start", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                                 "end", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                                 mk_range_ctype(pia));

    convert_c_fn(relic_range, &fn_ctype, type, ass, a, point); 
}

Range get_range(RawTree syntax) {
    return syntax.range;
}

void build_get_range_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    // Proc type
    CType fn_ctype = mk_fn_ctype(pia, 1, "Synatx", mk_syntax_ctype(pia), mk_range_ctype(pia));

    convert_c_fn(get_range, &fn_ctype, type, ass, a, point); 
}

void add_gen_module(Assembler* ass, Module* base, Allocator* a) {
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, a),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("gen")),
        .imports = imports,
        .exports = exports,
    };
    PiAllocator pico_module_allocator = convert_to_pallocator(a);
    Module* module = mk_module(header, get_package(base), NULL, pico_module_allocator);
    delete_module_header(header);
    Symbol sym;

    PiType type;
    PiType* typep;
    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    // TODO: we use int64_t as it has the requisite size (8 bytes)
    // for pico values: currently don't support non-64 bit values 
    TermFormer former;
    // TermFormer former;
    type.sort = TPrim;
    type.prim = TFormer;

    Segments null_segments = (Segments) {
        .code = mk_u8_array(0, a),
        .data = mk_u8_array(0, a),
    };

    // Now that we have setup appropriately, override the allocator
    Allocator arena = mk_arena_allocator(4096, a);
    a = &arena;
    PiAllocator pia = convert_to_pallocator(a);

    // ------------------------------------------------------------------------
    // Term Formers
    // ------------------------------------------------------------------------
    former = FTypeOf;
    sym = string_to_symbol(mv_string("type-of"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FQuote;
    sym = string_to_symbol(mv_string("quote"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FCapture;
    sym = string_to_symbol(mv_string("capture"));
    add_def(module, sym, type, &former, null_segments, NULL);

    // ------------------------------------------------------------------------
    // Types 
    // ------------------------------------------------------------------------
    {
        type = (PiType){.sort = TKind, .kind.nargs = 0};
        ModuleEntry* e;


        symbol_type = mk_named_type(&pia, "Symbol",
                                  mk_struct_type(&pia, 2,
                                                 "name", mk_prim_type(&pia, UInt_64), 
                                                 "did", mk_prim_type(&pia, UInt_64)));
        typep = symbol_type;
        sym = string_to_symbol(mv_string("Symbol"));
        add_def(module, sym, type, &typep, null_segments, NULL);
        delete_pi_type_p(typep, &pia);

        e = get_def(sym, module);
        symbol_type = e->value;

        PiType* atom_type = mk_enum_type(&pia, 6,
                                         "bool", 1, mk_prim_type(&pia, Bool),
                                         "integral", 1, mk_prim_type(&pia, Int_64),
                                         "floating", 1, mk_prim_type(&pia, Float_64),
                                         "symbol", 1,  copy_pi_type_p(symbol_type, &pia),
                                         "string", 1, mk_string_type(&pia),
                                         "capture", 2, mk_prim_type(&pia, Address), mk_prim_type(&pia, Address));
        typep = atom_type;
        sym = string_to_symbol(mv_string("Atom"));
        add_def(module, sym, type, &typep, null_segments, NULL);

        PiType* hint_type = mk_enum_type(&pia, 3, "expr", 0, "special", 0, "implicit", 0);
        typep = hint_type;
        sym = string_to_symbol(mv_string("Hint"));
        add_def(module, sym, type, &typep, null_segments, NULL);

        range_type = mk_struct_type(&pia, 2, "start", mk_prim_type(&pia, UInt_64), "end", mk_prim_type(&pia, UInt_64));
        typep = range_type;
        sym = string_to_symbol(mv_string("Range"));
        add_def(module, sym, type, &typep, null_segments, NULL);

        e = get_def(sym, module);
        range_type = e->value;

        PiType* syn_name_ty = mk_var_type(&pia, "Syntax");
        PiType* syn_array = mk_app_type(&pia, get_list_type(), syn_name_ty);
        delete_pi_type_p(syn_name_ty, &pia);

        typep = mk_named_type(&pia, "Syntax",
                                 mk_enum_type(&pia, 2,
                                              "atom", 2, copy_pi_type_p(range_type, &pia), atom_type,
                                              "node", 3, copy_pi_type_p(range_type, &pia), hint_type, syn_array));

        sym = string_to_symbol(mv_string("Syntax"));
        add_def(module, sym, type, &typep, null_segments, NULL);
        e = get_def(sym, module);
        syntax_type = e->value;

        delete_pi_type_p(typep, &pia);

        typep = mk_struct_type(&pia, 2, "message", mk_string_type(&pia), "range", copy_pi_type_p(range_type, &pia));
        sym = string_to_symbol(mv_string("MacroError"));
        add_def(module, sym, type, &typep, null_segments, NULL);
        e = get_def(sym, module);
        macro_error_type = e->value;

        delete_pi_type_p(typep, &pia);

        typep = mk_app_type(&pia, get_either_type(), copy_pi_type_p(macro_error_type, &pia), copy_pi_type_p(syntax_type, &pia));
        sym = string_to_symbol(mv_string("MacroResult"));
        add_def(module, sym, type, &typep, null_segments, NULL);
        e = get_def(sym, module);
        macro_result_type = e->value;

        delete_pi_type_p(typep, &pia);
    }

    // ------------------------------------------------------------------------
    // Values 
    // ------------------------------------------------------------------------

    type = (PiType) {
        .sort = TKind,
        .kind.nargs = 0,
    };

    Segments fn_segments = (Segments) {.data = mk_u8_array(0, a),};
    Segments prepped;

    typep = mk_proc_type(&pia, 1, mk_string_type(&pia), mk_prim_type(&pia, UInt_64));
    build_mk_name_fn(typep, ass, &pia, a, &point);
    sym = string_to_symbol(mv_string("mk-name"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(&pia, 1, mk_string_type(&pia), copy_pi_type_p(get_symbol_type(), &pia));
    build_mk_symbol_fn(typep, ass, &pia, a, &point);
    sym = string_to_symbol(mv_string("mk-symbol"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(&pia, 1, mk_string_type(&pia), copy_pi_type_p(get_symbol_type(), &pia));
    build_mk_unique_symbol_fn(typep, ass, &pia, a, &point);
    sym = string_to_symbol(mv_string("mk-unique-symbol"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(&pia, 1, mk_string_type(&pia), copy_pi_type_p(get_symbol_type(), &pia));
    build_mk_unique_symbol_fn(typep, ass, &pia, a, &point);
    sym = string_to_symbol(mv_string("mk-unique-symbol"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(&pia, 2, mk_prim_type(&pia, UInt_64), mk_prim_type(&pia, UInt_64), copy_pi_type_p(range_type, &pia));
    build_range_fn(typep, ass, &pia, a, &point);
    sym = string_to_symbol(mv_string("range"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(&pia, 1, get_syntax_type(), copy_pi_type_p(range_type, &pia));
    build_get_range_fn(typep, ass, &pia, a, &point);
    sym = string_to_symbol(mv_string("get-range"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    add_module_def(base, string_to_symbol(mv_string("gen")), module);

    sdelete_u8_array(null_segments.code);
    sdelete_u8_array(null_segments.data);
    // Note: we do NOT delete the 'fn_segments.code' because it is the
    // assembler, and needs to be used later!
    sdelete_u8_array(fn_segments.data);
    release_arena_allocator(arena);
}
