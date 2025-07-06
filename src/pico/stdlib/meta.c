#include "platform/signals.h"
#include "platform/memory/arena.h"

#include "pico/data/range.h"
#include "pico/syntax/concrete.h"
#include "pico/values/modular.h"
#include "pico/codegen/foreign_adapters.h"
#include "pico/stdlib/core.h"

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

CType mk_symbol_ctype(Allocator* a) {
    return mk_struct_ctype(a, 2,
                           "name", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                           "did", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}));
}

CType mk_range_ctype(Allocator* a) {
    return mk_struct_ctype(a, 2,
                           "start", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                           "end", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}));
}

CType mk_atom_ctype(Allocator* a) {
    CType atom_union = mk_union_ctype(a, 5,
                                      "bool", mk_primint_ctype((CPrimInt){.prim = CChar, .is_signed = Unsigned}),
                                      "int_64", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Signed}),
                                      "double", (CType){.sort = CSDouble},
                                      "Symbol", mk_symbol_ctype(a),
                                      "String", mk_string_ctype(a));
    return mk_struct_ctype(a, 2,
                           "tag", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                           "term", atom_union);
}

CType mk_branch_ctype(Allocator* a) {
    return mk_struct_ctype(a, 3,
                           "range", mk_range_ctype(a),
                           "hint", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                           "branch", mk_list_ctype(a));
}

CType mk_syntax_ctype(Allocator* a) {
    CType atom_range = mk_struct_ctype(a, 2,
                                       "range", mk_range_ctype(a),
                                       "atom", mk_atom_ctype(a));

    CType union_ctype = mk_union_ctype(a, 2, "atom", atom_range, "node", mk_branch_ctype(a));

    // Enum [:atom Range (Enum Atom)] [:branch Range Hint (Array Syntax)]

    return mk_struct_ctype(a, 2,
                           "tag", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                           "term", union_ctype);
}

CType mk_macro_result_ctype(Allocator* a) {
    CType macro_error = mk_struct_ctype(a, 2, "message", mk_string_ctype(a), "range", mk_range_ctype(a));

    CType union_ctype = mk_union_ctype(a, 2, "left", macro_error, "right", mk_syntax_ctype(a));

    // Enum [:left (Pair message range)] [:right Syntax]

    return mk_struct_ctype(a, 2,
                           "tag", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                           "term", union_ctype);
}

void build_mk_name_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // Proc type
    CType name_ctype = mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned});

    CType fn_ctype = mk_fn_ctype(a, 1, "name", mk_string_ctype(a), name_ctype);

    convert_c_fn(string_to_name, &fn_ctype, type, ass, a, point); 
}

void build_mk_symbol_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // Proc type
    CType fn_ctype = mk_fn_ctype(a, 1, "symbol", mk_string_ctype(a), mk_symbol_ctype(a));

    convert_c_fn(string_to_symbol, &fn_ctype, type, ass, a, point); 
}

void build_mk_unique_symbol_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // Proc type
    CType fn_ctype = mk_fn_ctype(a, 1, "symbol", mk_string_ctype(a), mk_symbol_ctype(a));

    convert_c_fn(string_to_unique_symbol, &fn_ctype, type, ass, a, point); 
}

Range relic_range(uint64_t start, uint64_t end) {
  return (Range) {
      .start = start, .end = end
  };
}

void build_range_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // Proc type
    CType fn_ctype = mk_fn_ctype(a, 2,
                                 "start", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                                 "end", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                                 mk_range_ctype(a));

    convert_c_fn(relic_range, &fn_ctype, type, ass, a, point); 
}

Range get_range(RawTree syntax) {
    return syntax.range;
}

void build_get_range_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // Proc type
    CType fn_ctype = mk_fn_ctype(a, 1, "Synatx", mk_syntax_ctype(a), mk_range_ctype(a));

    convert_c_fn(get_range, &fn_ctype, type, ass, a, point); 
}

void add_meta_module(Assembler* ass, Package* base, Allocator* a) {
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, a),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("meta")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, base, NULL, a);
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

    // ------------------------------------------------------------------------
    // Term Formers
    // ------------------------------------------------------------------------
    former = FTypeOf;
    sym = string_to_symbol(mv_string("type-of"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FQuote;
    sym = string_to_symbol(mv_string("quote"));
    add_def(module, sym, type, &former, null_segments, NULL);

    // ------------------------------------------------------------------------
    // Types 
    // ------------------------------------------------------------------------
    {
        type = (PiType){.sort = TKind, .kind.nargs = 0};
        ModuleEntry* e;


        symbol_type = mk_named_type(a, "Symbol",
                                  mk_struct_type(a, 2,
                                                 "name", mk_prim_type(a, UInt_64), 
                                                 "did", mk_prim_type(a, UInt_64)));
        typep = symbol_type;
        sym = string_to_symbol(mv_string("Symbol"));
        add_def(module, sym, type, &typep, null_segments, NULL);
        delete_pi_type_p(typep, a);

        e = get_def(sym, module);
        symbol_type = e->value;

        PiType* atom_type = mk_enum_type(a, 5,
                                        "bool", 1, mk_prim_type(a, Bool),
                                        "integral", 1, mk_prim_type(a, Int_64),
                                        "floating", 1, mk_prim_type(a, Float_64),
                                        "symbol", 1,  copy_pi_type_p(symbol_type, a),
                                        "string", 1, mk_string_type(a));
        typep = atom_type;
        sym = string_to_symbol(mv_string("Atom"));
        add_def(module, sym, type, &typep, null_segments, NULL);

        PiType* hint_type = mk_enum_type(a, 4, "none", 0, "expr", 0, "special", 0, "implicit", 0);
        typep = hint_type;
        sym = string_to_symbol(mv_string("Hint"));
        add_def(module, sym, type, &typep, null_segments, NULL);

        range_type = mk_struct_type(a, 2, "start", mk_prim_type(a, UInt_64), "end", mk_prim_type(a, UInt_64));
        typep = range_type;
        sym = string_to_symbol(mv_string("Range"));
        add_def(module, sym, type, &typep, null_segments, NULL);

        e = get_def(sym, module);
        range_type = e->value;

        PiType* syn_name_ty = mk_var_type(a, "Syntax");
        PiType* syn_array = mk_app_type(a, get_list_type(), syn_name_ty);
        delete_pi_type_p(syn_name_ty, a);

        typep = mk_named_type(a, "Syntax",
                                 mk_enum_type(a, 2,
                                              "atom", 2, copy_pi_type_p(range_type, a), atom_type,
                                              "node", 3, copy_pi_type_p(range_type, a), hint_type, syn_array));

        sym = string_to_symbol(mv_string("Syntax"));
        add_def(module, sym, type, &typep, null_segments, NULL);
        e = get_def(sym, module);
        syntax_type = e->value;

        delete_pi_type_p(typep, a);

        typep = mk_struct_type(a, 2, "message", mk_string_type(a), "range", copy_pi_type_p(range_type, a));
        sym = string_to_symbol(mv_string("MacroError"));
        add_def(module, sym, type, &typep, null_segments, NULL);
        e = get_def(sym, module);
        macro_error_type = e->value;

        delete_pi_type_p(typep, a);

        typep = mk_app_type(a, get_either_type(), copy_pi_type_p(macro_error_type, a), copy_pi_type_p(syntax_type, a));
        sym = string_to_symbol(mv_string("MacroResult"));
        add_def(module, sym, type, &typep, null_segments, NULL);
        e = get_def(sym, module);
        macro_result_type = e->value;

        delete_pi_type_p(typep, a);
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

    typep = mk_proc_type(a, 1, mk_string_type(a), mk_prim_type(a, UInt_64));
    build_mk_name_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("mk-name"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(a, 1, mk_string_type(a), copy_pi_type_p(get_symbol_type(), a));
    build_mk_symbol_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("mk-symbol"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(a, 1, mk_string_type(a), copy_pi_type_p(get_symbol_type(), a));
    build_mk_unique_symbol_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("mk-unique-symbol"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(a, 1, mk_string_type(a), copy_pi_type_p(get_symbol_type(), a));
    build_mk_unique_symbol_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("mk-unique-symbol"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(a, 2, mk_prim_type(a, UInt_64), mk_prim_type(a, UInt_64), copy_pi_type_p(range_type, a));
    build_range_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("range"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(a, 1, get_syntax_type(), copy_pi_type_p(range_type, a));
    build_get_range_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("get-range"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    add_module(string_to_symbol(mv_string("meta")), module, base);

    sdelete_u8_array(null_segments.code);
    sdelete_u8_array(null_segments.data);
    // Note: we do NOT delete the 'fn_segments.code' because it is the
    // assembler, and needs to be used later!
    sdelete_u8_array(fn_segments.data);
    release_arena_allocator(arena);
}

