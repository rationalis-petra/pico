#include <string.h>

#include "platform/signals.h"
#include "platform/memory/arena.h"
#include "platform/machine_info.h"

#include "pico/codegen/backend-direct/internal.h"
#include "pico/stdlib/core.h"

static PiType* ptr_type;
PiType* get_ptr_type() {
    return ptr_type;
}

static PiType* list_type;
PiType* get_list_type() {
    return list_type;
}

static PiType* maybe_type;
PiType* get_maybe_type() {
    return maybe_type;
}

static PiType* either_type;
PiType* get_either_type() {
    return either_type;
}

static PiType* pair_type;
PiType* get_pair_type() {
    return pair_type;
}

static PiType* allocator_type;
PiType* get_allocator_type() {
    return allocator_type;
}

PiType build_store_fn_ty(PiAllocator* pia) {
    PiType* proc_ty  = mk_proc_type(pia, 2, mk_prim_type(pia, Address), mk_var_type(pia, "A"), mk_prim_type(pia, Unit));

    SymbolPiList types = mk_sym_list(1, pia);
    push_sym(string_to_symbol(mv_string("A")), &types);

    return (PiType) {.sort = TAll, .binder.vars = types, .binder.body = proc_ty};
}

void build_store_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // The usual calling convention for polymorphic functions is assumed, hence
    // stack has the form:
    // RSP-24  | type size
    // RSP-16  | store address
    // RSP-8   | variable stack index (value/ptr)
    // RSP     | return address 

    build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);

#if ABI == SYSTEM_V_64
    // memcpy (dest = rdi, src = rsi, size = rdx)
    build_unary_op(Pop, reg(RSI, sz_64), ass, a, point);
    build_unary_op(Pop, reg(RDI, sz_64), ass, a, point);

    build_binary_op(Mov, reg(RDX, sz_64), rref8(RSP, 0, sz_64), ass, a, point);
    build_binary_op(SHR, reg(RDX, sz_64), imm8(28), ass, a, point);
    build_binary_op(And, reg(RDX, sz_64), imm32(0xFFFFFFF), ass, a, point);

#elif ABI == WIN_64
    // memcpy (dest = rcx, src = rdx, size = r8)
    build_unary_op(Pop, reg(RDX, sz_64), ass, a, point);
    build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);

    build_binary_op(Mov, reg(R8, sz_64), rref8(RSP, 0, sz_64), ass, a, point);
    build_binary_op(SHR, reg(R8, sz_64), imm8(28), ass, a, point);
    build_binary_op(And, reg(R8, sz_64), imm32(0xFFFFFFF), ass, a, point);
#else
#error "Unknown calling convention"
#endif
    // Push return address
    build_unary_op(Push, reg(RAX, sz_64), ass, a, point);

    // copy memcpy into RCX & call
    generate_c_call(memcpy, ass, a, point);

    build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);

    // Stack size of & return
    build_unary_op(Pop, reg(R9, sz_64), ass, a, point);
    build_binary_op(And, reg(R9, sz_64), imm32(0xFFFFFFF), ass, a, point);
    build_binary_op(Add, reg(R14, sz_64), reg(R9, sz_64), ass, a, point);

    build_unary_op(Push, reg(RAX, sz_64), ass, a, point);

    build_nullary_op(Ret, ass, a, point);
}

PiType build_load_fn_ty(PiAllocator* pia) {
    PiType* proc_ty = mk_proc_type(pia, 1, mk_prim_type(pia, Address), mk_var_type(pia, "A"));

    SymbolPiList types = mk_sym_list(1, pia);
    push_sym(string_to_symbol(mv_string("A")), &types);

    return (PiType) {.sort = TAll, .binder.vars = types, .binder.body = proc_ty};
}

void relic_memcpy(char *dest, char *src, size_t size) {
    for (size_t i = 0; i < size; i++) {
        dest[i] = src[i];
    }
}

void build_load_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // The usual calling convention for polymorphic functions is assumed, hence
    // stack has the form:
    // RSP-16  | type size
    // RSP-8   | load address
    // RSP     | return address 

    // Stash return address in RAX
    build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);

    // Stash load src address
#if ABI == SYSTEM_V_64
    // memcpy (dest = rdi, src = rsi, size = rdx)
    build_unary_op(Pop, reg(RSI, sz_64), ass, a, point);

    // Store size in RDX, stack size in R9
    build_unary_op(Pop, reg(RDX, sz_64), ass, a, point);
    build_binary_op(Mov, reg(R9, sz_64), reg(RDX, sz_64), ass, a, point);
    build_binary_op(And, reg(R9, sz_64), imm32(0xFFFFFFF), ass, a, point);

    build_binary_op(SHR, reg(RDX, sz_64), imm8(28), ass, a, point);
    build_binary_op(And, reg(RDX, sz_64), imm32(0xFFFFFFF), ass, a, point);

    build_binary_op(Sub, reg(R14, sz_64), reg(R9, sz_64), ass, a, point);
    build_binary_op(Mov, reg(RDI, sz_64), reg(R14, sz_64), ass, a, point);

    build_unary_op(Push, reg(R14, sz_64), ass, a, point);
#elif ABI == WIN_64
    // memcpy (dest = rcx, src = rdx, size = r8)
    build_unary_op(Pop, reg(RDX, sz_64), ass, a, point);

    // Store size in R8, stack size in R9
    build_unary_op(Pop, reg(R8, sz_64), ass, a, point);
    build_binary_op(Mov, reg(R9, sz_64), reg(R8, sz_64), ass, a, point);
    build_binary_op(And, reg(R9, sz_64), imm32(0xFFFFFFF), ass, a, point);

    build_binary_op(SHR, reg(R8, sz_64), imm8(28), ass, a, point);
    build_binary_op(And, reg(R8, sz_64), imm32(0xFFFFFFF), ass, a, point);

    build_binary_op(Sub, reg(R14, sz_64), reg(R9, sz_64), ass, a, point);
    build_binary_op(Mov, reg(RCX, sz_64), reg(R14, sz_64), ass, a, point);

    build_unary_op(Push, reg(R14, sz_64), ass, a, point);

#else
#error "Unknown calling convention"
#endif

    // Stash 
    build_unary_op(Push, reg(RAX, sz_64), ass, a, point);

    // copy memcpy into RCX & call
    generate_c_call(relic_memcpy, ass, a, point);

    // Return
    build_nullary_op(Ret, ass, a, point);
}

void build_nop_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    build_nullary_op(Ret, ass, a, point);
}

void add_core_module(Assembler* ass, Package* base, Allocator* a) {
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
    PiAllocator pico_module_allocator = convert_to_pallocator(a);
    Module* module = mk_module(header, base, NULL, pico_module_allocator);
    delete_module_header(header);
    Symbol sym;

    PiType type;
    PiType* typep;
    PiType type_val;
    PiType* type_data = &type_val;
    ErrorPoint point;
    Allocator arena = mk_arena_allocator(16384, a);
    PiAllocator pia = convert_to_pallocator(&arena);
    if (catch_error(point)) {
        panic(point.error_message);
    }

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
    former = FDefine;
    sym = string_to_symbol(mv_string("def"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FDeclare;
    sym = string_to_symbol(mv_string("declare"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FImport;
    sym = string_to_symbol(mv_string("import"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FProcedure;
    sym = string_to_symbol(mv_string("proc"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FAll;
    sym = string_to_symbol(mv_string("all"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FSeal;
    sym = string_to_symbol(mv_string("seal"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FMacro;
    sym = string_to_symbol(mv_string("macro"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FApplication;
    sym = string_to_symbol(mv_string("apply"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FUnseal;
    sym = string_to_symbol(mv_string("unseal"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FProjector;
    sym = string_to_symbol(mv_string("."));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FDynamic;
    sym = string_to_symbol(mv_string("dynamic"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FDynamicUse;
    sym = string_to_symbol(mv_string("use"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FDynamicSet;
    sym = string_to_symbol(mv_string("set"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FDynamicLet;
    sym = string_to_symbol(mv_string("bind"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FInstance;
    sym = string_to_symbol(mv_string("instance"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FStructure;
    sym = string_to_symbol(mv_string("struct"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FVariant;
    sym = string_to_symbol(mv_string(":"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FMatch;
    sym = string_to_symbol(mv_string("match"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FIf;
    sym = string_to_symbol(mv_string("if"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FLabels;
    sym = string_to_symbol(mv_string("labels"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FGoTo;
    sym = string_to_symbol(mv_string("go-to"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FWithReset;
    sym = string_to_symbol(mv_string("with-reset"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FResetTo;
    sym = string_to_symbol(mv_string("reset-to"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FSequence;
    sym = string_to_symbol(mv_string("seq"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FLet;
    sym = string_to_symbol(mv_string("let"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FIs;
    sym = string_to_symbol(mv_string("is"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FInTo;
    sym = string_to_symbol(mv_string("into"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FOutOf;
    sym = string_to_symbol(mv_string("out-of"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FName;
    sym = string_to_symbol(mv_string("name"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FWiden;
    sym = string_to_symbol(mv_string("widen"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FNarrow;
    sym = string_to_symbol(mv_string("narrow"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FUnName;
    sym = string_to_symbol(mv_string("unname"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FSizeOf;
    sym = string_to_symbol(mv_string("size-of"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FAlignOf;
    sym = string_to_symbol(mv_string("align-of"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FOffsetOf;
    sym = string_to_symbol(mv_string("offset-of"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FProcType;
    sym = string_to_symbol(mv_string("Proc"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FStructType;
    sym = string_to_symbol(mv_string("Struct"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FEnumType;
    sym = string_to_symbol(mv_string("Enum"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FResetType;
    sym = string_to_symbol(mv_string("Reset"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FDynamicType;
    sym = string_to_symbol(mv_string("Dynamic"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FNamedType;
    sym = string_to_symbol(mv_string("Named"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FDistinctType;
    sym = string_to_symbol(mv_string("Distinct"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FOpaqueType;
    sym = string_to_symbol(mv_string("Opaque"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FTraitType;
    sym = string_to_symbol(mv_string("Trait"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FAllType;
    sym = string_to_symbol(mv_string("All"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FSealedType;
    sym = string_to_symbol(mv_string("Sealed"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FFamily;
    sym = string_to_symbol(mv_string("Family"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FLiftCType;
    sym = string_to_symbol(mv_string("LiftCType"));
    add_def(module, sym, type, &former, null_segments, NULL);

    // ------------------------------------------------------------------------
    // Types 
    // ------------------------------------------------------------------------

    type = (PiType) {
        .sort = TKind,
        .kind.nargs = 0,
    };

    type_val = type;
    sym = string_to_symbol(mv_string("Type"));
    add_def(module, sym, type, &type_data, null_segments, NULL);

    type_val = (PiType) {.sort = TPrim, .prim = Unit};
    sym = string_to_symbol(mv_string("Unit"));
    add_def(module, sym, type, &type_data, null_segments, NULL);

    type_val = (PiType) {.sort = TPrim, .prim = Bool};
    sym = string_to_symbol(mv_string("Bool"));
    add_def(module, sym, type, &type_data, null_segments, NULL);

    type_val = (PiType) {.sort = TPrim, .prim = Address};
    sym = string_to_symbol(mv_string("Address"));
    add_def(module, sym, type, &type_data, null_segments, NULL);

    type_val = (PiType) {.sort = TPrim, .prim = Int_64};
    sym = string_to_symbol(mv_string("I64"));
    add_def(module, sym, type, &type_data, null_segments, NULL);

    type_val = (PiType) {.sort = TPrim, .prim = Int_32};
    sym = string_to_symbol(mv_string("I32"));
    add_def(module, sym, type, &type_data, null_segments, NULL);

    type_val = (PiType) {.sort = TPrim, .prim = Int_16};
    sym = string_to_symbol(mv_string("I16"));
    add_def(module, sym, type, &type_data, null_segments, NULL);

    type_val = (PiType) {.sort = TPrim, .prim = Int_8};
    sym = string_to_symbol(mv_string("I8"));
    add_def(module, sym, type, &type_data, null_segments, NULL);

    type_val = (PiType) {.sort = TPrim, .prim = UInt_64};
    sym = string_to_symbol(mv_string("U64"));
    add_def(module, sym, type, &type_data, null_segments, NULL);

    type_val = (PiType) {.sort = TPrim, .prim = UInt_32};
    sym = string_to_symbol(mv_string("U32"));
    add_def(module, sym, type, &type_data, null_segments, NULL);

    type_val = (PiType) {.sort = TPrim, .prim = UInt_16};
    sym = string_to_symbol(mv_string("U16"));
    add_def(module, sym, type, &type_data, null_segments, NULL);

    type_val = (PiType) {.sort = TPrim, .prim = UInt_8};
    sym = string_to_symbol(mv_string("U8"));
    add_def(module, sym, type, &type_data, null_segments, NULL);

    type_val = (PiType) {.sort = TPrim, .prim = Float_32};
    sym = string_to_symbol(mv_string("F32"));
    add_def(module, sym, type, &type_data, null_segments, NULL);

    type_val = (PiType) {.sort = TPrim, .prim = Float_64};
    sym = string_to_symbol(mv_string("F64"));
    add_def(module, sym, type, &type_data, null_segments, NULL);

    // All standard library types: components and definition 
    // These are aggregated here, even if they are present in other modules, 
    // as some core types 
    {
        PiType *type_val;
        SymbolPiList vars;
        ModuleEntry* e;

        // Ptr Type 
        vars = mk_sym_list(1, &pia);
        push_sym(string_to_symbol(mv_string("A")), &vars);
        type.kind.nargs = 1;
        type_val = mk_named_type(&pia, "Ptr", mk_type_family(&pia,
                                                          vars,
                                                          mk_prim_type(&pia, Address)));
        type_data = type_val;
        sym = string_to_symbol(mv_string("Ptr"));
        add_def(module, sym, type, &type_data, null_segments, NULL);
        delete_pi_type_p(type_val, &pia);

        e = get_def(sym, module);
        ptr_type = e->value;

        // Allocator Type
        vars = mk_sym_list(1, &pia);
        push_sym(string_to_symbol(mv_string("A")), &vars);
        type.kind.nargs = 1;

        PiType *alloc_fn_type = mk_proc_type(&pia, 2,
                         mk_app_type(&pia, ptr_type, mk_var_type(&pia, "A")),
                         mk_prim_type(&pia, UInt_64),
                         mk_prim_type(&pia, Address));
        PiType *realloc_fn_type = mk_proc_type(&pia, 3,
                         mk_app_type(&pia, ptr_type, mk_var_type(&pia, "A")),
                         mk_prim_type(&pia, Address),
                         mk_prim_type(&pia, UInt_64),
                         mk_prim_type(&pia, Address));
        PiType *free_fn_type = mk_proc_type(&pia, 2,
                         mk_app_type(&pia, ptr_type, mk_var_type(&pia, "A")),
                         mk_prim_type(&pia, Address),
                         mk_prim_type(&pia, Address));
        type_val = mk_named_type(&pia, "AllocTable",
                                 mk_type_family(&pia, vars,
                                                mk_struct_type(&pia, 3,
                                                               "alloc", alloc_fn_type,
                                                               "realloc", realloc_fn_type,
                                                               "free", free_fn_type)));
        type_data = type_val;
        sym = string_to_symbol(mv_string("AllocTable"));
        add_def(module, sym, type, &type_data, null_segments, NULL);
        delete_pi_type_p(type_val, &pia);
        e = get_def(sym, module);
        PiType* vtable_type = e->value;

        // Allocator Type
        type_val = mk_named_type(&pia, "Allocator", mk_sealed_type(&pia,
                                                                1, "A", 0,
                                                                
                                                      mk_struct_type(&pia, 2,
                                                                     "vtable", mk_app_type(&pia, ptr_type, mk_app_type(&pia, vtable_type, mk_var_type(&pia, "A"))),
                                                                     "context", mk_app_type(&pia, ptr_type, mk_var_type(&pia, "A")))));
        type.kind.nargs = 0;
        type_data = type_val;
        sym = string_to_symbol(mv_string("Allocator"));
        add_def(module, sym, type, &type_data, null_segments, NULL);
        delete_pi_type_p(type_val, &pia);

        e = get_def(sym, module);
        allocator_type = e->value;

        // List Type 
        // Make a ptr
        type.kind.nargs = 1;
        vars = mk_sym_list(1, &pia);
        push_sym(string_to_symbol(mv_string("A")), &vars);
        type_val = 
            mk_named_type(&pia, "List",
                          mk_type_family(&pia,
                                         vars,
                                         mk_struct_type(&pia, 4,
                                                        "data", mk_prim_type(&pia, Address),
                                                        "len", mk_prim_type(&pia, UInt_64),
                                                        "capacity", mk_prim_type(&pia, UInt_64),
                                                        "gpa", copy_pi_type_p(allocator_type, &pia))));
        type_data = type_val;
        sym = string_to_symbol(mv_string("List"));
        add_def(module, sym, type, &type_data, null_segments, NULL);
        delete_pi_type_p(type_val, &pia);

        e = get_def(sym, module);
        list_type = e->value;
        
        // Maybe Type 
        vars = mk_sym_list(1, &pia);
        push_sym(string_to_symbol(mv_string("A")), &vars);
        type.kind.nargs = 1;
        type_val = mk_named_type(&pia, "Maybe", mk_type_family(&pia,
                                                      vars,
                                                      mk_enum_type(&pia, 2,
                                                                   "some", 1, mk_var_type(&pia, "A"),
                                                                   "none", 0)));
        type_data = type_val;
        sym = string_to_symbol(mv_string("Maybe"));
        add_def(module, sym, type, &type_data, null_segments, NULL);
        delete_pi_type_p(type_val, &pia);

        e = get_def(sym, module);
        maybe_type = e->value;

        // Either Type 
        vars = mk_sym_list(2, &pia);
        push_sym(string_to_symbol(mv_string("A")), &vars);
        push_sym(string_to_symbol(mv_string("B")), &vars);
        type.kind.nargs = 2;

        type_val = mk_named_type(&pia, "Either", mk_type_family(&pia,
                                                             vars,
                                                             mk_enum_type(&pia, 2,
                                                                          "left", 1, mk_var_type(&pia, "A"),
                                                                          "right", 1, mk_var_type(&pia, "B"))));
        type_data = type_val;
        sym = string_to_symbol(mv_string("Either"));
        add_def(module, sym, type, &type_data, null_segments, NULL);
        delete_pi_type_p(type_val, &pia);

        e = get_def(sym, module);
        either_type = e->value;

        // Pair Type 
        vars = mk_sym_list(2, &pia);
        push_sym(string_to_symbol(mv_string("A")), &vars);
        push_sym(string_to_symbol(mv_string("B")), &vars);
        type.kind.nargs = 2;

        type_val = mk_named_type(&pia, "Pair", mk_type_family(&pia,
                                                      vars,
                                                      mk_struct_type(&pia, 2,
                                                                   "_1", mk_var_type(&pia, "A"),
                                                                   "_2", mk_var_type(&pia, "B"))));
        type_data = type_val;
        sym = string_to_symbol(mv_string("Pair"));
        add_def(module, sym, type, &type_data, null_segments, NULL);
        delete_pi_type_p(type_val, &pia);

        e = get_def(sym, module);
        pair_type = e->value;
    }

    // Unit value

    Segments fn_segments = (Segments) {.data = mk_u8_array(0, a),};
    Segments prepped;

    type = build_store_fn_ty(&pia);
    build_store_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("store"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, type, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type(type, &pia);

    type = build_load_fn_ty(&pia);
    build_load_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("load"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, type, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type(type, &pia);

    typep = mk_proc_type(&pia, 1, mk_prim_type(&pia, Address), mk_prim_type(&pia, UInt_64));
    build_nop_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("address-to-num"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, &pia);

    typep = mk_proc_type(&pia, 1, mk_prim_type(&pia, UInt_64), mk_prim_type(&pia, Address));
    build_nop_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("num-to-address"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, &pia);

    add_module(string_to_symbol(mv_string("core")), module, base);

    sdelete_u8_array(null_segments.code);
    sdelete_u8_array(null_segments.data);
    // Note: we do NOT delete the 'fn_segments.code' because it is the
    // assembler, and needs to be used later!
    sdelete_u8_array(fn_segments.data);
    release_arena_allocator(arena);
}

