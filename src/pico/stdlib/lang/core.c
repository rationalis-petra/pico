#include <string.h>

#include "platform/signals.h"
#include "platform/machine_info.h"

#include "components/pretty/string_printer.h"

#include "pico/codegen/backend-direct/internal.h"
#include "pico/stdlib/lang/core.h"

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

static PiType* result_type;
PiType* get_result_type() {
    return result_type;
}

static PiType* pair_type;
PiType* get_pair_type() {
    return pair_type;
}

static PiType* allocator_type;
PiType* get_allocator_type() {
    return allocator_type;
}

static PiType* allocator_vtable_type;
PiType* get_allocator_vtable_type() {
    return allocator_vtable_type;
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

    // TODO: delegate this work to the backend
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
#elif ABI == SYSTEM_V_AARCH64
    panic(mv_string("Not implemented: build_store_fn for aarch64"));
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

#elif ABI == SYSTEM_V_AARCH64
    panic(mv_string("Not implemented: build_load_fn for aarch64"));
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

void add_core_module(Assembler* ass, Module* lang, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, &ra),
    };
    ReExports re_exports = (ReExports) {
        .clauses = mk_import_clause_array(0, &ra),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_name(mv_string("core")),
        .imports = imports,
        .re_exports = re_exports,
        .exports = exports,
    };
    Package* base = get_package(lang);
    Module* module = mk_module(header, base, lang);
    Name name;

    PiType type;
    PiType* typep;
    PiType type_val;
    PiType* type_data = &type_val;
    ErrorPoint point;
    PiAllocator pia = convert_to_pallocator(&ra);
    if (catch_error(point)) {
        panic(doc_to_str(point.error_message, 120, &ra));
    }

    TermFormer former;
    //TermFormer former;
    type.sort = TPrim;
    type.prim = TFormer;

    Segments null_segments = (Segments) {
        .code = mk_u8_array(0, &ra),
        .data = mk_u8_array(0, &ra),
    };

    // ------------------------------------------------------------------------
    // Term Formers
    // ------------------------------------------------------------------------
    former = FDefine;
    name = string_to_name(mv_string("def"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FDeclare;
    name = string_to_name(mv_string("declare"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FImport;
    name = string_to_name(mv_string("import"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FProcedure;
    name = string_to_name(mv_string("proc"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FAll;
    name = string_to_name(mv_string("all"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FSeal;
    name = string_to_name(mv_string("seal"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FMacro;
    name = string_to_name(mv_string("macro"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FApplication;
    name = string_to_name(mv_string("apply"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FUnseal;
    name = string_to_name(mv_string("unseal"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FDynamic;
    name = string_to_name(mv_string("dynamic"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FDynamicUse;
    name = string_to_name(mv_string("use"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FDynamicSet;
    name = string_to_name(mv_string("modify"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FDynamicLet;
    name = string_to_name(mv_string("bind"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FInstance;
    name = string_to_name(mv_string("instance"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FArray;
    name = string_to_name(mv_string("array"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FArrayElt;
    name = string_to_name(mv_string("aelt"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FStructure;
    name = string_to_name(mv_string("struct"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FProjector;
    name = string_to_name(mv_string("."));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FVariant;
    name = string_to_name(mv_string(":"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FMatch;
    name = string_to_name(mv_string("match"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FIf;
    name = string_to_name(mv_string("if"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FCond;
    name = string_to_name(mv_string("cond"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FLabels;
    name = string_to_name(mv_string("labels"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FGoTo;
    name = string_to_name(mv_string("go-to"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FWithReset;
    name = string_to_name(mv_string("with-reset"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FResetTo;
    name = string_to_name(mv_string("reset-to"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FSequence;
    name = string_to_name(mv_string("seq"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FLet;
    name = string_to_name(mv_string("let"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FIs;
    name = string_to_name(mv_string("is"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FInTo;
    name = string_to_name(mv_string("into"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FOutOf;
    name = string_to_name(mv_string("out-of"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FName;
    name = string_to_name(mv_string("name"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FWiden;
    name = string_to_name(mv_string("widen"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FNarrow;
    name = string_to_name(mv_string("narrow"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FUnName;
    name = string_to_name(mv_string("unname"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FSizeOf;
    name = string_to_name(mv_string("size-of"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FAlignOf;
    name = string_to_name(mv_string("align-of"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FOffsetOf;
    name = string_to_name(mv_string("offset-of"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FDynAlloc;
    name = string_to_name(mv_string("dyn-alloc"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FProcType;
    name = string_to_name(mv_string("Proc"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FArrayType;
    name = string_to_name(mv_string("Array"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FStructType;
    name = string_to_name(mv_string("Struct"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FEnumType;
    name = string_to_name(mv_string("Enum"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FResetType;
    name = string_to_name(mv_string("Reset"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FDynamicType;
    name = string_to_name(mv_string("Dynamic"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FNamedType;
    name = string_to_name(mv_string("Named"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FDistinctType;
    name = string_to_name(mv_string("Distinct"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FOpaqueType;
    name = string_to_name(mv_string("Opaque"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FTraitType;
    name = string_to_name(mv_string("Trait"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FAllType;
    name = string_to_name(mv_string("All"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FSealedType;
    name = string_to_name(mv_string("Sealed"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FFamily;
    name = string_to_name(mv_string("Family"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FLiftCType;
    name = string_to_name(mv_string("LiftCType"));
    add_def(module, name, type, &former, null_segments, NULL);

    // ------------------------------------------------------------------------
    // Types 
    // ------------------------------------------------------------------------

    type = (PiType) {
        .sort = TKind,
        .kind.nargs = 0,
    };

    type_val = type;
    name = string_to_name(mv_string("Type"));
    add_def(module, name, type, &type_data, null_segments, NULL);

    type_val = (PiType) {.sort = TPrim, .prim = Unit};
    name = string_to_name(mv_string("Unit"));
    add_def(module, name, type, &type_data, null_segments, NULL);

    type_val = (PiType) {.sort = TPrim, .prim = Bool};
    name = string_to_name(mv_string("Bool"));
    add_def(module, name, type, &type_data, null_segments, NULL);

    type_val = (PiType) {.sort = TPrim, .prim = Address};
    name = string_to_name(mv_string("Address"));
    add_def(module, name, type, &type_data, null_segments, NULL);

    type_val = (PiType) {.sort = TPrim, .prim = Int_64};
    name = string_to_name(mv_string("I64"));
    add_def(module, name, type, &type_data, null_segments, NULL);

    type_val = (PiType) {.sort = TPrim, .prim = Int_32};
    name = string_to_name(mv_string("I32"));
    add_def(module, name, type, &type_data, null_segments, NULL);

    type_val = (PiType) {.sort = TPrim, .prim = Int_16};
    name = string_to_name(mv_string("I16"));
    add_def(module, name, type, &type_data, null_segments, NULL);

    type_val = (PiType) {.sort = TPrim, .prim = Int_8};
    name = string_to_name(mv_string("I8"));
    add_def(module, name, type, &type_data, null_segments, NULL);

    type_val = (PiType) {.sort = TPrim, .prim = UInt_64};
    name = string_to_name(mv_string("U64"));
    add_def(module, name, type, &type_data, null_segments, NULL);

    type_val = (PiType) {.sort = TPrim, .prim = UInt_32};
    name = string_to_name(mv_string("U32"));
    add_def(module, name, type, &type_data, null_segments, NULL);

    type_val = (PiType) {.sort = TPrim, .prim = UInt_16};
    name = string_to_name(mv_string("U16"));
    add_def(module, name, type, &type_data, null_segments, NULL);

    type_val = (PiType) {.sort = TPrim, .prim = UInt_8};
    name = string_to_name(mv_string("U8"));
    add_def(module, name, type, &type_data, null_segments, NULL);

    type_val = (PiType) {.sort = TPrim, .prim = Float_32};
    name = string_to_name(mv_string("F32"));
    add_def(module, name, type, &type_data, null_segments, NULL);

    type_val = (PiType) {.sort = TPrim, .prim = Float_64};
    name = string_to_name(mv_string("F64"));
    add_def(module, name, type, &type_data, null_segments, NULL);

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
        name = string_to_name(mv_string("Ptr"));
        add_def(module, name, type, &type_data, null_segments, NULL);

        e = get_def_internal(name, module);
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
        type_val = mk_named_type(&pia, "AllocVTable",
                                 mk_type_family(&pia, vars,
                                                mk_struct_type(&pia, 3,
                                                               "alloc", alloc_fn_type,
                                                               "realloc", realloc_fn_type,
                                                               "free", free_fn_type)));
        type_data = type_val;
        name = string_to_name(mv_string("AllocVTable"));
        add_def(module, name, type, &type_data, null_segments, NULL);
        e = get_def_internal(name, module);
        allocator_vtable_type = e->value;

        // Allocator Type
        type_val = mk_named_type(&pia, "Allocator", mk_sealed_type(&pia,
                                                                1, "A", 0,
                                                                
                                                      mk_struct_type(&pia, 2,
                                                                     "vtable", mk_app_type(&pia, ptr_type, mk_app_type(&pia, allocator_vtable_type, mk_var_type(&pia, "A"))),
                                                                     "context", mk_app_type(&pia, ptr_type, mk_var_type(&pia, "A")))));
        type.kind.nargs = 0;
        type_data = type_val;
        name = string_to_name(mv_string("Allocator"));
        add_def(module, name, type, &type_data, null_segments, NULL);

        e = get_def_internal(name, module);
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
        name = string_to_name(mv_string("List"));
        add_def(module, name, type, &type_data, null_segments, NULL);

        e = get_def_internal(name, module);
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
        name = string_to_name(mv_string("Maybe"));
        add_def(module, name, type, &type_data, null_segments, NULL);

        e = get_def_internal(name, module);
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
        name = string_to_name(mv_string("Either"));
        add_def(module, name, type, &type_data, null_segments, NULL);

        e = get_def_internal(name, module);
        either_type = e->value;

        // Result Type 
        vars = mk_sym_list(2, &pia);
        push_sym(string_to_symbol(mv_string("Value")), &vars);
        push_sym(string_to_symbol(mv_string("Error")), &vars);
        type.kind.nargs = 2;

        type_val = mk_named_type(&pia, "Result", mk_type_family(&pia,
                                                             vars,
                                                             mk_enum_type(&pia, 2,
                                                                          "ok", 1, mk_var_type(&pia, "Value"),
                                                                          "error", 1, mk_var_type(&pia, "Error"))));
        type_data = type_val;
        name = string_to_name(mv_string("Result"));
        add_def(module, name, type, &type_data, null_segments, NULL);

        e = get_def_internal(name, module);
        result_type = e->value;

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
        name = string_to_name(mv_string("Pair"));
        add_def(module, name, type, &type_data, null_segments, NULL);

        e = get_def_internal(name, module);
        pair_type = e->value;
    }

    // Unit value

    Segments fn_segments = (Segments) {.data = mk_u8_array(0, &ra),};
    Segments prepped;

    type = build_store_fn_ty(&pia);
    build_store_fn(ass, &ra, &point);
    name = string_to_name(mv_string("store"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, type, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    type = build_load_fn_ty(&pia);
    build_load_fn(ass, &ra, &point);
    name = string_to_name(mv_string("load"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, type, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(&pia, 1, mk_prim_type(&pia, Address), mk_prim_type(&pia, UInt_64));
    build_nop_fn(ass, &ra, &point);
    name = string_to_name(mv_string("address-to-num"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(&pia, 1, mk_prim_type(&pia, UInt_64), mk_prim_type(&pia, Address));
    build_nop_fn(ass, &ra, &point);
    name = string_to_name(mv_string("num-to-address"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
}

