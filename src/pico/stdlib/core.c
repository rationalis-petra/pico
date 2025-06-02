#include <string.h>

#include "platform/signals.h"
#include "platform/machine_info.h"

#include "pico/stdlib/core.h"

static PiType* ptr_type;
PiType* get_ptr_type() {
    return ptr_type;
}

static PiType* array_type;
PiType* get_array_type() {
    return array_type;
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

PiType build_store_fn_ty(Allocator* a) {
    PiType* proc_ty  = mk_proc_type(a, 2, mk_prim_type(a, Address), mk_var_type(a, "A"), mk_prim_type(a, Unit));

    SymbolArray types = mk_symbol_array(1, a);
    push_symbol(string_to_symbol(mv_string("A")), &types);

    return (PiType) {.sort = TAll, .binder.vars = types, .binder.body = proc_ty};
}

void build_store_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // The usual calling convention for polymorphic functions is assumed
    // RBP+32| size
    // RBP+24| offset (address)
    // RBP+16| offset (value)
    // RBP+8 | return address
    // RBP   | OLD RBP
    // RBP-8 | address
    // -- size of value -- 
    // RSP+8 | value
    // RSP   | return address 
    // See polymorphic.c for details

    // Note: as there is only two args, we can guarantee that RSP = pointer to SRC
    // also note that size = RBP + 0x10
    // Store the return address in RBP + 8
    build_unary_op(ass, Pop, reg(R9, sz_64), a, point);

    build_binary_op(ass, Mov, rref8(RBP, 8, sz_64), reg(R9, sz_64), a, point);

    // Store Dest address (located @ RBP - 8)
    build_binary_op(ass, Mov, reg(RDI, sz_64), rref8(RBP, -8, sz_64), a, point);

    // SRC address = RSP 

    // Store size in R9
    build_binary_op(ass, Mov, reg(R9, sz_64), rref8(RBP, 4*ADDRESS_SIZE, sz_64), a, point); 
    build_binary_op(ass, SHR, reg(R9, sz_64), imm8(28), a, point);
    build_binary_op(ass, And, reg(R9, sz_64), imm32(0xFFFFFFF), a, point);

#if ABI == SYSTEM_V_64
    // memcpy (dest = rdi, src = rsi, size = rdx)
    // copy size into RDX
    build_binary_op(ass, Mov, reg(RSI, sz_64), reg(RSP, sz_64), a, point);
    build_binary_op(ass, Mov, reg(RDX, sz_64), reg(R9, sz_64), a, point);

#elif ABI == WIN_64
    // memcpy (dest = rcx, src = rdx, size = r8)
    build_binary_op(ass, Mov, reg(RCX, sz_64), reg(RDI, sz_64), a, point);
    build_binary_op(ass, Mov, reg(RDX, sz_64), reg(RSP, sz_64), a, point);
    build_binary_op(ass, Mov, reg(R8, sz_64), reg(R9, sz_64), a, point);

    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#else
#error "Unknown calling convention"
#endif

    // call memcpy
    build_binary_op(ass, Mov, reg(RAX, sz_64), imm64((uint64_t)&memcpy), a, point);
    build_unary_op(ass, Call, reg(RAX, sz_64), a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(32), a, point);
#endif

    // Store return address in R9
    build_binary_op(ass, Mov, reg(R9, sz_64), rref8(RBP, 8, sz_64), a, point);

    // set RSP = current RBP + 5*ADDRESS
    build_binary_op(ass, Mov, reg(RSP, sz_64), reg(RBP, sz_64), a, point);
    build_binary_op(ass, Add, reg(RSP, sz_64), imm8(5*ADDRESS_SIZE), a, point);

    // Restore the old RBP
    build_binary_op(ass, Mov, reg(RBP, sz_64), rref8(RBP, 0, sz_64), a, point);

    // push return address
    build_unary_op(ass, Push, reg(R9, sz_64), a, point);

    build_nullary_op(ass, Ret, a, point);
}

PiType build_load_fn_ty(Allocator* a) {
    PiType* proc_ty = mk_proc_type(a, 1, mk_prim_type(a, Address), mk_var_type(a, "A"));

    SymbolArray types = mk_symbol_array(1, a);
    push_symbol(string_to_symbol(mv_string("A")), &types);

    return (PiType) {.sort = TAll, .binder.vars = types, .binder.body = proc_ty};
}

void build_load_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // The usual calling convention for polymorphic functions is assumed, hence
    // stack has the form:
    // RBP+24  | type/size
    // RBP+16  | offset (address)
    // RBP+8   | padding (for return address)
    // RBP     | OLD RBP
    // RBP-8   | load address
    // RSP     | return address 
    // See polymorphic.c for details

    // Note: what we want to do is make the stack look like this
    // RBP = old RBP
    // -- OLD Stack FRAME
    //  ------------------------------
    //  -- Enough Padding for Value --
    //  ------------------------------
    // RSP -> Return address
    // Then call memcpy & return

    // Therefore, we need to do the following steps to wrangle the stack
    // 1. Store Size
    // 2. Stash return address
    // 3. Stash load src address 
    // 3. Set RSP = RBP + 4 ADDRESSES - Size
    // 4. Set RBP = [RBP]
    // 5. Push return address

    // Store size in R8, stack size in R9
    build_binary_op(ass, Mov, reg(R8, sz_64), rref8(RBP, 3*ADDRESS_SIZE, sz_64), a, point); 
    build_binary_op(ass, Mov, reg(R9, sz_64), reg(R8, sz_64), a, point); 

    build_binary_op(ass, And, reg(R9, sz_64), imm32(0xFFFFFFF), a, point);

    build_binary_op(ass, SHR, reg(R8, sz_64), imm8(28), a, point);
    build_binary_op(ass, And, reg(R8, sz_64), imm32(0xFFFFFFF), a, point);

    // Stash return address in RAX
    build_unary_op(ass, Pop, reg(RAX, sz_64), a, point); 

    // Stash load src address
    build_unary_op(ass, Pop, reg(RSI, sz_64), a, point);

    // Set RSP = RBP + 4 Addresses - Stack Size (note that at this point, RSP = RBP)
    build_binary_op(ass, Add, reg(RSP, sz_64), imm8(4*ADDRESS_SIZE), a, point);
    build_binary_op(ass, Sub, reg(RSP, sz_64), reg(R9, sz_64), a, point);

    // Set RBP = [RBP]
    build_binary_op(ass, Mov, reg(RBP, sz_64), rref8(RBP, 0, sz_64), a, point);

    // Make sure return address is available when we Ret
    build_unary_op(ass, Push, reg(RAX, sz_64), a, point); 

#if ABI == SYSTEM_V_64
    // memcpy (dest = rdi, src = rsi, size = rdx)
    build_binary_op(ass, Mov, reg(RDI, sz_64), reg(RSP, sz_64), a, point);
    build_binary_op(ass, Add, reg(RDI, sz_64), imm8(ADDRESS_SIZE), a, point);

    // build_binary_op(ass, Mov, reg(RSI), reg(RSP), a, point);
    build_binary_op(ass, Mov, reg(RDX, sz_64), reg(R8, sz_64), a, point);

#elif ABI == WIN_64
    // memcpy (dest = rcx, src = rdx, size = r8)
    build_binary_op(ass, Mov, reg(RCX, sz_64), reg(RSP, sz_64), a, point);
    build_binary_op(ass, Add, reg(RCX, sz_64), imm8(ADDRESS_SIZE), a, point);

    build_binary_op(ass, Mov, reg(RDX, sz_64), reg(RSI, sz_64), a, point);
    // build_binary_op(ass, Mov, reg(R8, sz_64), reg(R8, sz_64), a, point);
    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#else
#error "Unknown calling convention"
#endif

    // copy memcpy into RCX & call
    build_binary_op(ass, Mov, reg(RAX, sz_64), imm64((uint64_t)&memcpy), a, point);
    build_unary_op(ass, Call, reg(RAX, sz_64), a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(32), a, point);
#endif

    // Return
    build_nullary_op(ass, Ret, a, point);
}

void build_nop_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    build_nullary_op(ass, Ret, a, point);
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
    Module* module = mk_module(header, base, NULL, a);
    delete_module_header(header);
    Symbol sym;

    PiType type;
    PiType* typep;
    PiType type_val;
    PiType* type_data = &type_val;
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
    former = FDefine;
    sym = string_to_symbol(mv_string("def"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FDefine;
    sym = string_to_symbol(mv_string("declare"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FOpen;
    sym = string_to_symbol(mv_string("open"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FProcedure;
    sym = string_to_symbol(mv_string("proc"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FAll;
    sym = string_to_symbol(mv_string("all"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FMacro;
    sym = string_to_symbol(mv_string("macro"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FApplication;
    sym = string_to_symbol(mv_string("$"));
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

    former = FDynamicLet;
    sym = string_to_symbol(mv_string("bind"));
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

    former = FUnName;
    sym = string_to_symbol(mv_string("unname"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FDynAlloc;
    sym = string_to_symbol(mv_string("dyn-alloc"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FSizeOf;
    sym = string_to_symbol(mv_string("size-of"));
    add_def(module, sym, type, &former, null_segments, NULL);

    former = FAlignOf;
    sym = string_to_symbol(mv_string("align-of"));
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
        SymbolArray vars;
        ModuleEntry* e;

        // Ptr Type 
        vars = mk_symbol_array(1, a);
        push_symbol(string_to_symbol(mv_string("A")), &vars);
        type.kind.nargs = 1;
        type_val = mk_named_type(a, "Ptr", mk_type_family(a,
                                                          vars,
                                                          mk_prim_type(a, Address)));
        type_data = type_val;
        sym = string_to_symbol(mv_string("Ptr"));
        add_def(module, sym, type, &type_data, null_segments, NULL);
        delete_pi_type_p(type_val, a);

        e = get_def(sym, module);
        ptr_type = e->value;

        // Allocator Type 
        PiType* alloc_type = mk_struct_type(a, 3,
                                            "alloc", mk_proc_type(a, 1, mk_prim_type(a, UInt_64), mk_prim_type(a, Address)),
                                            "realloc", mk_proc_type(a, 2, mk_prim_type(a, Address), mk_prim_type(a, UInt_64), mk_prim_type(a, Address)),
                                            "free", mk_proc_type(a, 1, mk_prim_type(a, Address), mk_prim_type(a, Unit)));
        type_data = alloc_type;
        sym = string_to_symbol(mv_string("Allocator"));
        type.kind.nargs = 0;
        add_def(module, sym, type, &type_data, null_segments, NULL);

        // (Ptr Alloc)
        PiType* alloc_ptr_type = mk_app_type(a, ptr_type, alloc_type);
        delete_pi_type_p(alloc_type, a);
        
        // Array Type 
        // Make a ptr
        vars = mk_symbol_array(1, a);
        push_symbol(string_to_symbol(mv_string("A")), &vars);
        type.kind.nargs = 1;
        type_val = 
            mk_named_type(a, "Array",
                          mk_type_family(a,
                                         vars,
                                         mk_struct_type(a, 4,
                                                        "data", mk_prim_type(a, Address),
                                                        "len", mk_prim_type(a, UInt_64),
                                                        "capacity", mk_prim_type(a, UInt_64),
                                                        "gpa", alloc_ptr_type)));
        type_data = type_val;
        sym = string_to_symbol(mv_string("Array"));
        add_def(module, sym, type, &type_data, null_segments, NULL);
        delete_pi_type_p(type_val, a);

        e = get_def(sym, module);
        array_type = e->value;
        
        // Maybe Type 
        vars = mk_symbol_array(1, a);
        push_symbol(string_to_symbol(mv_string("A")), &vars);
        type.kind.nargs = 1;
        type_val = mk_named_type(a, "Maybe", mk_type_family(a,
                                                      vars,
                                                      mk_enum_type(a, 2,
                                                                   "some", 1, mk_var_type(a, "A"),
                                                                   "none", 0)));
        type_data = type_val;
        sym = string_to_symbol(mv_string("Maybe"));
        add_def(module, sym, type, &type_data, null_segments, NULL);
        delete_pi_type_p(type_val, a);

        e = get_def(sym, module);
        maybe_type = e->value;

        // Either Type 
        vars = mk_symbol_array(2, a);
        push_symbol(string_to_symbol(mv_string("A")), &vars);
        push_symbol(string_to_symbol(mv_string("B")), &vars);
        type.kind.nargs = 2;

        type_val = mk_named_type(a, "Either", mk_type_family(a,
                                                             vars,
                                                             mk_enum_type(a, 2,
                                                                          "left", 1, mk_var_type(a, "A"),
                                                                          "right", 1, mk_var_type(a, "B"))));
        type_data = type_val;
        sym = string_to_symbol(mv_string("Either"));
        add_def(module, sym, type, &type_data, null_segments, NULL);
        delete_pi_type_p(type_val, a);

        e = get_def(sym, module);
        either_type = e->value;

        // Pair Type 
        vars = mk_symbol_array(2, a);
        push_symbol(string_to_symbol(mv_string("A")), &vars);
        push_symbol(string_to_symbol(mv_string("B")), &vars);
        type.kind.nargs = 2;

        type_val = mk_named_type(a, "Pair", mk_type_family(a,
                                                      vars,
                                                      mk_struct_type(a, 2,
                                                                   "_1", mk_var_type(a, "A"),
                                                                   "_2", mk_var_type(a, "B"))));
        type_data = type_val;
        sym = string_to_symbol(mv_string("Pair"));
        add_def(module, sym, type, &type_data, null_segments, NULL);
        delete_pi_type_p(type_val, a);

        e = get_def(sym, module);
        pair_type = e->value;

    }

    // Unit value

    /* type_data = type_val; */
    /* sym = string_to_symbol(mv_string("unit")); */
    /* add_def(module, sym, type, &type_data, null_segments, NULL); */
    /* e = get_def(sym, module); */
    /* syntax_type = e->value; */

    Segments fn_segments = (Segments) {.data = mk_u8_array(0, a),};
    Segments prepped;

    type = build_store_fn_ty(a);
    build_store_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("store"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, type, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    type = build_load_fn_ty(a);
    build_load_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("load"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, type, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    typep = mk_proc_type(a, 1, mk_prim_type(a, Address), mk_prim_type(a, UInt_64));
    build_nop_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("address-to-num"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 1, mk_prim_type(a, UInt_64), mk_prim_type(a, Address));
    build_nop_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("num-to-address"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    add_module(string_to_symbol(mv_string("core")), module, base);

    sdelete_u8_array(null_segments.code);
    sdelete_u8_array(null_segments.data);
    // Note: we do NOT delete the 'fn_segments.code' because it is the
    // assembler, and needs to be used later!
    sdelete_u8_array(fn_segments.data);
}

