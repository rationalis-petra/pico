#include "platform/signals.h"
#include "pico/values/stdlib.h"

PiType mk_binop_type(Allocator* a, PrimType a1, PrimType a2, PrimType r) {
    PiType* i1 = mem_alloc(sizeof(PiType), a);
    PiType* i2 = mem_alloc(sizeof(PiType), a);
    PiType* i3 = mem_alloc(sizeof(PiType), a);

    i1->sort = TPrim;
    i1->prim = a1;
    i2->sort = TPrim;
    i2->prim = a2;
    i3->sort = TPrim;
    i3->prim = r;

    PiType type;
    type.sort = TProc;
    PtrArray args = mk_ptr_array(2, a);
    push_ptr(i1, &args);
    push_ptr(i2, &args);

    type.proc.args = args;
    type.proc.ret = i3;

    return type;
}

PiType mk_null_proc_type(Allocator* a) {
    PiType* ret = mem_alloc(sizeof(PiType), a);
    *ret = (PiType) {.sort = TPrim, .prim = Unit};
    return (PiType) {.sort = TProc, .proc.args = mk_ptr_array(0, a), .proc.ret = ret};
}

void build_binary_fun(Assembler* ass, BinaryOp op, Allocator* a, ErrorPoint* point) {
    build_unary_op (ass, Pop, reg(RCX), a, point);
    build_unary_op (ass, Pop, reg(RBX), a, point);
    build_unary_op (ass, Pop, reg(RAX), a, point);
    build_binary_op (ass, op, reg(RAX), reg(RBX), a, point);
    build_unary_op (ass, Push, reg(RAX), a, point);
    build_unary_op (ass, Push, reg(RCX), a, point);
    build_nullary_op (ass, Ret, a, point);
}

void build_comp_fun(Assembler* ass, UnaryOp op, Allocator* a, ErrorPoint* point) {
    build_unary_op (ass, Pop, reg(RCX), a, point);
    build_unary_op (ass, Pop, reg(RBX), a, point);
    build_unary_op (ass, Pop, reg(RAX), a, point);
    build_binary_op (ass, Cmp, reg(RAX), reg(RBX), a, point);
    build_unary_op (ass, op, reg(RAX), a, point);
    build_unary_op (ass, Push, reg(RAX), a, point);
    build_unary_op (ass, Push, reg(RCX), a, point);
    build_nullary_op (ass, Ret, a, point);
}


// TODO: thread local??
static jmp_buf* m_buf;
void set_exit_callback(jmp_buf* buf) {
    m_buf = buf;
}

void exit_callback() {
    longjmp(m_buf, 1);
}

void build_exit_callback(Assembler* ass, Allocator* a, ErrorPoint* point) {
    build_binary_op(ass, Mov, reg(RAX), imm64((uint64_t)exit_callback), a, point);
    build_unary_op(ass, Call, reg(RAX), a, point);
}

Module* base_module(Assembler* ass, Allocator* a) {
    Module* module = mk_module(a);
    Symbol sym;

    PiType type;
    PiType type_val;
    PiType* type_data = &type_val;
    type = (PiType) {
        .sort = TKind,
        .kind.nargs = 0,
    };
    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    // TODO: we use int64_t as it has the requisite size (8 bytes)
    // for pico values: currently don't support non-64 bit values 
    int64_t former;
    //TermFormer former;
    type.sort = TPrim;
    type.prim = TFormer;

    // ------------------------------------------------------------------------
    // Term Formers
    // ------------------------------------------------------------------------
    former = FDefine;
    sym = string_to_symbol(mv_string("def"));
    add_def(module, sym, type, &former);

    former = FDefine;
    sym = string_to_symbol(mv_string("declare"));
    add_def(module, sym, type, &former);

    former = FProcedure;
    sym = string_to_symbol(mv_string("proc"));
    add_def(module, sym, type, &former);

    former = FAll;
    sym = string_to_symbol(mv_string("all"));
    add_def(module, sym, type, &former);

    former = FApplication;
    sym = string_to_symbol(mv_string("$"));
    add_def(module, sym, type, &former);

    former = FProjector;
    sym = string_to_symbol(mv_string("."));
    add_def(module, sym, type, &former);

    former = FStructure;
    sym = string_to_symbol(mv_string("struct"));
    add_def(module, sym, type, &former);

    former = FVariant;
    sym = string_to_symbol(mv_string(":"));
    add_def(module, sym, type, &former);

    former = FMatch;
    sym = string_to_symbol(mv_string("match"));
    add_def(module, sym, type, &former);

    former = FIf;
    sym = string_to_symbol(mv_string("if"));
    add_def(module, sym, type, &former);

    former = FLabels;
    sym = string_to_symbol(mv_string("labels"));
    add_def(module, sym, type, &former);

    former = FSequence;
    sym = string_to_symbol(mv_string("seq"));
    add_def(module, sym, type, &former);

    former = FLet;
    sym = string_to_symbol(mv_string("let"));
    add_def(module, sym, type, &former);

    former = FIs;
    sym = string_to_symbol(mv_string("is"));
    add_def(module, sym, type, &former);

    // ------------------------------------------------------------------------
    // Types 
    // ------------------------------------------------------------------------
    former = FProcType;
    sym = string_to_symbol(mv_string("Proc"));
    add_def(module, sym, type, &former);

    former = FStructType;
    sym = string_to_symbol(mv_string("Struct"));
    add_def(module, sym, type, &former);

    former = FEnumType;
    sym = string_to_symbol(mv_string("Enum"));
    add_def(module, sym, type, &former);

    former = FAllType;
    sym = string_to_symbol(mv_string("All"));
    add_def(module, sym, type, &former);


    // Types
    type_val = mk_prim_type(Unit);
    sym = string_to_symbol(mv_string("Unit"));
    add_def(module, sym, type, &type_data);

    type_val = mk_prim_type(Bool);
    sym = string_to_symbol(mv_string("Bool"));
    add_def(module, sym, type, &type_data);

    type_val = mk_prim_type(Address);
    sym = string_to_symbol(mv_string("Address"));
    add_def(module, sym, type, &type_data);

    type_val = mk_prim_type(Int_64);
    sym = string_to_symbol(mv_string("I64"));
    add_def(module, sym, type, &type_data);

    type_val = mk_prim_type(Int_32);
    sym = string_to_symbol(mv_string("I32"));
    add_def(module, sym, type, &type_data);

    type_val = mk_prim_type(Int_16);
    sym = string_to_symbol(mv_string("I16"));
    add_def(module, sym, type, &type_data);

    type_val = mk_prim_type(Int_8);
    sym = string_to_symbol(mv_string("I8"));
    add_def(module, sym, type, &type_data);

    // ------------------------------------------------------------------------
    // Operators & Functions
    // ------------------------------------------------------------------------

    build_binary_fun(ass, Add, a, &point);
    type = mk_binop_type(a, Int_64, Int_64, Int_64);
    sym = string_to_symbol(mv_string("+"));
    add_fn_def(module, sym, type, ass, NULL);
    clear_assembler(ass);

    build_binary_fun(ass, Sub, a, &point);
    sym = string_to_symbol(mv_string("-"));
    add_fn_def(module, sym, type, ass, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    build_comp_fun(ass, SetL, a, &point);
    type = mk_binop_type(a, Int_64, Int_64, Bool);
    sym = string_to_symbol(mv_string("<"));
    add_fn_def(module, sym, type, ass, NULL);
    clear_assembler(ass);

    build_comp_fun(ass, SetG, a, &point);
    sym = string_to_symbol(mv_string(">"));
    add_fn_def(module, sym, type, ass, NULL);
    clear_assembler(ass);

    build_comp_fun(ass, SetE, a, &point);
    sym = string_to_symbol(mv_string("="));
    add_fn_def(module, sym, type, ass, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    type = mk_null_proc_type(a);
    build_exit_callback(ass, a, &point);
    sym = string_to_symbol(mv_string("exit"));
    add_fn_def(module, sym, type, ass, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    return module;
}
