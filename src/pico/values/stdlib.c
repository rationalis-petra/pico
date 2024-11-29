#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "platform/machine_info.h"
#include "platform/memory/std_allocator.h"
#include "platform/signals.h"
#include "platform/jump.h"

#include "pico/values/stdlib.h"
#include "app/module_load.h"

//------------------------------------------------------------------------------
// Implementatino of C API 
//------------------------------------------------------------------------------

static jump_buf* m_buf;
void set_exit_callback(jump_buf* buf) { m_buf = buf; }

static Module* current_module;
void set_current_module(Module* current) { current_module = current; }

static Package* current_package;
void set_current_package(Package* current) { current_package = current; }

static IStream* current_istream;
void set_std_istream(IStream* current) { current_istream = current; }

static OStream* current_ostream;
void set_std_ostream(OStream* current) { current_ostream = current; }


//------------------------------------------------------------------------------
// Helper functions for pico base package
//------------------------------------------------------------------------------

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

PiType mk_unary_op_type(Allocator* a, PiType arg, PrimType ret) {
    PiType* i1 = mem_alloc(sizeof(PiType), a);
    PiType* i2 = mem_alloc(sizeof(PiType), a);

    *i1 = arg;
    i2->sort = TPrim;
    i2->prim = ret;

    PiType type;
    type.sort = TProc;
    PtrArray args = mk_ptr_array(1, a);
    push_ptr(i1, &args);

    type.proc.args = args;
    type.proc.ret = i2;

    return type;
}

void build_binary_fun(Assembler* ass, BinaryOp op, Allocator* a, ErrorPoint* point) {
    build_unary_op (ass, Pop, reg(RCX), a, point);
    build_unary_op (ass, Pop, reg(R9), a, point);
    build_unary_op (ass, Pop, reg(RAX), a, point);
    build_binary_op (ass, op, reg(RAX), reg(R9), a, point);
    build_unary_op (ass, Push, reg(RAX), a, point);
    build_unary_op (ass, Push, reg(RCX), a, point);
    build_nullary_op (ass, Ret, a, point);
}

void build_special_binary_fun(Assembler* ass, UnaryOp op, Allocator* a, ErrorPoint* point) {
    build_unary_op (ass, Pop, reg(RCX), a, point);
    build_unary_op (ass, Pop, reg(R9), a, point);
    build_unary_op (ass, Pop, reg(RAX), a, point);
    build_binary_op (ass, Mov, reg(RDX), imm32(0), a, point);
    build_unary_op (ass, op, reg(R9), a, point);
    build_unary_op (ass, Push, reg(RAX), a, point);
    build_unary_op (ass, Push, reg(RCX), a, point);
    build_nullary_op (ass, Ret, a, point);
}

void build_comp_fun(Assembler* ass, UnaryOp op, Allocator* a, ErrorPoint* point) {
    build_unary_op (ass, Pop, reg(RCX), a, point);
    build_unary_op (ass, Pop, reg(R9), a, point);
    build_unary_op (ass, Pop, reg(RAX), a, point);
    build_binary_op (ass, Cmp, reg(RAX), reg(R9), a, point);
    build_unary_op (ass, op, reg(RAX), a, point);
    build_unary_op (ass, Push, reg(RAX), a, point);
    build_unary_op (ass, Push, reg(RCX), a, point);
    build_nullary_op (ass, Ret, a, point);
}


//------------------------------------------------------------------------------
// Helper functions: module extra
//------------------------------------------------------------------------------

void build_print_fun(Assembler* ass, Allocator* a, ErrorPoint* point) {

#if ABI == SYSTEM_V_64
    // puts (bytes = rdi)
    build_binary_op (ass, Mov, reg(RDI), rref8(RSP, 16), a, point);

#elif ABI == WIN_64
    // puts (bytes = rcx)
    build_binary_op (ass, Mov, reg(RCX), rref8(RSP, 16), a, point);

    build_binary_op(ass, Sub, reg(RSP), imm32(32), a, point);
#else
#error "Unknown calling convention"
#endif

    build_binary_op(ass, Mov, reg(RAX), imm64((uint64_t)&puts), a, point);
    build_unary_op(ass, Call, reg(RAX), a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP), imm32(32), a, point);
#endif

    // Store RSI, pop args & return
    build_unary_op(ass, Pop, reg(RSI), a, point);
    build_binary_op(ass, Add, reg(RSP), imm32(16), a, point);
    build_unary_op(ass, Push, reg(RSI), a, point);
    build_nullary_op (ass, Ret, a, point);
}


// C implementation (called from pico!)
void load_module_c_fun(String filename) {
    // (ann load-module String → Unit) : load the module/code located in file! 
    
    Allocator* a = get_std_allocator();
    IStream* sfile = open_file_istream(filename, a);
    load_module_from_istream(sfile, current_ostream, current_package, NULL, a);
    delete_istream(sfile, a);
}

void build_load_module_fun(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // (ann load-module String → Unit) : load the module/code located in file! 

#if ABI == SYSTEM_V_64
    // load_module_c_fun (struct on stack)
    // pass in platform/memory/on stack(?)
    build_unary_op (ass, Push, imm32(0), a, point);
    build_unary_op (ass, Push, rref8(RSP, 24), a, point);
    // note: use 24 twice as RSP grows with push! 
    build_unary_op (ass, Push, rref8(RSP, 24), a, point);

#elif ABI == WIN_64
    // load_module_c_fun: push struct
    // pass in platform/memory/on stack(?)
    build_unary_op (ass, Push, imm32(0), a, point);
    build_unary_op (ass, Push, rref8(RSP, 24), a, point);
    // note: use 24 twice as RSP grows with push! 
    build_unary_op (ass, Push, rref8(RSP, 24), a, point);

    // store ptr to struct in rcx
    build_binary_op(ass, Mov, reg(RCX), reg(RSP), a, point);
    build_binary_op(ass, Sub, reg(RSP), imm32(32), a, point);
#else
#error "Unknown calling convention"
#endif

    build_binary_op(ass, Mov, reg(RAX), imm64((uint64_t)&load_module_c_fun), a, point);
    build_unary_op(ass, Call, reg(RAX), a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP), imm32(32), a, point);
#endif

    // To return:
    // + pop argument we pushed onto stack
    // + stash ret addr
    // + pop argument we were called with
    // + push ret addr & return
    build_binary_op(ass, Add, reg(RSP), imm32(24), a, point);
    build_unary_op (ass, Pop, reg(RAX), a, point);
    build_binary_op(ass, Add, reg(RSP), imm32(16), a, point);
    build_unary_op (ass, Push, reg(RAX), a, point);
    build_nullary_op (ass, Ret, a, point);
}

void run_script_c_fun(String filename) {
    // (ann load-module String → Unit) : load the module/code located in file! 
    
    Allocator* a = get_std_allocator();
    IStream* sfile = open_file_istream(filename, a);
    run_script_from_istream(sfile, current_ostream, current_module, a);
    delete_istream(sfile, a);
}

void build_run_script_fun(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // (ann load-module String → Unit) : load the module/code located in file! 

#if ABI == SYSTEM_V_64
    // load_module_c_fun ({.memsize = rcx, .bytes = rdi, .allocator = rcx = NULL})
    // pass in platform/memory/on stack(?)
    build_unary_op (ass, Push, imm32(0), a, point);
    build_unary_op (ass, Push, rref8(RSP, 24), a, point);
    // note: use 24 twice as RSP grows with push! 
    build_unary_op (ass, Push, rref8(RSP, 24), a, point);

#elif ABI == WIN_64
    // load_module_c_fun: push struct
    // pass in platform/memory/on stack(?)
    build_unary_op (ass, Push, imm32(0), a, point);
    build_unary_op (ass, Push, rref8(RSP, 24), a, point);
    // note: use 24 twice as RSP grows with push! 
    build_unary_op (ass, Push, rref8(RSP, 24), a, point);

    // store ptr to struct in rcx
    build_binary_op(ass, Mov, reg(RCX), reg(RSP), a, point);
    build_binary_op(ass, Sub, reg(RSP), imm32(32), a, point);
#else
#error "Unknown calling convention"
#endif

    build_binary_op(ass, Mov, reg(RAX), imm64((uint64_t)&run_script_c_fun), a, point);
    build_unary_op(ass, Call, reg(RAX), a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP), imm32(32), a, point);
#endif

    // To return:
    // + pop argument we pushed onto stack
    // + stash ret addr
    // + pop argument we were called with
    // + push ret addr & return
    build_binary_op(ass, Add, reg(RSP), imm32(24), a, point);
    build_unary_op (ass, Pop, reg(RAX), a, point);
    build_binary_op(ass, Add, reg(RSP), imm32(16), a, point);
    build_unary_op (ass, Push, reg(RAX), a, point);
    build_nullary_op (ass, Ret, a, point);
}

void exit_callback() {
    long_jump(*m_buf, 1);
}

void build_exit_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    build_binary_op(ass, Mov, reg(RAX), imm64((uint64_t)exit_callback), a, point);
    build_unary_op(ass, Call, reg(RAX), a, point);
}



//------------------------------------------------------------------------------
// Helper functions: module core
//------------------------------------------------------------------------------
uint64_t stdlib_size_of(PiType* t) {
    return pi_size_of(*t);
}

void build_size_of_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // size-of: PiType* -> uint64_t
#if OS_FAMILY == UNIX
    build_binary_op(ass, Mov, reg(RDI), rref8(RSP, 8), a, point);
#elif OS_FAMILY == WINDOWS
    build_binary_op(ass, Mov, reg(RCX), rref8(RSP, 8), a, point);
    build_binary_op(ass, Sub, reg(RSP), imm32(32), a, point);
#else 
#error "build_size_of_fn does not support this OS!"
#endif

    // call pi_size_of
    build_binary_op(ass, Mov, reg(RAX), imm64((uint64_t)&stdlib_size_of), a, point);
    build_unary_op(ass, Call, reg(RAX), a, point);

#if OS_FAMILY == WINDOWS
    build_binary_op(ass, Add, reg(RSP), imm32(32), a, point);
#endif 

    build_unary_op(ass, Pop, reg(RBX), a, point);
    build_binary_op(ass, Add, reg(RSP), imm32(8), a, point);
    build_unary_op(ass, Push, reg(RAX), a, point);
    build_unary_op(ass, Push, reg(RBX), a, point);
    build_nullary_op(ass, Ret, a, point);
}

PiType build_store_fn_ty(Allocator* a) {
    Symbol ty_sym = string_to_symbol(mv_string("A"));

    PiType* arg1_ty = mem_alloc(sizeof(PiType), a);
    PiType* arg2_ty = mem_alloc(sizeof(PiType), a);
    PiType* ret_ty = mem_alloc(sizeof(PiType), a);

    *arg1_ty = (PiType) {.sort = TPrim, .prim = Address, };
    *arg2_ty = (PiType) {.sort = TVar, .var = ty_sym, };
    *ret_ty = (PiType) {.sort = TPrim, .prim = Unit, };

    PtrArray args = mk_ptr_array(2, a);
    push_ptr(arg1_ty, &args);
    push_ptr(arg2_ty, &args);

    PiType* proc_ty = mem_alloc(sizeof(PiType), a);
    *proc_ty = (PiType) {.sort = TProc, .proc.args = args, .proc.ret = ret_ty};

    SymbolArray types = mk_u64_array(1, a);
    push_u64(ty_sym, &types);

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
    build_unary_op(ass, Pop, reg(R9), a, point);
    build_binary_op(ass, Mov, rref8(RBP, 8), reg(R9), a, point);

    // Store Dest address (located @ RBP - 8)
    build_binary_op(ass, Mov, reg(RDI), rref8(RBP, -8), a, point);

    // SRC address = RSP 

    // Store size in R9
    build_binary_op(ass, Mov, reg(R9), rref8(RBP, 4*ADDRESS_SIZE), a, point); 

#if ABI == SYSTEM_V_64
    // memcpy (dest = rdi, src = rsi, size = rdx)
    // copy size into RDX
    build_binary_op(ass, Mov, reg(RSI), reg(RSP), a, point);
    build_binary_op(ass, Mov, reg(RDX), reg(R9), a, point);

#elif ABI == WIN_64
    // memcpy (dest = rcx, src = rdx, size = r8)
    build_binary_op(ass, Mov, reg(RCX), reg(RDI), a, point);
    build_binary_op(ass, Mov, reg(RDX), reg(RSP), a, point);
    build_binary_op(ass, Mov, reg(R8), reg(R9), a, point);

    build_binary_op(ass, Sub, reg(RSP), imm32(32), a, point);
#else
#error "Unknown calling convention"
#endif

    // call memcpy
    build_binary_op(ass, Mov, reg(RAX), imm64((uint64_t)&memcpy), a, point);
    build_unary_op(ass, Call, reg(RAX), a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP), imm32(32), a, point);
#endif

    // Store return address in R9
    build_binary_op(ass, Mov, reg(R9), rref8(RBP, 8), a, point);

    // set RSP = current RBP + 5*ADDRESS
    build_binary_op(ass, Mov, reg(RSP), reg(RBP), a, point);
    build_binary_op(ass, Add, reg(RSP), imm8(5*ADDRESS_SIZE), a, point);

    // Restore the old RBP
    build_binary_op(ass, Mov, reg(RBP), rref8(RBP, 0), a, point);

    // push return address
    build_unary_op(ass, Push, reg(R9), a, point);

    build_nullary_op(ass, Ret, a, point);
}

PiType build_load_fn_ty(Allocator* a) {
    Symbol ty_sym = string_to_symbol(mv_string("A"));

    PiType* arg1_ty = mem_alloc(sizeof(PiType), a);
    PiType* ret_ty = mem_alloc(sizeof(PiType), a);

    *arg1_ty = (PiType) {.sort = TPrim, .prim = Address, };
    *ret_ty = (PiType) {.sort = TVar, .prim = ty_sym, };

    PtrArray args = mk_ptr_array(1, a);
    push_ptr(arg1_ty, &args);

    PiType* proc_ty = mem_alloc(sizeof(PiType), a);
    *proc_ty = (PiType) {.sort = TProc, .proc.args = args, .proc.ret = ret_ty};

    SymbolArray types = mk_u64_array(1, a);
    push_u64(ty_sym, &types);

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

    // Store size in R9
    build_binary_op(ass, Mov, reg(R9), rref8(RBP, 3*ADDRESS_SIZE), a, point); 

    // Stash return address in RAX
    build_unary_op(ass, Pop, reg(RAX), a, point); 

    // Stash load src address
    build_unary_op(ass, Pop, reg(RSI), a, point);

    // Set RSP = RBP + 4 Addresses - Size (note that at this point, RSP = RBP
    build_binary_op(ass, Add, reg(RSP), imm8(4*ADDRESS_SIZE), a, point);
    build_binary_op(ass, Sub, reg(RSP), reg(R9), a, point);

    // Set RBP = [RBP]
    build_binary_op(ass, Mov, reg(RBP), rref8(RBP, 0), a, point);

    // Make sure return address is available when we Ret
    build_unary_op(ass, Push, reg(RAX), a, point); 

#if ABI == SYSTEM_V_64
    // memcpy (dest = rdi, src = rsi, size = rdx)
    build_binary_op(ass, Mov, reg(RDI), reg(RSP), a, point);
    build_binary_op(ass, Add, reg(RDI), imm8(ADDRESS_SIZE), a, point);

    //build_binary_op(ass, Mov, reg(RSI), reg(RSP), a, point);
    build_binary_op(ass, Mov, reg(RDX), reg(R9), a, point);

#elif ABI == WIN_64
    // memcpy (dest = rcx, src = rdx, size = r8)
    build_binary_op(ass, Mov, reg(RCX), reg(RSP), a, point);
    build_binary_op(ass, Add, reg(RCX), imm8(ADDRESS_SIZE), a, point);

    build_binary_op(ass, Mov, reg(RDX), reg(RSI), a, point);
    build_binary_op(ass, Mov, reg(R8), reg(R9), a, point);
    build_binary_op(ass, Sub, reg(RSP), imm32(32), a, point);
#else
#error "Unknown calling convention"
#endif

    // copy memcpy into RCX & call
    build_binary_op(ass, Mov, reg(RAX), imm64((uint64_t)&memcpy), a, point);
    build_unary_op(ass, Call, reg(RAX), a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP), imm32(32), a, point);
#endif

    // Return
    build_nullary_op(ass, Ret, a, point);
}

void build_realloc_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // realloc : Proc (Address U64) Unit
    build_unary_op(ass, Pop, reg(RAX), a, point);

#if ABI == SYSTEM_V_64
    // realloc (ptr = rdi, size = rsi)
    // copy size into RDX
    build_unary_op(ass, Pop, reg(RSI), a, point);
    build_unary_op(ass, Pop, reg(RDI), a, point);
    build_unary_op(ass, Push, reg(RAX), a, point);

#elif ABI == WIN_64
    // realloc (ptr = RCX, size = RDX)
    build_unary_op(ass, Pop, reg(RDX), a, point);
    build_unary_op(ass, Pop, reg(RCX), a, point);
    build_unary_op(ass, Push, reg(RAX), a, point);
    build_binary_op(ass, Sub, reg(RSP), imm32(32), a, point);
#endif


    build_binary_op(ass, Mov, reg(RAX), imm64((uint64_t)&realloc),  a, point);
    build_unary_op(ass, Call, reg(RAX), a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP), imm32(32), a, point);
#endif

    build_unary_op(ass, Pop, reg(R9), a, point);
    build_unary_op(ass, Push, reg(RAX), a, point);
    build_unary_op(ass, Push, reg(R9), a, point);

    build_nullary_op(ass, Ret, a, point);
    
}

void build_malloc_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // malloc : Proc (U64) Unit
    build_unary_op(ass, Pop, reg(RAX), a, point);

#if ABI == SYSTEM_V_64
    // memcpy (dest = rdi, src = rsi, size = rdx)
    // copy size into RDX
    build_unary_op(ass, Pop, reg(RDI), a, point);
    build_unary_op(ass, Push, reg(RAX), a, point);

#elif ABI == WIN_64
    build_unary_op(ass, Pop, reg(RCX), a, point);
    build_unary_op(ass, Push, reg(RAX), a, point);
    build_binary_op(ass, Sub, reg(RSP), imm32(32), a, point);
#endif

    // Get the malloc dynamic variable
    /* build_binary_op(ass, Mov, reg(RAX), imm64((uint64_t)&malloc_dyn_var),  a, point); */
    /* build_unary_op(ass, Call, reg(RAX), a, point); */

    build_binary_op(ass, Mov, reg(RAX), imm64((uint64_t)&malloc),  a, point);
    build_unary_op(ass, Call, reg(RAX), a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP), imm32(32), a, point);
#endif

    build_unary_op(ass, Pop, reg(R9), a, point);
    build_unary_op(ass, Push, reg(RAX), a, point);
    build_unary_op(ass, Push, reg(R9), a, point);

    build_nullary_op(ass, Ret, a, point);
}

void build_free_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // free : Proc (Address) Unit
    build_unary_op(ass, Pop, reg(RAX), a, point);

#if ABI == SYSTEM_V_64
    // free (dest = rdi)
    // copy address into RDI
    build_unary_op(ass, Pop, reg(RDI), a, point);
    build_unary_op(ass, Push, reg(RAX), a, point);

#elif ABI == WIN_64
    // free (addr = rcx)
    // copy address into RCX
    build_unary_op(ass, Pop, reg(RCX), a, point);
    build_unary_op(ass, Push, reg(RAX), a, point);
    build_binary_op(ass, Sub, reg(RSP), imm32(32), a, point);
#endif

    build_binary_op(ass, Mov, reg(RAX), imm64((uint64_t)&free),  a, point);
    build_unary_op(ass, Call, reg(RAX), a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP), imm32(32), a, point);
#endif

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
    PiType type_val;
    PiType* type_data = &type_val;
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

    former = FDynamic;
    sym = string_to_symbol(mv_string("dynamic"));
    add_def(module, sym, type, &former);

    former = FDynamicUse;
    sym = string_to_symbol(mv_string("use"));
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

    former = FGoTo;
    sym = string_to_symbol(mv_string("go-to"));
    add_def(module, sym, type, &former);

    former = FWithReset;
    sym = string_to_symbol(mv_string("with-reset"));
    add_def(module, sym, type, &former);

    former = FResetTo;
    sym = string_to_symbol(mv_string("reset-to"));
    add_def(module, sym, type, &former);

    former = FSequence;
    sym = string_to_symbol(mv_string("seq"));
    add_def(module, sym, type, &former);

    former = FLet;
    sym = string_to_symbol(mv_string("let"));
    add_def(module, sym, type, &former);

    former = FDynamicLet;
    sym = string_to_symbol(mv_string("bind"));
    add_def(module, sym, type, &former);

    former = FIs;
    sym = string_to_symbol(mv_string("is"));
    add_def(module, sym, type, &former);

    former = FDynAlloc;
    sym = string_to_symbol(mv_string("dyn-alloc"));
    add_def(module, sym, type, &former);

    former = FProcType;
    sym = string_to_symbol(mv_string("Proc"));
    add_def(module, sym, type, &former);

    former = FStructType;
    sym = string_to_symbol(mv_string("Struct"));
    add_def(module, sym, type, &former);

    former = FEnumType;
    sym = string_to_symbol(mv_string("Enum"));
    add_def(module, sym, type, &former);

    former = FResetType;
    sym = string_to_symbol(mv_string("Reset"));
    add_def(module, sym, type, &former);

    former = FDynamicType;
    sym = string_to_symbol(mv_string("Dynamic"));
    add_def(module, sym, type, &former);

    former = FAllType;
    sym = string_to_symbol(mv_string("All"));
    add_def(module, sym, type, &former);

    // ------------------------------------------------------------------------
    // Types 
    // ------------------------------------------------------------------------

    type = (PiType) {
        .sort = TKind,
        .kind.nargs = 0,
    };

    type_val = type;
    sym = string_to_symbol(mv_string("Type"));
    add_def(module, sym, type, &type_data);

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

    type_val = mk_prim_type(UInt_64);
    sym = string_to_symbol(mv_string("U64"));
    add_def(module, sym, type, &type_data);

    type_val = mk_prim_type(UInt_32);
    sym = string_to_symbol(mv_string("U32"));
    add_def(module, sym, type, &type_data);

    type_val = mk_prim_type(UInt_16);
    sym = string_to_symbol(mv_string("U16"));
    add_def(module, sym, type, &type_data);

    type_val = mk_prim_type(UInt_8);
    sym = string_to_symbol(mv_string("U8"));
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

    build_special_binary_fun(ass, IMul, a, &point);
    sym = string_to_symbol(mv_string("*"));
    add_fn_def(module, sym, type, ass, NULL);
    clear_assembler(ass);

    build_special_binary_fun(ass, IDiv, a, &point);
    sym = string_to_symbol(mv_string("/"));
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

    type = mk_unary_op_type(a, (PiType){.sort = TKind, .kind = {.nargs = 0}}, UInt_64);
    build_size_of_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("size-of"));
    add_fn_def(module, sym, type, ass, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    type = build_store_fn_ty(a);
    build_store_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("store"));
    add_fn_def(module, sym, type, ass, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    type = build_load_fn_ty(a);
    build_load_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("load"));
    add_fn_def(module, sym, type, ass, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    add_module(string_to_symbol(mv_string("core")), module, base);
}

void add_extra_module(Assembler* ass, Package* base, Allocator* default_allocator, Allocator* a) {
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
    
    //uint64_t dyn_curr_package = mk_dynamic_var(sizeof(void*), &base); 
    //uint64_t dyn_curr_module = mk_dynamic_var(sizeof(void*), NULL); 
    uint64_t dynamic_allocator = mk_dynamic_var(sizeof(Allocator), default_allocator); 

    //type = mk_dynamic_type(a, );
    /* build_exit_fn(ass, a, &point); */
    /* sym = string_to_symbol(mv_string("exit")); */
    /* add_fn_def(module, sym, type, ass, NULL); */
    /* clear_assembler(ass); */
    /* delete_pi_type(type, a); */

    // C Wrappers!
    // exit : Proc [] Unit
    type = mk_proc_type(a, 0, mk_prim_type(Unit));
    build_exit_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("exit"));
    add_fn_def(module, sym, type, ass, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    // malloc : Proc [U64] Address
    type = mk_proc_type(a, 1, mk_prim_type(UInt_64), mk_prim_type(Address));
    build_malloc_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("malloc"));
    add_fn_def(module, sym, type, ass, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    // realloc : Proc (Address U64) Address
    type = mk_proc_type(a, 2, mk_prim_type(Address),
                        mk_prim_type(UInt_64),
                        mk_prim_type(Address));
    build_realloc_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("realloc"));
    add_fn_def(module, sym, type, ass, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    // realloc : Proc [String] Unit
    type = mk_proc_type(a, 1, mk_prim_type(Address), mk_prim_type(Unit));
    build_free_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("free"));
    add_fn_def(module, sym, type, ass, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    // print : Proc [String] Unit
    type = mk_proc_type(a, 1, mk_string_type(a), mk_prim_type(Unit));
    build_print_fun(ass, a, &point);
    sym = string_to_symbol(mv_string("print"));
    add_fn_def(module, sym, type, ass, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    // print : Proc [String] Unit
    type = mk_proc_type(a, 1, mk_string_type(a), mk_prim_type(Unit));
    build_load_module_fun(ass, a, &point);
    sym = string_to_symbol(mv_string("load-module"));
    add_fn_def(module, sym, type, ass, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    // run-script : Proc [String] Unit
    type = mk_proc_type(a, 1, mk_string_type(a), mk_prim_type(Unit));
    build_run_script_fun(ass, a, &point);
    sym = string_to_symbol(mv_string("run-script"));
    add_fn_def(module, sym, type, ass, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    add_module(string_to_symbol(mv_string("extra")), module, base);
}

void add_user_module(Package* base, Allocator* a) {
    Imports imports = (Imports) {.clauses = mk_import_clause_array(2, a),};
    push_import_clause((ImportClause) {
            .type = ImportPathAll,
            .name = string_to_symbol(mv_string("core")),
        },
        &imports.clauses);
    push_import_clause((ImportClause) {
            .type = ImportPathAll,
            .name = string_to_symbol(mv_string("extra")),
        },
        &imports.clauses);

    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };

    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("user")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, base, NULL, a);
    delete_module_header(header);

    add_module(string_to_symbol(mv_string("user")), module, base);
}

Package* base_package(Assembler* ass, Allocator* a, Allocator* default_allocator) {
    Package* base = mk_package(string_to_symbol(mv_string("base")), a);
    add_core_module(ass, base, a);
    add_extra_module(ass, base, default_allocator, a);
    add_user_module(base, a);

    return base;
}
