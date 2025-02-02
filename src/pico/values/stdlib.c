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
// Implementation of C API 
//------------------------------------------------------------------------------

static jump_buf* m_buf;
void set_exit_callback(jump_buf* buf) { m_buf = buf; }

static uint64_t std_current_module;
Module* get_std_current_module() {
    void** data = get_dynamic_memory();
    Module** dyn = data[std_current_module]; 
    return *dyn;
}

Module* set_std_current_module(Module* md) {
    void** data = get_dynamic_memory();
    Module** mdle = data[std_current_module]; 
    Module* old = *mdle;
    *mdle = md;
    return old;
}

static PiType* syntax_type;
PiType* get_syntax_type() {
    return syntax_type;
}

static Package* current_package;
void set_current_package(Package* current) { current_package = current; }

static IStream* current_istream;
void set_std_istream(IStream* current) { current_istream = current; }

static OStream* current_ostream;
void set_std_ostream(OStream* current) { current_ostream = current; }

static uint64_t std_allocator; 

static uint64_t std_tmp_allocator; 
Allocator* get_std_tmp_allocator() {
    void** data = get_dynamic_memory();
    Allocator** dyn = data[std_tmp_allocator]; 
    return *dyn;
}

Allocator* set_std_tmp_allocator(Allocator* al) {
    void** data = get_dynamic_memory();
    Allocator** dyn = data[std_tmp_allocator]; 
    Allocator* old = *dyn;
    *dyn = al;
    return old;
}

//------------------------------------------------------------------------------
// Helper functions for pico base package
//------------------------------------------------------------------------------

PiType mk_binop_type(Allocator* a, PrimType a1, PrimType a2, PrimType r) {
    return mk_proc_type(a, 2, mk_prim_type(a1), mk_prim_type(a2), mk_prim_type(r));
}

PiType mk_unary_op_type(Allocator* a, PiType arg, PrimType ret) {
    return mk_proc_type(a, 1, arg, mk_prim_type(ret));
}

void build_binary_fn(Assembler* ass, BinaryOp op, LocationSize sz, Allocator* a, ErrorPoint* point) {
    build_unary_op (ass, Pop, reg(RCX, sz_64), a, point);
    build_unary_op (ass, Pop, reg(RDX, sz_64), a, point);
    build_unary_op (ass, Pop, reg(RAX, sz_64), a, point);
    build_binary_op (ass, op, reg(RAX, sz), reg(RDX, sz), a, point);
    build_unary_op (ass, Push, reg(RAX, sz_64), a, point);
    build_unary_op (ass, Push, reg(RCX, sz_64), a, point);
    build_nullary_op (ass, Ret, a, point);
}

void build_special_binary_fn(Assembler* ass, UnaryOp op, LocationSize sz, Allocator* a, ErrorPoint* point) {
    build_unary_op (ass, Pop, reg(RCX, sz_64), a, point);
    build_unary_op (ass, Pop, reg(RDI, sz_64), a, point);
    build_unary_op (ass, Pop, reg(RAX, sz_64), a, point);

    switch (sz) {
    case sz_64:
    case sz_32:
        build_binary_op (ass, Mov, reg(RDX, sz), imm32(0), a, point);
        break;
    case sz_16:
        build_binary_op (ass, Mov, reg(RDX, sz), imm16(0), a, point);
        break;
    case sz_8:
        build_binary_op (ass, Mov, reg(RDX, sz), imm8(0), a, point);
        break;
    }
    build_unary_op (ass, op, reg(RDI, sz), a, point);

    build_unary_op (ass, Push, reg(RAX, sz_64), a, point);
    build_unary_op (ass, Push, reg(RCX, sz_64), a, point);
    build_nullary_op (ass, Ret, a, point);
}

void build_comp_fn(Assembler* ass, UnaryOp op, LocationSize sz, Allocator* a, ErrorPoint* point) {
    build_unary_op (ass, Pop, reg(RCX, sz_64), a, point);
    build_unary_op (ass, Pop, reg(RDX, sz_64), a, point);
    build_unary_op (ass, Pop, reg(RAX, sz_64), a, point);
    build_binary_op (ass, Cmp, reg(RAX, sz), reg(RDX, sz), a, point);
    build_unary_op (ass, op, reg(RAX, sz_64), a, point);
    build_unary_op (ass, Push, reg(RAX, sz_64), a, point);
    build_unary_op (ass, Push, reg(RCX, sz_64), a, point);
    build_nullary_op (ass, Ret, a, point);
}


//------------------------------------------------------------------------------
// Helper functions: module extra
//------------------------------------------------------------------------------

void build_print_fun(Assembler* ass, Allocator* a, ErrorPoint* point) {

#if ABI == SYSTEM_V_64
    // puts (bytes = rdi)
    build_binary_op (ass, Mov, reg(RDI, sz_64), rref8(RSP, 16, sz_64), a, point);

#elif ABI == WIN_64
    // puts (bytes = rcx)
    build_binary_op (ass, Mov, reg(RCX, sz_64), rref8(RSP, 16, sz_64), a, point);

    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#else
#error "Unknown calling convention"
#endif

    build_binary_op(ass, Mov, reg(RAX, sz_64), imm64((uint64_t)&puts), a, point);
    build_unary_op(ass, Call, reg(RAX, sz_64), a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(32), a, point);
#endif

    // Store RSI, pop args & return
    build_unary_op(ass, Pop, reg(RSI, sz_64), a, point);
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(16), a, point);
    build_unary_op(ass, Push, reg(RSI, sz_64), a, point);
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
    build_unary_op (ass, Push, rref8(RSP, 24, sz_64), a, point);
    // note: use 24 twice as RSP grows with push! 
    build_unary_op (ass, Push, rref8(RSP, 24, sz_64), a, point);

#elif ABI == WIN_64
    // load_module_c_fun: push struct
    // pass in platform/memory/on stack(?)
    build_unary_op (ass, Push, imm32(0), a, point);
    build_unary_op (ass, Push, rref8(RSP, 24, sz_64), a, point);
    // note: use 24 twice as RSP grows with push! 
    build_unary_op (ass, Push, rref8(RSP, 24, sz_64), a, point);

    // store ptr to struct in rcx
    build_binary_op(ass, Mov, reg(RCX, sz_64), reg(RSP, sz_64), a, point);
    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#else
#error "Unknown calling convention"
#endif

    build_binary_op(ass, Mov, reg(RAX, sz_64), imm64((uint64_t)&load_module_c_fun), a, point);
    build_unary_op(ass, Call, reg(RAX, sz_64), a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(32), a, point);
#endif

    // To return:
    // + pop argument we pushed onto stack
    // + stash ret addr
    // + pop argument we were called with
    // + push ret addr & return
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(24), a, point);
    build_unary_op (ass, Pop, reg(RAX, sz_64), a, point);
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(16), a, point);
    build_unary_op (ass, Push, reg(RAX, sz_64), a, point);
    build_nullary_op (ass, Ret, a, point);
}

void run_script_c_fun(String filename) {
    // (ann load-module String → Unit) : load the module/code located in file! 
    
    Allocator* a = get_std_allocator();
    IStream* sfile = open_file_istream(filename, a);
    Module* current_module = get_std_current_module();
    run_script_from_istream(sfile, current_ostream, current_module, a);
    delete_istream(sfile, a);
}

void build_run_script_fun(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // (ann load-module String → Unit) : load the module/code located in file! 

#if ABI == SYSTEM_V_64
    // load_module_c_fun ({.memsize = rcx, .bytes = rdi, .allocator = rcx = NULL})
    // pass in platform/memory/on stack(?)
    build_unary_op (ass, Push, imm32(0), a, point);
    build_unary_op (ass, Push, rref8(RSP, 24, sz_64), a, point);
    // note: use 24 twice as RSP grows with push! 
    build_unary_op (ass, Push, rref8(RSP, 24, sz_64), a, point);

#elif ABI == WIN_64
    // load_module_c_fun: push struct
    // pass in platform/memory/on stack(?)
    build_unary_op (ass, Push, imm32(0), a, point);
    build_unary_op (ass, Push, rref8(RSP, 24, sz_64), a, point);
    // note: use 24 twice as RSP grows with push! 
    build_unary_op (ass, Push, rref8(RSP, 24, sz_64), a, point);

    // store ptr to struct in rcx
    build_binary_op(ass, Mov, reg(RCX, sz_64), reg(RSP, sz_64), a, point);
    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#else
#error "Unknown calling convention"
#endif

    build_binary_op(ass, Mov, reg(RAX, sz_64), imm64((uint64_t)&run_script_c_fun), a, point);
    build_unary_op(ass, Call, reg(RAX, sz_64), a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(32), a, point);
#endif

    // To return:
    // + pop argument we pushed onto stack
    // + stash ret addr
    // + pop argument we were called with
    // + push ret addr & return
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(24), a, point);
    build_unary_op (ass, Pop, reg(RAX, sz_64), a, point);
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(16), a, point);
    build_unary_op (ass, Push, reg(RAX, sz_64), a, point);
    build_nullary_op (ass, Ret, a, point);
}

void exit_callback() {
    long_jump(*m_buf, 1);
}

void build_exit_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    build_binary_op(ass, Mov, reg(RAX, sz_64), imm64((uint64_t)exit_callback), a, point);
    build_unary_op(ass, Call, reg(RAX, sz_64), a, point);
}



//------------------------------------------------------------------------------
// Helper functions: module core
//------------------------------------------------------------------------------
uint64_t stdlib_size_of(PiType* t) {
    return pi_size_of(*t);
}

void build_size_of_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // size-of: PiType* -> uint64_t
#if ABI == SYSTEM_V_64
    build_binary_op(ass, Mov, reg(RDI, sz_64), rref8(RSP, 8, sz_64), a, point);
#elif ABI == WIN_64
    build_binary_op(ass, Mov, reg(RCX, sz_64), rref8(RSP, 8, sz_64), a, point);
    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#else 
#error "build_size_of_fn does not support this ABI!"
#endif

    // call pi_size_of
    build_binary_op(ass, Mov, reg(RAX, sz_64), imm64((uint64_t)&stdlib_size_of), a, point);
    build_unary_op(ass, Call, reg(RAX, sz_64), a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(32), a, point);
#endif 

    build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(8), a, point);
    build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
    build_unary_op(ass, Push, reg(RCX, sz_64), a, point);
    build_nullary_op(ass, Ret, a, point);
}

uint64_t stdlib_align_of(PiType* t) {
    return pi_align_of(*t);
}

void build_align_of_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // size-of: PiType* -> uint64_t
#if ABI == SYSTEM_V_64
    build_binary_op(ass, Mov, reg(RDI, sz_64), rref8(RSP, 8, sz_64), a, point);
#elif ABI == WIN_64
    build_binary_op(ass, Mov, reg(RCX, sz_64), rref8(RSP, 8, sz_64), a, point);
    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#else 
#error "build_size_of_fn does not support this ABI!"
#endif

    // call pi_size_of
    build_binary_op(ass, Mov, reg(RAX, sz_64), imm64((uint64_t)&stdlib_align_of), a, point);
    build_unary_op(ass, Call, reg(RAX, sz_64), a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(32), a, point);
#endif 

    build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(8), a, point);
    build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
    build_unary_op(ass, Push, reg(RCX, sz_64), a, point);
    build_nullary_op(ass, Ret, a, point);
}

PiType build_store_fn_ty(Allocator* a) {
    Symbol ty_sym = string_to_symbol(mv_string("A"));

    PiType* proc_ty = mem_alloc(sizeof(PiType), a);

    PiType tvar = (PiType) {.sort = TVar, .var = ty_sym, };
    *proc_ty = mk_proc_type(a, 2, mk_prim_type(Address), tvar, mk_prim_type(Unit));

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
    Symbol ty_sym = string_to_symbol(mv_string("A"));

    PiType ret_ty = (PiType) {.sort = TVar, .prim = ty_sym, };
    PiType* proc_ty = mem_alloc(sizeof(PiType), a);
    *proc_ty = mk_proc_type(a, 1, mk_prim_type(Address), ret_ty);

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

void build_realloc_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // realloc : Proc (Address U64) Unit
    build_unary_op(ass, Pop, reg(RAX, sz_64), a, point);

#if ABI == SYSTEM_V_64
    // realloc (ptr = rdi, size = rsi)
    // copy size into RDX
    build_unary_op(ass, Pop, reg(RSI, sz_64), a, point);
    build_unary_op(ass, Pop, reg(RDI, sz_64), a, point);
    build_unary_op(ass, Push, reg(RAX, sz_64), a, point);

#elif ABI == WIN_64
    // realloc (ptr = RCX, size = RDX)
    build_unary_op(ass, Pop, reg(RDX, sz_64), a, point);
    build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);
    build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#endif


    build_binary_op(ass, Mov, reg(RAX, sz_64), imm64((uint64_t)&realloc),  a, point);
    build_unary_op(ass, Call, reg(RAX, sz_64), a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(32), a, point);
#endif

    build_unary_op(ass, Pop, reg(R9, sz_64), a, point);
    build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
    build_unary_op(ass, Push, reg(R9, sz_64), a, point);

    build_nullary_op(ass, Ret, a, point);
    
}

void build_malloc_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // malloc : Proc (U64) Unit
    build_unary_op(ass, Pop, reg(RAX, sz_64), a, point);

#if ABI == SYSTEM_V_64
    // memcpy (dest = rdi, src = rsi, size = rdx)
    // copy size into RDX
    build_unary_op(ass, Pop, reg(RDI, sz_64), a, point);
    build_unary_op(ass, Push, reg(RAX, sz_64), a, point);

#elif ABI == WIN_64
    build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);
    build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#endif

    // Get the malloc dynamic variable
    /* build_binary_op(ass, Mov, reg(RAX), imm64((uint64_t)&malloc_dyn_var),  a, point); */
    /* build_unary_op(ass, Call, reg(RAX), a, point); */

    build_binary_op(ass, Mov, reg(RAX, sz_64), imm64((uint64_t)&malloc),  a, point);
    build_unary_op(ass, Call, reg(RAX, sz_64), a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(32), a, point);
#endif

    build_unary_op(ass, Pop, reg(R9, sz_64), a, point);
    build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
    build_unary_op(ass, Push, reg(R9, sz_64), a, point);

    build_nullary_op(ass, Ret, a, point);
}

void build_free_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // free : Proc (Address) Unit
    build_unary_op(ass, Pop, reg(RAX, sz_64), a, point);

#if ABI == SYSTEM_V_64
    // free (dest = rdi)
    // copy address into RDI
    build_unary_op(ass, Pop, reg(RDI, sz_64), a, point);
    build_unary_op(ass, Push, reg(RAX, sz_64), a, point);

#elif ABI == WIN_64
    // free (addr = rcx)
    // copy address into RCX
    build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);
    build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#endif

    build_binary_op(ass, Mov, reg(RAX, sz_64), imm64((uint64_t)&free),  a, point);
    build_unary_op(ass, Call, reg(RAX, sz_64), a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(32), a, point);
#endif

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

    type_val = mk_prim_type(Unit);
    sym = string_to_symbol(mv_string("Unit"));
    add_def(module, sym, type, &type_data, null_segments, NULL);

    type_val = mk_prim_type(Bool);
    sym = string_to_symbol(mv_string("Bool"));
    add_def(module, sym, type, &type_data, null_segments, NULL);

    type_val = mk_prim_type(Address);
    sym = string_to_symbol(mv_string("Address"));
    add_def(module, sym, type, &type_data, null_segments, NULL);

    type_val = mk_prim_type(Int_64);
    sym = string_to_symbol(mv_string("I64"));
    add_def(module, sym, type, &type_data, null_segments, NULL);

    type_val = mk_prim_type(Int_32);
    sym = string_to_symbol(mv_string("I32"));
    add_def(module, sym, type, &type_data, null_segments, NULL);

    type_val = mk_prim_type(Int_16);
    sym = string_to_symbol(mv_string("I16"));
    add_def(module, sym, type, &type_data, null_segments, NULL);

    type_val = mk_prim_type(Int_8);
    sym = string_to_symbol(mv_string("I8"));
    add_def(module, sym, type, &type_data, null_segments, NULL);

    type_val = mk_prim_type(UInt_64);
    sym = string_to_symbol(mv_string("U64"));
    add_def(module, sym, type, &type_data, null_segments, NULL);

    type_val = mk_prim_type(UInt_32);
    sym = string_to_symbol(mv_string("U32"));
    add_def(module, sym, type, &type_data, null_segments, NULL);

    type_val = mk_prim_type(UInt_16);
    sym = string_to_symbol(mv_string("U16"));
    add_def(module, sym, type, &type_data, null_segments, NULL);

    type_val = mk_prim_type(UInt_8);
    sym = string_to_symbol(mv_string("U8"));
    add_def(module, sym, type, &type_data, null_segments, NULL);

    // Syntax Type : components and definition 
    {
        PiType atom_type = mk_enum_type(a, 4,
                                        "bool", 1, mk_prim_type(Bool),
                                        "integral", 1, mk_prim_type(Int_64),
                                        "symbol", 1,  mk_prim_type(Int_64),
                                        "string", 1, mk_string_type(a));
        type_data = &atom_type;
        sym = string_to_symbol(mv_string("Atom"));
        add_def(module, sym, type, &type_data, null_segments, NULL);

        PiType hint_type = mk_enum_type(a, 4, "none", 0, "expr", 0, "special", 0, "implicit", 0);
        type_data = &hint_type;
        sym = string_to_symbol(mv_string("Hint"));
        add_def(module, sym, type, &type_data, null_segments, NULL);

        type_val = mk_enum_type(a, 2,
                                "atom", 1, atom_type,
                                "node", 2, hint_type, mk_prim_type(Address));

        type_data = &type_val;
        sym = string_to_symbol(mv_string("Syntax"));
        add_def(module, sym, type, &type_data, null_segments, NULL);
        delete_pi_type(type_val, a);
        ModuleEntry* e = get_def(sym, module);
        syntax_type = e->value;
    }

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

    type = mk_proc_type(a, 1, mk_prim_type(Address), mk_prim_type(UInt_64));
    build_nop_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("address-to-num"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, type, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    type = mk_proc_type(a, 1, mk_prim_type(UInt_64), mk_prim_type(Address));
    build_nop_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("num-to-address"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, type, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    add_module(string_to_symbol(mv_string("core")), module, base);

    sdelete_u8_array(null_segments.code);
    sdelete_u8_array(null_segments.data);
    // Note: we do NOT delete the 'fn_segments.code' because it is the
    // assembler, and needs to be used later!
    sdelete_u8_array(fn_segments.data);
}

void add_primitive_module(String name, LocationSize sz, bool is_signed, Assembler* ass, Module* num, Allocator* a) {
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
    Module* module = mk_module(header, get_package(num), NULL, a);
    delete_module_header(header);
    Symbol sym;

    PiType type;
    //PiType type_val;
    //PiType* type_data = &type_val;
    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    PrimType prims[2][4] = {
        {UInt_8, UInt_16, UInt_32, UInt_64},
        {Int_8, Int_16, Int_32, Int_64},
    };
    PrimType prim = prims[is_signed][sz];

    Segments fn_segments = (Segments) {.data = mk_u8_array(0, a)};
    Segments prepped;

    build_binary_fn(ass, Add, sz, a, &point);
    type = mk_binop_type(a, prim, prim, prim);
    sym = string_to_symbol(mv_string("+"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, type, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    build_binary_fn(ass, Sub, sz, a, &point);
    sym = string_to_symbol(mv_string("-"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, type, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    build_special_binary_fn(ass, is_signed ? IMul : Mul, sz, a, &point);
    sym = string_to_symbol(mv_string("*"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, type, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    build_special_binary_fn(ass, is_signed ? IDiv : Div, sz, a, &point);
    sym = string_to_symbol(mv_string("/"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, type, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    build_comp_fn(ass, is_signed ? SetL : SetB, sz, a, &point);
    type = mk_binop_type(a, prim, prim, Bool);
    sym = string_to_symbol(mv_string("<"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, type, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    build_comp_fn(ass, is_signed ? SetG : SetA, sz, a, &point);
    sym = string_to_symbol(mv_string(">"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, type, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    build_comp_fn(ass, SetE, sz, a, &point);
    sym = string_to_symbol(mv_string("="));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, type, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    delete_pi_type(type, a);
    sdelete_u8_array(fn_segments.data);

    Result r = add_module_def(num, string_to_symbol(name), module);
    if (r.type == Err) panic(r.error_message);

}

void add_num_module(Assembler* ass, Package* base, Allocator* a) {
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

    add_primitive_module(mv_string("u8"), sz_8, false, ass, module, a);
    add_primitive_module(mv_string("u16"), sz_16, false, ass, module, a);
    add_primitive_module(mv_string("u32"), sz_32, false, ass, module, a);
    add_primitive_module(mv_string("u64"), sz_64, false, ass, module, a);

    add_primitive_module(mv_string("i8"), sz_8, true, ass, module, a);
    add_primitive_module(mv_string("i16"), sz_16, true, ass, module, a);
    add_primitive_module(mv_string("i32"), sz_32, true, ass, module, a);
    add_primitive_module(mv_string("i64"), sz_64, true, ass, module, a);

    add_module(string_to_symbol(mv_string("num")), module, base);
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

    Segments null_segments = (Segments) {
        .code = mk_u8_array(0, a),
        .data = mk_u8_array(0, a),
    };
    
    // uint64_t dyn_curr_package = mk_dynamic_var(sizeof(void*), &base); 
    std_allocator = mk_dynamic_var(sizeof(Allocator), default_allocator); 
    type = mk_dynamic_type(a, mk_struct_type(a, 4,
                                             "malloc", mk_prim_type(Address),
                                             "realloc", mk_prim_type(Address),
                                             "free", mk_prim_type(Address),
                                             "ctx", mk_prim_type(Address)));
    sym = string_to_symbol(mv_string("allocator"));
    add_def(module, sym, type, &std_allocator, null_segments, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    void* nul = NULL;
    std_tmp_allocator = mk_dynamic_var(sizeof(void*), &nul); 
    std_current_module = mk_dynamic_var(sizeof(Module*), &nul); 

    type = mk_dynamic_type(a, mk_prim_type(Address));
    sym = string_to_symbol(mv_string("temp-allocator"));
    add_def(module, sym, type, &std_tmp_allocator, null_segments, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    // C Wrappers!
    Segments fn_segments = {.data = mk_u8_array(0, a),};
    Segments prepped;

    // exit : Proc [] Unit
    type = mk_proc_type(a, 0, mk_prim_type(Unit));
    build_exit_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("exit"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, type, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    // malloc : Proc [U64] Address
    type = mk_proc_type(a, 1, mk_prim_type(UInt_64), mk_prim_type(Address));
    build_malloc_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("malloc"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, type, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    // realloc : Proc (Address U64) Address
    type = mk_proc_type(a, 2, mk_prim_type(Address),
                        mk_prim_type(UInt_64),
                        mk_prim_type(Address));
    build_realloc_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("realloc"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, type, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    // realloc : Proc [String] Unit
    type = mk_proc_type(a, 1, mk_prim_type(Address), mk_prim_type(Unit));
    build_free_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("free"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, type, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    // print : Proc [String] Unit
    type = mk_proc_type(a, 1, mk_string_type(a), mk_prim_type(Unit));
    build_print_fun(ass, a, &point);
    sym = string_to_symbol(mv_string("print"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, type, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    // print : Proc [String] Unit
    type = mk_proc_type(a, 1, mk_string_type(a), mk_prim_type(Unit));
    build_load_module_fun(ass, a, &point);
    sym = string_to_symbol(mv_string("load-module"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, type, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    // run-script : Proc [String] Unit
    type = mk_proc_type(a, 1, mk_string_type(a), mk_prim_type(Unit));
    build_run_script_fun(ass, a, &point);
    sym = string_to_symbol(mv_string("run-script"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, type, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    add_module(string_to_symbol(mv_string("extra")), module, base);

    sdelete_u8_array(null_segments.code);
    sdelete_u8_array(null_segments.data);
    sdelete_u8_array(fn_segments.data);
}

void add_user_module(Package* base, Allocator* a) {
    Imports imports = (Imports) {.clauses = mk_import_clause_array(3, a),};
    push_import_clause((ImportClause) {
            .type = ImportPathAll,
            .name = string_to_symbol(mv_string("core")),
        },
        &imports.clauses);
    push_import_clause((ImportClause) {
            .type = ImportPathAll,
            .name = string_to_symbol(mv_string("num")),
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
    add_num_module(ass, base, a);
    add_extra_module(ass, base, default_allocator, a);
    add_user_module(base, a);

    return base;
}
