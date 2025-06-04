#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "platform/machine_info.h"
#include "platform/signals.h"
#include "platform/signals.h"
#include "platform/memory/arena.h"

#include "pico/stdlib/extra.h"
#include "pico/codegen/foreign_adapters.h"

#include "app/module_load.h"

static jump_buf* m_buf;
void set_exit_callback(jump_buf* buf) { m_buf = buf; }

static uint64_t std_current_allocator; 
static uint64_t std_perm_allocator; 
static uint64_t std_region_allocator; 
static uint64_t std_comptime_allocator; 
static uint64_t std_temp_allocator; 

static Package* current_package;
void set_current_package(Package* current) { current_package = current; }

static IStream* current_istream;
void set_std_istream(IStream* current) { current_istream = current; }

static OStream* current_ostream;
void set_std_ostream(OStream* current) { current_ostream = current; }

Allocator* get_std_temp_allocator() {
    void** data = get_dynamic_memory();
    Allocator** dyn = data[std_temp_allocator]; 
    return *dyn;
}

Allocator* set_std_temp_allocator(Allocator* al) {
    void** data = get_dynamic_memory();
    Allocator** dyn = data[std_temp_allocator]; 
    Allocator* old = *dyn;
    *dyn = al;
    return old;
}

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

void build_realloc_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // realloc : Proc (Address U64) Address
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

void *relic_malloc(uint64_t size) {
    
}

void build_malloc_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // malloc : Proc (U64) Address
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

void build_panic_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // Pop return address (we don't need it)
    build_binary_op(ass, Add, reg(RSP, sz_64), imm8(8), a, point);

    // panic : All [A] Proc (String) A
#if ABI == SYSTEM_V_64
    // panic (str.memsize = rdi, str.bytes = rsi)
    // 
    // copy address into RDI
    build_unary_op(ass, Pop, reg(RDI, sz_64), a, point);
    build_unary_op(ass, Pop, reg(RSI, sz_64), a, point);

#elif ABI == WIN_64
    // panic (&str = rcx)
    // copy address into RCX
    build_binary_op(ass, Mov, reg(RCX, sz_64), reg(RSP, sz_64), a, point);
    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#endif
    build_binary_op(ass, Mov, reg(RAX, sz_64), imm64((uint64_t)&panic),  a, point);
    build_unary_op(ass, Call, reg(RAX, sz_64), a, point);
    // Panic does not return
}

PiType* build_panic_fn_ty(Allocator* a) {
    PiType* proc_ty = mk_proc_type(a, 1, mk_string_type(a), mk_var_type(a, "A"));

    SymbolArray types = mk_symbol_array(1, a);
    push_symbol(string_to_symbol(mv_string("A")), &types);

    PiType* out_ty = mem_alloc(sizeof(PiType), a);
    *out_ty =  (PiType) {.sort = TAll, .binder.vars = types, .binder.body = proc_ty};
    return out_ty;
}

void exit_callback() {
    long_jump(*m_buf, 1);
}

void build_exit_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    build_binary_op(ass, Mov, reg(RAX, sz_64), imm64((uint64_t)exit_callback), a, point);
    build_unary_op(ass, Call, reg(RAX, sz_64), a, point);
}

void relic_print_fn(String s) {
    fputs((char*)s.bytes, stdout);
}

void relic_println_fn(String s) {
    puts((char*)s.bytes);
}

void build_print_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 1, "string", mk_string_ctype(a), (CType){.sort = CSVoid});
    convert_c_fn(relic_print_fn, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

void build_println_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 1, "string", mk_string_ctype(a), (CType){.sort = CSVoid});
    convert_c_fn(relic_println_fn, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

// C implementation (called from pico!)
void load_module_c_fun(String filename) {
    // (ann load-module String → Unit) : load the module/code located in file! 
    
    Allocator* a = get_std_allocator();
    IStream* sfile = open_file_istream(filename, a);
    load_module_from_istream(sfile, mk_formatted_ostream(current_ostream, a), current_package, NULL, a);
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

Result run_script_c_fun(String filename) {
    // (ann load-module String → Unit) : load the module/code located in file! 
    
    Allocator* a = get_std_allocator();
    IStream* sfile = open_file_istream(filename, a);
    if (!sfile) {
      return (Result) {
        .type = Err, .error_message = mv_string("failed to open file!"),
      };
    }
    Module* current_module = get_std_current_module();
    run_script_from_istream(sfile, mk_formatted_ostream(current_ostream, a), current_module, a);
    delete_istream(sfile, a);
    return (Result) {.type = Ok};
}

void build_run_script_fun(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // Proc type
    // TODO: this does not accurately represent the result type!
    CType result_ctype = mk_struct_ctype(a, 2,
                                         "type", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                                         "error_message", copy_c_type(mk_string_ctype(a), a));


    CType fn_ctype = mk_fn_ctype(a, 1, "filename", mk_string_ctype(a), result_ctype);

    convert_c_fn(run_script_c_fun, &fn_ctype, type, ass, a, point); 
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

    Allocator arena = mk_arena_allocator(4096, a);
    a = &arena;
    
    // uint64_t dyn_curr_package = mk_dynamic_var(sizeof(void*), &base); 
    std_perm_allocator = mk_dynamic_var(sizeof(Allocator), default_allocator); 
    typep = mk_dynamic_type(a, mk_struct_type(a, 4,
                                             "malloc", mk_prim_type(a, Address),
                                             "realloc", mk_prim_type(a, Address),
                                             "free", mk_prim_type(a, Address),
                                             "ctx", mk_prim_type(a, Address)));
    sym = string_to_symbol(mv_string("perm-allocator"));
    add_def(module, sym, *typep, &std_perm_allocator, null_segments, NULL);

    std_current_allocator = mk_dynamic_var(sizeof(Allocator), default_allocator); 
    typep = mk_dynamic_type(a, mk_struct_type(a, 4,
                                             "malloc", mk_prim_type(a, Address),
                                             "realloc", mk_prim_type(a, Address),
                                             "free", mk_prim_type(a, Address),
                                             "ctx", mk_prim_type(a, Address)));
    sym = string_to_symbol(mv_string("current-allocator"));
    add_def(module, sym, *typep, &std_current_allocator, null_segments, NULL);

    void* nul = NULL;
    std_temp_allocator = mk_dynamic_var(sizeof(void*), &nul); 
    std_current_module = mk_dynamic_var(sizeof(Module*), &nul); 

    typep = mk_dynamic_type(a, mk_prim_type(a, Address));
    sym = string_to_symbol(mv_string("temp-allocator"));
    add_def(module, sym, *typep, &std_temp_allocator, null_segments, NULL);
    clear_assembler(ass);

    // C Wrappers!
    Segments fn_segments = {.data = mk_u8_array(0, a),};
    Segments prepped;

    // exit : Proc [] Unit
    typep = mk_proc_type(a, 0, mk_prim_type(a, Unit));
    build_exit_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("exit"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    // panic : All [A] Proc [String] A
    typep = build_panic_fn_ty(a);
    build_panic_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("panic"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    // malloc : Proc [U64] Address
    typep = mk_proc_type(a, 1, mk_prim_type(a, UInt_64), mk_prim_type(a, Address));
    build_malloc_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("malloc"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    // realloc : Proc (Address U64) Address
    typep = mk_proc_type(a, 2, mk_prim_type(a, Address),
                        mk_prim_type(a, UInt_64),
                        mk_prim_type(a, Address));
    build_realloc_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("realloc"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    // realloc : Proc [String] Unit
    typep = mk_proc_type(a, 1, mk_prim_type(a, Address), mk_prim_type(a, Unit));
    build_free_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("free"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    // print : Proc [String] Unit
    typep = mk_proc_type(a, 1, mk_string_type(a), mk_prim_type(a, Unit));
    build_print_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("print"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    build_println_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("print-ln"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    // load-module : Proc [String] Unit
    typep = mk_proc_type(a, 1, mk_string_type(a), mk_prim_type(a, Unit));
    build_load_module_fun(ass, a, &point);
    sym = string_to_symbol(mv_string("load-module"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    // run-script : Proc [String] Unit
    typep = mk_proc_type(a, 1, mk_string_type(a), mk_enum_type(a, 2, "Ok", 0, "Err", 1, mk_string_type(a)));
    build_run_script_fun(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("run-script"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    add_module(string_to_symbol(mv_string("extra")), module, base);

    sdelete_u8_array(null_segments.code);
    sdelete_u8_array(null_segments.data);
    sdelete_u8_array(fn_segments.data);
    release_arena_allocator(arena);
}
