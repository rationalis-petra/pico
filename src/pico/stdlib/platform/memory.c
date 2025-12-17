#include "platform/machine_info.h"
#include "platform/signals.h"
#include "platform/memory/platform.h"

#include "pico/data/client/allocator.h"
#include "pico/values/ctypes.h"
#include "pico/codegen/codegen.h"
#include "pico/stdlib/core.h"
#include "pico/stdlib/platform/submodules.h"
#include "pico/codegen/backend-direct/internal.h"

//   Memory Allocators
// -----------------------------------------------------------------------------

static uint64_t std_current_allocator; 
static uint64_t std_perm_allocator; 
static uint64_t std_region_allocator; 
//static uint64_t std_comptime_allocator; 
static uint64_t std_temp_allocator; 
PiAllocator get_std_current_allocator() {
    PiAllocator** data = get_dynamic_memory();
    PiAllocator* dyn = data[std_current_allocator]; 
    return *dyn;
}

PiAllocator set_std_current_allocator(PiAllocator al) {
    void** data = get_dynamic_memory();
    PiAllocator* dyn = data[std_current_allocator]; 
    PiAllocator old = *dyn;
    *dyn = al;
    return old;
}

PiAllocator get_std_perm_allocator() {
    PiAllocator** data = get_dynamic_memory();
    PiAllocator* dyn = data[std_perm_allocator]; 
    return *dyn;
}

PiAllocator set_std_perm_allocator(PiAllocator al) {
    void** data = get_dynamic_memory();
    PiAllocator* dyn = data[std_perm_allocator]; 
    PiAllocator old = *dyn;
    *dyn = al;
    return old;
}

PiAllocator get_std_temp_allocator() {
    void** data = get_dynamic_memory();
    PiAllocator* dyn = data[std_temp_allocator]; 
    return *dyn;
}

PiAllocator set_std_temp_allocator(PiAllocator al) {
    void** data = get_dynamic_memory();
    PiAllocator* dyn = data[std_temp_allocator]; 
    PiAllocator old = *dyn;
    *dyn = al;
    return old;
}

PiAllocator get_std_region_allocator() {
    void** data = get_dynamic_memory();
    PiAllocator* dyn = data[std_region_allocator]; 
    return *dyn;
}

PiAllocator set_std_region_allocator(PiAllocator al) {
    void** data = get_dynamic_memory();
    PiAllocator* dyn = data[std_region_allocator]; 
    PiAllocator old = *dyn;
    *dyn = al;
    return old;
}

//   Allocation Functions
// -----------------------------------------------------------------------------

void* relic_realloc(void* address, uint64_t size) {
    PiAllocator a = get_std_current_allocator();
    return call_alloc(size, &a);
}

void build_realloc_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // realloc : Proc (Address U64) Address
    build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);

#if ABI == SYSTEM_V_64
    // realloc (ptr = rdi, size = rsi)
    // copy size into RDX
    build_unary_op(Pop, reg(RSI, sz_64), ass, a, point);
    build_unary_op(Pop, reg(RDI, sz_64), ass, a, point);
    build_unary_op(Push, reg(RAX, sz_64), ass, a, point);

#elif ABI == WIN_64
    // realloc (ptr = RCX, size = RDX)
    build_unary_op(Pop, reg(RDX, sz_64), ass, a, point);
    build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);
    build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
#endif

    generate_c_call(relic_realloc, ass, a, point);

    build_unary_op(Pop, reg(R9, sz_64), ass, a, point);
    build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
    build_unary_op(Push, reg(R9, sz_64), ass, a, point);

    build_nullary_op(Ret, ass, a, point);
}

void* relic_malloc(uint64_t size) {
    PiAllocator a = get_std_current_allocator();
    return call_alloc(size, &a);
}

void build_malloc_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // malloc : Proc (U64) Address
    build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);

#if ABI == SYSTEM_V_64
    // memcpy (dest = rdi, src = rsi, size = rdx)
    // copy size into RDX
    build_unary_op(Pop, reg(RDI, sz_64), ass, a, point);
    build_unary_op(Push, reg(RAX, sz_64), ass, a, point);

#elif ABI == WIN_64
    build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);
    build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
#endif

    generate_c_call(relic_malloc, ass, a, point);

    build_unary_op(Pop, reg(R9, sz_64), ass, a, point);
    build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
    build_unary_op(Push, reg(R9, sz_64), ass, a, point);

    build_nullary_op(Ret, ass, a, point);
}

void relic_free(void* address) {
    PiAllocator a = get_std_current_allocator();
    call_free(address, &a);
}

void build_free_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // free : Proc (Address) Unit
    build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);

#if ABI == SYSTEM_V_64
    // free (dest = rdi)
    // copy address into RDI
    build_unary_op(Pop, reg(RDI, sz_64), ass, a, point);
    build_unary_op(Push, reg(RAX, sz_64), ass, a, point);

#elif ABI == WIN_64
    // free (addr = rcx)
    // copy address into RCX
    build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);
    build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
#endif

    generate_c_call(relic_free, ass, a, point);

    build_nullary_op(Ret, ass, a, point);
}

void* relic_temp_malloc(uint64_t size) {
    PiAllocator a = get_std_temp_allocator();
    return call_alloc(size, &a);
}

void build_temp_malloc_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // malloc : Proc (U64) Address
    build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);

#if ABI == SYSTEM_V_64
    // memcpy (dest = rdi, src = rsi, size = rdx)
    // copy size into RDX
    build_unary_op(Pop, reg(RDI, sz_64), ass, a, point);
    build_unary_op(Push, reg(RAX, sz_64), ass, a, point);

#elif ABI == WIN_64
    build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);
    build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
#endif

    generate_c_call(relic_temp_malloc, ass, a, point);

    build_unary_op(Pop, reg(R9, sz_64), ass, a, point);
    build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
    build_unary_op(Push, reg(R9, sz_64), ass, a, point);

    build_nullary_op(Ret, ass, a, point);
}

void build_platform_alloc_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType memblk = mk_struct_ctype(pia, 2,
                                   "data", mk_voidptr_ctype(pia),
                                   "size", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CLongLong}));
    CType fn_ctype = mk_fn_ctype(pia, 2,
                                 "min_size", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CLongLong}),
                                 "flags", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}),
                                 memblk);

    convert_c_fn(platform_allocate, &fn_ctype, type, ass, a, point); 
}

void build_platform_free_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType memblk = mk_struct_ctype(pia, 2,
                                   "data", mk_voidptr_ctype(pia),
                                   "size", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CLongLong}));
    CType fn_ctype = mk_fn_ctype(pia, 1, "block", memblk, (CType){.sort = CSVoid});

    convert_c_fn(platform_free, &fn_ctype, type, ass, a, point); 
}

void add_platform_memory_module(Assembler *ass, Module *platform, Allocator* default_allocator, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    PiAllocator pico_region = convert_to_pallocator(&ra);
    PiAllocator* pia = &pico_region;

    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, &ra),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("memory")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(platform), NULL);
    Symbol sym;

    PiType kind;
    PiType* typep;
    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    Segments prepped;
    Segments fn_segments = {.data = mk_u8_array(0, &ra),};
    Segments null_segments = (Segments) {
        .code = mk_u8_array(0, &ra),
        .data = mk_u8_array(0, &ra),
    };

    
    typep = mk_struct_type(pia, 2, "data", mk_prim_type(pia, Address), "size", mk_prim_type(pia, UInt_64));
    kind = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("MemBlock"));
    add_def(module, sym, kind, &typep, null_segments, NULL);
    clear_assembler(ass);

    //  Paged Allocation 
    // -------------------------------------------------------------------------
    uint32_t flag_val = 1; // ARead
    typep = mk_prim_type(pia, UInt_32);
    kind = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("read"));
    add_def(module, sym, *typep, &flag_val, null_segments, NULL);
    clear_assembler(ass);

    flag_val = 2; // AWrite
    typep = mk_prim_type(pia, UInt_32);
    kind = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("write"));
    add_def(module, sym, *typep, &flag_val, null_segments, NULL);
    clear_assembler(ass);

    flag_val = 4; // AExecute
    typep = mk_prim_type(pia, UInt_32);
    kind = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("execute"));
    add_def(module, sym, *typep, &flag_val, null_segments, NULL);
    clear_assembler(ass);

    PiType* memblk = mk_struct_type(pia, 2, "data", mk_prim_type(pia, Address), "size", mk_prim_type(pia, UInt_64));
    typep = mk_proc_type(pia, 2, mk_prim_type(pia, UInt_64), mk_prim_type(pia, UInt_32), memblk);
    build_platform_alloc_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("paged-allocate"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    memblk = mk_struct_type(pia, 2, "data", mk_prim_type(pia, Address), "size", mk_prim_type(pia, UInt_64));
    typep = mk_proc_type(pia, 1, memblk, mk_prim_type(pia, Unit));
    build_platform_free_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("paged-free"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    //  Allocators
    // -------------------------------------------------------------------------
    typep = mk_dynamic_type(pia, get_allocator_type());
    PiAllocator pico_default_allocator = convert_to_pallocator(default_allocator);
    std_perm_allocator = mk_dynamic_var(sizeof(PiAllocator), &pico_default_allocator);
    sym = string_to_symbol(mv_string("perm-allocator"));
    add_def(module, sym, *typep, &std_perm_allocator, null_segments, NULL);

    std_current_allocator = mk_dynamic_var(sizeof(PiAllocator), &pico_default_allocator); 
    sym = string_to_symbol(mv_string("current-allocator"));
    add_def(module, sym, *typep, &std_current_allocator, null_segments, NULL);

    PiAllocator nul_alloc = (PiAllocator){};
    std_temp_allocator = mk_dynamic_var(sizeof(PiAllocator), &nul_alloc); 

    typep = mk_dynamic_type(pia, mk_prim_type(pia, Address));
    sym = string_to_symbol(mv_string("temp-allocator"));
    add_def(module, sym, *typep, &std_temp_allocator, null_segments, NULL);
    clear_assembler(ass);


    //  Allocation Functions
    // -------------------------------------------------------------------------
    // malloc : Proc [U64] Address
    typep = mk_proc_type(pia, 1, mk_prim_type(pia, UInt_64), mk_prim_type(pia, Address));
    build_malloc_fn(ass, &ra, &point);
    sym = string_to_symbol(mv_string("alloc"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    // realloc : Proc (Address U64) Address
    typep = mk_proc_type(pia, 2, mk_prim_type(pia, Address),
                        mk_prim_type(pia, UInt_64),
                        mk_prim_type(pia, Address));
    build_realloc_fn(ass, &ra, &point);
    sym = string_to_symbol(mv_string("realloc"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    // realloc : Proc [String] Unit
    typep = mk_proc_type(pia, 1, mk_prim_type(pia, Address), mk_prim_type(pia, Unit));
    build_free_fn(ass, &ra, &point);
    sym = string_to_symbol(mv_string("free"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    // malloc : Proc [U64] Address
    typep = mk_proc_type(pia, 1, mk_prim_type(pia, UInt_64), mk_prim_type(pia, Address));
    build_temp_malloc_fn(ass, &ra, &point);
    sym = string_to_symbol(mv_string("temp-alloc"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    Result r = add_module_def(platform, string_to_symbol(mv_string("memory")), module);
    if (r.type == Err) panic(r.error_message);
}
