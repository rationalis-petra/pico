#include "platform/signals.h"
#include "platform/memory/arena.h"

#include "components/pretty/string_printer.h"

#include "pico/stdlib/data/submodules.h"
#include "pico/stdlib/core.h"
#include "pico/values/modular.h"
#include "pico/values/ctypes.h"

PiType* arena_ty;

typedef struct {
    PiAllocator adapted_arena;
    Allocator* c_arena;
    ArenaAllocator* arena;
    Allocator alloc;
    PiAllocator pia;
} PicoArena;

void* relic_arena_malloc_adapter(size_t memsize, void* arena) {
    PicoArena* parena = (PicoArena*)arena;
    return arena_malloc(parena->arena, memsize);
}

void* relic_arena_realloc(void* ptr, size_t memsize, void* arena) {
    PicoArena* parena = (PicoArena*)arena;
    return arena_realloc(ptr, memsize, parena->arena);
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
void relic_arena_free(void* ptr, void* ctx) { }
#pragma GCC diagnostic pop

PicoArena* relic_make_arena_allocator(PiAllocator from, size_t blocksize) {
    PicoArena* ra = call_alloc(sizeof(PicoArena), &from);
    ra->pia = from;
    ra->alloc = convert_to_callocator(&ra->pia);
    ArenaAllocator* arena = make_arena_allocator(blocksize, &ra->alloc);
    ra->arena = arena;

    static AllocatorVTable arena_vtable = {
        .malloc = &relic_arena_malloc_adapter,
        .realloc = &relic_arena_realloc,
        .free = &relic_arena_free,
    };
    
    Allocator* a = call_alloc(sizeof(Allocator), &from);
    *a = (Allocator) {.vtable = &arena_vtable, .ctx =  ra};
    ra->c_arena = a;
    ra->adapted_arena = convert_to_pallocator(a);
    return ra;
}

void build_make_arena_allocator_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 2,
                                 "allocator", mk_allocator_ctype(pia),
                                 "blocksize", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                                 mk_voidptr_ctype(pia));
    convert_c_fn(relic_make_arena_allocator, &fn_ctype, type, ass, a, point); 
}

void relic_destroy_arena_allocator(PicoArena* arena) {
    PicoArena parena = *arena;
    delete_arena_allocator(parena.arena);
    call_free(arena->c_arena, &parena.pia);
    call_free(arena, &parena.pia);
}

void build_destroy_arena_allocator_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1,
                                 "allocator", mk_voidptr_ctype(pia),
                                 (CType){.sort = CSVoid});
    convert_c_fn(relic_destroy_arena_allocator, &fn_ctype, type, ass, a, point); 
}


void relic_reset_arena_allocator(PicoArena* arena) {
    reset_arena_allocator(arena->arena);
}

void build_reset_arena_allocator_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1,
                                 "arena", mk_voidptr_ctype(pia),
                                 (CType){.sort = CSVoid});
    convert_c_fn(relic_reset_arena_allocator, &fn_ctype, type, ass, a, point); 
}


PiAllocator relic_adapt_arena_allocator(PicoArena* arena) {
    return arena->adapted_arena;
}

void build_adapt_arena_allocator_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1,
                                 "allocator", mk_voidptr_ctype(pia),
                                 mk_allocator_ctype(pia));
    convert_c_fn(relic_adapt_arena_allocator, &fn_ctype, type, ass, a, point); 
}

void add_allocators_module(Assembler* ass, Module* data, RegionAllocator* region) {
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
        .name = string_to_symbol(mv_string("allocators")),
        .imports = imports,
        .exports = exports,
    };
    Package* base = get_package(data);
    Module* module = mk_module(header, base, data);
    delete_module_header(header);

    PiType* typep;
    PiType type;
    Symbol sym;
    ModuleEntry* e;
    ErrorPoint point;
    if (catch_error(point)) {
        panic(doc_to_str(point.error_message, 120, &ra));
    }

    Segments prepped;
    Segments fn_segments = {.data = mk_u8_array(0, &ra),};
    Segments null_segments = (Segments) {
        .code = mk_u8_array(0, &ra),
        .data = mk_u8_array(0, &ra),
    };

    typep = mk_opaque_type(pia, module, mk_named_type(pia, "Arena", mk_prim_type(pia, Address)));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("Arena"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    e = get_def(sym, module);
    arena_ty = e->value;

    typep = mk_proc_type(pia, 2, get_allocator_type(), mk_prim_type(pia, UInt_64), arena_ty);
    build_make_arena_allocator_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("make-arena"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, arena_ty, mk_prim_type(pia, Unit));
    build_destroy_arena_allocator_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("destroy-arena"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, arena_ty, mk_prim_type(pia, Unit));
    build_reset_arena_allocator_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("reset-arena"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, arena_ty, get_allocator_type());
    build_adapt_arena_allocator_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("adapt-arena"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    /* reset_subregion(subregion); */
    /* add_region_allocator_module(target, module, subregion); */
    /* reset_subregion(subregion); */
    /* add_perm_allocator_module(target, module, subregion); */

    add_module_def(data, string_to_symbol(mv_string("allocators")), module);
}
