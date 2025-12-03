#include "platform/signals.h"
#include "platform/filesystem/filesystem.h"

#include "pico/values/ctypes.h"
#include "pico/codegen/codegen.h"
#include "pico/stdlib/platform/submodules.h"
#include "pico/stdlib/core.h"
#include "pico/stdlib/extra.h"

static PiType* file_ty;
static PiType* file_mode_ty;

typedef struct {
    uint64_t tag;
    uint64_t size;
} MaybeSize;

File *relic_open_file(String name, FilePermissions perms) {
    PiAllocator pia = get_std_perm_allocator();
    Allocator a = convert_to_callocator(&pia);
    return open_file(name, perms, &a);
}

uint8_t relic_read_byte(File *file) {
    uint8_t out;
    if (read_byte(file, &out)) {
        panic(mv_string("failed to read byte!"));
    }
    return out;
}

U8Array relic_read_chunk(File *file, MaybeSize msize) {
    // TODO: The allocator pointer goes on to live in the output array,
    //       and so it will be dangling when this function exits
    PiAllocator pia = get_std_perm_allocator();
    Allocator a = convert_to_callocator(&pia);
    return read_chunk(file, !msize.tag, msize.size, &a);
}

void build_open_file_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 2,
                                 "name", mk_string_ctype(pia),
                                 "mode", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CLongLong}),
                                 mk_voidptr_ctype(pia));

    convert_c_fn(relic_open_file, &fn_ctype, type, ass, a, point); 
}

void build_close_file_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1,
                                 "file", mk_voidptr_ctype(pia),
                                 (CType){.sort = CSVoid});

    convert_c_fn(close_file, &fn_ctype, type, ass, a, point); 
}

void build_read_byte_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1,
                                 "file", mk_voidptr_ctype(pia),
                                 mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CChar}));

    convert_c_fn(relic_read_byte, &fn_ctype, type, ass, a, point); 
}

void build_read_chunk_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType msize_ctype = mk_struct_ctype(pia, 2,
                                 "cap", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CLongLong}),
                                 "len", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CLongLong}));

    CType fn_ctype = mk_fn_ctype(pia, 2,
                                 "file", mk_voidptr_ctype(pia),
                                 "maybe_size", msize_ctype,
                                 mk_list_ctype(pia));

    convert_c_fn(relic_read_chunk, &fn_ctype, type, ass, a, point); 
}


void build_write_byte_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 2,
                                 "file", mk_voidptr_ctype(pia),
                                 "byte", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CChar}),
                                 (CType){.sort = CSVoid});

    convert_c_fn(write_byte, &fn_ctype, type, ass, a, point); 
}

void build_write_chunk_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 2,
                                 "file", mk_voidptr_ctype(pia),
                                 "chunk", mk_list_ctype(pia),
                                 (CType){.sort = CSVoid});

    convert_c_fn(write_chunk, &fn_ctype, type, ass, a, point); 
}

void add_filesystem_module(Assembler *ass, Module *platform, PiAllocator *module_allocator, RegionAllocator* region) {
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
        .name = string_to_symbol(mv_string("filesystem")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(platform), NULL, *module_allocator);
    Symbol sym;

    ModuleEntry* e;
    PiType type;
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

    typep = mk_opaque_type(pia, module, mk_prim_type(pia, Address));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("File"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    file_ty = e->value;

    typep = mk_enum_type(pia, 3, "read", 0, "write", 0, "read-write", 0, "append", 0, "read-append", 0);
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("Mode"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    file_mode_ty = e->value;

    typep = mk_proc_type(pia, 2, mk_string_type(pia),
                         copy_pi_type_p(file_mode_ty, pia),
                         copy_pi_type_p(file_ty, pia));
    build_open_file_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("open-file"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(file_ty, pia), mk_prim_type(pia, Unit));
    build_close_file_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("close-file"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(file_ty, pia), mk_prim_type(pia, UInt_8));
    build_read_byte_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("read-byte"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    PiType* u64 = mk_prim_type(pia, UInt_64);
    PiType* u8 = mk_prim_type(pia, UInt_8);
    typep = mk_proc_type(pia, 2, copy_pi_type_p(file_ty, pia),
                         mk_app_type(pia, get_maybe_type(), u64),
                         mk_app_type(pia, get_list_type(), u8));
    build_read_chunk_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("read-chunk"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(u64, pia);

    typep = mk_proc_type(pia, 2, copy_pi_type_p(file_ty, pia), mk_prim_type(pia, UInt_8), mk_prim_type(pia, Unit));
    build_write_byte_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("write-byte"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 2, copy_pi_type_p(file_ty, pia),
                         mk_app_type(pia, get_list_type(), u8),
                         mk_prim_type(pia, Unit));
    build_write_chunk_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("write-chunk"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    Result r = add_module_def(platform, string_to_symbol(mv_string("filesystem")), module);
    if (r.type == Err) panic(r.error_message);
}
