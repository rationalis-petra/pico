#include "platform/signals.h"
#include "platform/filesystem/filesystem.h"

#include "pico/values/ctypes.h"
#include "pico/codegen/foreign_adapters.h"
#include "pico/stdlib/platform/filesystem.h"
#include "pico/stdlib/core.h"
#include "pico/stdlib/extra.h"

static PiType* file_ty;
static PiType* file_mode_ty;

typedef struct {
    uint64_t tag;
    uint64_t size;
} MaybeSize;

File *relic_open_file(String name, FilePermissions perms) {
    Allocator a = get_std_perm_allocator();
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
    Allocator a = get_std_perm_allocator();
    return read_chunk(file, !msize.tag, msize.size, &a);
}

void build_open_file_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 2,
                                 "name", mk_string_ctype(a),
                                 "mode", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CLongLong}),
                                 mk_voidptr_ctype(a));

    convert_c_fn(relic_open_file, &fn_ctype, type, ass, a, point); 

    delete_c_type(fn_ctype, a);
}

void build_close_file_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 1,
                                 "file", mk_voidptr_ctype(a),
                                 (CType){.sort = CSVoid});

    convert_c_fn(close_file, &fn_ctype, type, ass, a, point); 

    delete_c_type(fn_ctype, a);
}

void build_read_byte_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 1,
                                 "file", mk_voidptr_ctype(a),
                                 mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CChar}));

    convert_c_fn(relic_read_byte, &fn_ctype, type, ass, a, point); 

    delete_c_type(fn_ctype, a);
}

void build_read_chunk_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType msize_ctype = mk_struct_ctype(a, 2,
                                 "cap", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CLongLong}),
                                 "len", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CLongLong}));

    CType fn_ctype = mk_fn_ctype(a, 2,
                                 "file", mk_voidptr_ctype(a),
                                 "maybe_size", msize_ctype,
                                 mk_array_ctype(a));

    convert_c_fn(relic_read_chunk, &fn_ctype, type, ass, a, point); 

    delete_c_type(fn_ctype, a);
}


void build_write_byte_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 2,
                                 "file", mk_voidptr_ctype(a),
                                 "byte", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CChar}),
                                 (CType){.sort = CSVoid});

    convert_c_fn(write_byte, &fn_ctype, type, ass, a, point); 

    delete_c_type(fn_ctype, a);
}

void build_write_chunk_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType array_ctype = mk_struct_ctype(a, 4,
                                 "data", mk_voidptr_ctype(a),
                                 "cap", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CLongLong}),
                                 "len", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CLongLong}),
                                 "gpa", mk_voidptr_ctype(a));

    CType fn_ctype = mk_fn_ctype(a, 2,
                                 "file", mk_voidptr_ctype(a),
                                 "chunk", array_ctype,
                                 (CType){.sort = CSVoid});

    convert_c_fn(write_chunk, &fn_ctype, type, ass, a, point); 

    delete_c_type(fn_ctype, a);
}

void add_filesystem_module(Assembler *ass, Module *platform, Allocator *a) {
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, a),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("filesystem")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(platform), NULL, a);
    delete_module_header(header);
    Symbol sym;

    ModuleEntry* e;
    PiType type;
    PiType* typep;
    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    Segments prepped;
    Segments fn_segments = {.data = mk_u8_array(0, a),};
    Segments null_segments = (Segments) {
        .code = mk_u8_array(0, a),
        .data = mk_u8_array(0, a),
    };

    typep = mk_opaque_type(a, module, mk_prim_type(a, Address));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("File"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    file_ty = e->value;
    delete_pi_type_p(typep, a);

    typep = mk_enum_type(a, 3, "read", 0, "write", 0, "read-write", 0, "append", 0, "read-append", 0);
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("Mode"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    file_mode_ty = e->value;
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 2, mk_string_type(a),
                         copy_pi_type_p(file_mode_ty, a),
                         copy_pi_type_p(file_ty, a));
    build_open_file_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("open-file"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 1, copy_pi_type_p(file_ty, a), mk_prim_type(a, Unit));
    build_close_file_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("close-file"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 1, copy_pi_type_p(file_ty, a), mk_prim_type(a, UInt_8));
    build_read_byte_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("read-byte"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    PiType* u64 = mk_prim_type(a, UInt_64);
    PiType* u8 = mk_prim_type(a, UInt_8);
    typep = mk_proc_type(a, 2, copy_pi_type_p(file_ty, a),
                         mk_app_type(a, get_maybe_type(), u64),
                         mk_app_type(a, get_list_type(), u8));
    build_read_chunk_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("read-chunk"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);
    delete_pi_type_p(u64, a);

    typep = mk_proc_type(a, 2, copy_pi_type_p(file_ty, a), mk_prim_type(a, UInt_8), mk_prim_type(a, Unit));
    build_write_byte_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("write-byte"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 2, copy_pi_type_p(file_ty, a),
                         mk_app_type(a, get_list_type(), u8),
                         mk_prim_type(a, Unit));
    build_write_chunk_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("write-chunk"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);
    delete_pi_type_p(u8, a);

    sdelete_u8_array(fn_segments.data);
    sdelete_u8_array(null_segments.data);
    sdelete_u8_array(null_segments.code);

    Result r = add_module_def(platform, string_to_symbol(mv_string("filesystem")), module);
    if (r.type == Err) panic(r.error_message);
}
