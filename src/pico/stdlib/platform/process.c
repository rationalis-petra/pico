
#include "platform/signals.h"
#include "platform/process.h"

#include "components/pretty/string_printer.h"
#include "pico/data/client/list.h"

#include "pico/values/ctypes.h"
#include "pico/codegen/codegen.h"
#include "pico/stdlib/core/kernel.h"
#include "pico/stdlib/platform/submodules.h"


ProcessResult relic_create_process(String string, StringPiList strlist) {
    StringArray arr = (StringArray) {
        .data = strlist.data,
        .len = strlist.len,
        .size = strlist.size,
    };
    return create_process(string, arr);
}

void build_create_process_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 2,
                               "command", mk_string_ctype(pia), 
                               "args", mk_list_ctype(pia),
                               mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Signed}));

    convert_c_fn(relic_create_process, &fn_ctype, type, ass, a, point); 
}

void build_wait_on_process_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    // TODO (possible bug: maybe the retcode is more than 8 bits??  
    CType retcode = mk_primint_ctype((CPrimInt){.prim = CChar, .is_signed = Signed});
    CType result_type = mk_result_ctype(pia, retcode, (CType){.sort = CSVoid});
    CType fn_ctype = mk_fn_ctype(pia, 1,
                                 "process", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Signed}),
                                 result_type);

    convert_c_fn(wait_on_process, &fn_ctype, type, ass, a, point); 
}

void build_kill_process_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1,
                                 "process", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Signed}),
                                 (CType){.sort = CSVoid});

    convert_c_fn(kill_process, &fn_ctype, type, ass, a, point); 
}

void add_process_module(Assembler *ass, Module *platform, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    PiAllocator pico_region = convert_to_pallocator(&ra);
    PiAllocator* pia = &pico_region;

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
        .name = string_to_name(mv_string("process")),
        .imports = imports,
        .re_exports = re_exports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(platform), platform);
    Name name;

    ModuleEntry* e;
    PiType type;
    PiType* typep;
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

    typep = mk_opaque_type(pia, "Process", module, mk_prim_type(pia, Int_64));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    name = string_to_name(mv_string("Process"));
    add_def(module, name, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def_internal(name, module);
    PiType* process_ty = e->value;

    typep = mk_proc_type(pia, 2, mk_string_type(pia), mk_app_type(pia, get_list_type(), mk_string_type(pia)), process_ty);
    build_create_process_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("create-process"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, process_ty, mk_app_type(pia, get_result_type(), mk_prim_type(pia, Int_8), mk_prim_type(pia, Unit)));
    build_wait_on_process_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("wait-on-process"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, process_ty, mk_prim_type(pia, Unit));
    build_kill_process_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("kill-process"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
}
