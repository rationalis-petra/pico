#include "data/float.h"

#include "platform/signals.h"
#include "platform/thread.h"

#include "components/pretty/string_printer.h"

#include "pico/values/ctypes.h"
#include "pico/codegen/codegen.h"
#include "pico/stdlib/platform/submodules.h"

void relic_sleep_for(float64_t seconds) {
    sleep_for((Microseconds){seconds * 1000000});
}

void build_sleep_for_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "time", (CType){.sort = CSDouble}, (CType){.sort = CSVoid});
    convert_c_fn(relic_sleep_for, &fn_ctype, type, ass, a, point); 
}

void add_thread_module(Assembler *ass, Module *platform, RegionAllocator* region) {
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
        .name = string_to_symbol(mv_string("thread")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(platform), NULL);
    Symbol sym;

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

    typep = mk_named_type(pia, "Seconds", mk_prim_type(pia, Float_64));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("Seconds"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    PiType* seconds_ty = e->value;

    typep = mk_proc_type(pia, 1, copy_pi_type_p(seconds_ty, pia), mk_prim_type(pia, Unit));
    build_sleep_for_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("sleep-for"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    Result r = add_module_def(platform, string_to_symbol(mv_string("thread")), module);
    if (r.type == Err) panic(r.error_message);
}
