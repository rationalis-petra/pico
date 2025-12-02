#include "data/float.h"

#include "platform/signals.h"
#include "platform/time/time.h"

#include "pico/values/ctypes.h"
#include "pico/codegen/codegen.h"
#include "pico/stdlib/platform/submodules.h"
#include "pico/stdlib/core.h"
#include "pico/stdlib/extra.h"

float64_t start_timer() {
    return time_to_double(query_mono_timer(), Seconds);
}

float64_t time_elapsed(float64_t start_time) {
    float64_t current_time = time_to_double(query_mono_timer(), Seconds);
    return current_time - start_time;
}

void build_start_timer_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 0, (CType){.sort = CSDouble});

    convert_c_fn(start_timer, &fn_ctype, type, ass, a, point); 

    delete_c_type(fn_ctype, pia);
}

void build_time_elapsed_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1,
                                 "timer", (CType){.sort = CSDouble},
                                 (CType){.sort = CSDouble});

    convert_c_fn(time_elapsed, &fn_ctype, type, ass, a, point); 

    delete_c_type(fn_ctype, pia);
}

void add_time_module(Assembler *ass, Module *platform, Allocator *a) {
    PiAllocator pico_allocator = convert_to_pallocator(a);
    PiAllocator* pia = &pico_allocator;

    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, a),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("time")),
        .imports = imports,
        .exports = exports,
    };
    PiAllocator pico_module_allocator = convert_to_pallocator(a);
    Module* module = mk_module(header, get_package(platform), NULL, pico_module_allocator);
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

    typep = mk_opaque_type(pia, module, mk_named_type(pia, "Timer", mk_prim_type(pia, Float_64)));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("Timer"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    PiType* timer_ty = e->value;
    delete_pi_type_p(typep, pia);

    typep = mk_distinct_type(pia, mk_named_type(pia, "Seconds", mk_prim_type(pia, Float_64)));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("Seconds"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    PiType* seconds_ty = e->value;
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 0, copy_pi_type_p(timer_ty, pia));
    build_start_timer_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("start-timer"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(timer_ty, pia), copy_pi_type_p(seconds_ty, pia));
    build_time_elapsed_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("time-elapsed"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    sdelete_u8_array(fn_segments.data);
    sdelete_u8_array(null_segments.data);
    sdelete_u8_array(null_segments.code);

    Result r = add_module_def(platform, string_to_symbol(mv_string("time")), module);
    if (r.type == Err) panic(r.error_message);
}
