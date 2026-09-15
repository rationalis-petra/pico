#include "data/float.h"

#include "platform/signals.h"
#include "platform/time/time.h"

#include "components/pretty/string_printer.h"

#include "pico/values/ctypes.h"
#include "pico/codegen/codegen.h"
#include "pico/stdlib/platform/submodules.h"

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
}

void build_time_elapsed_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1,
                                 "timer", (CType){.sort = CSDouble},
                                 (CType){.sort = CSDouble});

    convert_c_fn(time_elapsed, &fn_ctype, type, ass, a, point); 
}

double seconds_to_microseconds(double seconds) {
    return seconds * 1000000.0;
}
double seconds_to_milliseconds(double seconds) {
    return seconds * 1000.0;
}
double seconds_to_seconds(double seconds) {
    return seconds;
}
double seconds_to_minutes(double seconds) {
    return seconds / 60.0;
}
double seconds_to_hours(double seconds) {
    return seconds / 3600.0;
}

void build_microseconds_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1,
                                 "timer", (CType){.sort = CSDouble},
                                 (CType){.sort = CSDouble});

    convert_c_fn(seconds_to_microseconds, &fn_ctype, type, ass, a, point); 
}
void build_milliseconds_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1,
                                 "timer", (CType){.sort = CSDouble},
                                 (CType){.sort = CSDouble});

    convert_c_fn(seconds_to_milliseconds, &fn_ctype, type, ass, a, point); 
}
void build_seconds_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1,
                                 "timer", (CType){.sort = CSDouble},
                                 (CType){.sort = CSDouble});

    convert_c_fn(seconds_to_seconds, &fn_ctype, type, ass, a, point); 
}
void build_minutes_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1,
                                 "timer", (CType){.sort = CSDouble},
                                 (CType){.sort = CSDouble});

    convert_c_fn(seconds_to_minutes, &fn_ctype, type, ass, a, point); 
}
void build_hours_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1,
                                 "timer", (CType){.sort = CSDouble},
                                 (CType){.sort = CSDouble});

    convert_c_fn(seconds_to_hours, &fn_ctype, type, ass, a, point); 
}

void add_time_module(Assembler *ass, Module *platform, RegionAllocator* region) {
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
        .name = string_to_name(mv_string("time")),
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

    typep = mk_opaque_type(pia, "Timer", module, mk_prim_type(pia, Float_64));
    type = (PiType) {.sort = TType};
    name = string_to_name(mv_string("Timer"));
    add_def(module, name, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def_internal(name, module);
    PiType* timer_ty = e->value;

    typep = mk_opaque_type(pia, "Duration", module, mk_prim_type(pia, Float_64));
    type = (PiType) {.sort = TType};
    name = string_to_name(mv_string("Duration"));
    add_def(module, name, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def_internal(name, module);
    PiType* duration_ty = e->value;

    typep = mk_proc_type(pia, 0, timer_ty);
    build_start_timer_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("start-timer"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, timer_ty, duration_ty, pia);
    build_time_elapsed_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("time-elapsed"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, timer_ty, duration_ty, pia);
    build_time_elapsed_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("time-elapsed"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, duration_ty, mk_prim_type(pia, Float_64), pia);
    build_microseconds_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("microseconds"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, duration_ty, mk_prim_type(pia, Float_64), pia);
    build_milliseconds_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("milliseconds"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, duration_ty, mk_prim_type(pia, Float_64), pia);
    build_seconds_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("seconds"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, duration_ty, mk_prim_type(pia, Float_64), pia);
    build_minutes_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("minutes"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, duration_ty, mk_prim_type(pia, Float_64), pia);
    build_hours_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("hours"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
}
