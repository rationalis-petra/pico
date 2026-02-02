#include "platform/signals.h"
#include "platform/terminal/terminal.h"

#include "components/pretty/string_printer.h"

#include "pico/values/ctypes.h"
#include "pico/codegen/codegen.h"
#include "pico/stdlib/platform/submodules.h"

typedef struct {
    uint64_t tag;
    uint64_t size;
} MaybeSize;


static IStream* current_istream;
IStream* set_std_istream(IStream *current) {
    IStream* old = current_istream;
    current_istream = current;
    return old;
}

IStream* get_std_istream() {
    return current_istream; 
}

static OStream* current_ostream;
OStream* set_std_ostream(OStream *current) {
    OStream* old = current_ostream;
    current_ostream = current;
    return old;
}

OStream* get_std_ostream() {
    return current_ostream;
}

uint32_t relic_read_codepoint() {
    uint32_t out;
    next(current_istream, &out);
    return out;
}

void relic_write_codepoint(uint32_t point) {
    write_codepoint(point, current_ostream);
}

void relic_write_string(String str) {
    write_string(str, current_ostream);
}

void relic_set_terminal_bg_colour(uint8_t r, uint8_t g, uint8_t b) {
    FormattedOStream* out = get_formatted_stdout();
    set_bg_colour((Colour){.r  = r, .g = g, .b= b}, out);
}

void build_read_codepoint_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 0, mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}));

    convert_c_fn(relic_read_codepoint, &fn_ctype, type, ass, a, point); 

    delete_c_type(fn_ctype, pia);
}

void build_write_codepoint_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 1,
                               "codepoint", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}),
                               (CType){.sort = CSVoid});

    convert_c_fn(relic_write_codepoint, &fn_ctype, type, ass, a, point); 

    delete_c_type(fn_ctype, pia);
}

void build_set_background_colour_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 3,
                                 "r", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CChar}),
                                 "g", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CChar}),
                                 "b", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CChar}),
                                 (CType){.sort = CSVoid});

    convert_c_fn(relic_set_terminal_bg_colour, &fn_ctype, type, ass, a, point); 

    delete_c_type(fn_ctype, pia);
}

void build_write_string_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1,
                                 "string", mk_string_ctype(pia),
                                 (CType){.sort = CSVoid});

    convert_c_fn(relic_write_string, &fn_ctype, type, ass, a, point); 

    delete_c_type(fn_ctype, pia);
}

void add_terminal_module(Assembler *ass, Module *platform, RegionAllocator* region) {
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
        .name = string_to_symbol(mv_string("terminal")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(platform), NULL);
    delete_module_header(header);
    Symbol sym;

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

    typep = mk_proc_type(pia, 0, mk_prim_type(pia, UInt_32));
    build_read_codepoint_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("read-codepoint"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 1, mk_prim_type(pia, UInt_32), mk_prim_type(pia, Unit));
    build_write_codepoint_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("write-codepoint"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 1, mk_string_type(pia), mk_prim_type(pia, Unit));
    build_write_string_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("write-string"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 3, mk_prim_type(pia, UInt_8), mk_prim_type(pia, UInt_8), mk_prim_type(pia, UInt_8), mk_prim_type(pia, Unit));
    build_set_background_colour_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("set-bg-colour"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    sdelete_u8_array(fn_segments.data);
    sdelete_u8_array(null_segments.data);
    sdelete_u8_array(null_segments.code);

    Result r = add_module_def(platform, string_to_symbol(mv_string("terminal")), module);
    if (r.type == Err) panic(r.error_message);
}
