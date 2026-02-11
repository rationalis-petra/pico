#include "platform/signals.h"
#include "platform/window/window.h"

#include "components/pretty/string_printer.h"

#include "pico/data/client/allocator.h"
#include "pico/data/client/meta/list_header.h"
#include "pico/data/client/meta/list_impl.h"
#include "pico/values/ctypes.h"
#include "pico/codegen/codegen.h"
#include "pico/stdlib/core.h"
#include "pico/stdlib/platform/submodules.h"

#ifdef WINDOW_SYSTEM

static PiType* window_ty;
PiType* get_window_ty() { return window_ty; };
static PiType* window_message_ty;
static PiType* key_ty;

PICO_LIST_HEADER(WinMessage, msg, WinMessage);
PICO_LIST_COMMON_IMPL(WinMessage, msg, WinMessage);

void build_create_window_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 3, "name", mk_string_ctype(pia),
                                       "width", mk_primint_ctype((CPrimInt){.prim = CInt, .is_signed = Unspecified}),
                                       "height", mk_primint_ctype((CPrimInt){.prim = CInt, .is_signed = Unspecified}),
                                    mk_voidptr_ctype(pia));

    convert_c_fn(pl_create_window, &fn_ctype, type, ass, a, point); 

    delete_c_type(fn_ctype, pia);
}

void build_destroy_window_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "window", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});

    convert_c_fn(pl_destroy_window, &fn_ctype, type, ass, a, point); 

    delete_c_type(fn_ctype, pia);
}

void build_window_should_close_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "window", mk_voidptr_ctype(pia),
                                 mk_primint_ctype((CPrimInt){.prim = CChar, .is_signed = Unsigned}));

    convert_c_fn(pl_window_should_close, &fn_ctype, type, ass, a, point);

    delete_c_type(fn_ctype, pia);
}

WinMessagePiList relic_poll_events(PlWindow* window) {
    PiAllocator pia = get_std_current_allocator();
    Allocator a = convert_to_callocator(&pia);
    WinMessageArray arr = pl_poll_events(window, &a);
    return (WinMessagePiList) {
        .data = arr.data,
        .len = arr.len,
        .size = arr.size,
        .gpa = pia,
    };
}

void build_poll_events_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "window", mk_voidptr_ctype(pia), mk_list_ctype(pia));

    convert_c_fn(relic_poll_events, &fn_ctype, type, ass, a, point);

    delete_c_type(fn_ctype, pia);
}

void add_window_module(Assembler *ass, Module *platform, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    PiAllocator pico_allocator = convert_to_pallocator(&ra);
    PiAllocator* pia = &pico_allocator;

    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, &ra),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("window")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(platform), NULL);
    delete_module_header(header);
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

    // The window type is simple an opaque pointer (address)
    typep = mk_opaque_type(pia, module, mk_named_type(pia, "Window", mk_prim_type(pia, Address)));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("Window"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    window_ty = e->value;

    // Message Type

    typep = mk_enum_type(
        pia, 58, "a", 0, "b", 0, "c", 0, "d", 0, "e", 0, "f", 0, "g", 0, "h", 0,
        "i", 0, "j", 0, "k", 0, "l", 0, "m", 0, "n", 0, "o", 0, "p", 0, "q", 0,
        "r", 0, "s", 0, "t", 0, "u", 0, "v", 0, "w", 0, "x", 0, "y", 0, "z", 0,

        "one", 0, "two", 0, "three", 0, "four", 0, "five", 0, "six", 0, "seven",
        0, "eight", 0, "nine", 0, "zero", 0,

        "exclamation", 0, "at", 0, "hash",
        0, "dollar", 0, "percent", 0, "caret", 0, "ampersand", 0, "asterisk", 0,
        "lparen", 0, "rparen", 0, "minus", 0, "plus", 0,

        "rbrace", 0, "lbrace", 0, "colon", 0, "semicolon", 0, "comma", 0, "dot", 
        0, "query", 0,

        "space", 0, "enter", 0, "backspace", 0);
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("Key"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    key_ty = e->value;

    typep = mk_enum_type(pia, 2,
                         "resize", 2, mk_prim_type(pia, UInt_32), mk_prim_type(pia, UInt_32),
                         "key-event", 3, key_ty, mk_prim_type(pia, UInt_16), mk_prim_type(pia, Bool));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("Message"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    window_message_ty = e->value;

    typep = mk_proc_type(pia, 3, mk_string_type(pia), mk_prim_type(pia, Int_32), mk_prim_type(pia, Int_32), copy_pi_type_p(window_ty, pia));
    build_create_window_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("create-window"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(window_ty, pia), mk_prim_type(pia, Unit));
    build_destroy_window_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("destroy-window"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(window_ty, pia), mk_prim_type(pia, Bool));
    build_window_should_close_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("should-close"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    //typep = mk_proc_type(a, 0, copy_pi_type_p(window_message_ty, a));
    typep = mk_proc_type(pia, 1,  copy_pi_type_p(window_ty, pia), mk_app_type(pia, get_list_type(), window_message_ty));
    build_poll_events_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("poll-events"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    sdelete_u8_array(fn_segments.data);
    sdelete_u8_array(null_segments.data);
    sdelete_u8_array(null_segments.code);

    Result r = add_module_def(platform, string_to_symbol(mv_string("window")), module);
    if (r.type == Err) panic(r.error_message);
}

#endif
