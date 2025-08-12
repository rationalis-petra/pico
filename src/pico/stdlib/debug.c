#include "platform/machine_info.h"
#include "platform/signals.h"
#include "platform/memory/arena.h"

#include "pico/codegen/foreign_adapters.h"
#include "pico/stdlib/debug.h"

#if OS_FAMILY == UNIX
#include <signal.h>
#endif

void debug_break() {  
#if OS_FAMILY == WINDOWS
    __debugbreak();
#elif OS_FAMILY == UNIX
    raise(SIGTRAP);
#endif
}

void build_debug_break_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 0, (CType){.sort = CSVoid});

    convert_c_fn(debug_break, &fn_ctype, type, ass, a, point); 
}

void add_debug_module(Target target, Package* base, Allocator* a) {
    Allocator arena = mk_arena_allocator(8096, a);
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, a),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("debug")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, base, NULL, a);
    delete_module_header(header);
    Symbol sym;

    PiType type;
    //PiType* typep;
    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    // TODO: we use int64_t as it has the requisite size (8 bytes)
    // for pico values: currently don't support non-64 bit values 
    TermFormer former;
    //TermFormer former;
    type.sort = TPrim;
    type.prim = TFormer;

    Segments null_segments = (Segments) {
        .code = mk_u8_array(0, a),
        .data = mk_u8_array(0, a),
    };

    // ------------------------------------------------------------------------
    // Term Formers
    // ------------------------------------------------------------------------
    former = FDescribe;
    sym = string_to_symbol(mv_string("describe"));
    add_def(module, sym, type, &former, null_segments, NULL);

    // ------------------------------------------------------------------------
    // Functions
    // ------------------------------------------------------------------------

    Segments fn_segments = (Segments) {
        .code = get_instructions(target.code_aux),
        .data = *target.data_aux,
    };
    Segments prepped;

    PiType* typep;
    typep = mk_proc_type(&arena, 0, mk_prim_type(&arena, Unit));
    build_debug_break_fn(typep, target.target, &arena, &point);
    sym = string_to_symbol(mv_string("debug-break"));
    fn_segments.code = get_instructions(target.target);
    prepped = prep_target(module, fn_segments, target.target, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(target.target);

    add_module(string_to_symbol(mv_string("debug")), module, base);

    release_arena_allocator(arena);
    sdelete_u8_array(null_segments.code);
    sdelete_u8_array(null_segments.data);
    // Note: we do NOT delete the 'fn_segments.code' because it is the
    //       assembler, and needs to be used later!
}

