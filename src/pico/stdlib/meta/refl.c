#include "platform/signals.h"
#include "platform/memory/arena.h"

#include "pico/data/range.h"
#include "pico/syntax/concrete.h"
#include "pico/values/modular.h"
#include "pico/codegen/foreign_adapters.h"
#include "pico/stdlib/core.h"
#include "pico/stdlib/meta/submodules.h"

void add_refl_module(Assembler* ass, Module* base, Allocator* a) {
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, a),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("refl")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(base), NULL, a);
    delete_module_header(header);
    Symbol sym;

    PiType type;
    PiType* typep;
    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    // TODO: we use int64_t as it has the requisite size (8 bytes)
    // for pico values: currently don't support non-64 bit values 
    TermFormer former;
    // TermFormer former;
    type.sort = TPrim;
    type.prim = TFormer;

    Segments null_segments = (Segments) {
        .code = mk_u8_array(0, a),
        .data = mk_u8_array(0, a),
    };

    // Now that we have setup appropriately, override the allocator
    Allocator arena = mk_arena_allocator(4096, a);
    a = &arena;

    // ------------------------------------------------------------------------
    // Term Formers
    // ------------------------------------------------------------------------

    // ------------------------------------------------------------------------
    // Types 
    // ------------------------------------------------------------------------

    // ------------------------------------------------------------------------
    // Values 
    // ------------------------------------------------------------------------

    type = (PiType) {
        .sort = TKind,
        .kind.nargs = 0,
    };

    Segments fn_segments = (Segments) {.data = mk_u8_array(0, a),};
    Segments prepped;

    add_module_def(base, string_to_symbol(mv_string("refl")), module);

    sdelete_u8_array(null_segments.code);
    sdelete_u8_array(null_segments.data);
    // Note: we do NOT delete the 'fn_segments.code' because it is the
    // assembler, and needs to be used later!
    sdelete_u8_array(fn_segments.data);
    release_arena_allocator(arena);
}
