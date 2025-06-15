#include "platform/signals.h"
#include "pico/stdlib/platform/window.h"

bool relic_init_windowsystem() {
}

/* void build_init_windowsystem_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) { */
/*     CType fn_ctype = mk_fn_ctype(a, 1, "nodes", mk_array_ctype(a), mk_macro_result_ctype(a)); */

/*     convert_c_fn(relic_init_windowsystem, &fn_ctype, type, ass, a, point);  */
/* } */

void add_window_module(Assembler *ass, Module *platform, Allocator *a) {
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, a),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("window")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(platform), NULL, a);
    delete_module_header(header);
    Symbol sym;

    PiType* typep;
    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    

    Result r = add_module_def(platform, string_to_symbol(mv_string("window")), module);
    if (r.type == Err) panic(r.error_message);
}
