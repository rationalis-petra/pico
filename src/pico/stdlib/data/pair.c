#include "platform/signals.h"
#include "pico/stdlib/helpers.h"
#include "pico/stdlib/data/submodules.h"

void add_pair_module(Target target, Module *data, Allocator *a) {
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(4, a),
    };
    add_import_all(&imports.clauses, a, 1, "core");
    add_import_all(&imports.clauses, a, 1, "num");
    add_import_all(&imports.clauses, a, 1, "extra");
    add_import_all(&imports.clauses, a, 1, "meta");

    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("pair")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(data), NULL, a);
    delete_module_header(header);

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) {
        //panic(doc_to_str(pi_point.error.message, 120, a));
        panic(mv_string("pico error in pair.c"));
    }

    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    // TODO (FEAT): add/implement the following:
    //  - array-free or delete-array
    //  - map
    //  - each

    const char *mk_pair_fn =
        "(def pair all [A B] proc [x y] struct (Pair A B) [._1 x] [._2 y])";
    compile_toplevel(mk_pair_fn, module, target, &point, &pi_point, a);

    Result r = add_module_def(data, string_to_symbol(mv_string("pair")), module);
    if (r.type == Err) panic(r.error_message);
}
