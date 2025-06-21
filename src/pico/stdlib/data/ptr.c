#include "platform/signals.h"
#include "pretty/string_printer.h"
#include "pico/stdlib/helpers.h"
#include "pico/stdlib/data/ptr.h"

void add_ptr_module(Module *data, Allocator *a) {
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(3, a),
    };
    add_import_all(&imports.clauses, a, 1, "core");
    add_import_all(&imports.clauses, a, 1, "num");
    add_import_all(&imports.clauses, a, 1, "extra");

    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("ptr")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(data), NULL, a);
    delete_module_header(header);

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) {
        //panic(doc_to_str(pi_point.error.message, 120, a));
        panic(mv_string("pi error in ptr.c"));
    }

    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    const char* null_fn = "(def null all [A] (name (Ptr A) (num-to-address 0)))";
    compile_toplevel(null_fn, module, &point, &pi_point, a);

    const char* get_fn = "(def get all [A] proc [(p (Ptr A))] (load {A} (unname p)))";
    compile_toplevel(get_fn, module, &point, &pi_point, a);

    const char* set_fn = "(def set all [A] proc [(p (Ptr A)) (val A)] (store (unname p) val))";
    compile_toplevel(set_fn, module, &point, &pi_point, a);

    const char *new_fn = 
        "(def new all [A] proc [(v A)] seq\n"
        "  [let! p (name (Ptr A) (malloc (size-of A)))]\n"
        "  (set p v)\n"
        "  p)";
    compile_toplevel(new_fn, module, &point, &pi_point, a);

    Result r = add_module_def(data, string_to_symbol(mv_string("pointer")), module);
    if (r.type == Err) panic(r.error_message);
}
