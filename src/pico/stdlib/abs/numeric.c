#include "platform/signals.h"
#include "pico/stdlib/abs/numeric.h"

#include "pico/stdlib/helpers.h"

void add_numeric_module(Module *data, Package* base, Allocator *a) {
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(4, a),
    };
    add_import_all(&imports.clauses, a, 1, "core");
    /* add_import_all(&imports.clauses, a, 1, "num"); */
    /* add_import_all(&imports.clauses, a, 1, "extra"); */
    /* add_import_all(&imports.clauses, a, 1, "meta"); */

    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("list")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, base, NULL, a);
    delete_module_header(header);

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) {
        //panic(doc_to_str(pi_point.error.message, 120, a));
        panic(mv_string("pico error in abs.c"));
    }

    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    // TODO (FEAT): add/implement the following:
    //  - list-free or delete-list
    //  - push/pop (using alter)

    // TODO (BUG): the array should set the allocator
    const char* num_trait = 
        "(def Num Trait [A]"
        "  [.zero A]"
        "  [.one A]"
        "  [.+ Proc [A A] A]"
        "  [.- Proc [A A] A]"
        "  [.* Proc [A A] A]"
        "  [./ Proc [A A] A])\n";
    compile_toplevel(num_trait, module, &point, &pi_point, a);

    Result r = add_module_def(data, string_to_symbol(mv_string("list")), module);
    if (r.type == Err) panic(r.error_message);
}
