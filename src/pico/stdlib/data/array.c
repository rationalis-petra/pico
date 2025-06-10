#include "platform/signals.h"
#include "pico/stdlib/helpers.h"
#include "pico/stdlib/data/array.h"

void add_array_module(Module *data, Allocator *a) {
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
        .name = string_to_symbol(mv_string("array")),
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

    // TODO (FEAT): add/implement the following:
    //  - array-free or delete-array
    //  - map
    //  - each

    // TODO (BUG): the array should set the allocator
    const char *mk_array_fn = 
        "(def mk-array all [A] proc [capacity len]\n"
        "  (struct (Array A)\n"
        "    [.gpa (name (Ptr Allocator) (num-to-address 0))]\n"
        "    [.capacity capacity]\n"
        "    [.len len]\n"
        "    [.data (malloc (u64.* (size-of A) len))]))";
    compile_toplevel(mk_array_fn, module, &point, &pi_point, a);

    const char *elt_fn =
        "(def elt all [A] proc [idx (arr (Array A))]\n"
        "  (load {A} (num-to-address (u64.+ (u64.* idx (size-of A))\n"
        "                                   (address-to-num arr.data)))))";
    compile_toplevel(elt_fn, module, &point, &pi_point, a);

    const char *aset_fn = 
        "(def aset all [A] proc [idx (val A) (arr (Array A))]\n"
        "  (store {A}\n"
        "    (num-to-address (u64.+ (u64.* idx (size-of A))\n"
        "                           (address-to-num arr.data)))\n"
        "    val))";
    compile_toplevel(aset_fn, module, &point, &pi_point, a);


    Result r = add_module_def(data, string_to_symbol(mv_string("array")), module);
    if (r.type == Err) panic(r.error_message);
}
