#include "platform/signals.h"

#include "components/pretty/string_printer.h"

#include "pico/stdlib/helpers.h"
#include "pico/stdlib/data/submodules.h"

void add_pointer_module(Target target, Module *data, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);

    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(3, &ra),
    };
    add_import_all(&imports.clauses, &ra, 1, "core");
    add_import_all(&imports.clauses, &ra, 1, "num");
    add_import_all(&imports.clauses, &ra, 1, "extra");
    add_import_all(&imports.clauses, &ra, 1, "platform");

    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("pointer")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(data), NULL);

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) {
        //panic(doc_to_str(pi_point.error.message, 120, a));
        panic(mv_string("pi error in ptr.c"));
    }

    ErrorPoint point;
    if (catch_error(point)) {
        panic(doc_to_str(point.error_message, 120, &ra));
    }

    const char* null_fn = "(def null all [A] (name (Ptr A) (num-to-address 0)))";
    compile_toplevel(null_fn, module, target, &point, &pi_point, region);

    const char* get_fn = "(def get all [A] proc [(p (Ptr A))] (load {A} (unname p)))";
    compile_toplevel(get_fn, module, target, &point, &pi_point, region);

    const char* set_fn = "(def set all [A] proc [(p (Ptr A)) (val A)] (store (unname p) val))";
    compile_toplevel(set_fn, module, target, &point, &pi_point, region);

    const char *new_fn = 
        "(def new all [A] proc [(v A)] seq\n"
        "  [let! p (name (Ptr A) (memory.alloc (size-of A)))]\n"
        "  (set p v)\n"
        "  p)";
    compile_toplevel(new_fn, module,target,  &point, &pi_point, region);

    const char *delete_fn = 
        "(def delete all [A] proc [(ptr (Ptr A))] \n"
        "  (memory.free (unname ptr)))";
    compile_toplevel(delete_fn, module,target,  &point, &pi_point, region);

    Result r = add_module_def(data, string_to_symbol(mv_string("pointer")), module);
    if (r.type == Err) panic(r.error_message);
}
