#include "platform/signals.h"

#include "components/pretty/string_printer.h"

#include "pico/stdlib/helpers.h"
#include "pico/stdlib/data/submodules.h"

void add_slice_module(Target target, Module *data, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);

    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(4, &ra),
    };
    add_import_all(&imports.clauses, &ra, 1, "core");
    add_import_all(&imports.clauses, &ra, 1, "num");
    add_import_all(&imports.clauses, &ra, 1, "extra");
    add_import_all(&imports.clauses, &ra, 1, "meta");
    add_import(&imports.clauses, &ra, 2, "platform", "memory");

    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("slice")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(data), NULL);
    delete_module_header(header);

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) {
        //panic(doc_to_str(pi_point.error.message, 120, a));
        panic(mv_string("pico error in pair.c"));
    }

    ErrorPoint point;
    if (catch_error(point)) {
        panic(doc_to_str(point.error_message, 120, &ra));
    }

    const char* slice_ty_def = "(def Slice Named Slice Family [Type] Address)";
    compile_toplevel(slice_ty_def, module, target, &point, &pi_point, region);

    const char* slice_null_fn = "(def null all [A] (name (Slice A) (num-to-address 0)))";
    compile_toplevel(slice_null_fn, module, target, &point, &pi_point, region);

    const char* slice_new_fn = "(def new all [A] proc [len] (name (Slice A) (memory.alloc (u64.* len (size-of A)))))";
    compile_toplevel(slice_new_fn, module, target, &point, &pi_point, region);

    const char* slice_delte_fn = "(def delete all [A] proc [(slice (Slice A))] "
                               "  (memory.free (unname slice)))";
    compile_toplevel(slice_delte_fn, module, target, &point, &pi_point, region);

    const char *slice_elt_fn = "(def elt all [A] proc [(i U64) (slice (Slice A))] seq\n"
                               "  [let! sl-num (address-to-num (unname slice))]"
                               "  [let! elt-idx (u64.+ sl-num (u64.* i (size-of A)))]"
                               "  (load {A} (num-to-address elt-idx)))" ;
    compile_toplevel(slice_elt_fn, module, target, &point, &pi_point, region);

    const char* slice_eset_fn = "(def eset all [A] proc [(i U64) (val A) (slice (Slice A))] seq\n"
                               "  [let! sl-num (address-to-num (unname slice))]"
                               "  [let! elt-idx (u64.+ sl-num (u64.* i (size-of A)))]"
                               "  (store {A} (num-to-address elt-idx) val))";
    compile_toplevel(slice_eset_fn, module, target, &point, &pi_point, region);

    Result r = add_module_def(data, string_to_symbol(mv_string("slice")), module);
    if (r.type == Err) panic(r.error_message);
}
