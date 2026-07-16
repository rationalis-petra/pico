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
        // TODO: better error reporting here and in adjacent files!
        //panic(doc_to_str(pi_point.error.message, 120, a));
        panic(mv_string("pico error in slice.c"));
    }

    ErrorPoint point;
    if (catch_error(point)) {
        panic(doc_to_str(point.error_message, 120, &ra));
    }

    const char* slice_ty_def = "(def Slice Named Slice Family [Type] Struct [.addr Address] [.len U64])";
    compile_toplevel(slice_ty_def, module, target, &point, &pi_point, region);

    const char* slice_null_fn = "(def null all [A] (name (Slice A) struct [.addr (num-to-address 0)] [.len 0]))";
    compile_toplevel(slice_null_fn, module, target, &point, &pi_point, region);

    const char* slice_new_fn = "(def new all [A] proc [len] (name (Slice A) struct [.addr (memory.alloc (u64.* len (size-of A)))] [.len len]))";
    compile_toplevel(slice_new_fn, module, target, &point, &pi_point, region);

    const char* slice_delte_fn = "(def delete all [A] proc [(slice (Slice A))] "
                               "  (memory.free slice.addr))";
    compile_toplevel(slice_delte_fn, module, target, &point, &pi_point, region);

    const char *slice_elt_fn = "(def elt all [A] proc [(i U64) (slice (Slice A))] seq\n"
                               "  [let! sl-num (address-to-num slice.addr)]"
                               "  [let! elt-idx (u64.+ sl-num (u64.* i (size-of A)))]"
                               "  (load {A} (num-to-address elt-idx)))" ;
    compile_toplevel(slice_elt_fn, module, target, &point, &pi_point, region);

    const char* slice_eset_fn = "(def eset all [A] proc [(i U64) (val A) (slice (Slice A))] seq\n"
                               "  [let! sl-num (address-to-num slice.addr)]"
                               "  [let! elt-idx (u64.+ sl-num (u64.* i (size-of A)))]"
                               "  (store {A} (num-to-address elt-idx) val))";
    compile_toplevel(slice_eset_fn, module, target, &point, &pi_point, region);

    const char *slice_subview =
        "(def subview all [A] proc [(start U64) (end U64) (slice (Slice A))] \n"
        "  (struct (Slice A) [.addr (num-to-address (u64.+ (u64.* (size-of A) start) (address-to-num slice.addr)))] [.len (u64.- end start)]))";
    compile_toplevel(slice_subview, module, target, &point, &pi_point, region);

    const char *slice_copy_fn =
        "(def copy all [A] proc [(slice (Slice A))] seq\n"
        "  [let! out (new {A} slice.len)]"
        "  (loop [for i from 0 below slice.len]\n"
        "    [let! val (elt i slice)]\n"
        "    [let! dest-address (num-to-address (u64.+ (u64.* i (size-of A)) (address-to-num out.addr)))]\n"
        "    (store dest-address val))\n"
        "  out)" ;
    compile_toplevel(slice_copy_fn, module, target, &point, &pi_point, region);

    const char *slice_join_fn =
        "(def join all [A] proc [(x (Slice A)) (y (Slice A))] seq\n"
        "  [let! out (new {A} (u64.+ x.len y.len))]"
        "  (loop [for i from 0 below x.len]\n"
        "    [let! val (elt i x)]\n"
        "    [let! dest-address (num-to-address (u64.+ (u64.* i (size-of A)) (address-to-num out.addr)))]\n"
        "    (store dest-address val))\n"
        "  (loop [for i from 0 below y.len]\n"
        "    [let! val (elt (u64.+ i x.len) y)]\n"
        "    [let! dest-address (num-to-address (u64.+ (u64.* i (size-of A)) (address-to-num out.addr)))]\n"
        "    (store dest-address val))\n"
        "  out)" ;
    compile_toplevel(slice_join_fn, module, target, &point, &pi_point, region);

    Result r = add_module_def(data, string_to_symbol(mv_string("slice")), module);
    if (r.type == Err) panic(r.error_message);
}
