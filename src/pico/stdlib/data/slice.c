#include "platform/signals.h"

#include "components/pretty/string_printer.h"

#include "pico/stdlib/helpers.h"
#include "pico/stdlib/data/submodules.h"

void add_slice_module(Target target, Module *data, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);

    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(8, &ra),
    };
    add_import_all(&imports.clauses, &ra, 2, "lang", "relic");
    add_import_all(&imports.clauses, &ra, 2, "core", "prim");
    add_import_all(&imports.clauses, &ra, 1, "meta");
    add_import(&imports.clauses, &ra, 2, "platform", "memory");

    add_import_flags(&imports.clauses, &ra, ImportTypes | ImportInstances,
                     2, seg_name("num"), seg_wild());
    add_import_all(&imports.clauses, &ra, 2, "abs", "equality");
    add_import_all(&imports.clauses, &ra, 2, "abs", "order");
    add_import_all(&imports.clauses, &ra, 2, "abs", "numeric");
    add_import_all(&imports.clauses, &ra, 2, "abs", "sequence");

    ReExports re_exports = (ReExports) {
        .clauses = mk_import_clause_array(0, &ra),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_name(mv_string("slice")),
        .imports = imports,
        .re_exports = re_exports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(data), data);
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

    const char* slice_null_fn = "(def null all [A] (name (Slice A) struct [.addr (address.num-to-address 0)] [.len (is U64 0)]))";
    compile_toplevel(slice_null_fn, module, target, &point, &pi_point, region);

    const char* slice_new_fn = "(def new all [A] proc [(len U64)] (name (Slice A) struct [.addr (memory.alloc (u64.* len (size-of A)))] [.len len]))";
    compile_toplevel(slice_new_fn, module, target, &point, &pi_point, region);

    const char* slice_delte_fn = "(def de-init all [A] proc [(slice (Slice A))] "
                                 "  (memory.free slice.addr))";
    compile_toplevel(slice_delte_fn, module, target, &point, &pi_point, region);

    const char *slice_elt_fn = "(def elt all [A] proc [(i U64) (slice (Slice A))] seq\n"
                               "  [let! sl-num (address.address-to-num slice.addr)]"
                               "  [let! elt-idx (u64.+ sl-num (u64.* i (size-of A)))]"
                               "  (address.load {A} (address.num-to-address elt-idx)))" ;
    compile_toplevel(slice_elt_fn, module, target, &point, &pi_point, region);

    const char* slice_eset_fn = "(def eset all [A] proc [(i U64) (val A) (slice (Slice A))] seq\n"
                               "  [let! sl-num (address.address-to-num slice.addr)]"
                               "  [let! elt-idx (u64.+ sl-num (u64.* i (size-of A)))]"
                               "  (address.store {A} (address.num-to-address elt-idx) val))";
    compile_toplevel(slice_eset_fn, module, target, &point, &pi_point, region);

    const char *slice_subview =
        "(def subview all [A] proc [(start U64) (end U64) (slice (Slice A))] \n"
        "  (struct (Slice A) [.addr (address.num-to-address (u64.+ (u64.* (size-of A) start) (address.address-to-num slice.addr)))] [.len (u64.- end start)]))";
    compile_toplevel(slice_subview, module, target, &point, &pi_point, region);

    const char *slice_copy_fn =
        "(def copy all [A] proc [(slice (Slice A))] seq\n"
        "  [let! out (new {A} slice.len)]"
        "  (loop [for i from 0 below slice.len]\n"
        "    [let! val (elt i slice)]\n"
        "    [let! dest-address (address.num-to-address (u64.+ (u64.* i (size-of A)) (address.address-to-num out.addr)))]\n"
        "    (address.store dest-address val))\n"
        "  out)" ;
    compile_toplevel(slice_copy_fn, module, target, &point, &pi_point, region);

    const char *slice_copy_to_fn =
        "(def copy-to all [A] proc [(dest (Slice A)) (src (Slice A))] seq\n"
        "  [let! copy-len (min dest.len src.len)]"
        "  (loop [for i from 0 below copy-len]\n"
        "    [let! val (elt i src)]\n"
        "    [let! dest-address (address.num-to-address (u64.+ (u64.* i (size-of A)) (address.address-to-num dest.addr)))]\n"
        "    (address.store dest-address val))\n"
        "  Unit)" ;
    compile_toplevel(slice_copy_to_fn, module, target, &point, &pi_point, region);

    const char *slice_join_fn =
        "(def join all [A] proc [(x (Slice A)) (y (Slice A))] seq\n"
        "  [let! out (new {A} (u64.+ x.len y.len))]"
        "  (loop [for i from 0 below x.len]\n"
        "    [let! val (elt i x)]\n"
        "    [let! dest-address (address.num-to-address (u64.+ (u64.* i (size-of A)) (address.address-to-num out.addr)))]\n"
        "    (address.store dest-address val))\n"
        "  (loop [for i from 0 below y.len]\n"
        "    [let! val (elt (u64.+ i x.len) y)]\n"
        "    [let! dest-address (address.num-to-address (u64.+ (u64.* i (size-of A)) (address.address-to-num out.addr)))]\n"
        "    (address.store dest-address val))\n"
        "  out)" ;
    compile_toplevel(slice_join_fn, module, target, &point, &pi_point, region);

    /**
     *  Implementations for Abstractions: 
     */
    const char *slice_seq =
        "(def slice-seq instance (Seq Slice)\n"
        "  [.elt all [A] proc [(idx U64) (slice (Slice A))] \n"
        "    if (u64.< idx slice.len)\n"
        "       (:some (elt {A} idx slice))"
        "       :none])\n";
    compile_toplevel(slice_seq, module, target, &point, &pi_point, region);
}
