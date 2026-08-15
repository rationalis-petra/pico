#include "platform/signals.h"

#include "components/pretty/string_printer.h"

#include "pico/stdlib/helpers.h"
#include "pico/stdlib/data/submodules.h"

void add_list_module(Target target, Module *data, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);

    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(8, &ra),
    };
    add_import_all(&imports.clauses, &ra, 2, "lang", "relic");
    add_import_all(&imports.clauses, &ra, 2, "core", "prim");
    add_import_all(&imports.clauses, &ra, 2, "meta", "gen");
    add_import_all(&imports.clauses, &ra, 2, "platform", "memory");
    add_import_all(&imports.clauses, &ra, 2, "data", "pointer");
    add_import(&imports.clauses, &ra, 2, "data", "slice");

    add_import_flags(&imports.clauses, &ra, ImportTypes | ImportInstances,
                     2, seg_name("num"), seg_wild());
    add_import_all(&imports.clauses, &ra, 2, "abs", "equality");
    add_import_all(&imports.clauses, &ra, 2, "abs", "order");
    add_import_all(&imports.clauses, &ra, 2, "abs", "numeric");
    add_import_all(&imports.clauses, &ra, 2, "abs", "lifetime");

    ReExports re_exports = (ReExports) {
        .clauses = mk_import_clause_array(0, &ra),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_name(mv_string("list")),
        .imports = imports,
        .re_exports = re_exports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(data), data);

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) {
        panic(mv_string("pico error in list.c"));
    }

    ErrorPoint point;
    if (catch_error(point)) {
        panic(doc_to_str(point.error_message, 120, &ra));
    }

    // TODO (FEAT): add/implement the following:
    const char *mk_list_type =
        "(def List Named List Family [A] Struct\n"
        "  [.data (slice.Slice A)]\n"
        "  [.len U64]\n"
        "  [.gpa Allocator])\n";
    compile_toplevel(mk_list_type, module, target, &point, &pi_point, region);

    // TODO (BUG): the array should set the allocator
    const char *mk_list_fn = 
        "(def init all [A] proc [len capacity]\n"
        "  (struct (List A)\n"
        "    [.gpa (use current-allocator)]\n"
        "    [.len len ]\n"
        "    [.data slice.new capacity]))";
    compile_toplevel(mk_list_fn, module, target, &point, &pi_point, region);

    const char *mk_null_list_fn = 
        "(def null all [A] proc []\n"
        "  (struct (List A)\n"
        "    [.gpa (use current-allocator)]\n"
        "    [.len 0]\n"
        "    [.data (slice.null)]))";
    compile_toplevel(mk_null_list_fn, module, target, &point, &pi_point, region);
    
    const char *mk_deinit_fn = 
        "(def de-init all [A] proc [(list (List A))]\n"
        "  (bind [current-allocator list.gpa]"
        "    (slice.de-init list.data)))";
    compile_toplevel(mk_deinit_fn, module, target, &point, &pi_point, region);

    const char *elt_fn =
        "(def elt all [A] proc [idx (lst (List A))]\n"
        "  (slice.elt {A} idx lst.data))";
    compile_toplevel(elt_fn, module, target, &point, &pi_point, region);

    const char *eset_fn = 
        "(def eset all [A] proc [idx (val A) (lst (List A))]\n"
        "  (slice.eset idx val lst.data))";
    compile_toplevel(eset_fn, module, target, &point, &pi_point, region);

    const char *each_fn =
        "(def each all [A] proc [(fn (Proc [A] Unit)) (lst (List A))]\n"
        "  (loop [for i from 0 below lst.len]\n"
        "    (seq (fn (elt i lst)) :unit)))";
    compile_toplevel(each_fn, module, target, &point, &pi_point, region);

    const char *map_fn =
        "(def map all [A B] proc [(fn (Proc [A] B)) (lst (List A))]\n"
        "  (let [new-list (init {B} lst.len lst.len)] (seq\n"
        "    (loop [for i from 0 below lst.len]\n"
        "      (eset i (fn (elt i lst)) new-list))\n"
        "      new-list)))";
    compile_toplevel(map_fn, module, target, &point, &pi_point, region);

    const char *list_macro = 
        "(def list macro proc [terms] seq\n"
        "  [let! new-terms init {Syntax} (u64.+ 2 terms.len) (u64.+ 2 terms.len)]\n"
        "  [let! let-terms init {Syntax} 3 3]\n"
        "  [let! arr-terms init {Syntax} 3 3]\n"
        "\n"
        "  [let! ar get-range (elt 0 terms)]\n"
        "\n"
        "  [let! local-sym Syntax:atom ar (Atom:symbol (mk-unique-symbol \"local-list\"))]\n"
        "  [let! eset-sym capture eset]\n"
        "\n"
        "  [let! eset-elt-terms init {Syntax} 4 4]\n"
        "\n"
        "\n"
        "  (eset 0 (capture init) arr-terms)\n"
        "  (eset 1 (Syntax:atom ar (:integral (narrow I64 (u64.- terms.len 1)))) arr-terms)\n"
        "  (eset 2 (Syntax:atom ar (:integral (narrow I64 (u64.- terms.len 1)))) arr-terms)\n"
        "\n"
        "  (eset 0 (Syntax:atom ar (:symbol (mk-symbol \"let!\"))) let-terms)\n"
        "  (eset 1 local-sym let-terms)\n"
        "  (eset 2 (Syntax:node ar :expr arr-terms) let-terms)\n"
        "\n"
        "  (eset 0 (Syntax:atom ar (:symbol (mk-symbol \"seq\"))) new-terms)\n"
        "  (eset 1 (Syntax:node ar :special let-terms) new-terms)\n"
        "\n"
        "  (labels (go-to loop 1)\n"
        "    [loop [i] seq\n"
        "      [let! eset-elt-terms init {Syntax} 4 4]\n"
        "      [let! elt-range get-range (elt i terms)]\n"
        "      [let! idx-node Syntax:atom elt-range (:integral (narrow I64 (u64.- i 1)))]\n"
        "      (eset 0 eset-sym eset-elt-terms)\n"
        "      (eset 1 idx-node eset-elt-terms)\n"
        "      (eset 2 (elt i terms) eset-elt-terms)\n"
        "      (eset 3 local-sym eset-elt-terms)\n"
        "\n"
        "      (eset (u64.+ 1 i) (Syntax:node elt-range :expr eset-elt-terms) new-terms)\n"
        "\n"
        "      (if (u64.= i terms.len) (go-to end) (go-to loop (u64.+ i 1)))]\n"
        "    [end :unit])\n"
        "\n"
        "  (eset (u64.+ 1 terms.len) local-sym new-terms)\n"
        "  \n"
        "  (:right (Syntax:node ar :special new-terms)))\n";
    compile_toplevel(list_macro, module, target, &point, &pi_point, region);

    // Imperative Interface
    const char *list_push_fn =
        "(def push all [A] proc [(val A) (l (Ptr (List A)))] \n"
        "  let [lst (get l)]\n"
        "    (if (u64.< lst.len lst.data.len)\n"
        "      (seq (eset lst.len val lst) (set l (struct lst [.len (u64.+ lst.len 1)])))\n"
        "      (panic {Unit} \"unimplemented: grow on list push\")))";
    compile_toplevel(list_push_fn, module, target, &point, &pi_point, region);

    const char *list_pop_fn =
        "(def pop all [A] proc [(lst (Ptr (List A)))] seq\n"
        "  [let! old (get lst)]\n"
        "  (set lst (struct old [.len (u64.- old.len 1)]))\n"
        "  (elt (u64.- old.len 1) old))\n";
    compile_toplevel(list_pop_fn, module, target, &point, &pi_point, region);

    const char *list_clear_fn =
        "(def clear all [A] proc [(l (Ptr (List A)))] \n"
        "  (set l (struct (get l) [.len 0])))";
    compile_toplevel(list_clear_fn, module, target, &point, &pi_point, region);

    /**
     *  Implementations for Abstractions
     */

    const char *list_eq =
        "(def list-eq instance [A] {(eq (Eq A))} (Eq (List A))\n"
        "  [.= proc [(l1 (List A)) (l2 (List A))] \n"
        "    (labels (seq \n"
        "              (when (u64.!= l1.len l2.len) (go-to not-eq))\n"
        "              (loop [for i from 0 below l1.len] \n"
        "                (when (eq.!= (elt i l1) (elt i l2)) (go-to not-eq)))\n"
        "              :true) \n"
        "      [not-eq :false])]\n"
        "  [.!= proc [(l1 (List A)) (l2 (List A))] \n"
        "    (labels (seq \n"
        "              (when (u64.!= l1.len l2.len) (go-to not-eq))\n"
        "              (loop [for i from 0 below l1.len] \n"
        "                (when (eq.!= (elt i l1) (elt i l2)) (go-to not-eq)))\n"
        "              :false) \n"
        "      [not-eq :true])])\n";
    compile_toplevel(list_eq, module, target, &point, &pi_point, region);

    const char *list_delete =
        "(def list-delete instance [A] {(del (Delete A))} (Delete (List A))\n"
        "  [.delete proc [list] seq\n"
        "    (loop [for i from 0 below list.len]\n"
        "      (del.delete (elt i list)))\n"
        "    (de-init list)])\n";
    compile_toplevel(list_delete, module, target, &point, &pi_point, region);
}
