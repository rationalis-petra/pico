#include "platform/signals.h"

#include "components/pretty/string_printer.h"

#include "pico/stdlib/helpers.h"
#include "pico/stdlib/data/submodules.h"

void add_list_module(Target target, Module *data, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);

    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(4, &ra),
    };
    add_import_all(&imports.clauses, &ra, 1, "core");
    add_import_all(&imports.clauses, &ra, 1, "num");
    add_import_all(&imports.clauses, &ra, 1, "extra");
    add_import_all(&imports.clauses, &ra, 2, "meta", "gen");
    add_import_all(&imports.clauses, &ra, 2, "platform", "memory");

    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("list")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(data), NULL);

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) {
        panic(mv_string("pico error in list.c"));
    }

    ErrorPoint point;
    if (catch_error(point)) {
        panic(doc_to_str(point.error_message, 120, &ra));
    }

    // TODO (FEAT): add/implement the following:
    //  - list-free or delete-list

    const char *mk_list_type =
        "(def List Named List Family [A] Struct\n"
        "  [.data Address]\n"
        "  [.len U64]\n"
        "  [.capacity U64]\n"
        "  [.gpa Allocator])\n";
    compile_toplevel(mk_list_type, module, target, &point, &pi_point, region);

    // TODO (BUG): the array should set the allocator
    const char *mk_list_fn = 
        "(def mk-list all [A] proc [len capacity]\n"
        "  (struct (List A)\n"
        "    [.gpa (use current-allocator)]\n"
        "    [.capacity capacity]\n"
        "    [.len len]\n"
        "    [.data (alloc (u64.* (size-of A) capacity))]))";
    compile_toplevel(mk_list_fn, module, target, &point, &pi_point, region);
    
    // TODO (BUG): use list allocator
    const char *mk_free_fn = 
        "(def free-list all [A] proc [(list (List A))]\n"
        "  (free list.data))";
    compile_toplevel(mk_free_fn, module, target, &point, &pi_point, region);

    const char *elt_fn =
        "(def elt all [A] proc [idx (arr (List A))]\n"
        "  (load {A} (num-to-address (u64.+ (u64.* idx (size-of A))\n"
        "                                   (address-to-num arr.data)))))";
    compile_toplevel(elt_fn, module, target, &point, &pi_point, region);

    const char *eset_fn = 
        "(def eset all [A] proc [idx (val A) (arr (List A))]\n"
        "  (store {A}\n"
        "    (num-to-address (u64.+ (u64.* idx (size-of A))\n"
        "                           (address-to-num arr.data)))\n"
        "    val))";
    compile_toplevel(eset_fn, module, target, &point, &pi_point, region);

    const char *each_fn =
        "(def each all [A] proc [(fn (Proc [A] Unit)) (lst (List A))]\n"
        "  (loop [for i from 0 below lst.len]\n"
        "    (seq (fn (elt i lst)) :unit)))";
    compile_toplevel(each_fn, module, target, &point, &pi_point, region);

    const char *map_fn =
        "(def map all [A B] proc [(fn (Proc [A] B)) (lst (List A))]\n"
        "  (let [new-list (mk-list {B} lst.len lst.len)] (seq\n"
        "    (loop [for i from 0 below lst.len]\n"
        "      (eset i (fn (elt i lst)) new-list))\n"
        "      new-list)))";
    compile_toplevel(map_fn, module, target, &point, &pi_point, region);

    const char *list_macro = 
        "(def list macro proc [terms] seq\n"
        "  [let! new-terms mk-list {Syntax} (u64.+ 2 terms.len) (u64.+ 2 terms.len)]\n"
        "  [let! let-terms mk-list {Syntax} 3 3]\n"
        "  [let! arr-terms mk-list {Syntax} 3 3]\n"
        "\n"
        "  [let! ar get-range (elt 0 terms)]\n"
        "\n"
        "  [let! local-sym Syntax:atom ar (Atom:symbol (mk-unique-symbol \"local-list\"))]\n"
        "  [let! eset-sym capture eset]\n"
        "\n"
        "  [let! eset-elt-terms mk-list {Syntax} 4 4]\n"
        "\n"
        "\n"
        "  (eset 0 (capture mk-list) arr-terms)\n"
        "  (eset 1 (Syntax:atom ar (:integral (narrow (u64.- terms.len 1) I64))) arr-terms)\n"
        "  (eset 2 (Syntax:atom ar (:integral (narrow (u64.- terms.len 1) I64))) arr-terms)\n"
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
        "      [let! eset-elt-terms mk-list {Syntax} 4 4]\n"
        "      [let! elt-range get-range (elt i terms)]\n"
        "      [let! idx-node Syntax:atom elt-range (:integral (narrow (u64.- i 1) I64))]\n"
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
        "(def push all [A] proc [(val A) (l (Dynamic List A))] \n"
        "  let [lst (use l)]\n"
        "    (if (u64.< lst.len lst.capacity)\n"
        "      (seq (eset lst.len val lst) (set l (struct lst [.len (u64.+ lst.len 1)])))\n"
        "      (panic {Unit} \"unimplemented\")))";
    compile_toplevel(list_push_fn, module, target, &point, &pi_point, region);

    const char *list_pop_fn =
        "(def pop all [A] proc [(lst (Dynamic List A))] seq\n"
        "  [let! old (use lst)]\n"
        "  (set lst (struct old [.len (u64.- old.len 1)]))\n"
        "  (elt (u64.- old.len 1) old))\n";
    compile_toplevel(list_pop_fn, module, target, &point, &pi_point, region);

    Result r = add_module_def(data, string_to_symbol(mv_string("list")), module);
    if (r.type == Err) panic(r.error_message);
}
