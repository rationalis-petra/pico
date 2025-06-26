#include "platform/signals.h"
#include "pico/stdlib/helpers.h"
#include "pico/stdlib/data/list.h"

void add_list_module(Module *data, Allocator *a) {
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
        .name = string_to_symbol(mv_string("list")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(data), NULL, a);
    delete_module_header(header);

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) {
        //panic(doc_to_str(pi_point.error.message, 120, a));
        panic(mv_string("pico error in list.c"));
    }

    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    // TODO (FEAT): add/implement the following:
    //  - list-free or delete-list
    //  - map
    //  - each

    // TODO (BUG): the array should set the allocator
    const char *mk_list_fn = 
        "(def mk-list all [A] proc [capacity len]\n"
        "  (struct (List A)\n"
        "    [.gpa (name (Ptr Allocator) (num-to-address 0))]\n"
        "    [.capacity capacity]\n"
        "    [.len len]\n"
        "    [.data (malloc (u64.* (size-of A) len))]))";
    compile_toplevel(mk_list_fn, module, &point, &pi_point, a);

    const char *elt_fn =
        "(def elt all [A] proc [idx (arr (List A))]\n"
        "  (load {A} (num-to-address (u64.+ (u64.* idx (size-of A))\n"
        "                                   (address-to-num arr.data)))))";
    compile_toplevel(elt_fn, module, &point, &pi_point, a);

    const char *eset_fn = 
        "(def eset all [A] proc [idx (val A) (arr (List A))]\n"
        "  (store {A}\n"
        "    (num-to-address (u64.+ (u64.* idx (size-of A))\n"
        "                           (address-to-num arr.data)))\n"
        "    val))";
    compile_toplevel(eset_fn, module, &point, &pi_point, a);

    const char *each_fn = 
        "(def each all [A B] proc [(fn (Proc [A] B)) (lst (List A))]\n"
        "  (let [new-list (mk-list {B} lst.len lst.len)]\n"
        "    (loop [for i from 0 below lst.len]\n"
        "      (eset i (fn (elt i lst)) new-list))))";
    compile_toplevel(each_fn, module, &point, &pi_point, a);

    const char *list_macro = 
        "(def list macro proc [terms] seq\n"
        "  [let! new-terms (mk-list {Syntax} (u64.+ 2 terms.len) (u64.+ 2 terms.len))\n"
        "        let-terms (mk-list {Syntax} 3 3)\n"
        "        arr-terms (mk-list {Syntax} 3 3)\n"
        "\n"
        "        ar (get-range (elt 0 terms))\n"
        "\n"
        "        local-sym (Syntax:atom ar (Atom:symbol (mk-unique-symbol \"local-list\")))\n"
        "        eset-sym (Syntax:atom ar (:symbol (mk-symbol \"eset\")))\n"
        "\n"
        "        eset-elt-terms (mk-list {Syntax} 4 4)\n"
        "\n"
        "        ]\n"
        "\n"
        "  (eset 0 (Syntax:atom ar (:symbol (mk-symbol \"mk-list\"))) arr-terms)\n"
        "  (eset 1 (Syntax:atom ar (:integral (narrow (u64.- terms.len 1) I64))) arr-terms)\n"
        "  (eset 2 (Syntax:atom ar (:integral (narrow (u64.- terms.len 1) I64))) arr-terms)\n"
        "\n"
        "  (eset 0 (Syntax:atom ar (:symbol (mk-symbol \"let!\"))) let-terms)\n"
        "  (eset 1 local-sym let-terms)\n"
        "  (eset 2 (Syntax:node ar :none arr-terms) let-terms)\n"
        "\n"
        "  (eset 0 (Syntax:atom ar (:symbol (mk-symbol \"seq\"))) new-terms)\n"
        "  (eset 1 (Syntax:node ar :special let-terms) new-terms)\n"
        "\n"
        "  (labels (go-to loop 1)\n"
        "    [loop [i] seq\n"
        "      [let! eset-elt-terms (mk-list {Syntax} 4 4)\n"
        "            elt-range (get-range (elt i terms))\n"
        "            idx-node (Syntax:atom elt-range (:integral (narrow (u64.- i 1) I64)))]\n"
        "      (eset 0 eset-sym eset-elt-terms)\n"
        "      (eset 1 idx-node eset-elt-terms)\n"
        "      (eset 2 (elt i terms) eset-elt-terms)\n"
        "      (eset 3 local-sym eset-elt-terms)\n"
        "\n"
        "      (eset (u64.+ 1 i) (Syntax:node elt-range :none eset-elt-terms) new-terms)\n"
        "\n"
        "      (if (u64.= i terms.len) (go-to end) (go-to loop (u64.+ i 1)))]\n"
        "    [end :unit])\n"
        "\n"
        "  (eset (u64.+ 1 terms.len) local-sym new-terms)\n"
        "  \n"
        "  (:right (Syntax:node ar :special new-terms)))\n";
    compile_toplevel(list_macro, module, &point, &pi_point, a);

    Result r = add_module_def(data, string_to_symbol(mv_string("list")), module);
    if (r.type == Err) panic(r.error_message);
}
