#include "platform/signals.h"
#include "pico/stdlib/abs/numeric.h"

#include "pico/stdlib/helpers.h"

void add_numeric_module(Module *data, Package* base, Allocator *a) {
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(4, a),
    };
    add_import_all(&imports.clauses, a, 1, "core");
    add_import_all(&imports.clauses, a, 1, "num");
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

    const char* num_trait = 
        "(def Num Trait [A]"
        "  [.zero A]"
        "  [.one A]"
        "  [.+ Proc [A A] A]"
        "  [.- Proc [A A] A]"
        "  [.* Proc [A A] A]"
        "  [./ Proc [A A] A])\n";
    compile_toplevel(num_trait, module, &point, &pi_point, a);

    const char* add_fn = 
        "(def + all [A] proc {(n (Num A))} [(x A) (y A)] n.+ x y)";
    compile_toplevel(add_fn, module, &point, &pi_point, a);

    const char* sub_fn = 
        "(def - all [A] proc {(n (Num A))} [(x A) (y A)] n.- x y)";
    compile_toplevel(sub_fn, module, &point, &pi_point, a);

    const char* mul_fn = 
        "(def * all [A] proc {(n (Num A))} [(x A) (y A)] n.* x y)";
    compile_toplevel(mul_fn, module, &point, &pi_point, a);

    const char* div_fn = 
        "(def / all [A] proc {(n (Num A))} [(x A) (y A)] n./ x y)";
    compile_toplevel(div_fn, module, &point, &pi_point, a);

    const char* num_i64_trait = 
        "(def i64-num instance (Num I64)"
        "  [.zero 0]"
        "  [.one 1]"
        "  [.+ i64.+]"
        "  [.- i64.-]"
        "  [.* i64.*]"
        "  [./ i64./])\n";
    compile_toplevel(num_i64_trait, module, &point, &pi_point, a);

    const char* num_f64_trait =
        "(def f64-num instance (Num F64)"
        "  [.zero 0.0]"
        "  [.one 1.0]"
        "  [.+ f64.+]"
        "  [.- f64.-]"
        "  [.* f64.*]"
        "  [./ f64./])\n";
    compile_toplevel(num_f64_trait, module, &point, &pi_point, a);

    Result r = add_module_def(data, string_to_symbol(mv_string("numeric")), module);
    if (r.type == Err) panic(r.error_message);
}
