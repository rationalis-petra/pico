#include "platform/signals.h"

#include "components/pretty/string_printer.h"

#include "pico/stdlib/abs/submodules.h"
#include "pico/stdlib/helpers.h"

void add_order_module(Target target, Module *abs, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(4, &ra),
    };
    add_import_all(&imports.clauses, &ra, 2, "lang", "relic");
    add_import_all(&imports.clauses, &ra, 1, "num");
    add_import_all(&imports.clauses, &ra, 1, "data");
    add_import_all(&imports.clauses, &ra, 2, "abs", "equality");

    ReExports re_exports = (ReExports) {
        .clauses = mk_import_clause_array(0, &ra),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_name(mv_string("order")),
        .imports = imports,
        .re_exports = re_exports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(abs), abs);

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) {
        //panic(doc_to_str(pi_point.error.message, 120, a));
        panic(mv_string("pico error in pico/stdlib/abs/order.c"));
    }

    ErrorPoint point;
    if (catch_error(point)) {
        panic(doc_to_str(point.error_message, 120, &ra));
    }

    // TODO: make eq a superclass?
    const char* ord_trait = 
        "(def Ord Trait Ord [A]"
        "  {.eq (Eq A)}"
        "  [.<  Proc [A A] Bool]"
        "  [.<= Proc [A A] Bool]" 
        "  [.>  Proc [A A] Bool]"
        "  [.>= Proc [A A] Bool])\n" ;
    compile_toplevel(ord_trait, module, target, &point, &pi_point, region);

    const char* le_fn =
        "(def < all [A] proc {(ord (Ord A))} [(x A) (y A)] ord.< x y)";
    compile_toplevel(le_fn, module, target, &point, &pi_point, region);

    const char* leq_fn =
        "(def <= all [A] proc {(ord (Ord A))} [(x A) (y A)] ord.<= x y)";
    compile_toplevel(leq_fn, module, target, &point, &pi_point, region);

    const char* ge_fn =
        "(def > all [A] proc {(ord (Ord A))} [(x A) (y A)] ord.> x y)";
    compile_toplevel(ge_fn, module, target, &point, &pi_point, region);

    const char* geq_fn =
        "(def >= all [A] proc {(ord (Ord A))} [(x A) (y A)] ord.>= x y)";
    compile_toplevel(geq_fn, module, target, &point, &pi_point, region);

    const char* ord_i64_trait = 
        "(def i64-ord instance (Ord I64)"
        "  [.< i64.<]"
        "  [.<= i64.<=]"
        "  [.> i64.>]"
        "  [.>= i64.>=])\n";
    compile_toplevel(ord_i64_trait, module, target, &point, &pi_point, region);
}
