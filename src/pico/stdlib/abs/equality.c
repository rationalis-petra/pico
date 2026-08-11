#include "platform/signals.h"

#include "components/pretty/string_printer.h"

#include "pico/stdlib/abs/submodules.h"
#include "pico/stdlib/helpers.h"

void add_equality_module(Target target, Module *abs, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(1, &ra),
    };
    add_import_all(&imports.clauses, &ra, 2, "lang", "relic");

    ReExports re_exports = (ReExports) {
        .clauses = mk_import_clause_array(0, &ra),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_name(mv_string("equality")),
        .imports = imports,
        .re_exports = re_exports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(abs), abs);

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) {
        panic(mv_string("pico error in pico/stdlib/abs/equality.c"));
    }

    ErrorPoint point;
    if (catch_error(point)) {
        panic(doc_to_str(point.error_message, 120, &ra));
    }

    const char* eq_trait = 
        "(def Eq Trait Eq [A]"
        "  [.= Proc [A A] Bool]"
        "  [.!= Proc [A A] Bool])\n";
    compile_toplevel(eq_trait, module, target, &point, &pi_point, region);

    const char* eq_fn =
        "(def = all [A] proc {(eq (Eq A))} [(x A) (y A)] eq.= x y)";
    compile_toplevel(eq_fn, module, target, &point, &pi_point, region);

    const char* neq_fn =
        "(def != all [A] proc {(eq (Eq A))} [(x A) (y A)] eq.!= x y)";
    compile_toplevel(neq_fn, module, target, &point, &pi_point, region);
}
