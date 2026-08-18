#include "platform/signals.h"

#include "components/pretty/string_printer.h"

#include "pico/stdlib/abs/submodules.h"
#include "pico/stdlib/helpers.h"

void add_numeric_module(Target target, Module *abs, RegionAllocator* region) {
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
        .name = string_to_name(mv_string("numeric")),
        .imports = imports,
        .re_exports = re_exports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(abs), abs);

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) {
        panic(mv_string("pico error in pico/stdlib/abs/numeric.c"));
    }

    ErrorPoint point;
    if (catch_error(point)) {
        panic(doc_to_str(point.error_message, 120, &ra));
    }

    const char* ring_trait = 
        "(def Ring Trait Ring [A]"
        "  [.+ Proc [A A] A]"
        "  [.- Proc [A A] A]"
        "  [.* Proc [A A] A]"
        "  [.zero A]"
        "  [.one A])\n";
    compile_toplevel(ring_trait, module, target, &point, &pi_point, region);

    const char* num_trait = 
        "(def Num Trait Num [A]"
        "  {.ring Ring A}"
        "  [./ Proc [A A] A])";
    compile_toplevel(num_trait, module, target, &point, &pi_point, region);

    const char* real_trait = 
        "(def Real Trait Real [A]"
        "  {.num Num A}"
        "  [.sin Proc [A] A]"
        "  [.cos Proc [A] A])";
    compile_toplevel(real_trait, module, target, &point, &pi_point, region);

    const char* add_fn =
        "(def + all [A] proc {(n (Ring A))} [(x A) (y A)] n.+ x y)";
    compile_toplevel(add_fn, module, target, &point, &pi_point, region);

    const char* sub_fn = 
        "(def - all [A] proc {(n (Ring A))} [(x A) (y A)] n.- x y)";
    compile_toplevel(sub_fn, module, target, &point, &pi_point, region);

    const char* mul_fn = 
        "(def * all [A] proc {(n (Ring A))} [(x A) (y A)] n.* x y)";
    compile_toplevel(mul_fn, module, target, &point, &pi_point, region);

    const char* div_fn = 
        "(def / all [A] proc {(n (Num A))} [(x A) (y A)] n./ x y)";
    compile_toplevel(div_fn, module, target, &point, &pi_point, region);

    const char* sin_fn = 
        "(def sin all [A] proc {(n (Real A))} [(x A)] n.sin x)";
    compile_toplevel(sin_fn, module, target, &point, &pi_point, region);

    const char* cos_fn = 
        "(def cos all [A] proc {(n (Real A))} [(x A)] n.cos x)";
    compile_toplevel(cos_fn, module, target, &point, &pi_point, region);
}
