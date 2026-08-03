#include "platform/signals.h"

#include "components/pretty/string_printer.h"

#include "pico/stdlib/abs/submodules.h"
#include "pico/stdlib/helpers.h"

void add_lifetime_module(Target target, Module *abs, RegionAllocator* region) {
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
        .name = string_to_name(mv_string("lifetime")),
        .imports = imports,
        .re_exports = re_exports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(abs), abs);

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) {
        panic(mv_string("pico error in pico/stdlib/abs/lifetime.c"));
    }

    ErrorPoint point;
    if (catch_error(point)) {
        panic(doc_to_str(point.error_message, 120, &ra));
    }

    const char* copy_trait = 
        "(def Copy Trait Copy [A]"
        "  [.copy Proc [A] A])\n";
    compile_toplevel(copy_trait, module, target, &point, &pi_point, region);

    const char* copy_fn =
        "(def copy all [A] proc {(copy (Copy A))} [(x A)] copy.copy x)";
    compile_toplevel(copy_fn, module, target, &point, &pi_point, region);

    const char* delete_trait = 
        "(def Delete Trait Delete [A]"
        "  [.delete Proc [A] Unit])\n";
    compile_toplevel(delete_trait, module, target, &point, &pi_point, region);

    const char* delete_fn =
        "(def delete all [A] proc {(del (Delete A))} [(x A)] del.delete x)";
    compile_toplevel(delete_fn, module, target, &point, &pi_point, region);
}
