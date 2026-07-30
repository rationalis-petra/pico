#include "platform/signals.h"

#include "components/pretty/string_printer.h"

#include "pico/stdlib/abs/submodules.h"
#include "pico/stdlib/helpers.h"

void add_show_module(Target target, Module *abs, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);

    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(4, &ra),
    };
    add_import_all(&imports.clauses, &ra, 2, "lang", "relic");
    //add_import_all(&imports.clauses, &ra, , "data", "string");

    ReExports re_exports = (ReExports) {
        .clauses = mk_import_clause_array(0, &ra),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_name(mv_string("show")),
        .imports = imports,
        .re_exports = re_exports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(abs), abs);
    delete_module_header(header);

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) {
        panic(mv_string("pico error in pico/stdlib/abs/show.c"));
    }

    ErrorPoint point;
    if (catch_error(point)) {
        panic(doc_to_str(point.error_message, 120, &ra));
    }

    const char* slice_type = "(def Slice Named Slice Family [Type] Struct [.addr Address] [.len U64])";
    compile_toplevel(slice_type, module, target, &point, &pi_point, region);

    const char* str_type = "(def String Named String (Slice U8))";
    compile_toplevel(str_type, module, target, &point, &pi_point, region);

    const char* show_trait = 
        "(def Show Trait Show [A]"
        "  [.to-string Proc [A] String])\n";
    compile_toplevel(show_trait, module, target, &point, &pi_point, region);
}
