#include "platform/signals.h"

#include "pico/stdlib/helpers.h"
#include "pico/stdlib/extra.h"
#include "pico/stdlib/data/submodules.h"

void add_result_module(Target target, Module *data, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);

    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(3, &ra),
    };
    add_import_all(&imports.clauses, &ra, 1, "core");
    add_import_all(&imports.clauses, &ra, 1, "num");
    add_import_all(&imports.clauses, &ra, 1, "extra");

    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("result")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(data), NULL);
    delete_module_header(header);

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) {
        panic(mv_string("pi error in result.c"));
    }

    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    const char* str_type = "(def Result Named Result Family [Value Error] Enum [:ok Value] [:error Error])";
    compile_toplevel(str_type, module, target, &point, &pi_point, region);

    /* const char* str_type = "(def String Named String Struct [.memsize U64] [.bytes Address])"; */
    /* compile_toplevel(str_type, module, target, &point, &pi_point, a); */
    // 
    /* const char* null_fn = "(def String Named String Struct [.memsize U64] [.bytes Address])"; */
    /* compile_toplevel(null_fn, module, target, &point, &pi_point, a); */

    Result r = add_module_def(data, string_to_symbol(mv_string("result")), module);
    if (r.type == Err) panic(r.error_message);
}
