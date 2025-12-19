#include "platform/signals.h"

#include "pico/stdlib/abs/submodules.h"
#include "pico/stdlib/helpers.h"

void add_show_module(Target target, Module *abs, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);

    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(4, &ra),
    };
    add_import_all(&imports.clauses, &ra, 1, "core");
    add_import_all(&imports.clauses, &ra, 1, "num");
    add_import_all(&imports.clauses, &ra, 2, "data", "string");

    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("show")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(abs), NULL);
    delete_module_header(header);

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) {
        panic(mv_string("pico error in pico/stdlib/abs/show.c"));
    }

    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    const char* num_trait = 
        "(def Show Trait [A]"
        "  [.to-string Proc [A] String])\n";
    compile_toplevel(num_trait, module, target, &point, &pi_point, region);

    const char* to_string_fn = 
        "(def to-string all [A] proc {(show (Show A))} [(x A)] show.to-string x)";
    compile_toplevel(to_string_fn, module, target, &point, &pi_point, region);

    const char* show_i64_instnace = 
        "(def i64-show instance (Show I64)"
        "  [.to-string i64.to-string])\n";
    compile_toplevel(show_i64_instnace, module, target, &point, &pi_point, region);

    const char* show_u64_instnace = 
        "(def u64-show instance (Show U64)"
        "  [.to-string u64.to-string])\n";
    compile_toplevel(show_u64_instnace, module, target, &point, &pi_point, region);

    const char* show_i32_instnace = 
        "(def i32-show instance (Show I32)"
        "  [.to-string i32.to-string])\n";
    compile_toplevel(show_i32_instnace, module, target, &point, &pi_point, region);

    const char* show_u32_instnace = 
        "(def u32-show instance (Show U32)"
        "  [.to-string u32.to-string])\n";
    compile_toplevel(show_u32_instnace, module, target, &point, &pi_point, region);

    const char* show_i16_instnace = 
        "(def i16-show instance (Show I16)"
        "  [.to-string i16.to-string])\n";
    compile_toplevel(show_i16_instnace, module, target, &point, &pi_point, region);

    const char* show_u16_instnace = 
        "(def u16-show instance (Show U16)"
        "  [.to-string u16.to-string])\n";
    compile_toplevel(show_u16_instnace, module, target, &point, &pi_point, region);

    const char* show_i8_instnace = 
        "(def i8-show instance (Show I8)"
        "  [.to-string i8.to-string])\n";
    compile_toplevel(show_i8_instnace, module, target, &point, &pi_point, region);

    const char* show_u8_instnace = 
        "(def u8-show instance (Show U8)"
        "  [.to-string u8.to-string])\n";
    compile_toplevel(show_u8_instnace, module, target, &point, &pi_point, region);

    const char* show_f64_instnace = 
        "(def f64-show instance (Show F64)"
        "  [.to-string f64.to-string])\n";
    compile_toplevel(show_f64_instnace, module, target, &point, &pi_point, region);

    const char* show_f32_instnace = 
        "(def f32-show instance (Show F32)"
        "  [.to-string f32.to-string])\n";
    compile_toplevel(show_f32_instnace, module, target, &point, &pi_point, region);

    Result r = add_module_def(abs, string_to_symbol(mv_string("show")), module);
    if (r.type == Err) panic(r.error_message);
}
