#include "platform/signals.h"

#include "pico/stdlib/abs/submodules.h"
#include "pico/stdlib/helpers.h"

void add_numeric_module(Target target, Module *abs, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(4, &ra),
    };
    add_import_all(&imports.clauses, &ra, 1, "core");
    add_import_all(&imports.clauses, &ra, 1, "num");

    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("numeric")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(abs), NULL);

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) {
        //panic(doc_to_str(pi_point.error.message, 120, a));
        panic(mv_string("pico error in abs/numeric.c"));
    }

    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    const char* num_trait = 
        "(def Num Trait [A]"
        "  [.+ Proc [A A] A]"
        "  [.- Proc [A A] A]"
        "  [.* Proc [A A] A]"
        "  [./ Proc [A A] A]"
        "  [.zero A]"
        "  [.one A])\n";
    compile_toplevel(num_trait, module, target, &point, &pi_point, region);

    const char* add_fn =
        "(def + all [A] proc {(n (Num A))} [(x A) (y A)] n.+ x y)";
    compile_toplevel(add_fn, module, target, &point, &pi_point, region);

    const char* sub_fn = 
        "(def - all [A] proc {(n (Num A))} [(x A) (y A)] n.- x y)";
    compile_toplevel(sub_fn, module, target, &point, &pi_point, region);

    const char* mul_fn = 
        "(def * all [A] proc {(n (Num A))} [(x A) (y A)] n.* x y)";
    compile_toplevel(mul_fn, module, target, &point, &pi_point, region);

    const char* div_fn = 
        "(def / all [A] proc {(n (Num A))} [(x A) (y A)] n./ x y)";
    compile_toplevel(div_fn, module, target, &point, &pi_point, region);

    const char* num_i64_trait = 
        "(def i64-num instance (Num I64)"
        "  [.+ i64.+]"
        "  [.- i64.-]"
        "  [.* i64.*]"
        "  [./ i64./]"
        "  [.zero 0]"
        "  [.one 1])\n";
    compile_toplevel(num_i64_trait, module, target, &point, &pi_point, region);

    const char* num_u64_trait = 
        "(def u64-num instance (Num U64)"
        "  [.+ u64.+]"
        "  [.- u64.-]"
        "  [.* u64.*]"
        "  [./ u64./]"
        "  [.zero 0]"
        "  [.one 1])\n";
    compile_toplevel(num_u64_trait, module, target, &point, &pi_point, region);

    const char* num_i32_trait = 
        "(def i32-num instance (Num I32)"
        "  [.+ i32.+]"
        "  [.- i32.-]"
        "  [.* i32.*]"
        "  [./ i32./]"
        "  [.zero 0]"
        "  [.one 1])\n";
    compile_toplevel(num_i32_trait, module, target, &point, &pi_point, region);

    const char* num_u32_trait = 
        "(def u32-num instance (Num U32)"
        "  [.+ u32.+]"
        "  [.- u32.-]"
        "  [.* u32.*]"
        "  [./ u32./]"
        "  [.zero 0]"
        "  [.one 1])\n";
    compile_toplevel(num_u32_trait, module, target, &point, &pi_point, region);

    const char* num_i16_trait = 
        "(def i16-num instance (Num I16)"
        "  [.+ i16.+]"
        "  [.- i16.-]"
        "  [.* i16.*]"
        "  [./ i16./]"
        "  [.zero 0]"
        "  [.one 1])\n";
    compile_toplevel(num_i16_trait, module, target, &point, &pi_point, region);

    const char* num_u16_trait = 
        "(def u16-num instance (Num U16)"
        "  [.+ u16.+]"
        "  [.- u16.-]"
        "  [.* u16.*]"
        "  [./ u16./]"
        "  [.zero 0]"
        "  [.one 1])\n";
    compile_toplevel(num_u16_trait, module, target, &point, &pi_point, region);

    const char* num_i8_trait = 
        "(def i8-num instance (Num I8)"
        "  [.+ i8.+]"
        "  [.- i8.-]"
        "  [.* i8.*]"
        "  [./ i8./]"
        "  [.zero 0]"
        "  [.one 1])\n";
    compile_toplevel(num_i8_trait, module, target, &point, &pi_point, region);

    const char* num_u8_trait = 
        "(def u8-num instance (Num U8)"
        "  [.+ u8.+]"
        "  [.- u8.-]"
        "  [.* u8.*]"
        "  [./ u8./]"
        "  [.zero 0]"
        "  [.one 1])\n";
    compile_toplevel(num_u8_trait, module, target, &point, &pi_point, region);

    const char* num_f64_trait =
        "(def f64-num instance (Num F64)"
        "  [.+ f64.+]"
        "  [.- f64.-]"
        "  [.* f64.*]"
        "  [./ f64./]"
        "  [.zero 0.0]"
        "  [.one 1.0])\n";
    compile_toplevel(num_f64_trait, module, target, &point, &pi_point, region);

    const char* num_f32_trait =
        "(def f32-num instance (Num F32)"
        "  [.+ f32.+]"
        "  [.- f32.-]"
        "  [.* f32.*]"
        "  [./ f32./]"
        "  [.zero 0.0]"
        "  [.one 1.0])\n";
    compile_toplevel(num_f32_trait, module, target, &point, &pi_point, region);

    Result r = add_module_def(abs, string_to_symbol(mv_string("numeric")), module);
    if (r.type == Err) panic(r.error_message);
}
