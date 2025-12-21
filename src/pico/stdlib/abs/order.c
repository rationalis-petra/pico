#include "platform/signals.h"

#include "components/pretty/string_printer.h"

#include "pico/stdlib/abs/submodules.h"
#include "pico/stdlib/helpers.h"

void add_order_module(Target target, Module *abs, RegionAllocator* region) {
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
        .name = string_to_symbol(mv_string("order")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(abs), NULL);

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) {
        //panic(doc_to_str(pi_point.error.message, 120, a));
        panic(mv_string("pico error in pico/stdlib/abs/order.c"));
    }

    ErrorPoint point;
    if (catch_error(point)) {
        panic(doc_to_str(point.error_message, 120, &ra));
    }

    const char* eq_trait = 
        "(def Eq Trait [A]"
        "  [.= Proc [A A] Bool]"
        "  [.!= Proc [A A] Bool])\n";
    compile_toplevel(eq_trait, module, target, &point, &pi_point, region);

    // TODO: make eq a superclass?
    const char* ord_trait = 
        "(def Ord Trait [A]"
        "  [.=  Proc [A A] Bool]" 
        "  [.!= Proc [A A] Bool]" 
        "  [.<  Proc [A A] Bool]"
        "  [.<= Proc [A A] Bool]" 
        "  [.>  Proc [A A] Bool]"
        "  [.>= Proc [A A] Bool])\n" ;
    compile_toplevel(ord_trait, module, target, &point, &pi_point, region);

    const char* eq_fn =
        "(def = all [A] proc {(eq (Eq A))} [(x A) (y A)] eq.= x y)";
    compile_toplevel(eq_fn, module, target, &point, &pi_point, region);

    const char* neq_fn =
        "(def != all [A] proc {(eq (Eq A))} [(x A) (y A)] eq.!= x y)";
    compile_toplevel(neq_fn, module, target, &point, &pi_point, region);

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

    const char* eq_i64_trait = 
        "(def i64-eq instance (Eq I64)"
        "  [.= i64.=]"
        "  [.!= i64.!=])\n";
    compile_toplevel(eq_i64_trait, module, target, &point, &pi_point, region);

    const char* eq_u64_trait = 
        "(def u64-eq instance (Eq U64)"
        "  [.= u64.=]"
        "  [.!= u64.!=])\n";
    compile_toplevel(eq_u64_trait, module, target, &point, &pi_point, region);

    const char* eq_i32_trait = 
        "(def i32-eq instance (Eq I32)"
        "  [.= i32.=]"
        "  [.!= i32.!=])\n";
    compile_toplevel(eq_i32_trait, module, target, &point, &pi_point, region);

    const char* eq_u32_trait = 
        "(def u32-eq instance (Eq U32)"
        "  [.= u32.=]"
        "  [.!= u32.!=])\n";
    compile_toplevel(eq_u32_trait, module, target, &point, &pi_point, region);

    const char* eq_i16_trait = 
        "(def i16-eq instance (Eq I16)"
        "  [.= i16.=]"
        "  [.!= i16.!=])\n";
    compile_toplevel(eq_i16_trait, module, target, &point, &pi_point, region);

    const char* eq_u16_trait = 
        "(def u16-eq instance (Eq U16)"
        "  [.= u16.=]"
        "  [.!= u16.!=])\n";
    compile_toplevel(eq_u16_trait, module, target, &point, &pi_point, region);

    const char* eq_i8_trait = 
        "(def i8-eq instance (Eq I8)"
        "  [.= i8.=]"
        "  [.!= i8.!=])\n";
    compile_toplevel(eq_i8_trait, module, target, &point, &pi_point, region);

    const char* eq_u8_trait = 
        "(def u8-eq instance (Eq U8)"
        "  [.= u8.=]"
        "  [.!= u8.!=])\n";
    compile_toplevel(eq_u8_trait, module, target, &point, &pi_point, region);

    Result r = add_module_def(abs, string_to_symbol(mv_string("order")), module);
    if (r.type == Err) panic(r.error_message);
}
