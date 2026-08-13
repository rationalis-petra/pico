#include "platform/signals.h"

#include "components/pretty/string_printer.h"

#include "pico/stdlib/abs/submodules.h"
#include "pico/stdlib/helpers.h"

void add_sequence_module(Target target, Module *abs, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(2, &ra),
    };
    add_import_all(&imports.clauses, &ra, 2, "lang", "relic");
    add_import_all(&imports.clauses, &ra, 2, "abs", "equality");

    ReExports re_exports = (ReExports) {
        .clauses = mk_import_clause_array(0, &ra),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_name(mv_string("sequence")),
        .imports = imports,
        .re_exports = re_exports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(abs), abs);

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) {
        panic(mv_string("pico error in pico/stdlib/abs/sequence.c"));
    }

    ErrorPoint point;
    if (catch_error(point)) {
        panic(doc_to_str(point.error_message, 120, &ra));
    }

    // TODO: ensure this isn't exported...
    const char* maybe_type = 
        "(def Maybe Named Maybe Family [A] Enum\n"
        "  [:some A]\n"
        "  [:none])\n" ;
    compile_toplevel(maybe_type, module, target, &point, &pi_point, region);

    const char* iterator_vtable = 
        "(def IteratorVTable Named IteratorVTable Family [I V] Struct\n"
        "  [.next Proc [I] (Maybe V)]\n"
        "  [.val Proc [I] V])\n" ;
    compile_toplevel(iterator_vtable, module, target, &point, &pi_point, region);

    const char* iterator_type = 
        "(def Iterator Named Iterator Family [A] Sealed [I] Struct\n"
        "  [.vtable (IteratorVTable I A)]\n"
        "  [.state I])\n" ;
    compile_toplevel(iterator_type, module, target, &point, &pi_point, region);

    /*
    const char* seq_trait = 
        "(def Seq Trait Ord [(C Kind [Type] Type)]\n"
        "  [.elt All [A] Proc [U64 (C A)] (Maybe A)]\n"
        "  [.begin All [A] Proc [(C A)] (Iterator (C A) A)])" ;
    compile_toplevel(seq_trait, module, target, &point, &pi_point, region);
    */
}
