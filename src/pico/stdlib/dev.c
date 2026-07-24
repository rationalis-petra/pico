#include "platform/signals.h"
#include "platform/memory/region.h"

#include "components/pretty/string_printer.h"

#include "pico/codegen/codegen.h"
#include "pico/stdlib/dev.h"

void add_dev_module(Target target, Package* base, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);

    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, &ra),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_name(mv_string("dev")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, base, NULL);
    Name name;

    PiType type;
    //PiType* typep;
    ErrorPoint point;
    if (catch_error(point)) {
        panic(doc_to_str(point.error_message, 120, &ra));
    }

    TermFormer former;
    type.sort = TPrim;
    type.prim = TFormer;

    Segments null_segments = (Segments) {
        .code = mk_u8_array(0, &ra),
        .data = mk_u8_array(0, &ra),
    };

    // ------------------------------------------------------------------------
    // Term Formers
    // ------------------------------------------------------------------------
    former = FDescribe;
    name = string_to_name(mv_string("describe"));
    add_def(module, name, type, &former, null_segments, NULL);

    former = FDevAnnotation;
    name = string_to_name(mv_string("dev"));
    add_def(module, name, type, &former, null_segments, NULL);
}

