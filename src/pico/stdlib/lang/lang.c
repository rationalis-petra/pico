#include "pico/stdlib/lang/lang.h"
#include "pico/stdlib/lang/relic.h"
#include "pico/stdlib/lang/extra.h"

Module* add_lang_module(Assembler* ass, Target target, Package* base, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    Imports imports = (Imports) {.clauses = mk_import_clause_array(0, &ra),};
    ReExports re_exports = (ReExports) {
        .clauses = mk_import_clause_array(0, &ra),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };

    ModuleHeader header = (ModuleHeader) {
        .name = string_to_name(mv_string("lang")),
        .imports = imports,
        .re_exports = re_exports,
        .exports = exports,
    };
    Module* lang = mk_module(header, base, NULL);

    RegionAllocator* subregion = make_subregion(region);
    add_relic_module(lang, subregion);
    reset_subregion(subregion);
    add_extra_module(ass, lang, region);
    reset_subregion(subregion);

    return lang;
}

