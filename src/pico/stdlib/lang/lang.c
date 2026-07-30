#include "pico/stdlib/lang/lang.h"
#include "pico/stdlib/lang/core.h"
#include "pico/stdlib/lang/extra.h"
#include "pico/stdlib/lang/foreign.h"
#include "pico/stdlib/lang/debug.h"
#include "pico/stdlib/lang/dev.h"
#include "pico/stdlib/lang/relic.h"

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
    add_core_module(ass, lang, subregion);
    reset_subregion(subregion);
    add_foreign_module(ass, lang, subregion);
    reset_subregion(subregion);
    add_relic_module(lang, subregion);
    reset_subregion(subregion);
    add_debug_module(target, lang, subregion);
    reset_subregion(subregion);
    add_dev_module(target, lang, subregion);
    reset_subregion(subregion);

    return lang;
}

void populate_lang_module_extras(Module* lang, Assembler* ass, RegionAllocator* region) {
    add_extra_module(ass, lang, region);
}
