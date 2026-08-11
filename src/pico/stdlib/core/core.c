#include "pico/stdlib/core/core.h"
#include "pico/stdlib/core/kernel.h"
#include "pico/stdlib/core/foreign.h"
#include "pico/stdlib/core/debug.h"
#include "pico/stdlib/core/dev.h"

void add_core_module(Assembler* ass, Target target, Package* base, RegionAllocator* region) {
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
        .name = string_to_name(mv_string("core")),
        .imports = imports,
        .re_exports = re_exports,
        .exports = exports,
    };
    Module* core = mk_module(header, base, NULL);

    RegionAllocator* subregion = make_subregion(region);
    add_kernel_module(ass, core, subregion);
    reset_subregion(subregion);
    add_foreign_module(ass, core, subregion);
    reset_subregion(subregion);
    add_debug_module(target, core, subregion);
    reset_subregion(subregion);

    /** By default, the development module is only available in debug builds. */
#ifdef DEBUG
    add_dev_module(target, core, subregion);
    reset_subregion(subregion);
#endif
}
