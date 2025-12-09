#include "pico/stdlib/platform/submodules.h"
#include "pico/stdlib/platform/platform.h"


void add_platform_module(Assembler* ass, Package* base, Allocator* default_allocator, PiAllocator* module_allocator, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, &ra),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("platform")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, base, NULL, *module_allocator);
    delete_module_header(header);

    RegionAllocator* subregion = make_subregion(region);
    add_platform_memory_module(ass, module, default_allocator, module_allocator, subregion);
    reset_subregion(subregion);
    add_time_module(ass, module, module_allocator, subregion);
    reset_subregion(subregion);
    add_filesystem_module(ass, module, module_allocator, subregion);
    reset_subregion(subregion);
    add_terminal_module(ass, module, module_allocator, subregion);
    reset_subregion(subregion);

#ifdef WINDOW_SYSTEM
    add_window_module(ass, module, module_allocator, subregion);
    reset_subregion(subregion);
#endif
    
#ifdef USE_VULKAN
    add_hedron_module(ass, module, module_allocator, subregion); // Dependencies: window
    release_subregion(subregion);
#endif

    add_module(string_to_symbol(mv_string("platform")), module, base);
}
