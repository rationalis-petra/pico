#include "platform/memory/executable.h"

#include "pico/stdlib/stdlib.h"
#include "pico/stdlib/lang/lang.h"
#include "pico/stdlib/data/data.h"
#include "pico/stdlib/abs/abs.h"
#include "pico/stdlib/platform/platform.h"
#include "pico/stdlib/num/num.h"
#include "pico/stdlib/meta/meta.h"
#include "pico/stdlib/prelude.h"

static Package* base;
Package* base_package(Assembler* ass, Allocator* default_allocator, PiAllocator* module_allocator, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    Allocator exalloc = mk_executable_allocator(&ra);
    Target target = (Target) {
        .data_aux = mem_alloc(sizeof(U8Array), &ra),
        .code_aux = mk_assembler(current_cpu_feature_flags(), &exalloc),
        .target = mk_assembler(current_cpu_feature_flags(), &exalloc),
    };
    *target.data_aux = mk_u8_array(256, &ra);

    base = mk_package(string_to_name(mv_string("base")), *module_allocator);

    RegionAllocator* subregion = make_subregion(region);
    /** 
     * Phase 1: Core (lang + meta) modules
     */
    Module* lang = add_lang_module(ass, target, base, subregion);
    reset_subregion(subregion);
    add_meta_module(ass, base, subregion);
    reset_subregion(subregion);
    populate_lang_module_extras(lang, ass, subregion);
    reset_subregion(subregion);

    /** 
     * Phase 2: Platform
     */
    add_platform_module(ass, base, default_allocator, subregion);
    reset_subregion(subregion);

    /** 
     * Phase 3: 'user facing' code: 
     *   Abs and Data happen after platform, as they depend on allocators present
     *   in 'platform.memory'.
     */
    add_abs_module(target, base, subregion);
    reset_subregion(subregion);
    add_num_module(ass, target, base, subregion);
    reset_subregion(subregion);
    add_data_module(target, base, subregion);
    reset_subregion(subregion);


    add_prelude_module(base, region);

    reset_subregion(subregion);

    release_executable_allocator(exalloc);
    return base;
}

Package* get_base_package() { return base; }
