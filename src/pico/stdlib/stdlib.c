#include "platform/memory/executable.h"

#include "pico/stdlib/stdlib.h"
#include "pico/stdlib/core.h"
#include "pico/stdlib/data/data.h"
#include "pico/stdlib/abs/abs.h"
#include "pico/stdlib/platform/platform.h"
#include "pico/stdlib/num.h"
#include "pico/stdlib/extra.h"
#include "pico/stdlib/meta/meta.h"
#include "pico/stdlib/debug.h"
#include "pico/stdlib/foreign.h"
#include "pico/stdlib/user.h"

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
    add_core_module(ass, base, subregion);
    reset_subregion(subregion);
    add_num_module(ass, base, subregion);
    reset_subregion(subregion);
    add_meta_module(ass, base, subregion);
    reset_subregion(subregion);
    add_debug_module(target, base, subregion);
    reset_subregion(subregion);

    // Extra happens AFTER meta, as extra has `loop` - a macro!
    add_extra_module(ass, base, subregion);
    reset_subregion(subregion);

    add_platform_module(ass, base, default_allocator, subregion);
    reset_subregion(subregion);

    // abs and data happen after platform, as they depend on allocators present
    // in 'platform.memory'
    add_data_module(target, base, subregion);
    reset_subregion(subregion);
    add_abs_module(target, base, subregion);
    reset_subregion(subregion);
    add_foreign_module(ass, base, subregion);
    reset_subregion(subregion);


    add_user_module(base, region);
    reset_subregion(subregion);

    release_executable_allocator(exalloc);
    return base;
}

Package* get_base_package() { return base; }
