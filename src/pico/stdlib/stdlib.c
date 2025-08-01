#include "platform/memory/executable.h"

#include "pico/stdlib/stdlib.h"
#include "pico/stdlib/core.h"
#include "pico/stdlib/data/data.h"
#include "pico/stdlib/abs/abs.h"
#include "pico/stdlib/platform/platform.h"
#include "pico/stdlib/num.h"
#include "pico/stdlib/extra.h"
#include "pico/stdlib/meta.h"
#include "pico/stdlib/debug.h"
#include "pico/stdlib/foreign.h"
#include "pico/stdlib/user.h"

static Package* base;
Package* base_package(Assembler* ass, Allocator* a, Allocator* default_allocator) {
    Allocator exalloc = mk_executable_allocator(a);
    Target target = (Target) {
        .data_aux = mem_alloc(sizeof(U8Array), a),
        .code_aux = mk_assembler(current_cpu_feature_flags(), &exalloc),
        .target = mk_assembler(current_cpu_feature_flags(), &exalloc),
    };
    *target.data_aux = mk_u8_array(256, a);

    base = mk_package(string_to_name(mv_string("base")), a);
    add_core_module(ass, base, a);
    add_num_module(ass, base, a);
    add_meta_module(ass, base, a);
    add_debug_module(target, base, a);

    // Extra happens AFTER meta, as extra has `loop` - a macro!
    add_extra_module(ass, base, default_allocator, a);

    // abs and data happen after extra, as they rely on allocators present in 'extra'  
    add_data_module(target, base, a);
    add_abs_module(target, base, a);
    add_foreign_module(ass, base, a);

    add_platform_module(ass, base, a);

    add_user_module(base, a);

    release_executable_allocator(exalloc);
    sdelete_u8_array(*target.data_aux);
    mem_free(target.data_aux, a);
    return base;
}

Package* get_base_package() { return base; }
