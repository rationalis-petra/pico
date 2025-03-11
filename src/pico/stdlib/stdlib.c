#include "platform/machine_info.h"
#include "platform/jump.h"

#include "pico/stdlib/stdlib.h"
#include "pico/stdlib/core.h"
#include "pico/stdlib/num.h"
#include "pico/stdlib/extra.h"
#include "pico/stdlib/foreign.h"
#include "pico/stdlib/user.h"

Package* base_package(Assembler* ass, Allocator* a, Allocator* default_allocator) {
    Package* base = mk_package(string_to_symbol(mv_string("base")), a);
    add_core_module(ass, base, a);
    add_num_module(ass, base, a);
    add_extra_module(ass, base, default_allocator, a);
    add_foreign_module(base, a);
    add_user_module(base, a);
    return base;
}
