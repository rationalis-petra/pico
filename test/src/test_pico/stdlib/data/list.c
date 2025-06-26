#include "platform/memory/arena.h"

#include "pico/stdlib/extra.h"

#include "test_pico/stdlib/components.h"
#include "test_pico/helper.h"

void run_pico_stdlib_data_list_tests(TestLog *log, Module* module, Allocator *a) {
    Allocator arena = mk_arena_allocator(4096, a);

    typedef struct {
        int64_t x;
        int64_t y;
    } Point;

    release_arena_allocator(arena);
}
