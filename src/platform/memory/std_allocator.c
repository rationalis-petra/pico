#include <stdlib.h>
#include <stdbool.h>

#include "platform/signals.h"
#include "platform/memory/std_allocator.h"


#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"

void* std_malloc(size_t memsize, void* ctx) {
    void* val = malloc(memsize);
    if (!val) {
        panic(mv_string("malloc failed!"));
    }
    return val;
}

void* std_realloc(void* location, size_t memsize, void* ctx) {
    void* val = realloc(location, memsize);
    if (!val) {
        panic(mv_string("malloc failed!"));
    }
    return val;
}

void std_free(void* location, void* ctx) {
    free(location);
}

#pragma GCC diagnostic pop

static AllocatorVTable std_vtable = {
    .malloc = std_malloc,
    .realloc = std_realloc,
    .free = std_free,
};

static Allocator std_alloc = {
    .vtable = &std_vtable,
    .ctx = NULL,
};

Allocator* get_std_allocator() {
    return &std_alloc;
}
