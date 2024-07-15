#include <stdlib.h>

#include "memory/std_allocator.h"


#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
void* std_malloc(size_t memsize, void* ctx) {
    return malloc(memsize);
}

void* std_realloc(void* location, size_t memsize, void* ctx) {
    return realloc(location, memsize);
}

void std_free(void* location, void* ctx) {
    free(location);
}
#pragma GCC diagnostic pop

allocator get_std_allocator() {
    allocator out;
    out.malloc = std_malloc;
    out.realloc = std_realloc;
    out.free = std_free;
    out.ctx = NULL;
    return out;
}
