#include <stdlib.h>
#include <stdbool.h>

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

Allocator* get_std_allocator() {
    static bool std_init = false; 
    static Allocator out;
    if (!std_init) {
        out.malloc = std_malloc;
        out.realloc = std_realloc;
        out.free = std_free;
        out.ctx = NULL;
        std_init = true;
    }
    return &out;
}
