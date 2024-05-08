#ifndef __MEMORY_ALLOCATOR_H
#define __MEMORY_ALLOCATOR_H

#include <stddef.h>

typedef struct allocator {
    void* (*malloc)(size_t memsize, void* ctx);
    void* (*realloc)(void* ptr, size_t memsize, void* ctx);
    void (*free)(void* location, void* ctx);
    void* ctx;
} allocator;

static inline void* mem_alloc(size_t memsize, allocator allocator) {
    return allocator.malloc(memsize, allocator.ctx);
}

static inline void* mem_realloc(void* location, size_t memsize, allocator allocator) {
    return allocator.realloc(location, memsize, allocator.ctx);
}

static inline void mem_free(void* location, allocator allocator) {
    allocator.free(location, allocator.ctx);
}

#endif
