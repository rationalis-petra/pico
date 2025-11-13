#ifndef __PLATFORM_MEMORY_ALLOCATOR_H
#define __PLATFORM_MEMORY_ALLOCATOR_H

#include <stddef.h>

typedef struct AllocatorVTable {
    void* (*malloc)(size_t memsize, void* ctx);
    void* (*realloc)(void* ptr, size_t memsize, void* ctx);
    void  (*free)(void* location, void* ctx);
} AllocatorVTable;

typedef struct Allocator {
    AllocatorVTable* vtable;
    void* ctx;
} Allocator;

static inline void* mem_alloc(size_t memsize, Allocator* allocator) {
    return allocator->vtable->malloc(memsize, allocator->ctx);
}

static inline void* mem_realloc(void* location, size_t memsize, Allocator* allocator) {
    return allocator->vtable->realloc(location, memsize, allocator->ctx);
}

static inline void mem_free(void* location, Allocator* allocator) {
    allocator->vtable->free(location, allocator->ctx);
}

#endif
