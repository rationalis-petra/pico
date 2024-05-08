#ifndef __MEMORY_ALLOCATOR_H
#define __MEMORY_ALLOCATOR_H

#include <stdlib.h>

typedef struct allocator {
    void* (*malloc)(size_t memsize, void* ctx);
    void* (*realloc)(void* ptr, size_t memsize, void* ctx);
    void (*free)(void* location, void* ctx);
    void* data;
} allocator;

void* mem_alloc(size_t memsize, allocator);
void* mem_realloc(void* ptr, size_t memsize, void* ctx, allocator);
void* mem_free(void* location, allocator);

#endif