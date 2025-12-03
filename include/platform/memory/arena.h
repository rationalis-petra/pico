#ifndef __PLATFORM_MEMORY_ARENA_H
#define __PLATFORM_MEMORY_ARENA_H

#include "platform/memory/allocator.h"

typedef struct ArenaAllocator ArenaAllocator;

ArenaAllocator* make_arena_allocator(size_t blocksize, Allocator* a);
void* arena_malloc(ArenaAllocator* arena, size_t memsize);
void reset_arena_allocator(ArenaAllocator* a);
void delete_arena_allocator(ArenaAllocator* a);

Allocator aa_to_gpa(ArenaAllocator* arena);


#endif
