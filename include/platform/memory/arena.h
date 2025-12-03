#ifndef __PLATFORM_MEMORY_ARENA_H
#define __PLATFORM_MEMORY_ARENA_H

#include "platform/memory/allocator.h"

typedef struct ArenaAllocator ArenaAllocator;

ArenaAllocator* mk_arena_allocator(size_t blocksize, Allocator* a);

Allocator aa_to_gpa(ArenaAllocator* arena);

void reset_arena_allocator(ArenaAllocator* a);

void delete_arena_allocator(ArenaAllocator* a);

#endif
