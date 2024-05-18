#ifndef __MEMORY_ARENA_H
#define __MEMORY_ARENA_H

#include "memory/allocator.h"

allocator mk_arena_allocator(size_t blocksize, allocator a);
void release_arena_allocator(allocator a);

#endif
