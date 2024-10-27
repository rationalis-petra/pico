#ifndef __MEMORY_ARENA_H
#define __MEMORY_ARENA_H

#include "memory/allocator.h"

Allocator mk_arena_allocator(size_t blocksize, Allocator* a);

void reset_arena_allocator(Allocator a);

void release_arena_allocator(Allocator a);

#endif
