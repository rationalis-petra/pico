#ifndef __PLATFORM_MEMORY_REGION_H
#define __PLATFORM_MEMORY_REGION_H

#include "platform/memory/allocator.h"

typedef struct RegionAllocator RegionAllocator;

RegionAllocator* mk_arena_allocator(size_t blocksize, Allocator* a);

Allocator ra_to_gpa(RegionAllocator* arena);

RegionAllocator* make_subregion(RegionAllocator* a);

void release_subregion(RegionAllocator* subregion, RegionAllocator* region);

void reset_arena_allocator(RegionAllocator* a);

void release_arena_allocator(RegionAllocator* a);

#endif
