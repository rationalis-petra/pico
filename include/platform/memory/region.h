#ifndef __PLATFORM_MEMORY_REGION_H
#define __PLATFORM_MEMORY_REGION_H

#include <stdbool.h>
#include "platform/memory/allocator.h"

typedef struct RegionAllocator RegionAllocator;

/* Create a new region allocator, with following parameters:
 * ---------------------------------------------------------
 * • initial_regionsize: When creating a subregion, the amount of memory
 *   allocated to that subregion will have this size.
 * • dynamic_regionsize: If true, then the initial regionsize variable will be
 *   updated based on how much memory previous regions have used.
 */
RegionAllocator* make_region_allocator(size_t initial_regionsize, bool dynamic_regionsize, Allocator* a);

/* Adapt the region allocator to a regular allocator interface,  
 * for usage in containers etc.
 */
Allocator ra_to_gpa(RegionAllocator* arena);

RegionAllocator* make_subregion(RegionAllocator* a);

void release_subregion(RegionAllocator* subregion);
void reset_subregion(RegionAllocator* subregion);

void delete_region_allocator(RegionAllocator* a);

#endif
