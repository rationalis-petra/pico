#ifndef __MEMORY_EXECUTABLE_H
#define __MEMORY_EXECUTABLE_H
#include "memory/allocator.h"

// utilities for getting executable memory
// currently, memory is fixed size
// A custom allocator is planned.

allocator mk_executable_allocator(allocator a);
void release_executable_allocator();

#endif
