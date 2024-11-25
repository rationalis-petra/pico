#ifndef __PLATFORM_MEMORY_EXECUTABLE_H
#define __PLATFORM_MEMORY_EXECUTABLE_H
#include "platform/memory/allocator.h"

// utilities for getting executable memory
// currently, memory is fixed size
// A custom allocator is planned.

Allocator mk_executable_allocator(Allocator* a);
void release_executable_allocator(Allocator a);

#endif
