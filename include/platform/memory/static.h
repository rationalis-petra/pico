#ifndef __PLATFORM_MEMORY_STATIC_ALLOCATOR_H
#define __PLATFORM_MEMORY_STATIC_ALLOCATOR_H

#include "platform/memory/allocator.h"

Allocator mk_static_allocator(void* memory, size_t size);

#endif
