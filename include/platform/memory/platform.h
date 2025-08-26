#ifndef __PLATFORM_MEMORY_PLATFORM_H
#define __PLATFORM_MEMORY_PLATFORM_H
#include <stddef.h>

typedef enum {
  ARead = 1,
  AWrite = 2,
  AExecute = 4,
} AllocateFlags;

typedef struct {
    void* data;
    size_t size;
} MemoryBlock;

// bool is_supported_flagset(AllocationFlags flags)

MemoryBlock platform_allocate(size_t min_size, AllocateFlags flags);
void platform_free(MemoryBlock block);

#endif
