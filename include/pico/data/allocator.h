#ifndef __PICO_DATA_ALLOCATOR_H
#define __PICO_DATA_ALLOCATOR_H

#include <stdint.h>
#include "platform/memory/allocator.h"

// Compatible with the 'AllocTable' type
// (def AllocTable Family [A] Struct
//    [.alloc   Proc [Size (Ptr A)] Address]
//    [.realloc Proc [Size Address (Ptr A)] Address]
//    [.free    Proc [Address (Ptr A)] Unit])
typedef struct {
    void* pi_alloc;
    void* pi_realloc;
    void* pi_free;
} PiAllocTable;

// Compatible with the 'sealed' type
// (def Allocator Sealed [A] Struct  [.vtable (AllocTable A)][.context Ptr A])
typedef struct {
    uint64_t type_data;
    PiAllocTable* vtable;
    void* ctx_ptr;
} PiAllocator;

void* call_alloc(size_t size, PiAllocator* pa); 
void* call_realloc(void* data, size_t size, PiAllocator* pa);
void call_free(void* data, PiAllocator* pa);

Allocator convert_allocator(PiAllocator* pa);

#endif
