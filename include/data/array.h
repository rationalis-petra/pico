#ifndef __DATA_ARRAY_H
#define __DATA_ARRAY_H

#include <stdbool.h>
#include <stdint.h>
#include "data/meta/array_header.h"

typedef void  (*PtrDelete)(void*, Allocator*);
typedef void* (*PtrCopy)(void*, Allocator*);
ARRAY_HEADER(void*, ptr, Ptr)

ARRAY_HEADER(uint8_t, u8, U8)
void add_u8_chunk(uint8_t* chunk, size_t memsize, U8Array* array);

ARRAY_HEADER(uint32_t, u32, U32)

ARRAY_HEADER(int32_t, i32, I32)

ARRAY_HEADER(uint64_t, u64, U64)

ARRAY_HEADER(size_t, size, Size)

#endif
