#ifndef __DATA_ARRAY_H
#define __DATA_ARRAY_H

#include <stdbool.h>
#include <stdint.h>
#include "data/meta/array_header.h"

ARRAY_HEADER(void*, ptr)

ARRAY_HEADER(uint8_t, u8)

ARRAY_HEADER(uint32_t, u32)

ARRAY_HEADER(uint64_t, u64)

ARRAY_HEADER(size_t, size)

#endif
