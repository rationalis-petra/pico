#ifndef __DATA_ARRAY_H
#define __DATA_ARRAY_H

#include <stdbool.h>
#include <stdint.h>
#include "data/meta/array_header.h"

ARRAY_HEADER(void*, Ptr)

ARRAY_HEADER(uint8_t, U8)

ARRAY_HEADER(uint32_t, U32)

ARRAY_HEADER(uint64_t, U64)

ARRAY_HEADER(size_t, Size)

#endif
