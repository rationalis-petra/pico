#ifndef __DATA_SLICE_H
#define __DATA_SLICE_H

#include <stdbool.h>
#include <stdint.h>
#include "data/meta/slice_header.h"

SLICE_TYPE(void*, Ptr)

SLICE_TYPE(uint8_t, U8)

SLICE_TYPE(uint32_t, U32)

#endif
