#ifndef __DATA_AMAP_H
#define __DATA_AMAP_H

#include <stdbool.h>
#include <stdint.h>
#include "data/meta/amap_header.h"
#include "data/string.h"

AMAP_HEADER(uint64_t, void*, u64_ptr)

AMAP_HEADER(string, void*, str_ptr)

AMAP_HEADER(string, uint64_t, str_u64)

//#undef AMAP_HEADER

#endif
