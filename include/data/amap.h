#ifndef __DATA_AMAP_H
#define __DATA_AMAP_H

#include <stdbool.h>
#include <stdint.h>
#include "data/meta/amap_header.h"
#include "data/string.h"

AMAP_HEADER(uint64_t, void*, u64_ptr, U64Ptr)

AMAP_HEADER(String, void*, str_ptr, StrPtr)

AMAP_HEADER(String, uint64_t, str_u64,StrU64)

//#undef AMAP_HEADER

#endif
