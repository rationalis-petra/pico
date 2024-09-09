#ifndef __DATA_AMAP_H
#define __DATA_AMAP_H

#include <stdbool.h>
#include <stdint.h>
#include "data/meta/amap_header.h"
#include "data/string.h"

AMAP_HEADER(uint64_t, void*, U64Ptr)

AMAP_HEADER(String, void*, StrPtr)

AMAP_HEADER(String, uint64_t, StrU64)

//#undef AMAP_HEADER

#endif
