#include "data/meta/amap_impl.h"
#include "data/amap.h"
#include "data/string.h"

AMAP_IMPL(uint64_t, void*, u64_ptr, U64Ptr)

AMAP_CMP_IMPL(String, void*, string_cmp, str_ptr, StrPtr)

AMAP_CMP_IMPL(String, uint64_t, string_cmp, str_u64, StrU64)
