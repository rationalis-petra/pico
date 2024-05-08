#include "data/meta/amap_impl.h"
#include "data/amap.h"
#include "data/string.h"

AMAP_IMPL(uint64_t, void*, u64_ptr)

AMAP_CMP_IMPL(string, void*, string_cmp, str_ptr)

AMAP_CMP_IMPL(string, uint64_t, string_cmp, str_u64)
