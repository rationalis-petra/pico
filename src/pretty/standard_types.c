#include <inttypes.h>
#include <stdio.h>

#include "pretty/standard_types.h"

document* pretty_i64(int64_t val, allocator a) {
    int len = snprintf(NULL, 0, "%" PRId64, val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%" PRId64, val);
    return mv_str_doc(mv_string(str), a);
}

document* pretty_u64(uint64_t val, allocator a) {
    int len = snprintf(NULL, 0, "%" PRIu64, val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%" PRId64, val);
    return mv_str_doc(mv_string(str), a);
}

document* pretty_hex_u8(uint8_t val, allocator a) {
    int len = snprintf(NULL, 0, "%" PRIx8, val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%02" PRIx8, val);
    return mv_str_doc(mv_string(str), a);
}
