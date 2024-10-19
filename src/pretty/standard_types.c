#include <inttypes.h>
#include <stdio.h>

#include "pretty/standard_types.h"

Document* pretty_i64(int64_t val, Allocator* a) {
    int len = snprintf(NULL, 0, "%" PRId64, val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%" PRId64, val);
    return mv_str_doc(mv_string(str), a);
}

Document* pretty_i32(int32_t val, Allocator* a) {
    int len = snprintf(NULL, 0, "%" PRId32, val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%" PRId32, val);
    return mv_str_doc(mv_string(str), a);
}

Document* pretty_i16(int16_t val, Allocator* a) {
    int len = snprintf(NULL, 0, "%" PRId16, val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%" PRId16, val);
    return mv_str_doc(mv_string(str), a);
}

Document* pretty_i8(int8_t val, Allocator* a) {
    int len = snprintf(NULL, 0, "%" PRId8, val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%" PRId8, val);
    return mv_str_doc(mv_string(str), a);
}

Document* pretty_u64(uint64_t val, Allocator* a) {
    int len = snprintf(NULL, 0, "%" PRIu64, val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%" PRIu64, val);
    return mv_str_doc(mv_string(str), a);
}

Document* pretty_u32(uint32_t val, Allocator* a) {
    int len = snprintf(NULL, 0, "%" PRIu32, val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%" PRIu32, val);
    return mv_str_doc(mv_string(str), a);
}

Document* pretty_u16(uint16_t val, Allocator* a) {
    int len = snprintf(NULL, 0, "%" PRIu16, val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%" PRIu16, val);
    return mv_str_doc(mv_string(str), a);
}

Document* pretty_u8(uint8_t val, Allocator* a) {
    int len = snprintf(NULL, 0, "%" PRIu8, val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%" PRIu8, val);
    return mv_str_doc(mv_string(str), a);
}

Document* pretty_ptr(void* val, Allocator* a) {
    int len = snprintf(NULL, 0, "%p", val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%p", val);
    return mv_str_doc(mv_string(str), a);
}

Document* pretty_hex_u8(uint8_t val, Allocator* a) {
    int len = snprintf(NULL, 0, "%" PRIx8, val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%" PRIx8, val);
    return mv_str_doc(mv_string(str), a);
}
