#include <inttypes.h>
#include <stdio.h>

#include "data/stringify.h"

String string_i64(int64_t val, Allocator* a) {
    int len = snprintf(NULL, 0, "%" PRId64, val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%" PRId64, val);
    return mv_string(str);
}

String string_i32(int32_t val, Allocator* a) {
    int len = snprintf(NULL, 0, "%" PRId32, val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%" PRId32, val);
    return mv_string(str);
}

String string_i16(int16_t val, Allocator* a) {
    int len = snprintf(NULL, 0, "%" PRId16, val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%" PRId16, val);
    return mv_string(str);
}

String string_i8(int8_t val, Allocator* a) {
    int len = snprintf(NULL, 0, "%" PRId8, val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%" PRId8, val);
    return mv_string(str);
}

String string_u64(uint64_t val, Allocator* a) {
    int len = snprintf(NULL, 0, "%" PRIu64, val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%" PRIu64, val);
    return mv_string(str);
}

String string_u32(uint32_t val, Allocator* a) {
    int len = snprintf(NULL, 0, "%" PRIu32, val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%" PRIu32, val);
    return mv_string(str);
}

String string_u16(uint16_t val, Allocator* a) {
    int len = snprintf(NULL, 0, "%" PRIu16, val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%" PRIu16, val);
    return mv_string(str);
}

String string_u8(uint8_t val, Allocator* a) {
    int len = snprintf(NULL, 0, "%" PRIu8, val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%" PRIu8, val);
    return mv_string(str);
}

String string_ptr(void* val, Allocator* a) {
    int len = snprintf(NULL, 0, "%p", val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%p", val);
    return mv_string(str);
}

String string_f32(float32_t val, Allocator *a) {
    int len = snprintf(NULL, 0, "%f", val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%f", val);
    return mv_string(str);
}

String string_f64(float64_t val, Allocator *a) {
    int len = snprintf(NULL, 0, "%lf", val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%lf", val);
    return mv_string(str);
}

String string_hex_u8(uint8_t val, Allocator* a) {
    int len = snprintf(NULL, 0, "%" PRIx8, val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%" PRIx8, val);
    return mv_string(str);
}

String string_char(char val, Allocator *a) {
    int len = snprintf(NULL, 0, "%d", (int)val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%d", (int)val);
    return mv_string(str);
}

String string_short(short val, Allocator *a) {
    int len = snprintf(NULL, 0, "%d", (int)val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%d", (int)val);
    return mv_string(str);
}

String string_int(int val, Allocator *a) {
    int len = snprintf(NULL, 0, "%d", val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%d", val);
    return mv_string(str);
}

String string_long(long val, Allocator *a) {
    int len = snprintf(NULL, 0, "%ld", val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%ld", val);
    return mv_string(str);
}

String string_long_long(long long val, Allocator *a) {
    int len = snprintf(NULL, 0, "%lld", val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%lld", val);
    return mv_string(str);
}

String string_uchar(unsigned char val, Allocator *a) {
    int len = snprintf(NULL, 0, "%u", (unsigned int)val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%u", (unsigned int)val);
    return mv_string(str);
}

String string_ushort(unsigned short val, Allocator *a) {
    int len = snprintf(NULL, 0, "%u", (unsigned int)val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%u", (unsigned int)val);
    return mv_string(str);
}

String string_uint(unsigned int val, Allocator *a) {
    int len = snprintf(NULL, 0, "%u", val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%u", val);
    return mv_string(str);
}

String string_ulong(unsigned long val, Allocator *a) {
    int len = snprintf(NULL, 0, "%lu", val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%lu", val);
    return mv_string(str);
}

String string_ulong_long(unsigned long long val, Allocator *a) {
    int len = snprintf(NULL, 0, "%llu", val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%llu", val);
    return mv_string(str);
}

String string_float(float val, Allocator *a) {
    int len = snprintf(NULL, 0, "%f", val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%f", val);
    return mv_string(str);
}

String string_double(double val, Allocator *a) {
    int len = snprintf(NULL, 0, "%lf", val) + 1;
    char* str = (char*)mem_alloc(sizeof(char) * len, a);
    snprintf(str, len, "%lf", val);
    return mv_string(str);
}
