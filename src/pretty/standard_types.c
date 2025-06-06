#include "pretty/standard_types.h"
#include "data/stringify.h"

Document* pretty_i64(int64_t val, Allocator* a) {
    return mv_str_doc(string_i64(val, a), a);
}

Document* pretty_i32(int32_t val, Allocator* a) {
    return mv_str_doc(string_i32(val, a), a);
}

Document* pretty_i16(int16_t val, Allocator* a) {
    return mv_str_doc(string_i16(val, a), a);
}

Document* pretty_i8(int8_t val, Allocator* a) {
    return mv_str_doc(string_i8(val, a), a);
}

Document* pretty_u64(uint64_t val, Allocator* a) {
    return mv_str_doc(string_u64(val, a), a);
}

Document* pretty_u32(uint32_t val, Allocator* a) {
    return mv_str_doc(string_u64(val, a), a);
}

Document* pretty_u16(uint16_t val, Allocator* a) {
    return mv_str_doc(string_u16(val, a), a);
}

Document* pretty_u8(uint8_t val, Allocator* a) {
    return mv_str_doc(string_u8(val, a), a);
}

Document* pretty_ptr(void* val, Allocator* a) {
    return mv_str_doc(string_ptr(val, a), a);
}

Document *pretty_f32(float32_t val, Allocator *a) {
    return mv_str_doc(string_f32(val, a), a);
}

Document *pretty_f64(float64_t val, Allocator *a) {
    return mv_str_doc(string_f64(val, a), a);
}

Document* pretty_hex_u8(uint8_t val, Allocator* a) {
    return mv_str_doc(string_hex_u8(val, a), a);
}

Document *pretty_char(char val, Allocator *a) {
    return mv_str_doc(string_char(val, a), a);
}

Document *pretty_short(short val, Allocator *a) {
    return mv_str_doc(string_short(val, a), a);
}

Document *pretty_int(int val, Allocator *a) {
    return mv_str_doc(string_int(val, a), a);
}

Document *pretty_long(long val, Allocator *a) {
    return mv_str_doc(string_long(val, a), a);
}

Document *pretty_long_long(long long val, Allocator *a) {
    return mv_str_doc(string_long_long(val, a), a);
}

Document *pretty_uchar(unsigned char val, Allocator *a) {
    return mv_str_doc(string_uchar(val, a), a);
}

Document *pretty_ushort(unsigned short val, Allocator *a) {
    return mv_str_doc(string_ushort(val, a), a);
}

Document *pretty_uint(unsigned int val, Allocator *a) {
    return mv_str_doc(string_uint(val, a), a);
}

Document *pretty_ulong(unsigned long val, Allocator *a) {
    return mv_str_doc(string_ulong(val, a), a);
}

Document *pretty_ulong_long(unsigned long long val, Allocator *a) {
    return mv_str_doc(string_ulong_long(val, a), a);
}

Document *pretty_float(float val, Allocator *a) {
    return mv_str_doc(string_float(val, a), a);
}

Document *pretty_double(double val, Allocator *a) {
    return mv_str_doc(string_double(val, a), a);
}
