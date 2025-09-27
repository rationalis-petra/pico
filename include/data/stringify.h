#ifndef __DATA_STRINGIFY_H
#define __DATA_STRINGIFY_H

#include <stdint.h>
#include "data/float.h"
#include "data/string.h"

String string_i64(int64_t  val, Allocator* a);
String string_i32(int32_t  val, Allocator* a);
String string_i16(int16_t  val, Allocator* a);
String string_i8 (int8_t   val, Allocator* a);
String string_u64(uint64_t val, Allocator* a);
String string_u32(uint32_t val, Allocator* a);
String string_u16(uint16_t val, Allocator* a);
String string_u8 (uint8_t  val, Allocator* a);
String string_f32(float32_t val, Allocator* a);
String string_f64(float64_t val,  Allocator* a);
String string_ptr(void*    val, Allocator* a);

String string_hex_u8(uint8_t val, Allocator* a);
String string_hex_mem(const void* data, size_t memsize, Allocator* a);

String string_char(char val, Allocator* a);
String string_short(short val, Allocator* a);
String string_int(int val, Allocator* a);
String string_long(long val, Allocator* a);
String string_long_long(long long val, Allocator* a);

String string_uchar(unsigned char val, Allocator* a);
String string_ushort(unsigned short val, Allocator* a);
String string_uint(unsigned int val, Allocator* a);
String string_ulong(unsigned long val, Allocator* a);
String string_ulong_long(unsigned long long val, Allocator* a);

String string_float(float val, Allocator* a);
String string_double(double val, Allocator* a);


#endif
