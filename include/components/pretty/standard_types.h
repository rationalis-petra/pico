#ifndef __COMPONENTS_PRETTY_STANDARD_TYPES_H
#define __COMPONENTS_PRETTY_STANDARD_TYPES_H

#include "data/float.h"
#include "platform/memory/allocator.h"
#include "components/pretty/document.h"

Document* pretty_i64(int64_t  val, Allocator* a);
Document* pretty_i32(int32_t  val, Allocator* a);
Document* pretty_i16(int16_t  val, Allocator* a);
Document* pretty_i8 (int8_t  val,  Allocator* a);
Document* pretty_u64(uint64_t val, Allocator* a);
Document* pretty_u32(uint32_t val, Allocator* a);
Document* pretty_u16(uint16_t val, Allocator* a);
Document* pretty_u8 (uint8_t val,  Allocator* a);
Document* pretty_f32(float32_t val, Allocator* a);
Document* pretty_f64(float64_t val,  Allocator* a);
Document* pretty_ptr(void*    val, Allocator* a);

Document* pretty_hex_u8(uint8_t val, Allocator* a);

Document* pretty_char(char val, Allocator* a);
Document* pretty_short(short val, Allocator* a);
Document* pretty_int(int val, Allocator* a);
Document* pretty_long(long val, Allocator* a);
Document* pretty_long_long(long long val, Allocator* a);

Document* pretty_uchar(unsigned char val, Allocator* a);
Document* pretty_ushort(unsigned short val, Allocator* a);
Document* pretty_uint(unsigned int val, Allocator* a);
Document* pretty_ulong(unsigned long val, Allocator* a);
Document* pretty_ulong_long(unsigned long long val, Allocator* a);

Document* pretty_float(float val, Allocator* a);
Document* pretty_double(double val, Allocator* a);

#endif
