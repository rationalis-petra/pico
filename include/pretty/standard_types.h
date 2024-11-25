#ifndef __PRETTY_STANDARD_TYPES_H
#define __PRETTY_STANDARD_TYPES_H

#include "platform/memory/allocator.h"
#include "pretty/document.h"

Document* pretty_i64(int64_t  val, Allocator* a);
Document* pretty_i32(int32_t  val, Allocator* a);
Document* pretty_i16(int16_t  val, Allocator* a);
Document* pretty_i8 (int8_t  val,  Allocator* a);
Document* pretty_u64(uint64_t val, Allocator* a);
Document* pretty_u32(uint32_t val, Allocator* a);
Document* pretty_u16(uint16_t val, Allocator* a);
Document* pretty_u8 (uint8_t val,  Allocator* a);
Document* pretty_ptr(void*    val, Allocator* a);

Document* pretty_hex_u8(uint8_t val, Allocator* a);

#endif
