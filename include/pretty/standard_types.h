#ifndef __PRETTY_STANDARD_TYPES_H
#define __PRETTY_STANDARD_TYPES_H

#include "memory/allocator.h"
#include "pretty/document.h"

document* pretty_i64(int64_t val, allocator a);
document* pretty_u64(uint64_t val, allocator a);

document* pretty_hex_u8(uint8_t val, allocator a);

#endif
