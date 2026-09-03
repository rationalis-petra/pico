#ifndef __DATA_OPTION_H
#define __DATA_OPTION_H

#include <stdint.h>
#include "data/string.h"

typedef enum Option_t : uint64_t {
    None,
    Some
} Option_t;

#define OPTION_TYPE(otype, prefix) typedef struct { Option_t type; otype val;} prefix##Option;

OPTION_TYPE(void*, Ptr);
OPTION_TYPE(uint8_t, U8);
OPTION_TYPE(uint16_t, U16);
OPTION_TYPE(uint32_t, U32);
OPTION_TYPE(uint64_t, U64);

OPTION_TYPE(int8_t, I8);
OPTION_TYPE(int16_t, I16);
OPTION_TYPE(int32_t, I32);
OPTION_TYPE(int64_t, I64);

OPTION_TYPE(String, String);

#endif
