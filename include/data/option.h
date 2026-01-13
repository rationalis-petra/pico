#ifndef __DATA_OPTION_H
#define __DATA_OPTION_H

#include <stdint.h>

typedef enum Option_t {
    Some,
    None
} Option_t;

typedef struct {
    Option_t type;
    void* val;
} PtrOption;

typedef struct {
    Option_t type;
    uint8_t val;
} U8Option;

typedef struct {
    Option_t type;
    uint8_t val;
} U16Option;

typedef struct {
    Option_t type;
    uint8_t val;
} U32Option;

typedef struct {
    Option_t type;
    uint8_t val;
} U64Option;

typedef struct {
    Option_t type;
    uint8_t val;
} I8Option;

typedef struct {
    Option_t type;
    uint8_t val;
} I16Option;

typedef struct {
    Option_t type;
    uint8_t val;
} I32Option;

typedef struct {
    Option_t type;
    uint8_t val;
} I64Option;

#endif
