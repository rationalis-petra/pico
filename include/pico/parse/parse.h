#ifndef __PICO_PARSE_PARSE_H
#define __PICO_PARSE_PARSE_H

#include "data/stream.h"
#include "pico/data/error.h"
#include "pico/syntax/concrete.h"

typedef enum ParseResult_t {
    ParseSuccess, 
    ParseNone, 
    ParseFail
} ParseResult_t;

typedef struct ParseResult {
    ParseResult_t type;
    union {
        PicoError error;
        RawTree result;
    };
} ParseResult;

ParseResult parse_rawtree(IStream* is, PiAllocator* pia, Allocator* a);

#endif
