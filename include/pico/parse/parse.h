#ifndef __PICO_PARSE_PARSE_H
#define __PICO_PARSE_PARSE_H

#include "data/stream.h"
#include "pico/syntax/concrete.h"

typedef enum ParseResult_t {
    ParseSuccess, 
    ParseNone, 
    ParseFail
} ParseResult_t;

typedef struct {
    String message;
    Range range;
} ParseError;

typedef struct ParseResult {
    ParseResult_t type;
    union {
        ParseError error;
        RawTree result;
    } data;
} ParseResult;

ParseResult parse_rawtree(IStream* is, Allocator* a);

#endif
