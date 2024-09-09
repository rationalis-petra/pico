#ifndef __PICO_PARSE_PARSE_H
#define __PICO_PARSE_PARSE_H

#include "data/stream.h"
#include "pico/syntax/concrete.h"

typedef struct SourcePos {
    size_t row;
    size_t col;
} SourcePos;

typedef struct SourceRange {
    SourcePos start;
    SourcePos end;
} SourceRange;

typedef enum ParseResult_t {
    ParseSuccess, 
    ParseFail
} ParseResult_t;

typedef struct ParseResult {
    ParseResult_t type;
    union {
        SourceRange range;
        RawTree result;
    } data;
} ParseResult;

ParseResult parse_rawtree(IStream* is, Allocator* a);

#endif
