#ifndef __PICO_PARSE_PARSE_H
#define __PICO_PARSE_PARSE_H

#include "data/stream.h"
#include "pico/syntax/concrete.h"

typedef struct sourcepos {
    size_t row;
    size_t col;
} sourcepos;

typedef struct sourcerange {
    sourcepos start;
    sourcepos end;
} sourcerange;

typedef enum parse_result_type {
    ParseSuccess, 
    ParseFail
} parse_result_type;

typedef struct parse_result {
    parse_result_type type;
    union {
        sourcerange range;
        ob_rawtree result;
    } data;
} parse_result;

parse_result parse_rawtree(istream* is, allocator a);

#endif
