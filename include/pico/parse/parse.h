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

/**
 * Pase the raw syntax for Relic 
 */
ParseResult parse_rawtree(IStream* is, PiAllocator* pia, Allocator* a);

/**
 * Pase the raw syntax for Rune. Rune has a very similar syntax tree to relic
 * (they share a representation). However, because memory management is
 * automatic, it has some extra features. These are: 
 * • String interpolation. Strings of the form "begin ~{expr} end" becomes the
 *   expression (join (join "begin" <expr>) "end")
 */
ParseResult parse_rune_rawtree(IStream* is, PiAllocator* pia, Allocator* a);

#endif
