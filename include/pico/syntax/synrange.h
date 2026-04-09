#ifndef __PICO_SYNTAX_SYNRANGE_H
#define __PICO_SYNTAX_SYNRANGE_H

#include <stdint.h>

#include "pico/data/range.h"
#include "pico/data/range_array.h"

typedef struct {
    Range args_range;
    RangeArray impl_ranges; 
    RangeArray arg_ranges; 
} ProcRange;

typedef struct {
    Range types_range;
    RangeArray type_ranges; 
} AllRange;

typedef struct {
    Range term;
    union {
        ProcRange proc;
        AllRange all;
    };
} SynRange;

ARRAY_HEADER(SynRange, synrange, SynRange);
typedef struct SynRangeArray* RangeTape;

#endif
