#ifndef __ATLAS_PARSE_H
#define __ATLAS_PARSE_H

#include "data/stream.h"
#include "platform/memory/region.h"

#include "pico/data/error.h"
#include "atlas/syntax/concrete.h"

typedef enum AtParseResult_t {
    ParseSuccess, 
    ParseNone, 
    ParseFail
} AtParseResult_t;

typedef struct AtParseResult {
    AtParseResult_t type;
    union {
        PicoError error;
        RawAtlas result;
    };
} AtParseResult;

AtParseResult parse_atlas_project(IStream* is, RegionAllocator* a);

AtParseResult parse_atlas_defs(IStream* is, RegionAllocator* a);

#endif
