#ifndef __ATLAS_ANALYSIS_ABSTRACTION_H
#define __ATLAS_ANALYSIS_ABSTRACTION_H

#include "platform/memory/region.h"

#include "pico/data/error.h"
#include "atlas/syntax/concrete.h"
#include "atlas/syntax/stanza.h"

typedef enum : uint64_t {
    Left, Right
} MResult_t;

Stanza abstract_atlas(RawAtlas raw, RegionAllocator* region, PiErrorPoint* point);

#endif
