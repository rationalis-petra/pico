#ifndef __ATLAS_INSTANCE_H
#define __ATLAS_INSTANCE_H

#include "platform/memory/region.h"
#include "pico/data/error.h"
#include "atlas/syntax/stanza.h"

// ----------------------------------------------------------------------------
//    Atlas Instance
// ----------------------------------------------------------------------------
//  Store values
//
// ----------------------------------------------------------------------------


typedef struct AtlasInstance AtlasInstance;

AtlasInstance* make_atlas_instance(Allocator* a);
void delete_atlas_instance(AtlasInstance* instance);

void atlas_run(AtlasInstance* instance, String target, RegionAllocator* region, PiErrorPoint* point);

void add_library(Library library, AtlasInstance* instance);
void add_executable(Executable executable, AtlasInstance* instance);

#endif
