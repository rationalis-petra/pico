#ifndef __ATLAS_INSTANCE_H
#define __ATLAS_INSTANCE_H

#include "data/stream.h"
#include "pico/data/string_array.h"

// ----------------------------------------------------------------------------
//    Atlas Instance
// ----------------------------------------------------------------------------
//  Store values
//
// ----------------------------------------------------------------------------


typedef struct {

} AtlasInstance;

void atlas_run(AtlasInstance* instance, String target);

#endif
