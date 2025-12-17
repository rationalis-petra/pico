#ifndef __APP_MODULE_LOAD
#define __APP_MODULE_LOAD

#include "data/stream.h"
#include "platform/memory/region.h"

#include "pico/values/modular.h"

void load_module_from_istream(IStream* in, FormattedOStream* err, String filename, Package* package, Module* parent, PiAllocator module_allocator, RegionAllocator* region);
void run_script_from_istream(IStream* in, FormattedOStream* err, String filename, Module* current, RegionAllocator* region);

#endif
