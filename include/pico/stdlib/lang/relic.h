#ifndef __PICO_STDLIB_LANG_RELIC_H
#define __PICO_STDLIB_LANG_RELIC_H

#include "platform/memory/region.h"

#include "pico/codegen/codegen.h"
#include "pico/values/modular.h"

void add_relic_module(Module* lang, RegionAllocator* region);

#endif
