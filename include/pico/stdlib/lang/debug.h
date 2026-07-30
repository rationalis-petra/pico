#ifndef __PICO_STDLIB_LANG_DEBUG_H
#define __PICO_STDLIB_LANG_DEBUG_H

#include "platform/memory/region.h"

#include "pico/codegen/codegen.h"
#include "pico/values/modular.h"

void add_debug_module(Target target, Module* lang, RegionAllocator* region);

#endif
