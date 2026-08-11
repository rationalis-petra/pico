#ifndef __PICO_STDLIB_CORE_DEV_H
#define __PICO_STDLIB_CORE_DEV_H

#include "platform/memory/region.h"

#include "pico/codegen/codegen.h"
#include "pico/values/modular.h"

void add_dev_module(Target target, Module* lang, RegionAllocator* region);

#endif
