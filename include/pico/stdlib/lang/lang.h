#ifndef __PICO_STDLIB_LANG_H
#define __PICO_STDLIB_LANG_H

#include "platform/memory/region.h"

#include "pico/codegen/codegen.h"
#include "pico/values/modular.h"

Module* add_lang_module(Assembler* ass, Target target, Package* base, RegionAllocator* region);

void populate_lang_module_extras(Module* lang, Assembler* ass, RegionAllocator* region);

#endif
