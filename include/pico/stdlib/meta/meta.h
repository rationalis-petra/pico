#ifndef __PICO_STDLIB_META_META_H
#define __PICO_STDLIB_META_META_H

#include "platform/memory/region.h"

#include "pico/values/modular.h"

//                Gen
//--------------------------------------
//

PiType* get_symbol_type();
PiType* get_syntax_type();
PiType* get_macro_result_type();

CType mk_syntax_ctype(PiAllocator* a);
CType mk_macro_result_ctype(PiAllocator* a);


//                Refl
//--------------------------------------
//

Module* get_std_current_module();
Module* set_std_current_module(Module* al);

Package* set_current_package(Package* current);
Package* get_current_package();

void add_meta_module(Assembler* ass, Package* base, RegionAllocator* region);

#endif
