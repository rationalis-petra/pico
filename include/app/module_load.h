#ifndef __APP_MODULE_LOAD
#define __APP_MODULE_LOAD

#include "data/stream.h"
#include "platform/memory/region.h"

#include "pico/values/modular.h"

//bool repl_iter(IStream* cin, OStream* cout, Allocator* a, Assembler* ass, Module* module, IterOpts opts);
void load_module_from_istream(IStream* in, FormattedOStream* err, const char* filename, Package* package, Module* parent, PiAllocator module_allocator, RegionAllocator* region);
void run_script_from_istream(IStream* in, FormattedOStream* err, const char* filename, Module* current, RegionAllocator* region);

#endif
