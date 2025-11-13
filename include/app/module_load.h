#ifndef __APP_MODULE_LOAD
#define __APP_MODULE_LOAD

#include "data/stream.h"
#include "pico/values/modular.h"

//bool repl_iter(IStream* cin, OStream* cout, Allocator* a, Assembler* ass, Module* module, IterOpts opts);
void load_module_from_istream(IStream* in, FormattedOStream* err, const char* filename, Package* package, Module* parent, PiAllocator pico_alloc);
void run_script_from_istream(IStream* in, FormattedOStream* err, const char* filename, Module* current, Allocator* a);

#endif
