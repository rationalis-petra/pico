#ifndef __APP_MODULE_LOAD
#define __APP_MODULE_LOAD

#include "data/stream.h"
#include "pico/values/modular.h"

//bool repl_iter(IStream* cin, OStream* cout, Allocator* a, Assembler* ass, Module* module, IterOpts opts);
void load_module_from_istream(IStream* in, OStream* err, Package* pkg_parent, Module* parent, Allocator* a);

#endif
