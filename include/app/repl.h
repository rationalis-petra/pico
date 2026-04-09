#ifndef __APP_REPL
#define __APP_REPL

#include <stdbool.h>

#include "platform/memory/region.h"

#include "pico/values/modular.h"

typedef struct {
    bool debug_print;
    bool is_eval;
} IterOpts;

// Repl Iter will run a single iteration of a read-eval-print loop, based 
// 
bool repl_iter(Allocator* stdalloc, RegionAllocator* region, Allocator* exec, Module* module, IterOpts opts);
bool noninteractive_repl_iter(Allocator* stdalloc, RegionAllocator* region, Allocator* exec, Module* module, IterOpts opts);

#endif
