#ifndef __PLATFORM_DYNAMIC_LIBRARY_H
#define __PLATFORM_DYNAMIC_LIBRARY_H

#include "data/string.h"
#include "data/result.h"

typedef struct DynLib DynLib;

Result open_lib(DynLib** out, String path, Allocator* a);
void close_lib(DynLib* lib);

Result lib_sym(void** out, DynLib* lib, String symbol);

#endif
