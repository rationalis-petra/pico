#ifndef __PICO_DATA_SYM_PTR_ASSOC_H
#define __PICO_DATA_SYM_PTR_ASSOC_H

#include "data/meta/array_header.h"
#include "pico/values/values.h"

typedef struct {
    Symbol sym;
    void* ptr;
} SymPtr;

ARRAY_HEADER(SymPtr, sym_ptr, SymPtr)

#endif
