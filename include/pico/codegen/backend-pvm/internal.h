#ifndef __PICO_CODEGEN_BACKEND_PVM_INTERNAL_H
#define __PICO_CODEGEN_BACKEND_PVM_INTERNAL_H

#include "pico/data/sym_sarr_amap.h"
#include "pico/values/modular.h"

typedef struct {
    SymSArrAMap gotolinks;
    LinkData links;
} InternalLinkData;

#endif
