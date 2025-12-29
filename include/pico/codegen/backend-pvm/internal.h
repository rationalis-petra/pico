#ifndef __PICO_CODEGEN_BACKEND_PVM_INTERNAL_H
#define __PICO_CODEGEN_BACKEND_PVM_INTERNAL_H

#include "pico/data/sym_sarr_amap.h"
#include "components/assembler/link_data.h"

typedef struct {
    SymSArrAMap gotolinks;
    LinkData links;
} InternalLinkData;

#endif
