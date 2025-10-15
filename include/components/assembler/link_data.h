#ifndef __COMPONENTS_ASSEMBLER_LINK_DATA_H
#define __COMPONENTS_ASSEMBLER_LINK_DATA_H

// TODO (STRUCTURE): 'components' should not include 'pico' specific files
#include "pico/data/sym_sarr_amap.h"

typedef struct {
    size_t source_offset;
    size_t dest_offset;
} LinkMetaData;

ARRAY_HEADER(LinkMetaData, link_meta, LinkMeta)

// See modular.h for an explanation of fields

typedef struct {
    SymSArrAMap external_code_links;

    LinkMetaArray ec_links;
    LinkMetaArray ed_links;

    LinkMetaArray cc_links;
    LinkMetaArray cd_links;

    LinkMetaArray dd_links;
} LinkData;

#endif
