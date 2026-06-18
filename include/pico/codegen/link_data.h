#ifndef __COMPONENTS_ASSEMBLER_LINK_DATA_H
#define __COMPONENTS_ASSEMBLER_LINK_DATA_H

// TODO (STRUCTURE): 'components' should not include 'pico' specific files
#include "pico/values/types.h"
#include "pico/data/sym_sarr_amap.h"

typedef struct {
    size_t source_offset;
    size_t dest_offset;
} LinkMetaData;

ARRAY_HEADER(LinkMetaData, link_meta, LinkMeta)

typedef struct {
    uint64_t fn_start;
    uint64_t defsite;

    // The type of the proc when the wrapped instance type params are added,
    // i.e. it is the 'true' type of the callee function.
    PiType* inner_type;

    // The type of the proc when that is exposed, i.e. instantiating the type parameters
    // of the inner type with specific types, and hiding the implicits.
    PiType* closure_type;
} ClosureLink;

ClosureLink copy_closure_link(ClosureLink link, PiAllocator* a);
void delete_closure_link(ClosureLink link, PiAllocator* a);

ARRAY_HEADER(ClosureLink, closure_link, ClosureLink)

// See modular.h for an explanation of fields
typedef struct {
    SymSArrAMap external_code_links;

    LinkMetaArray ec_links;
    LinkMetaArray ed_links;

    LinkMetaArray cc_links;
    LinkMetaArray cd_links;

    LinkMetaArray dd_links;

    // Instance Information
    ClosureLinkArray closure_links;
} LinkData;

#endif
