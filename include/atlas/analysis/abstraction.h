#ifndef __ATLAS_ANALYSIS_ABSTRACTION_H
#define __ATLAS_ANALYSIS_ABSTRACTION_H

#include "platform/memory/region.h"

#include "pico/data/error.h"
#include "pico/data/string_array.h"
#include "pico/syntax/concrete.h"
#include "atlas/syntax/project.h"
#include "rune/syntax/expression.h"

typedef enum : uint64_t {
    Left, Right
} MResult_t;

typedef struct {
    String path;
    ValRef target_type;
    ValRef lib_type;
    ValRef exec_type;
    ValRef generic_type;

    ValRef dependency_type;
} AtAbsCallbackData;

typedef enum {TLDef, TLRequire} ToplevelType;
typedef struct {
    ToplevelType type;
    union {
        Def def;
        StringArray requires;
    };
} Toplevel;

Toplevel abstract_atlas_toplevel(RawTree raw, AtAbsCallbackData cb_data, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point);

void abstract_atlas_project(Project* project, ProjectRecord* record, RawTree raw, RegionAllocator* region, PiErrorPoint* point);

#endif
