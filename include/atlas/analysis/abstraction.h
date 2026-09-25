#ifndef __ATLAS_ANALYSIS_ABSTRACTION_H
#define __ATLAS_ANALYSIS_ABSTRACTION_H

#include "platform/memory/region.h"

#include "pico/data/error.h"
#include "pico/syntax/concrete.h"
#include "atlas/syntax/target_expr.h"
#include "atlas/syntax/project.h"
#include "rune/syntax/expression.h"

typedef enum : uint64_t {
    Left, Right
} MResult_t;

Def abstract_atlas_def(RawTree raw, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point);

void abstract_atlas_project(Project* project, ProjectRecord* record, RawTree raw, RegionAllocator* region, PiErrorPoint* point);

#endif
