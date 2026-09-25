#ifndef __RUNE_ANALYSIS_ABSTRACTION_H
#define __RUNE_ANALYSIS_ABSTRACTION_H

#include "platform/memory/region.h"

#include "pico/data/error.h"
#include "pico/syntax/concrete.h"
#include "rune/syntax/expression.h"

typedef struct HostCallbackData HostCallbackData;
struct HostCallbackData {
    HostRef (*abstract)(RawTree raw, HostCallbackData host_data, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point);   
    NameArray names;
};

ExprRef abstract_rune_expr(RawTree raw, HostCallbackData host_data, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point);
Def abstract_rune_def(RawTree raw, HostCallbackData host_data, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point);

#endif
