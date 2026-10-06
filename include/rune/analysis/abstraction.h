#ifndef __RUNE_ANALYSIS_ABSTRACTION_H
#define __RUNE_ANALYSIS_ABSTRACTION_H

#include "platform/memory/region.h"

#include "pico/data/error.h"
#include "pico/syntax/concrete.h"
#include "rune/syntax/expression.h"

/**
 * A set of builtin syntax that can be provided by the host which is embedding Rune.
 */
typedef struct HostCallbackData HostCallbackData;

typedef void HostCallback(RawTree raw, HostCallbackData host_data, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point, void* closure_data, Expr* out);
struct HostCallbackData {
    HostCallback* abstract;   
    NameArray names;
    void* closure_data;
};

ExprRef abstract_rune_expr(RawTree raw, HostCallbackData host_data, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point);
void abstract_rune_to(RawTree raw, HostCallbackData host_data, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point, Expr* out);
Def abstract_rune_def(RawTree raw, HostCallbackData host_data, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point);

#endif
