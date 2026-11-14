#ifndef __RUNE_ANALYSIS_ELABORATION_H
#define __RUNE_ANALYSIS_ELABORATION_H

#include "platform/memory/region.h"

#include "pico/data/error.h"
#include "rune/syntax/rich_expr.h"
#include "rune/syntax/expression.h"

ExprRef elaborate_rune_expr(RExprRef rich, RExprPool* rpool, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point);
Def elaborate_rune_def(RDef rich, RExprPool* pool, ExprPool* out_pool, RegionAllocator* region, PiErrorPoint* point);

#endif
