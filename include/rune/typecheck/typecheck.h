#ifndef __RUNE_TYPECHECK_TYPECHECK_H
#define __RUNE_TYPECHECK_TYPECHECK_H

#include "platform/memory/region.h"

#include "rune/syntax/expression.h"

void typecheck_rune_expr(ExprRef expr, RegionAllocator* region, Allocator* perm);

#endif
