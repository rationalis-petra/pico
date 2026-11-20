#ifndef __RUNE_TYPECHECK_SOLVE_H
#define __RUNE_TYPECHECK_SOLVE_H

#include "platform/memory/region.h"

#include "rune/syntax/expression.h"
#include "rune/syntax/formula.h"

 
VarSubst* solve(Formula formula, RegionAllocator* region, Allocator* perm);

#endif
