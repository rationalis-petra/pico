#ifndef __RUNE_TYPECHECK_SUBST_H
#define __RUNE_TYPECHECK_SUBST_H

#include "rune/syntax/expression.h"

void expr_subst(VarSubst* subst, ExprRef expr);

#endif
