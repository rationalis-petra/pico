#include "platform/signals.h"

#include "rune/typecheck/typecheck.h"
#include "rune/typecheck/solve.h"
#include "rune/typecheck/subst.h"

typedef struct {
    FormulaPool* pool;
    RegionAllocator* region;
} TypecheckCtx;

typedef struct {
    FormulaRef formula;
    ExprRef type;
} InferResult;


static FormulaRef typecheck_rune_i(ExprRef expr, TypecheckCtx ctx);
//static InferResult typeinfer_rune_i(ExprRef expr, TypecheckCtx ctx);

/** 
 * Typecheck a rune expression:
 */

void typecheck_rune_expr(ExprRef expr, RegionAllocator* region, Allocator* perm) {
    FormulaPool* pool = mk_formula_pool(perm);
    TypecheckCtx ctx = {
        .pool = pool,
        .region = region,
    };
    FormulaRef formula = typecheck_rune_i(expr, ctx);
    VarSubst* subst = solve(get_formula(formula, pool), region, perm);
    expr_subst(subst, expr);
}


/*
static InferResult typeinfer_rune_i(ExprRef expr, TypecheckCtx ctx) {
    panic(mv_string("TODO: implement inference for rune"));
}
*/

static FormulaRef typecheck_rune_i(ExprRef expr, TypecheckCtx ctx) {
    panic(mv_string("TODO: implement check for rune"));
}
