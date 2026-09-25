#ifndef __RUNE_EVAL_EVAL_H
#define __RUNE_EVAL_EVAL_H

#include "rune/syntax/expression.h"
#include "rune/eval/values.h"

typedef struct {
    ValRef value;
    uint64_t action_tag;
    ValRef continuation;
} EvalResult;

typedef struct GlobalEnv GlobalEnv;

typedef struct EvalCtx EvalCtx;

EvalCtx* create_eval_ctx(Name name, Expr expr, EvalCtx* ctx);

void add_rune_def(Name name, ExprRef expr, EvalCtx* ctx);
void eval_ctx(EvalCtx* ctx);
EvalResult eval(ExprRef expression, EvalCtx ctx);

#endif
