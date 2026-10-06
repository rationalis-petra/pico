#ifndef __RUNE_EVAL_EVAL_H
#define __RUNE_EVAL_EVAL_H

#include "platform/memory/region.h"

#include "rune/syntax/expression.h"
#include "rune/eval/values.h"

/**
 * Possible 
 */
typedef enum {
    AValue,
    AError,
} ReResultType;

typedef struct {
    ReResultType type;
    union {
        ValRef value;
        Document* error_message;
    };
} RuneEvalResult;

typedef struct {
    ExprPool* expr;
    ValueHeap* value;
} Pools;

typedef struct RuneEnv RuneEnv;

RuneEnv* mk_rune_env(Allocator* a);
void populate_std_builtins(RuneEnv* env);
void delete_rune_env(RuneEnv* env);

Pools get_pools(RuneEnv* env);

void rune_add_def(Name name, ExprRef expr, RuneEnv* env);
void rune_add_val_def(Name name, ValRef ref, RuneEnv* env);

/**
 * Evaluate the given expression in the environment.
 */
RuneEvalResult eval_rune(ExprRef expression, RuneEnv* env, RegionAllocator* region);

/**
 * Get the value of a definition that was previously added to the environment.
 * Note that definitions are not evaluated until 
 */
RuneEvalResult get_value(Name name, RuneEnv* env, RegionAllocator* region);

/**
 * Construct a document of the pretty values.
 */
Document* pretty_rune_value(ValRef ref, RuneEnv* env, Allocator* a);

#endif
