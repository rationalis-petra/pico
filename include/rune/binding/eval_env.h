#ifndef __RUNE_BINDIING_EVAL_ENV_H
#define __RUNE_BINDIING_EVAL_ENV_H

#include "rune/eval/values.h"

typedef struct RuneEvalEnv RuneEvalEnv;

RuneEvalEnv* mk_eval_env(void* context, Allocator* a);
RuneEvalEnv* mk_closure_env(NameValAssoc closure_env, void* context, Allocator* a);
void delete_eval_env(RuneEvalEnv* env);

ValRef lookup(Name name, RuneEvalEnv* env);
ValRef insert(Name name, RuneEvalEnv* env);

NameValAssoc get_closure_env(Name name, RuneEvalEnv* env);

#endif
