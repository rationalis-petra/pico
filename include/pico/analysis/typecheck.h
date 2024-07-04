#ifndef __PICO_ANALYSIS_TYPECHECK_H
#define __PICO_ANALYSIS_TYPECHECK_H

#include "data/result.h"
#include "pico/binding/environment.h"
#include "pico/syntax/syntax.h"

result type_check(toplevel* untyped, environment* env, allocator a);

result type_check_expr(syntax* untyped, pi_type type, environment* env, allocator a);
result type_infer_expr(syntax* untyped, environment* env, allocator a);

#endif
