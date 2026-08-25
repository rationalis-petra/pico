#ifndef __PICO_TYPECHECK_UNIFY_ERRORS_H
#define __PICO_TYPECHECK_UNIFY_ERRORS_H

#include "pico/typecheck/unify.h"
#include "pico/data/range.h"
#include "pico/values/types.h"

// TCApp
UnifyResult unify_app_err_no_args(PiType* app, PiType* val, UnifyContext ctx);
UnifyResult unify_app_err_unequal_arglen(PiType* app, PiType* val, UnifyContext ctx);

// Enum
UnifyResult unify_error_variant_name_mismatch(Symbol lhs, Symbol rhs, UnifyContext ctx);

// Named
UnifyResult unify_error_name_has_args_match(PiType* lhs, PiType* rhs, Allocator* a);

#endif
