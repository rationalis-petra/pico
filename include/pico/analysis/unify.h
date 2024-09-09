#ifndef __PICO_ANALYSIS_UNIFY_H
#define __PICO_ANALYSIS_UNIFY_H

#include "data/result.h"
#include "pico/values/types.h"

// Instiatiate uvars in the LHS and RHS so that they become equal
// perform occurence checking.
Result unify(PiType* lhs, PiType* rhs, Allocator* a);
bool has_unification_vars_p(PiType type);

// Return a deep copy of the original type, with all unification vars replaced
// by their subsitutions.
void squash_type(PiType* type);

#endif
