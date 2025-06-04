#ifndef __PICO_ANALYSIS_UNIFY_H
#define __PICO_ANALYSIS_UNIFY_H

#include "data/result.h"
#include "pico/values/types.h"

typedef struct UVarType UVarType;

// Instantiate uvars in the LHS and RHS so that they become equal
// perform occurence checking.
// Note: this destructively mutates the type
Result unify(PiType* lhs, PiType* rhs, Allocator* a);

// Return true if and only if the type has uninstantiated unification variables
bool has_unification_vars_p(PiType type);

// Return a deep copy of the original type, with all unification vars replaced
// by their subsitutions, and all unification variables with default values
// instantiated with those defaults.
void squash_type(PiType* type);

PiType* mk_uvar(Allocator* a);
PiType* mk_uvar_integral(Allocator* a);
PiType* mk_uvar_floating(Allocator* a);

Result add_field_constraint(UVarType* uvar, Symbol field, PiType* field_ty, Allocator* a);


#endif
