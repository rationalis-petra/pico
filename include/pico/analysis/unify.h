#ifndef __PICO_ANALYSIS_UNIFY_H
#define __PICO_ANALYSIS_UNIFY_H

#include "pico/data/range.h"
#include "pico/values/types.h"

typedef struct UVarType UVarType;

typedef enum {
    UOk,
    USimpleError,
    UConstraintError,
} UnifyResultType;

typedef struct {
    UnifyResultType type;
    Range initial;
    Document* message;
} UnifyResult;

// Instantiate uvars in the LHS and RHS so that they become equal
// perform occurence checking.
// Note: this destructively mutates the type
UnifyResult unify(PiType* lhs, PiType* rhs, Allocator* a);

// Return true if and only if the type has uninstantiated unification variables
bool has_unification_vars_p(PiType type);

// Return a deep copy of the original type, with all unification vars replaced
// by their subsitutions, and all unification variables with default values
// instantiated with those defaults.
void squash_type(PiType* type, Allocator* a);

PiType* mk_uvar(Allocator* a);
PiType* mk_uvar_integral(Allocator* a);
PiType* mk_uvar_floating(Allocator* a);

UnifyResult add_field_constraint(UVarType* uvar, Range range, Symbol field, PiType* field_ty, Allocator* a);
UnifyResult add_variant_constraint(UVarType* uvar, Range range, Symbol field, PtrArray types, Allocator* a);


#endif
