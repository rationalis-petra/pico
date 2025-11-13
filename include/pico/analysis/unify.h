#ifndef __PICO_ANALYSIS_UNIFY_H
#define __PICO_ANALYSIS_UNIFY_H

#include "pico/data/range.h"
#include "pico/values/types.h"

// Unification Model:
// Memory and Lifetimes:
// Any type added as part of a constraint or unification should not be deleted
// until the UVar has been squashed.
// All memory belonging to the Uvar, or for errors is allocated by 'a'. Any memory
// required for types is allocated by the 'PiAllocator'.

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
UnifyResult unify(PiType* lhs, PiType* rhs, PiAllocator* pia, Allocator* a);

// Add a substitution to a uvar - when the uvar is resolved, the substitutions
// are applied.
void add_subst(UVarType* uvar, SymPtrAssoc binds, Allocator* a);

// Return true if and only if the type has uninstantiated unification variables
bool has_unification_vars_p(PiType type);

// Return a deep copy of the original type, with all unification vars replaced
// by their subsitutions, and all unification variables with default values
// instantiated with those defaults.
void squash_type(PiType* type, PiAllocator* pia, Allocator* a);

PiType* mk_uvar(PiAllocator* a);
PiType* mk_uvar_integral(PiAllocator* a, Range range);
PiType* mk_uvar_floating(PiAllocator* a, Range range);

UnifyResult add_field_constraint(UVarType* uvar, Range range, Symbol field, PiType* field_ty, PiAllocator* pia, Allocator* a);
UnifyResult add_variant_constraint(UVarType* uvar, Range range, Symbol field, AddrPiList types, PiAllocator* pia, Allocator* a);


#endif
