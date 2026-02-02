#ifndef __PICO_ANALYSIS_UNIFY_H
#define __PICO_ANALYSIS_UNIFY_H

#include "components/logging/structured_logging.h"

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

typedef struct {
    Allocator* a;
    PiAllocator* pia;
    Logger* logger;
} UnifyContext;

// Instantiate uvars in the LHS and RHS so that they become equal
// perform occurence checking.
// Note: this destructively mutates the type
UnifyResult unify(PiType* lhs, PiType* rhs, UnifyContext ctx);

// Copy uvar: note that this will create a separate copy, meaning
//   that updates to one will not affect another and vice-versa
UVarType* copy_uvar(UVarType* uvar, PiAllocator* pia);

PiType* try_get_uvar(UVarType* uvar);

// When typechecking, there may be a need to perform substitutions.
//  For example, given id : All [A] Proc [A] A applied to 3, i.e. (id 3),
//  it is necessary to substitute A for the type of 3. However, more generall
//  if we are applying a 'forall' function f, the type of this function may be
//  undecided, so we note in the unification variables representing its' type 
//  that we (in the future) will need to substitute a type variable for sometype.
void add_subst(UVarType* uvar, SymPtrAssoc binds, Allocator* a);

// Return true if and only if the type has uninstantiated unification variables
bool has_unification_vars_p(PiType type);

// Return a deep copy of the original type, with all unification vars replaced
// by their subsitutions, and all unification variables with default values
// instantiated with those defaults.
void squash_type(PiType* type, UnifyContext ctx);

PiType* mk_uvar(PiAllocator* a);
PiType* mk_uvar_integral(PiAllocator* a, Range range);
PiType* mk_uvar_floating(PiAllocator* a, Range range);

UnifyResult add_field_constraint(UVarType* uvar, Range range, Symbol field, PiType* field_ty, UnifyContext ctx);
UnifyResult add_variant_constraint(UVarType* uvar, Range range, Symbol field, AddrPiList types, UnifyContext ctx);

Document* pretty_uvar_type(UVarType* uvar, Allocator* a);


#endif
