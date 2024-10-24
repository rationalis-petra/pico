#include "platform/signals.h"
#include "pico/analysis/unify.h"

PiType* trace_uvar(PiType* uvar);
Result unify_eq(PiType* lhs, PiType* rhs, Allocator* a);
Result assert_maybe_integral(PiType* type);

Result unify(PiType* lhs, PiType* rhs, Allocator* a) {
    // Unification Implementation:
    // The LHS and RHS may contain unification variables
    // These are represented as a pair *(uid, type*) 
    // if the pointer is NULL, then the variable has not been instantiated.
    lhs = trace_uvar(lhs);
    rhs = trace_uvar(rhs);

    Result out;

    // Note that this is left-biased: if lhs and RHS are both uvars, lhs is
    // instantiated to be the same as RHS
    if (lhs->sort == TUVarDefaulted) {
        out = assert_maybe_integral(rhs);
        if (out.type == Ok) lhs->uvar->subst = rhs;
    }
    else if (rhs->sort == TUVarDefaulted) {
        out = assert_maybe_integral(lhs);
        if (out.type == Ok) rhs->uvar->subst = lhs;
    }
    else if (lhs->sort == TUVar) {
        lhs->uvar->subst = rhs;
        out.type = Ok;
    }
    else if (rhs->sort == TUVar) {
        rhs->uvar->subst = lhs;
        out.type = Ok;
    }
    else if (rhs->sort == lhs->sort)
        out = unify_eq(lhs, rhs, a);
    else {
        out = (Result) {
            .type = Err,
            .error_message = mv_string("Unification failed: given two types of differen sort"),
        };
    }
    return out;
}

Result unify_eq(PiType* lhs, PiType* rhs, Allocator* a) {
    if (lhs->sort == TPrim && rhs->sort == TPrim) {
        if (lhs->prim == rhs->prim) {
            return (Result) {.type = Ok};
        } else {
            return (Result) {
                .type = Err,
                .error_message = mk_string("Unification failed: could not unify two unqueal primitives", a),
            };
        }
    } else if (lhs->sort == TProc && rhs->sort == TProc) {
        if (lhs->proc.args.len != rhs->proc.args.len) {
            return (Result) {
                .type = Err,
                .error_message = mk_string("Unification failed: two different procedures of differing types", a)
            };
        }

        // Unify each argumet
        for (size_t i = 0; i < lhs->proc.args.len; i++) {
            Result out = unify(lhs->proc.args.data[i], rhs->proc.args.data[i], a);
            if (out.type == Err) return out;
        }

        // Unify the return values
        return unify(lhs->proc.ret, rhs->proc.ret, a);
        
    } else if (lhs->sort == TStruct && rhs->sort == TStruct) {
        if (lhs->structure.fields.len != rhs->structure.fields.len) {
            return (Result) {
                .type = Err,
                .error_message = mk_string("Unification failed: two different structures with differing number of fields.", a)
            };
        }

        for (size_t i = 0; i < lhs->structure.fields.len; i++) {

        }
        return (Result) {.type = Ok};
    } else {
        return (Result) {
            .type = Err,
            .error_message = mk_string("Unification failed: types have different forms", a),
        }
    }
}

Result assert_maybe_integral(PiType* type) {
    // Use a while loop to trace through uvars
    while (type) {
        // If it is a primtive, check it is integral type
        if (type->sort == TPrim && (type->prim & 0b0100)) {
            return (Result) {.type = Ok};
        } else if (type->sort == TUVar || type->sort == TUVarDefaulted) {
            // continue on to next iteration
            type = type->uvar->subst;
        } else {
            return (Result) {
                .type = Err,
                .error_message = mv_string("Cannot unify an integral type with a non-integral type!"),
            };
        }
    }
    return (Result) {.type = Ok};
}


bool has_unification_vars_p(PiType type) {
    // Only return t if uvars don't go anywhere
    switch (type.sort) {
    case TPrim:
        return false;
    case TProc: {
        for (size_t i = 0; i < type.proc.args.len; i++) {
            if (has_unification_vars_p(*(PiType*)type.proc.args.data[i]))
                return true;
        }
        return has_unification_vars_p(*type.proc.ret);
        break;
    }
    case TStruct: {
        for (size_t i = 0; i < type.structure.fields.len; i++) {
            if (has_unification_vars_p(*(PiType*)type.structure.fields.data[i].val))
                return true;
        }
        return false;
        break;
    }
    case TEnum: {
        for (size_t i = 0; i < type.enumeration.variants.len; i++) {
            PtrArray types = *(PtrArray*)type.enumeration.variants.data[i].val;
            for (size_t j = 0; j < types.len; j++) {
                if (has_unification_vars_p(*(PiType*)types.data[j]))
                    return true;
            }
        }
        return false;
        break;
    }

    case TVar: return false;
    
    case TAll: {
        return has_unification_vars_p(*type.binder.body);
    }

    case TKind: return false;

    // Special sort: unification variable
    case TUVar:
        if (type.uvar->subst == NULL) {
            return true;
        } else {
            return has_unification_vars_p(*type.uvar->subst);
        }

    case TUVarDefaulted: return false;

    default:
        panic(mv_string("Invalid type given to has_unification_vars_p"));
    }
}

PiType* trace_uvar(PiType* uvar) {
    while ((uvar->sort == TUVar || uvar->sort == TUVarDefaulted) && uvar->uvar->subst != NULL) {
        uvar = uvar->uvar->subst;
    } 
    return uvar;
}


void squash_type(PiType* type) {
    switch (type->sort) {
    case TPrim:
        break;
    case TProc: {
        for (size_t i = 0; i < type->proc.args.len; i++) {
            squash_type((PiType*)(type->proc.args.data[i]));
        }
        squash_type(type->proc.ret);
        break;
    }
    case TStruct: {
        for (size_t i = 0; i < type->structure.fields.len; i++) {
            squash_type((PiType*)((type->structure.fields.data + i)->val));
        }
        break;
    }
    case TEnum: {
        for (size_t i = 0; i < type->enumeration.variants.len; i++) {
            PtrArray types = *(PtrArray*)type->enumeration.variants.data[i].val;
            for (size_t j = 0; j < types.len; j++) {
                squash_type((PiType*)types.data[j]);
            }
        }
        break;
    }
    case TVar: break;
    case TAll: {
        squash_type(type->binder.body);
    }

    case TKind: break;
    // Special sort: unification variable
    case TUVar: {
        PiType* subst = type->uvar->subst;
        if (subst) {
            squash_type(subst);
            *type = *subst;
        } 
        break;
    }
    case TUVarDefaulted: {
        PiType* subst = type->uvar->subst;
        if (subst) {
            squash_type(subst);
            *type = *subst;
        } else {
            *type = (PiType) {.sort = TPrim, .prim = Int_64,};
        }
        break;
    }
    default: 
        panic(mv_string("squash_type received invalid type!"));
    }
}
