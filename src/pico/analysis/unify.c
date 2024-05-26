#include "pico/analysis/unify.h"



pi_type* trace_uvar(pi_type* uvar);
result unify_eq(pi_type* lhs, pi_type* rhs, allocator a);

result unify(pi_type* lhs, pi_type* rhs, allocator a) {
    // Unification Implementation:
    // The LHS and RHS may contain unification variables
    // These are represented as a pair *(uid, type*) 
    // if the pointer is NULL, then the variable has not been instantiated.
    lhs = trace_uvar(lhs);
    rhs = trace_uvar(rhs);

    result out;

    // Note that this is left-biased: if lhs and RHS are both uvars, lhs is
    // instantiated to be the same as RHS
    if (lhs->sort == TUVar) {
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
        out.type = Err;
        out.error_message = mk_string("Unification failed: given two types of differen sort", a);
    }
    return out;
}

result unify_eq(pi_type* lhs, pi_type* rhs, allocator a) {
    result out;
    if (lhs->sort == TPrim && rhs->sort == TPrim) {
        if (lhs->prim == rhs->prim) {
            out.type = Ok;
            return out;
        } else {
            out.type = Err;
            out.error_message = mk_string("Unification failed: could not unify two unqueal primitives", a);
            return out;
        }
    } else if (lhs->sort == TProc && rhs->sort == TProc) {
        if (lhs->proc.args.len != rhs->proc.args.len) {
            out.type = Err;
            out.error_message = mk_string("Unification failed: two different procedures of differing types", a);
            return out;
        }

        // Unify each argumet
        for (size_t i = 0; i < lhs->proc.args.len; i++) {
            out = unify(lhs->proc.args.data[i], rhs->proc.args.data[i], a);
            if (out.type == Err) return out;
        }

        // Unify the return values
        return unify(lhs->proc.ret, rhs->proc.ret, a);
        
    } else {
        out.type = Err;
        out.error_message = mk_string("Unification failed: types have different forms", a);
        return out;
    }
}

bool has_unification_vars_p(pi_type type) {
    // only return t if uvars don't go anywhere
    switch (type.sort) {
    case TPrim:
        return false;
    case TProc: {
        for (size_t i = 0; i < type.proc.args.len; i++) {
            if (has_unification_vars_p(*(pi_type*)type.proc.args.data[i]))
                return true;
        }
        return has_unification_vars_p(*type.proc.ret);
        break;
    }

    // Special sort: unification variable
    case TUVar:
        if (type.uvar->subst == NULL) {
            return true;
        } else {
            return has_unification_vars_p(*type.uvar->subst);
        }
    default:
        return true;
    }
}

pi_type* trace_uvar(pi_type* uvar) {
    while (uvar->sort == TUVar && uvar->uvar->subst != NULL) {
        uvar = uvar->uvar->subst;
    } 
    return uvar;
}


void squash_type(pi_type* type) {
    switch (type->sort) {
    case TPrim:
        break;
    case TProc: {
        for (size_t i = 0; i < type->proc.args.len; i++) {
            squash_type((pi_type*)type->proc.args.data + i);
        }
        squash_type(type->proc.ret);
        break;
    }

    // Special sort: unification variable
    case TUVar:
        if (type->uvar->subst != NULL) {
            squash_type(type->uvar->subst);
            *type = *type->uvar->subst;
        } 
    }
}

//pi_type* copy_type(pi_type* type, allocator a) {}
