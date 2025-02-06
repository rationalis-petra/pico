#include "platform/signals.h"
#include "pretty/string_printer.h"

#include "pico/analysis/unify.h"

PiType* trace_uvar(PiType* uvar);

// Unify two types such they are equal. Assumes they have the same sort
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
    // Shortcut: if lhs == rhs, then the types are identical and no work needs
    // to be done.
    if (lhs == rhs) {
        return (Result) {.type = Ok};
    }

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
    else if (lhs->sort == TUVarDefaulted) {
        out = assert_maybe_integral(rhs);
        if (out.type == Ok) lhs->uvar->subst = rhs;
    }
    else if (rhs->sort == TUVarDefaulted) {
        out = assert_maybe_integral(lhs);
        if (out.type == Ok) rhs->uvar->subst = lhs;
    }
    else if (rhs->sort == lhs->sort)
        out = unify_eq(lhs, rhs, a);
    else {
        PtrArray nodes = mk_ptr_array(8, a);
        push_ptr(mk_str_doc(mv_string("Unification failed: given two non-unifiable types"), a), &nodes);
        push_ptr(pretty_type(lhs, a), &nodes);
        push_ptr(mk_str_doc(mv_string("and"), a), &nodes);
        push_ptr(pretty_type(rhs, a), &nodes);

        out = (Result) {
            .type = Err,
            .error_message = doc_to_str(mv_sep_doc(nodes, a), a),
        };
    }
    return out;
}

Result unify_eq(PiType* lhs, PiType* rhs, Allocator* a) {
    switch (lhs->sort) {
    case TPrim: {
        if (lhs->prim == rhs->prim) {
            return (Result) {.type = Ok};
        } else {
            PtrArray nodes = mk_ptr_array(8, a);
            push_ptr(mk_str_doc(mv_string("Unification failed: could not unify unequal primitives"), a), &nodes);
            push_ptr(pretty_type(lhs, a), &nodes);
            push_ptr(mk_str_doc(mv_string("and"), a), &nodes);
            push_ptr(pretty_type(rhs, a), &nodes);

            return (Result) {
                .type = Err,
                .error_message = doc_to_str(mv_sep_doc(nodes, a), a),
            };
        }
        break;
    }
    case TProc: {
        if (lhs->proc.args.len != rhs->proc.args.len
            || lhs->proc.implicits.len != rhs->proc.implicits.len) {
            return (Result) {
                .type = Err,
                .error_message = mk_string("Unification failed: provided two different procedures with differing number of arguments or implicits.", a)
            };
        }

        for (size_t i = 0; i < lhs->proc.implicits.len; i++) {
            Result out = unify(lhs->proc.implicits.data[i], rhs->proc.implicits.data[i], a);
            if (out.type == Err) return out;
        }

        // Unify each argumet
        for (size_t i = 0; i < lhs->proc.args.len; i++) {
            Result out = unify(lhs->proc.args.data[i], rhs->proc.args.data[i], a);
            if (out.type == Err) return out;
        }

        // Unify the return values
        return unify(lhs->proc.ret, rhs->proc.ret, a);
        
        break;
    }
    case TStruct: {
        if (lhs->structure.fields.len != rhs->structure.fields.len) {
            return (Result) {
                .type = Err,
                .error_message = mk_string("Unification failed: two different structures with differing number of fields.", a)
            };
        }

        for (size_t i = 0; i < lhs->structure.fields.len; i++) {
            Symbol lhs_sym = lhs->structure.fields.data[i].key;
            PiType* lhs_ty = lhs->structure.fields.data[i].val;

            Symbol rhs_sym = rhs->structure.fields.data[i].key;
            PiType* rhs_ty = rhs->structure.fields.data[i].val;

            if (rhs_sym != lhs_sym) {
                return (Result) {
                    .type = Err,
                    .error_message = mk_string("Unification failed: RHS and LHS structures must have matching field-names.", a)
                };
            }

            Result out = unify(lhs_ty, rhs_ty, a);
            if (out.type == Err) return out;
        }

        return (Result) {.type = Ok,};
        break;
    }
    case TEnum: {
        if (lhs->enumeration.variants.len != rhs->enumeration.variants.len) {
            return (Result) {
                .type = Err,
                .error_message = mk_string("Unification failed: two different enums with differing number of variants.", a)
            };
        }

        for (size_t i = 0; i < lhs->enumeration.variants.len; i++) {
            Symbol lhs_sym = lhs->structure.fields.data[i].key;
            PtrArray lhs_args = *(PtrArray*) lhs->structure.fields.data[i].val;

            Symbol rhs_sym = lhs->structure.fields.data[i].key;
            PtrArray rhs_args = *(PtrArray*) lhs->structure.fields.data[i].val;

            if (rhs_sym != lhs_sym) {
                return (Result) {
                    .type = Err,
                    .error_message = mk_string("Unification failed: RHS and LHS enums must have matching variant-names.", a)
                };
            }

            if (lhs_args.len != rhs_args.len) {
                return (Result) {
                    .type = Err,
                    .error_message = mk_string("Unification failed: RHS and LHS enums-variants must have matching number of members.", a)
                };
            }

            for (size_t i = 0; i < lhs_args.len; i++) {
                Result out = unify(lhs_args.data[i], rhs_args.data[i], a);
                if (out.type == Err) return out;
            }
        }

        return (Result) {.type = Ok,};
        break;
    }
    case TReset: {
        Result out = unify(lhs->reset.in, rhs->reset.in, a);
        if (out.type == Err) return out;
        return unify(lhs->reset.out, rhs->reset.out, a);
        break;
    }
    case TDynamic: {
        return unify(lhs->dynamic, rhs->dynamic, a);
        break;
    }
    case TDistinct: {
        if (lhs->distinct.id != rhs->distinct.id || lhs->distinct.source_module != rhs->distinct.source_module) {
            return (Result) {
                .type = Err,
                .error_message = mk_string("Cannot Unify two distinct types of unequal IDs or source modules", a),
            };
        }

        // Note: we can assume that either LHS and RHS both have args or neither
        // do, as we have already checked they have the same IDs! (I think??)
        if (lhs->distinct.args) {
            Result res;
            PtrArray lhs_args = *lhs->distinct.args;
            PtrArray rhs_args = *rhs->distinct.args;
            for (size_t i = 0; i < lhs_args.len; i++) {
                res = unify(lhs_args.data[i], rhs_args.data[i], a);
                if (res.type == Err) return res;
            }
        }

        return unify(lhs->distinct.type, rhs->distinct.type, a);
        break;
    } case TKind: {
          if (lhs->kind.nargs == rhs->kind.nargs)
              return (Result) {.type = Ok};
          else 
              return (Result) {
                  .type = Err,
                  .error_message = mk_string("Cannot Unify two kinds of unequal nags", a),
              };
          break;
      }
    case TConstraint: {
        if (lhs->constraint.nargs == rhs->constraint.nargs)
            return (Result) {.type = Ok};
        else 
            return (Result) {
                .type = Err,
                .error_message = mk_string("Cannot Unify two constraints of unequal nags", a),
            };
        break;
    }
    case TVar: {
        // check they are the same var
        if (lhs->var != rhs->var) {
            return (Result) {
                .type = Err,
                .error_message = mk_string("Cannot Unify different type variables", a),
            };
        }
        return (Result) {.type = Ok} ;
        break;
    }
    default:  {
        PtrArray nodes = mk_ptr_array(8, a);
        push_ptr(mk_str_doc(mv_string("Unification failed: invalid types"), a), &nodes);
        push_ptr(pretty_type(lhs, a), &nodes);
        push_ptr(mk_str_doc(mv_string("and"), a), &nodes);
        push_ptr(pretty_type(rhs, a), &nodes);
        panic(doc_to_str(mv_sep_doc(nodes, a), a));
    } 
    }
}

Result assert_maybe_integral(PiType* type) {
    // Use a while loop to trace through uvars
    while (type) {
        // If it is a primtive, check it is integral type
        if (type->sort == TPrim && (type->prim < 0b1000)) {
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
        for (size_t i = 0; i < type.proc.implicits.len; i++) {
            if (has_unification_vars_p(*(PiType*)type.proc.implicits.data[i]))
                return true;
        }
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
    case TReset: {
        return has_unification_vars_p(*type.reset.in) || has_unification_vars_p(*type.reset.out);
    }
    case TDynamic: {
        return has_unification_vars_p(*type.dynamic);
    };
    case TDistinct: {
        return has_unification_vars_p(*type.distinct.type);
    }
    case TTrait: {
        return has_unification_vars_p(*type.distinct.type);
    }
    case TTraitInstance: {
        for (size_t i = 0; i < type.instance.args.len; i++) {
            if (has_unification_vars_p(*(PiType*)type.instance.args.data[i]))
                return true;
        }

        for (size_t i = 0; i < type.instance.fields.len; i++) {
            if (has_unification_vars_p(*(PiType*)type.instance.fields.data[i].val))
                return true;
        }
        return false;
    }
    case TVar: return false;
    
    case TAll: {
        return has_unification_vars_p(*type.binder.body);
    }
    case TFam: {
        return has_unification_vars_p(*type.binder.body);
    }

    case TKind: return false;
    case TConstraint: return false;

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
        for (size_t i = 0; i < type->proc.implicits.len; i++) {
            squash_type((PiType*)(type->proc.implicits.data[i]));
        }
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
    case TReset: {
        squash_type((PiType*)type->reset.in);
        squash_type((PiType*)type->reset.out);
        break;
    }
    case TDynamic: {
        squash_type((PiType*)type->dynamic);
        break;
    }
    case TVar: break;
    case TAll: 
    case TFam: {
        squash_type(type->binder.body);
        break;
    }
    case TDistinct: {
        squash_type(type->distinct.type);
        break;
    }
    case TTrait: {
        // TODO (INVESTIGATE BUG): do we need to sqyash implicits also?
        for (size_t i = 0; i < type->structure.fields.len; i++) {
            squash_type((type->trait.fields.data + i)->val);
        }
        break;
    }
    case TTraitInstance: {
        for (size_t i = 0; i < type->instance.args.len; i++) {
            squash_type(type->instance.args.data[i]);
        }

        for (size_t i = 0; i < type->instance.fields.len; i++) {
            squash_type(type->instance.fields.data[i].val);
        }
        break;
    }

    case TKind: break;
    case TConstraint: break;
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
