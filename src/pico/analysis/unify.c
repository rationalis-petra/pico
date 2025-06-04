#include "platform/signals.h"
#include "pretty/string_printer.h"
#include "data/meta/array_header.h"
#include "data/meta/array_impl.h"

#include "pico/data/range.h"
#include "pico/analysis/unify.h"

// Handling of named types
// Unification destructively modifies uvars 
// Thus, simply, e.g. copying and then renaming types won't work.
// A rename-map has to deal with shadowing variables, e.g.
// (Name x (Name x x)) ?= (Name x (Name y x))
// For this reason, we need a solution satisfying
// • Does not copy or modify named types
// • Takes care of naming and shadowing
// The solution is an array (stack) of lhs & rhs symbols 
// 
// lhs_symbol
// rhs_symbol

typedef enum {
    NoDefault, Integral, Floating, Struct
} UVarDefault;

typedef enum {
    ConInt, ConFloat, ConField,
} ConstraintType;

typedef struct {
    Symbol name;
    PiType* type;
} FieldConstraint;

typedef struct {
    ConstraintType type;
    Range range;
    union {
        int64_t fits;
        FieldConstraint has_field;
    };
} Constraint;

ARRAY_HEADER(Constraint, constraint, Constraint)
ARRAY_COMMON_IMPL(Constraint, constraint, Constraint)

struct UVarType {
    PiType* subst;
    ConstraintArray constraints;
    UVarDefault default_behaviour;
};

typedef struct {
    Symbol lhs;
    Symbol rhs;
} SymPair;

int64_t cmp_sym_pair(SymPair s1, SymPair s2) {
    int64_t r1 = cmp_symbol(s1.lhs, s2.lhs);

    return r1 == 0 ? cmp_symbol(s1.rhs, s2.rhs) : r1;
}

ARRAY_HEADER(SymPair, sym_pair, SymPair)
ARRAY_CMP_IMPL(SymPair, cmp_sym_pair, sym_pair, SymPair)

typedef SymPair SymbolPair;

PiType* trace_uvar(PiType* uvar);

// Unify two types such they are equal. Assumes they have the same sort
Result unify_eq(PiType* lhs, PiType* rhs, SymPairArray* rename, Allocator* a);
Result unify_internal(PiType* lhs, PiType* rhs, SymPairArray* rename, Allocator* a);
Result uvar_subst(UVarType* uvar, PiType* type, Allocator* a);


Result unify(PiType* lhs, PiType* rhs, Allocator* a) {
    SymPairArray renames = mk_sym_pair_array(8, a);
    Result r = unify_internal(lhs, rhs, &renames, a);
    sdelete_sym_pair_array(renames);
    return r;
}

Result unify_internal(PiType* lhs, PiType* rhs, SymPairArray* rename, Allocator* a) {
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
        out = uvar_subst(lhs->uvar, rhs, a);
        if (out.type != Ok) return out;
    }
    else if (rhs->sort == TUVar) {
        out = uvar_subst(rhs->uvar, lhs, a);
        if (out.type != Ok) return out;
    }
    else if (rhs->sort == lhs->sort)
        out = unify_eq(lhs, rhs, rename, a);
    else {
        PtrArray nodes = mk_ptr_array(8, a);
        push_ptr(mk_str_doc(mv_string("Unification failed: given two non-unifiable types"), a), &nodes);
        push_ptr(pretty_type(lhs, a), &nodes);
        push_ptr(mk_str_doc(mv_string("and"), a), &nodes);
        push_ptr(pretty_type(rhs, a), &nodes);

        out = (Result) {
            .type = Err,
            .error_message = doc_to_str(mv_sep_doc(nodes, a), 80, a),
        };
    }
    return out;
}

bool var_eq(Symbol lhs, Symbol rhs, SymPairArray *rename) {
    // bound
    for (size_t i = 0; i < rename->len; i++) {
        size_t idx = rename->len - (i + 1);
        if (symbol_eq(lhs, rename->data[idx].lhs) && symbol_eq(rhs, rename->data[idx].rhs)) {
            return true;
        } else if (symbol_eq(lhs, rename->data[idx].lhs) || symbol_eq(rhs, rename->data[idx].rhs)) {
            return false;
        } 
    }

    // unbound
    return symbol_eq(lhs, rhs);
}

Result unify_eq(PiType* lhs, PiType* rhs, SymPairArray* rename, Allocator* a) {
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
                .error_message = doc_to_str(mv_sep_doc(nodes, a), 80, a),
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
            Result out = unify_internal(lhs->proc.implicits.data[i], rhs->proc.implicits.data[i], rename, a);
            if (out.type == Err) return out;
        }

        // Unify each argumet
        for (size_t i = 0; i < lhs->proc.args.len; i++) {
            Result out = unify_internal(lhs->proc.args.data[i], rhs->proc.args.data[i], rename, a);
            if (out.type == Err) return out;
        }

        // Unify the return values
        return unify_internal(lhs->proc.ret, rhs->proc.ret, rename, a);
        
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

            if (!symbol_eq(rhs_sym, lhs_sym)) {
                return (Result) {
                    .type = Err,
                    .error_message = mk_string("Unification failed: RHS and LHS structures must have matching field-names.", a)
                };
            }

            Result out = unify_internal(lhs_ty, rhs_ty, rename, a);
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

            if (!symbol_eq(rhs_sym, lhs_sym)) {
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
                Result out = unify_internal(lhs_args.data[i], rhs_args.data[i], rename, a);
                if (out.type == Err) return out;
            }
        }

        return (Result) {.type = Ok,};
        break;
    }
    case TReset: {
        Result out = unify_internal(lhs->reset.in, rhs->reset.in, rename, a);
        if (out.type == Err) return out;
        return unify_internal(lhs->reset.out, rhs->reset.out, rename, a);
        break;
    }
    case TDynamic: {
        return unify_internal(lhs->dynamic, rhs->dynamic, rename, a);
        break;
    }
    case TNamed: {
      SymPair syms = (SymPair) {
          .lhs = lhs->named.name,
          .rhs = rhs->named.name
      };
      push_sym_pair(syms, rename);
      Result res = unify_internal(lhs->named.type, rhs->named.type, rename, a); 
      rename->len--;

      if (res.type == Err) return res;

      if (lhs->named.args && rhs->named.args) {
        if (lhs->named.args->len != rhs->named.args->len) {
            return (Result) {
                .type = Err,
                .error_message = mv_string("named type mismatch: different arg count!"),
            };
        }

        for (size_t i = 0; i < lhs->named.args->len; i++) {
            res = unify_internal(lhs->named.args->data[i], rhs->named.args->data[i], rename, a); 
            if (res.type == Err) return res;
        }
      } else if (lhs->named.args || rhs->named.args) {
        return (Result) {
            .type = Err,
            .error_message = mv_string("named type mismatch: one has args and one doesn't!"),
        };
      }
      return res;
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
                res = unify_internal(lhs_args.data[i], rhs_args.data[i], rename, a);
                if (res.type == Err) return res;
            }
        }

        return unify_internal(lhs->distinct.type, rhs->distinct.type, rename, a);
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
        // Check that they are alpha-equivalent

        // Check they are the same var
        if (!var_eq(lhs->var, rhs->var, rename)) {
            return (Result) {
                .type = Err,
                .error_message = mk_string("Cannot Unify different type variables", a),
            };
        }
        return (Result) {.type = Ok};
        break;
    }
    case TAll: {
        if (lhs->binder.vars.len != rhs->binder.vars.len) {
            return (Result) {.type = Err};
        }
        for (size_t i = 0; i < lhs->binder.vars.len; i++) {
            SymPair syms = (SymPair){
                .lhs = lhs->binder.vars.data[i],
                .rhs = rhs->binder.vars.data[i]
            };
            push_sym_pair(syms, rename);
        };
        Result res = unify_internal(lhs->binder.body, rhs->binder.body, rename, a);
        rename->len -= lhs->binder.vars.len;
        return res;
        break;
    }
    default:  {
        PtrArray nodes = mk_ptr_array(8, a);
        push_ptr(mk_str_doc(mv_string("Unification failed: invalid types"), a), &nodes);
        push_ptr(pretty_type(lhs, a), &nodes);
        push_ptr(mk_str_doc(mv_string("and"), a), &nodes);
        push_ptr(pretty_type(rhs, a), &nodes);
        panic(doc_to_str(mv_sep_doc(nodes, a), 80, a));
    } 
    }
}

Result uvar_subst(UVarType* uvar, PiType* type, Allocator* a) {
    if (type->sort == TUVar) {
        // type has been traced, so if it's a uvar, no need to chase!
        // check that the defaults are compatible
        if (uvar->default_behaviour ) {
        }
    } else {
        // TODO: better error messages
        PiType* unwrapped = unwrap_type(type, a);
        for (size_t i = 0; i < uvar->constraints.len; i++) {
            switch (uvar->constraints.data[i].type) {
            case ConInt:
                if (unwrapped->sort != TPrim || unwrapped->prim > 0b111) {
                    return (Result) {.type = Err, .error_message = mv_string("Does not satisfy integral constraint.")};
                }
                break;
            case ConFloat:
                if (unwrapped->sort != TPrim || unwrapped->prim != Float_32 || unwrapped->prim != Float_64) {
                    return (Result) {.type = Err, .error_message = mv_string("Does not satisfy floating constraint.")};
                }
                break;
            case ConField:
                if (unwrapped->sort != TStruct) {
                    return (Result) {.type = Err, .error_message = mv_string("Does not satisfy field constraint: not a struct")};
                }
                bool found_field = false;
                for (size_t i = 0; i < type->structure.fields.len; i++) {
                    if (symbol_eq(type->structure.fields.data[i].key,
                                  uvar->constraints.data[i].has_field.name)) {
                        Result out = unify(type->structure.fields.data[i].val, uvar->constraints.data[i].has_field.type, a); 
                        if (out.type != Ok) return out;
                        found_field = false;
                    }
                }
                if (!found_field) {
                    return (Result) {.type = Err, .error_message = mv_string("Does not satisfy field constraint: field not found")};
                }
                break;
            }
        }
    }
    
    uvar->subst = type;
    return (Result){.type = Ok};
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
    case TResumeMark: {
        panic(mv_string("has_unification_vars_p unimplemented for Resume Mark"));
    }
    case TDynamic: {
        return has_unification_vars_p(*type.dynamic);
    };
    case TNamed: {
        if (type.named.args) {
            for (size_t i = 0; i < type.named.args->len; i++) {
                if (has_unification_vars_p(*(PiType*)type.named.args->data[i]))
                    return true;
            }
        }
        return has_unification_vars_p(*type.named.type);
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
    case TCType: {
        // TODO (INVESTIGATE): can we have any type inference for c types/values?
        return false;
    }
    case TVar: return false;
    
    case TAll:
    case TExists: {
        return has_unification_vars_p(*type.binder.body);
    }
    case TCApp: {
        for (size_t i = 0; i < type.app.args.len; i++) {
            if (has_unification_vars_p(*(PiType*)type.app.args.data[i]))
                return true;
        }
        return has_unification_vars_p(*type.app.fam);
    }
    case TFam: {
        return has_unification_vars_p(*type.binder.body);
    }

    case TKind: return false;
    case TConstraint: return false;

    // Special sort: unification variable
    case TUVar:
        if (type.uvar->subst == NULL && type.uvar->default_behaviour == NoDefault) {
            return true;
        } else if (type.uvar->subst != NULL) {
            return has_unification_vars_p(*type.uvar->subst);
        } else {
            return false;
        }
    }

    // If we are here, then none of the branches were taken!
    panic(mv_string("Invalid type given to has_unification_vars_p"));
}

PiType* trace_uvar(PiType* uvar) {
  while ((uvar->sort == TUVar)
         && uvar->uvar->subst != NULL) {
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
    case TNamed: {
        squash_type(type->named.type);
        break;
    }
    case TDistinct: {
        squash_type(type->distinct.type);
        break;
    }
    case TTrait: {
        // TODO (INVESTIGATE PERFORMANCE): do we need to squash implicits also?
        for (size_t i = 0; i < type->trait.fields.len; i++) {
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

    case TCType: break;
    case TKind: break;
    case TConstraint: break;
    // Special sort: unification variable
    case TUVar: {
        PiType* subst = type->uvar->subst;
        if (subst) {
            squash_type(subst);
            *type = *subst;
        }

        // If still a unification variable, 
        // instantiate with default behavioru
        if (type->sort == TUVar) {
          switch (type->uvar->default_behaviour) {
          case NoDefault:
              break;
          case Integral:
              *type = (PiType){.sort = TPrim, .prim = Int_64};
              break;
          case Floating:
              *type = (PiType){.sort = TPrim, .prim = Float_64};
              break;
          case Struct:
              panic(mv_string("Not implemented: squash uvar with struct default!"));
              break;
          }
        }
        break;
    }
    default: 
        panic(mv_string("squash_type received invalid type!"));
    }
}

PiType* mk_uvar(Allocator* a) {
    PiType* uvar = mem_alloc(sizeof(PiType), a);
    uvar->sort = TUVar; 

    uvar->uvar = mem_alloc(sizeof(UVarType), a);
    *uvar->uvar = (UVarType) {
      .subst = NULL,
      .constraints = mk_constraint_array(4, a),
      .default_behaviour = NoDefault,
    };
    
    return uvar;
}

PiType* mk_uvar_integral(Allocator* a) {
    PiType* uvar = mem_alloc(sizeof(PiType), a);
    uvar->sort = TUVar; 

    uvar->uvar = mem_alloc(sizeof(UVarType), a);
    *uvar->uvar = (UVarType) {
      .subst = NULL,
      .constraints = mk_constraint_array(4, a),
      .default_behaviour = Integral,
    };
    
    return uvar;
}

PiType* mk_uvar_floating(Allocator* a) {
    PiType* uvar = mem_alloc(sizeof(PiType), a);
    uvar->sort = TUVar; 

    uvar->uvar = mem_alloc(sizeof(UVarType), a);
    *uvar->uvar = (UVarType) {
      .subst = NULL,
      .constraints = mk_constraint_array(4, a),
      .default_behaviour = Integral,
    };
    
    return uvar;
}
