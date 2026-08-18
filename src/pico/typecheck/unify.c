#include "platform/signals.h"
#include "components/pretty/string_printer.h"
#include "data/meta/array_header.h"
#include "data/meta/array_impl.h"
#include "data/stringify.h"

#include "pico/data/client/meta/list_header.h"
#include "pico/data/client/meta/list_impl.h"
#include "pico/data/client/sym_addr_piamap.h"
#include "pico/typecheck/type_errors.h"
#include "pico/typecheck/unify.h"

/**
 *
 * Unification Refactor Plan
 * ====================================
 * Currently, the unifier/typechecker is very much in a state of 'this is not
 * sound, but it *mostly* works unless you push it. There is a plan for a
 * refactor to improve soundness and speed of the typechecker, roughly outlined
 * as follows:
 * 1. Currently, UVars store 'substitutions', lists of [name ↦ type]
 *   substitutions, to make up for the fact that we don't keep track of scope
 *   properly. These should be removed (DONE), and instead, just rely on type-scoping
 *   information during the 'squash' phase instaed (TODO).
 * 
 * 2. During typechecking, instead of unifying in-place, produce a parallel
 *    syntax tree 'a problem' that keeps track of both type-scope, and all
 *    unification constraints. This problem is then 'solved' before moving 
 *    the solution into the syntax tree (this replaces the squashing phase).
 * 
 * 3. Once the problem/solution architecture is set-up, rework types to be much
 *    more efficient memory-wise:
 *    - UVars should be slimmed down to 32-bit indices into a pool/array (rather
 *      than full fat pointers).
 *    - Types Pointers should similarly be replaced with opaque references into
 *      some contextually relevant pool.
 *    - Instead of being full types, primitives should be encoded directly into
 *      the indices.
 * 
 * 4. At this point, the main thing to work on will be general correctness. Look
 *    into Higher-Order unification and possibly consult how Caledon structured
 *    its' type-checker. See if there is a way to report an easy error message
 *    when backtracking *would* normally be necessary. This may allow
 *    efficiency/speed improvemints, at the cost of only a little extra
 *    explicitness in the (Relic) code.
 * 
 * 5. At current, typechecking is by far the slowest step. Investigate whether
 *    it still is via profiling, and if so, look into more optimisations (I
 *    recall there being like, 1 paper for speeding up HM type inference that
 *    the creator of Elm mentioned on the 'Software Unscripted' podcast).
 */

/**
 *
 * Unification
 * ============
 * The unifier presented here is a relatively simple extension of a regular
 * HM-style unifier. Types whose values are unknown are initialized to a blank
 * UVar, where a UVar stores the type it is substituted for in an internal field
 * named `subst`. During unification, uvars are mutated in-place to handle the
 * new values. 
 * 
 * Implementation Details
 * -----------------------
 * Handling Occurs
 * ---------------
 * There are several legitimate scenarios where we might want to unify a uvar,
 * say α with, e.g. (struct [.x (Ptr α)] [.y I64]). In this case, doing the
 * substitution blindly would give α = (struct [.x (Ptr α)] [.y I64]). Then,
 * during the `squash` step (eliminating uvars), we would encounter an infinite
 * loop, as we constantly go down into the definition of α. To prevent this,
 * before substitution, (α, T) we first check if T contains α. The function
 * occurs(α, T) returns true if T contains α. 
 * 
 * 
 * Handling Scope
 * --------------
 * Handling of named types
 * Unification destructively modifies uvars 
 * Thus, simply, e.g. copying and then renaming types won't work.
 * A rename-map has to deal with shadowing variables, e.g.
 * (Name x (Name x x)) ?= (Name x (Name y x))
 * For this reason, we need a solution satisfying
 * • Does not copy or modify named types
 * • Takes care of naming and shadowing
 * 
 * The solution is an array (stack) of lhs & rhs symbols 
 * lhs_symbol rhs_symbol
 */

typedef enum {
    NoDefault, Integral, Floating, Struct, Enum
} UVarDefault;

typedef enum {
    ConInt, ConFloat, ConField, ConVariant,
} ConstraintType;

typedef struct {
    Symbol name;
    PiType* type;
} FieldConstraint;

typedef struct {
    Symbol name;
    AddrPiList* types;
} VariantConstraint;

typedef struct {
    ConstraintType type;
    Range range;
    union {
        int64_t fits;
        FieldConstraint has_field;
        VariantConstraint has_variant;
    };
} Constraint;

PICO_LIST_HEADER(Constraint, constraint, Constraint)
PICO_LIST_COMMON_IMPL(Constraint, constraint, Constraint)

struct UVarType {
    PiType* subst;
    ConstraintPiList constraints;
    UVarDefault default_behaviour;
};

typedef struct {
    Symbol lhs;
    Symbol rhs;
} SymPair;

int64_t cmp_sym_pair(SymPair s1, SymPair s2) {
    int64_t r1 = symbol_cmp(s1.lhs, s2.lhs);

    return r1 == 0 ? symbol_cmp(s1.rhs, s2.rhs) : r1;
}

ARRAY_HEADER(SymPair, sym_pair, SymPair)
ARRAY_CMP_IMPL(SymPair, cmp_sym_pair, sym_pair, SymPair)

typedef SymPair SymbolPair;

PiType* trace_uvar(PiType* uvar);
Dimension* trace_dim(Dimension* uvar);

bool occurs(UVarType* var, PiType *lhs);

// Unify two types such they are equal. Assumes they have the same sort
static UnifyResult unify_eq(PiType *lhs, PiType *rhs,
                     SymPairArray* rename, UnifyContext ctx);
static UnifyResult unify_internal(PiType *lhs, PiType *rhs,
                           SymPairArray* rename, UnifyContext ctx);
static UnifyResult unify_variant(Symbol lhs_sym, AddrPiList lhs_args,
                          Symbol rhs_sym, AddrPiList rhs_args,
                          SymPairArray *rename, UnifyContext ctx);
static UnifyResult uvar_subst(UVarType *uvar, PiType *type, UnifyContext ctx);
static UnifyResult add_constraint(Constraint con, UVarType* uvar, UnifyContext ctx);

UnifyResult unify(PiType* lhs, PiType* rhs, UnifyContext ctx) {
    SymPairArray renames = mk_sym_pair_array(8, ctx.a);
    UnifyResult r = unify_internal(lhs, rhs, &renames, ctx);
    sdelete_sym_pair_array(renames);
    return r;
}

UnifyResult unify_internal(PiType* lhs, PiType* rhs, SymPairArray* rename, UnifyContext ctx) {
    // Unification Implementation:
    // The LHS and RHS may contain unification variables
    // These are represented as a pair *(uid, type*) 
    // if the pointer is NULL, then the variable has not been instantiated.
    lhs = trace_uvar(lhs);
    rhs = trace_uvar(rhs);

    UnifyResult out;
    // Shortcut: if lhs == rhs, then the types are identical and no work needs
    // to be done.
    if (lhs == rhs) {
        return (UnifyResult) {.type = UOk};
    }

    // Note that this is left-biased: if lhs and RHS are both uvars, lhs is
    // instantiated to be the same as RHS
    if (lhs->sort == TUVar) {
        out = uvar_subst(lhs->uvar, rhs, ctx);
        if (out.type != UOk) return out;
    }
    else if (rhs->sort == TUVar) {
        out = uvar_subst(rhs->uvar, lhs, ctx);
        if (out.type != UOk) return out;
    }
    else if (rhs->sort == lhs->sort) {
        out = unify_eq(lhs, rhs, rename, ctx);
    } else {
        PtrArray nodes = mk_ptr_array(8, ctx.a);
        push_ptr(mk_str_doc(mv_string("Unification failed: given two non-unifiable types"), ctx.a), &nodes);
        push_ptr(mv_nest_doc(2, pretty_type(lhs, default_ptp, ctx.a), ctx.a), &nodes);
        push_ptr(mk_str_doc(mv_string("and"), ctx.a), &nodes);
        push_ptr(mv_nest_doc(2, pretty_type(rhs, default_ptp, ctx.a), ctx.a), &nodes);

        out = (UnifyResult) {
            .type = USimpleError,
            .message = mv_sep_doc(nodes, ctx.a),
        };
    }
    return out;
}

UnifyResult unify_variant(Symbol lhs_sym, AddrPiList lhs_args,
                          Symbol rhs_sym, AddrPiList rhs_args,
                          SymPairArray *rename, UnifyContext ctx) {
    Allocator* a = ctx.a;
    if (!symbol_eq(rhs_sym, lhs_sym)) {
        return unify_error_variant_name_mismatch(lhs_sym, rhs_sym, ctx);
    }

    if (lhs_args.len != rhs_args.len) {
        PtrArray lhs_nodes = mk_ptr_array(lhs_args.len + 4, a);
        push_ptr(mv_cstr_doc("LHS: ", a), &lhs_nodes);
        push_ptr(mv_str_doc(symbol_to_string(lhs_sym, a), a), &lhs_nodes);
        for (size_t i = 0; i < lhs_args.len; i++) {
            push_ptr(pretty_type(lhs_args.data[i], default_ptp, a), &lhs_nodes);
        }
        Document* doc_lhs = mv_sep_doc(lhs_nodes, a);

        PtrArray rhs_nodes = mk_ptr_array(rhs_args.len + 4, a);
        push_ptr(mv_cstr_doc("RHS: ", a), &rhs_nodes);
        push_ptr(mv_str_doc(symbol_to_string(rhs_sym, a), a), &rhs_nodes);
        for (size_t i = 0; i < rhs_args.len; i++) {
            push_ptr(pretty_type(rhs_args.data[i], default_ptp, a), &rhs_nodes);
        }
        Document* doc_rhs = mv_sep_doc(rhs_nodes, a);

        PtrArray nodes = mk_ptr_array(6, a);
        push_ptr(mv_cstr_doc("Unification failed: variants must have matching number of members.", a), &nodes);
        push_ptr(doc_lhs, &nodes);
        push_ptr(doc_rhs, &nodes);
        return (UnifyResult) {
            .type = USimpleError,
            .message = mv_vsep_doc(nodes, a),
        };
    }

    for (size_t i = 0; i < lhs_args.len; i++) {
        UnifyResult out = unify_internal(lhs_args.data[i], rhs_args.data[i], rename, ctx);
        if (out.type != UOk) return out;
    }

    return (UnifyResult) {.type = UOk};
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

UnifyResult unify_eq(PiType *lhs, PiType *rhs, SymPairArray* rename, UnifyContext ctx) {
    Allocator* a = ctx.a;
    switch (lhs->sort) {
    case TPrim: {
        if (lhs->prim == rhs->prim) {
            return (UnifyResult) {.type = UOk};
        } else {
            PtrArray nodes = mk_ptr_array(8, a);
            push_ptr(mk_str_doc(mv_string("Unification failed: could not unify unequal primitives"), a), &nodes);
            push_ptr(pretty_type(lhs, default_ptp, a), &nodes);
            push_ptr(mk_str_doc(mv_string("and"), a), &nodes);
            push_ptr(pretty_type(rhs, default_ptp, a), &nodes);

            return (UnifyResult) {
                .type = USimpleError,
                .message = mv_sep_doc(nodes, a),
            };
        }
        break;
    }
    case TProc: {
        if (lhs->proc.args.len != rhs->proc.args.len
            || lhs->proc.implicits.len != rhs->proc.implicits.len) {
            return (UnifyResult) {
                .type = USimpleError,
                .message = mv_cstr_doc("Unification failed: provided two different procedures with differing number of arguments or implicits.", a)
            };
        }

        for (size_t i = 0; i < lhs->proc.implicits.len; i++) {
            UnifyResult out = unify_internal(lhs->proc.implicits.data[i], rhs->proc.implicits.data[i], rename, ctx);
            if (out.type != UOk) return out;
        }

        // Unify each argumet
        for (size_t i = 0; i < lhs->proc.args.len; i++) {
            UnifyResult out = unify_internal(lhs->proc.args.data[i], rhs->proc.args.data[i], rename, ctx);
            if (out.type != UOk) return out;
        }

        // Unify the return values
        return unify_internal(lhs->proc.ret, rhs->proc.ret, rename, ctx);
        break;
    }
    case TArray: {
        DimPiList lhs_dims = lhs->array.dimensions;
        DimPiList rhs_dims = rhs->array.dimensions;
        if (lhs_dims.len != rhs_dims.len) {
            return (UnifyResult) {
                .type = USimpleError,
                .message = mv_cstr_doc("Unification failed: attempting to unify two arrays of different dimensionality.", a)
            };
        }
        for (size_t i = 0; i < lhs_dims.len; i++) {
            Dimension* lhd = trace_dim(&lhs_dims.data[i]);
            Dimension* rhd = trace_dim(&rhs_dims.data[i]);
            if (lhd->is_uvar) {
                lhd->uvar.target = rhd; 
            } else if (rhd->is_uvar) {
                rhd->uvar.target = lhd; 
            } else {
                if (lhd->val != rhd->val) {
                    return (UnifyResult) {
                        .type = USimpleError,
                        .message = mv_cstr_doc("Unification failed: attempting to unify two arrays where at least one dimension has a different length.", a)
                    };
                }
            }
        }
        return unify_internal(lhs->array.element, rhs->array.element, rename, ctx);
        break;
    }
    case TStruct: {
        if (lhs->structure.fields.len != rhs->structure.fields.len) {
            PtrArray nodes = mk_ptr_array(5, a);
            push_ptr(mv_cstr_doc("Unification failed: attempting to unify two different structures with differing number of fields.", a), &nodes);
            push_ptr(mv_cstr_doc("The types are:", a), &nodes);
            push_ptr(mv_nest_doc(2, pretty_type(lhs, default_ptp, a), a), &nodes);
            push_ptr(mv_cstr_doc("and", a), &nodes);
            push_ptr(mv_nest_doc(2, pretty_type(rhs, default_ptp, a), a), &nodes);

            return (UnifyResult) {
                .type = USimpleError,
                .message = mk_vsep_doc(nodes, a),
            };
        }

        for (size_t i = 0; i < lhs->structure.fields.len; i++) {
            Symbol lhs_sym = lhs->structure.fields.data[i].key;
            PiType* lhs_ty = lhs->structure.fields.data[i].val;

            Symbol rhs_sym = rhs->structure.fields.data[i].key;
            PiType* rhs_ty = rhs->structure.fields.data[i].val;

            if (!symbol_eq(rhs_sym, lhs_sym)) {
                PtrArray nodes = mk_ptr_array(5, a);
                push_ptr(mv_cstr_doc("Unification failed: RHS and LHS structures must have matching field-names.", a), &nodes);
                push_ptr(mv_cstr_doc("The types are:", a), &nodes);
                push_ptr(mv_nest_doc(2, pretty_type(lhs, default_ptp, a), a), &nodes);
                push_ptr(mv_cstr_doc("and", a), &nodes);
                push_ptr(mv_nest_doc(2, pretty_type(rhs, default_ptp, a), a), &nodes);
                return (UnifyResult){
                    .type = USimpleError,
                    .message = mk_vsep_doc(nodes, a),
                };
            }

            UnifyResult out = unify_internal(lhs_ty, rhs_ty, rename, ctx);
            if (out.type != UOk) return out;
        }

        return (UnifyResult) {.type = UOk,};
        break;
    }
    case TEnum: {
        if (lhs->enumeration.variants.len != rhs->enumeration.variants.len) {
            PtrArray nodes = mk_ptr_array(5, a);
            push_ptr(mv_cstr_doc("Unification failed: two different enums with differing number of variants.", a), &nodes);
            push_ptr(mv_cstr_doc("The types are:", a), &nodes);
            push_ptr(mv_nest_doc(2, pretty_type(lhs, default_ptp, a), a), &nodes);
            push_ptr(mv_cstr_doc("and", a), &nodes);
            push_ptr(mv_nest_doc(2, pretty_type(rhs, default_ptp, a), a), &nodes);
            return (UnifyResult) {
                .type = USimpleError,
                .message = mk_vsep_doc(nodes, a),
            };
        }
        if (lhs->enumeration.tag_size != rhs->enumeration.tag_size) {
            return (UnifyResult) {
                .type = USimpleError,
                .message = mv_cstr_doc("Unification failed: two different enums differing tag-size.", a)
            };
        }

        for (size_t i = 0; i < lhs->enumeration.variants.len; i++) {
            Symbol lhs_sym = lhs->enumeration.variants.data[i].key;
            AddrPiList lhs_args = *(AddrPiList*)lhs->enumeration.variants.data[i].val;

            Symbol rhs_sym = rhs->enumeration.variants.data[i].key;
            AddrPiList rhs_args = *(AddrPiList*)rhs->enumeration.variants.data[i].val;
            UnifyResult out = unify_variant(lhs_sym, lhs_args, rhs_sym, rhs_args, rename, ctx);
            if (out.type != UOk) return out;
        }


        return (UnifyResult) {.type = UOk,};
        break;
    }
    case TReset: {
        UnifyResult out = unify_internal(lhs->reset.in, rhs->reset.in, rename, ctx);
        if (out.type != UOk) return out;
        return unify_internal(lhs->reset.out, rhs->reset.out, rename, ctx);
        break;
    }
    case TDynamic: {
        return unify_internal(lhs->dynamic, rhs->dynamic, rename, ctx);
        break;
    }
    case TNamed: {
      UnifyResult res;

      if (lhs->named.args && rhs->named.args) {
        if (lhs->named.args->len != rhs->named.args->len) {
            return (UnifyResult) {
                .type = USimpleError,
                .message = mv_cstr_doc("named type mismatch: different arg count!", a),
            };
        }

        for (size_t i = 0; i < lhs->named.args->len; i++) {
            res = unify_internal(lhs->named.args->data[i], rhs->named.args->data[i], rename, ctx); 
            if (res.type != UOk) return res;
        }
      } else if (lhs->named.args || rhs->named.args) {
          return unify_error_name_has_args_match(lhs, rhs, a);
      }

      SymPair syms = (SymPair) {
          .lhs = lhs->named.name,
          .rhs = rhs->named.name
      };
      push_sym_pair(syms, rename);
      res = unify_internal(lhs->named.type, rhs->named.type, rename, ctx); 
      rename->len--;
      return res;
      break;
    }
    case TDistinct: {
        if (lhs->distinct.id != rhs->distinct.id || lhs->distinct.source_module != rhs->distinct.source_module) {
            return (UnifyResult) {
                .type = USimpleError,
                .message = mv_cstr_doc("Cannot Unify two distinct types of unequal IDs or source modules", a),
            };
        }

        // Note: we can assume that either LHS and RHS both have args or neither
        // do, as we have already checked they have the same IDs! (I think??)
        if (lhs->distinct.args) {
            UnifyResult res;
            AddrPiList lhs_args = *lhs->distinct.args;
            AddrPiList rhs_args = *rhs->distinct.args;
            for (size_t i = 0; i < lhs_args.len; i++) {
                res = unify_internal(lhs_args.data[i], rhs_args.data[i], rename, ctx);
                if (res.type != UOk) return res;
            }
        }

        return unify_internal(lhs->distinct.type, rhs->distinct.type, rename, ctx);
        break;
    } case TKind: {
          if (lhs->kind.nargs == rhs->kind.nargs)
              return (UnifyResult) {.type = UOk};
          else 
              return (UnifyResult) {
                  .type = USimpleError,
                  .message = mv_cstr_doc("Cannot Unify two kinds of unequal nags", a),
              };
          break;
      }
    case TConstraint: {
        if (lhs->constraint.nargs == rhs->constraint.nargs)
            return (UnifyResult) {.type = UOk};
        else 
            return (UnifyResult) {
                .type = USimpleError,
                .message = mv_cstr_doc("Cannot Unify two constraints of unequal nags", a),
            };
        break;
    }
    case TVar: {
        // Check that they are alpha-equivalent

        // Check they are the same var
        if (!var_eq(lhs->var, rhs->var, rename)) {
            return (UnifyResult) {
                .type = USimpleError,
                .message = mv_cstr_doc("Cannot Unify different type variables", a),
            };
        }
        return (UnifyResult) {.type = UOk};
        break;
    }
    case TAll: {
        if (lhs->binder.vars.len != rhs->binder.vars.len) {
            return (UnifyResult) {.type = USimpleError};
        }
        for (size_t i = 0; i < lhs->binder.vars.len; i++) {
            SymPair syms = (SymPair){
                .lhs = lhs->binder.vars.data[i],
                .rhs = rhs->binder.vars.data[i]
            };
            push_sym_pair(syms, rename);
        };
        UnifyResult res = unify_internal(lhs->binder.body, rhs->binder.body, rename, ctx);
        rename->len -= lhs->binder.vars.len;
        return res;
        break;
    }
    case TSealed: {
        if (lhs->sealed.vars.len != rhs->sealed.vars.len) {
            return (UnifyResult) {
                .type = USimpleError,
                .message = mv_cstr_doc("Sealed types seal away a different number of variables.", a),
            };
        }

        for (size_t i = 0; i < lhs->sealed.vars.len; i++) {
            SymPair syms = (SymPair){
                .lhs = lhs->sealed.vars.data[i],
                .rhs = rhs->sealed.vars.data[i]
            };
            push_sym_pair(syms, rename);
        }

        if (lhs->sealed.implicits.len != rhs->sealed.implicits.len) {
            return (UnifyResult) {
                .type = USimpleError,
                .message = mv_cstr_doc("Sealed types have differing number of implicit variables.", a),
            };
        }
        for (size_t i = 0; i < lhs->sealed.implicits.len; i++) {
            UnifyResult res = unify_internal(lhs->sealed.implicits.data[i], rhs->sealed.implicits.data[i], rename, ctx);
            if (res.type != UOk) return res;
        }
        UnifyResult res = unify_internal(lhs->sealed.body, rhs->sealed.body, rename, ctx);
        rename->len -= lhs->sealed.vars.len;
        return res;
        break;
    }
    default:  {
        PtrArray nodes = mk_ptr_array(8, a);
        push_ptr(mk_str_doc(mv_string("Unification failed: invalid types"), a), &nodes);
        push_ptr(pretty_type(lhs, default_ptp, a), &nodes);
        push_ptr(mk_str_doc(mv_string("and"), a), &nodes);
        push_ptr(pretty_type(rhs, default_ptp, a), &nodes);
        panic(doc_to_str(mv_sep_doc(nodes, a), 80, a));
    } 
    }
}

UnifyResult uvar_subst(UVarType* uvar, PiType* type, UnifyContext ctx) {
    /**
     * ------------------------------------------------------------
     * Uvar Subst
     * ============ 
     * 
     * The goal of uvar subst is to ensure the uvar points to a specific type by
     * assigning the 'subst' field. If the type in the 'subst' field is itself a
     * uvar, then the constraints from the uvar argument must be propagated into
     * the type argument. 
     * 
     * ------------------------------------------------------------
     */
    if (ctx.logger) {
        PtrArray docs = mk_ptr_array(2, ctx.a);
        push_ptr(mv_str_doc(mv_string("instantiating:"), ctx.a), &docs);
        push_ptr(pretty_uvar_type(uvar, ctx.a), &docs);
        push_ptr(mv_str_doc(mv_string("->"), ctx.a), &docs);
        push_ptr(pretty_type(type, default_ptp, ctx.a), &docs);
        log_doc(mv_sep_doc(docs, ctx.a), ctx.logger);
    }

    Allocator* a = ctx.a;
    if (type->sort == TUVar) {
        UVarType* rhs = type->uvar; 
        // Do an occurs check to ensure that we don't accidentally unify a type
        // with itself, creating an infinite loop;
        if (uvar == rhs) 
            return (UnifyResult){.type = UOk};
        // type has been traced, so if it's a uvar, no need to chase!
        // check that the defaults are compatible
        if (uvar->default_behaviour != NoDefault) {
            // Check that the two unification variables are compatible
            if (rhs->default_behaviour == NoDefault ||
                rhs->default_behaviour == uvar->default_behaviour) {
                rhs->default_behaviour = uvar->default_behaviour;
                for (size_t i = 0; i < uvar->constraints.len; i++) {
                    UnifyResult res = add_constraint(uvar->constraints.data[i], type->uvar, ctx);
                    if (res.type != UOk) return res;
                }
            } else {
                return (UnifyResult) {
                    .type = USimpleError,
                    .message = mv_cstr_doc("Cannot push forward deafult.", a)
                };
            }
        }
    } else {
        // Note: the reason we unwrap the type is because we want to be able to 
        //   unify literals with named types, particularly enumerations, without 
        //   having to specify the name.
        PiType* unwrapped = unname_type(type, ctx.current_module, ctx.pia, a);
        // TODO (BUG): what about if the named type has arugments???
        // TODO (BUG): Enable occurs check/work in proper higher-order
        //             unification support.
        if (occurs(uvar, type)) 
            return (UnifyResult){.type = UOk};

        for (size_t i = 0; i < uvar->constraints.len; i++) {
            switch (uvar->constraints.data[i].type) {
            case ConInt:
                if (unwrapped->sort != TPrim || unwrapped->prim > 0b111) {
                    PtrArray nodes = mk_ptr_array(2, a);
                    push_ptr(mv_cstr_doc("Type does not satisfy integral constraint:", a), &nodes);
                    push_ptr(pretty_type(type, default_ptp, a), &nodes);

                    return (UnifyResult) {
                        .type = UConstraintError,
                        .initial = uvar->constraints.data[i].range,
                        .message = mv_hsep_doc(nodes, a)
                    };
                }
                break;
            case ConFloat:
                if (unwrapped->sort != TPrim || (unwrapped->prim != Float_32 && unwrapped->prim != Float_64)) {
                    PtrArray nodes = mk_ptr_array(2, a);
                    push_ptr(mv_cstr_doc("Type does not satisfy floating constraint:", a), &nodes);
                    push_ptr(pretty_type(type, default_ptp, a), &nodes);
                    return (UnifyResult) {.type = USimpleError, .message = mv_hsep_doc(nodes, a)};
                }
                break;
            case ConField: {
                if (unwrapped->sort != TStruct) {
                    return (UnifyResult) {.type = USimpleError, .message = mv_cstr_doc("Does not satisfy field constraint: not a Struct", a)};
                }
                bool found_field = false;
                for (size_t j = 0; j < unwrapped->structure.fields.len; j++) {
                    if (symbol_eq(unwrapped->structure.fields.data[j].key,
                                  uvar->constraints.data[i].has_field.name)) {
                        UnifyResult out = unify(unwrapped->structure.fields.data[j].val, uvar->constraints.data[i].has_field.type, ctx); 
                        if (out.type != UOk) return out;
                        found_field = true;
                    }
                }

                if (!found_field) {
                    PtrArray nodes = mk_ptr_array(4, a);
                    push_ptr(mv_cstr_doc("Does not satisfy field constraint - field not found:", a), &nodes);
                    push_ptr(mv_str_doc(symbol_to_string(uvar->constraints.data[i].has_field.name, a), a), &nodes);
                    push_ptr(mv_cstr_doc("in type:", a), &nodes);
                    push_ptr(pretty_type(type, default_ptp, a), &nodes);
                                  
                    return (UnifyResult) {
                        .type = UConstraintError,
                        .initial = uvar->constraints.data[i].range,
                        .message = mv_hsep_doc(nodes, a),
                    };
                }
                break;
            }
            case ConVariant: {
                if (unwrapped->sort != TEnum) {
                    return (UnifyResult) {.type = USimpleError, .message = mv_cstr_doc("Does not satisfy variant constraint: not an Enum", a)};
                }
                bool found_variant = false;
                for (size_t j = 0; j < unwrapped->enumeration.variants.len; j++) {
                    if (symbol_eq(unwrapped->enumeration.variants.data[j].key,
                                  uvar->constraints.data[i].has_variant.name)) {
                        Symbol lhs_sym = unwrapped->enumeration.variants.data[j].key;
                        AddrPiList lhs_args = *(AddrPiList*)unwrapped->enumeration.variants.data[j].val;
                        Symbol rhs_sym = uvar->constraints.data[i].has_variant.name;
                        AddrPiList rhs_args = *uvar->constraints.data[i].has_variant.types;

                        SymPairArray renames = mk_sym_pair_array(8, a);
                        UnifyResult out = unify_variant(lhs_sym, lhs_args, rhs_sym, rhs_args, &renames, ctx);
                        sdelete_sym_pair_array(renames);
                        if (out.type != UOk) return out;
                        found_variant = true;
                    }
                }

                if (!found_variant) {
                    PtrArray nodes = mk_ptr_array(5, a);
                    push_ptr(mv_cstr_doc("Does not satisfy variant constraint - variant not found:", a), &nodes);
                    push_ptr(mv_str_doc(symbol_to_string(uvar->constraints.data[i].has_field.name, a), a), &nodes);
                    {
                        AddrPiList types = *uvar->constraints.data[i].has_variant.types;
                        PtrArray ptypes = mk_ptr_array(types.len, a);
                        for (size_t j = 0; j < uvar->constraints.data[i].has_variant.types->len; j++) {
                            push_ptr(pretty_type(type, default_ptp, a), &ptypes);
                        }
                        push_ptr(mv_grouped_sep_doc(ptypes, a), &ptypes);
                    }
                    push_ptr(mv_cstr_doc("in type:", a), &nodes);
                    push_ptr(pretty_type(type, default_ptp, a), &nodes);
                                  
                    return (UnifyResult) {
                        .type = UConstraintError,
                        .initial = uvar->constraints.data[i].range,
                        .message = mv_hsep_doc(nodes, a),
                    };
                }
                break;
            }
            }
        }
    }
    
    uvar->subst = type;
    return (UnifyResult){.type = UOk};
}

UVarType* copy_uvar(UVarType* uvar, PiAllocator* pia) {
    UVarType* new = call_alloc(sizeof(UVarType), pia);
    *new = (UVarType) {
        .subst = uvar->subst,
        .constraints = scopy_constraint_list(uvar->constraints, pia),
        .default_behaviour = uvar->default_behaviour,
    };
    return new;
}

PiType* try_get_uvar(UVarType *uvar) {
    return uvar->subst;
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
    }
    case TArray: {
        return has_unification_vars_p(*type.array.element);
    }
    case TStruct: {
        for (size_t i = 0; i < type.structure.fields.len; i++) {
            if (has_unification_vars_p(*(PiType*)type.structure.fields.data[i].val))
                return true;
        }
        return false;
    }
    case TEnum: {
        for (size_t i = 0; i < type.enumeration.variants.len; i++) {
            AddrPiList types = *(AddrPiList*)type.enumeration.variants.data[i].val;
            for (size_t j = 0; j < types.len; j++) {
                if (has_unification_vars_p(*(PiType*)types.data[j]))
                    return true;
            }
        }
        return false;
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

        for (size_t i = 0; i < type.instance.implicit_fields.len; i++) {
            if (has_unification_vars_p(*(PiType*)type.instance.implicit_fields.data[i].val))
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
        return has_unification_vars_p(*type.binder.body);
    case TSealed: {
        for (size_t i = 0; i < type.sealed.implicits.len; i++) {
            if (has_unification_vars_p(*(PiType*)type.sealed.implicits.data[i]))
                return true;
        }
        return has_unification_vars_p(*(PiType*)type.sealed.body);
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

Dimension* trace_dim(Dimension* uvar) {
    while ((uvar->is_uvar)
           && uvar->uvar.target != NULL) {
        uvar = uvar->uvar.target;
    } 
    return uvar;
}

bool occurs(UVarType* var, PiType *type) {
    switch (type->sort) {
    case TPrim:
        return false;
    case TProc: {
        for (size_t i = 0; i < type->proc.implicits.len; i++) {
            if (occurs(var, type->proc.implicits.data[i])) return true;
        }
        for (size_t i = 0; i < type->proc.args.len; i++) {
            if (occurs(var, type->proc.args.data[i])) return true;
        }
        return occurs(var, type->proc.ret);
    }
    case TArray: {
        return occurs(var, type->array.element);
    }
    case TStruct: {
        for (size_t i = 0; i < type->structure.fields.len; i++) {
            if (occurs(var, type->structure.fields.data[i].val)) return true;
        }
        return false;
    }
    case TEnum: {
        for (size_t i = 0; i < type->enumeration.variants.len; i++) {
            AddrPiList types = *(AddrPiList*)type->enumeration.variants.data[i].val;
            for (size_t j = 0; j < types.len; j++) {
                if (occurs(var, types.data[j])) return true;
            }
        }
        return false;
    }
    case TReset: {
        if (occurs(var, type->reset.in)) return true;
        if (occurs(var, type->reset.out)) return true;
        return false;
    }
    case TDynamic: {
        return occurs(var, type->dynamic);
    }
    case TVar:
        return false;
    case TAll: 
    case TFam: {
        return occurs(var, type->binder.body);
    }
    case TSealed: {
        for (size_t i = 0; i < type->sealed.implicits.len; i++) {
            if (occurs(var, type->sealed.implicits.data[i])) return true;
        }
        return occurs(var, type->sealed.body);
    }
    case TNamed: {
        if (occurs(var, type->named.type)) return true;
        if (type->named.args) {
            for (size_t i = 0; i < type->named.args->len; i++) {
                if (occurs(var, type->named.args->data[i])) return true;
            }
        }
        return false;
    }
    case TDistinct: {
        if (occurs(var, type->distinct.type)) return true;
        if (type->distinct.args) {
            for (size_t i = 0; i < type->distinct.args->len; i++) {
                if (occurs(var, type->distinct.args->data[i])) return true;
            }
        }
        return false;
    }
    case TTrait: {
        for (size_t i = 0; i < type->trait.implicit_fields.len; i++) {
            if (occurs(var, type->trait.implicit_fields.data[i].val)) return true;
        }
        for (size_t i = 0; i < type->trait.fields.len; i++) {
            if (occurs(var, type->trait.fields.data[i].val)) return true;
        }
        return false;
    }
    case TTraitInstance: {
        for (size_t i = 0; i < type->instance.args.len; i++) {
            if (occurs(var, type->instance.args.data[i])) return true;
        }
        for (size_t i = 0; i < type->instance.implicit_fields.len; i++) {
            if (occurs(var, type->instance.implicit_fields.data[i].val)) return true;
        }
        for (size_t i = 0; i < type->instance.fields.len; i++) {
            if (occurs(var, type->instance.fields.data[i].val)) return true;
        }
        return false;
    }

    case TCType: return false;
    case TKind: return false;
    case TConstraint: return false;
    // Special sort: unification variable
    case TUVar: {
        UVarType* uvar = type->uvar;
        if (var == uvar) return true;
        // Be conservative: also check substitutions
        if (uvar->subst) {
            if (occurs(var, uvar->subst)) return true;
        } 
        return false;
    }
    default: 
        panic(mv_string("squash_type received invalid type!"));
    }
}

void squash_type(PiType* type, UnifyContext ctx) {
    Allocator* a = ctx.a;
    PiAllocator* pia = ctx.pia;
    switch (type->sort) {
    case TPrim:
        break;
    case TProc: {
        for (size_t i = 0; i < type->proc.implicits.len; i++) {
            squash_type((PiType*)(type->proc.implicits.data[i]), ctx);
        }
        for (size_t i = 0; i < type->proc.args.len; i++) {
            squash_type((PiType*)(type->proc.args.data[i]), ctx);
        }
        squash_type(type->proc.ret, ctx);
        break;
    }
    case TArray: {
        squash_type(type->array.element, ctx);
        break;
    }
    case TStruct: {
        for (size_t i = 0; i < type->structure.fields.len; i++) {
            squash_type((PiType*)((type->structure.fields.data + i)->val), ctx);
        }
        break;
    }
    case TEnum: {
        for (size_t i = 0; i < type->enumeration.variants.len; i++) {
            AddrPiList types = *(AddrPiList*)type->enumeration.variants.data[i].val;
            for (size_t j = 0; j < types.len; j++) {
                squash_type((PiType*)types.data[j], ctx);
            }
        }
        break;
    }
    case TReset: {
        squash_type((PiType*)type->reset.in, ctx);
        squash_type((PiType*)type->reset.out, ctx);
        break;
    }
    case TDynamic: {
        squash_type((PiType*)type->dynamic, ctx);
        break;
    }
    case TVar: break;
    case TAll: 
    case TFam: {
        squash_type(type->binder.body, ctx);
        break;
    }
    case TSealed: {
        for (size_t i = 0; i < type->sealed.implicits.len; i++) {
            squash_type(type->sealed.implicits.data[i], ctx);
        }
        squash_type(type->sealed.body, ctx);
        break;
    }
    case TNamed: {
        squash_type(type->named.type, ctx);
        if (type->named.args) {
            for (size_t i = 0; i < type->named.args->len; i++) {
                squash_type(type->named.args->data[i], ctx);
            }
        }
        break;
    }
    case TDistinct: {
        squash_type(type->distinct.type, ctx);
        if (type->distinct.args) {
            for (size_t i = 0; i < type->distinct.args->len; i++) {
                squash_type(type->distinct.args->data[i], ctx);
            }
        }
        break;
    }
    case TTrait: {
        // TODO (INVESTIGATE PERFORMANCE): do we need to squash implicits also?
        for (size_t i = 0; i < type->trait.implicit_fields.len; i++) {
            squash_type((type->trait.implicit_fields.data + i)->val, ctx);
        }
        for (size_t i = 0; i < type->trait.fields.len; i++) {
            squash_type((type->trait.fields.data + i)->val, ctx);
        }
        break;
    }
    case TTraitInstance: {
        for (size_t i = 0; i < type->instance.args.len; i++) {
            squash_type(type->instance.args.data[i], ctx);
        }

        for (size_t i = 0; i < type->instance.implicit_fields.len; i++) {
            squash_type(type->instance.implicit_fields.data[i].val, ctx);
        }
        for (size_t i = 0; i < type->instance.fields.len; i++) {
            squash_type(type->instance.fields.data[i].val, ctx);
        }
        break;
    }

    case TCType: break;
    case TKind: break;
    case TConstraint: break;
    // Special sort: unification variable
    case TUVar: {
        UVarType* uvar = type->uvar;
        PiType* subst = type->uvar->subst;
        
        if (subst) {
            squash_type(subst, ctx);
            *type = *subst;
        }

        // If still a unification variable, 
        // instantiate with default behaviour
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
          case Struct: {
              SymAddrPiAMap out_fields = mk_sym_addr_piamap(type->uvar->constraints.len, pia);
              for (size_t i = 0; i < type->uvar->constraints.len; i++) {
                  Constraint con = type->uvar->constraints.data[i];
                  if (con.type != ConField)
                      panic(mv_string("Bad constraint: struct uvar should have only field constraints!"));

                  squash_type(con.has_field.type, ctx);
                  sym_addr_insert(con.has_field.name, con.has_field.type, &out_fields);
              }
              *type = (PiType){.sort = TStruct, .structure.fields = out_fields};
              break;
          }
          case Enum: {
              SymAddrPiAMap out_variants = mk_sym_addr_piamap(type->uvar->constraints.len, pia);
              for (size_t i = 0; i < type->uvar->constraints.len; i++) {
                  Constraint con = type->uvar->constraints.data[i];
                  if (con.type != ConVariant)
                      panic(mv_string("Bad constraint: enum uvar should have only variant constraints!"));

                  for (size_t i = 0; i < con.has_variant.types->len; i++) {
                      squash_type(con.has_variant.types->data[i], ctx);
                  }

                  sym_addr_insert(con.has_variant.name, con.has_variant.types, &out_variants);
              }
              *type = (PiType) {
                  .sort = TEnum,
                  .enumeration.tag_size = 64,
                  .enumeration.variants = out_variants
              };
              break;
          }
          }
        }

        if (ctx.logger) {
            start_section(mv_string("uvar squash"), ctx.logger);
            PtrArray docs = mk_ptr_array(2, ctx.a);
            push_ptr(mv_str_doc(mv_string("squashing: "), ctx.a), &docs);
            PiType show_type = (PiType){.sort = TUVar, .uvar = uvar};
            push_ptr(pretty_type(&show_type, default_ptp, ctx.a), &docs);
            push_ptr(mv_str_doc(mv_string("->"), ctx.a), &docs);
            if (type->sort == TUVar && type->uvar->subst == NULL) {
                DocStyle warn_style = scolour(colour(240, 150, 80), dstyle);
                push_ptr(mv_style_doc(warn_style, mv_str_doc(mv_string("NULL"), ctx.a), a), &docs);
            } else {
                push_ptr(pretty_type(type, default_ptp, ctx.a), &docs);
            }
            log_doc(mv_sep_doc(docs, ctx.a), ctx.logger);
            end_section(ctx.logger);
        }
        break;
    }
    default: 
        panic(mv_string("squash_type received invalid type!"));
    }
}

PiType* mk_uvar(PiAllocator* pia) {
    PiType* uvar = call_alloc(sizeof(PiType), pia);
    uvar->sort = TUVar; 

    uvar->uvar = call_alloc(sizeof(UVarType), pia);
    *uvar->uvar = (UVarType) {
        .subst = NULL,
        .constraints = mk_constraint_list(4, pia),
        .default_behaviour = NoDefault,
    };
    
    return uvar;
}

PiType* mk_uvar_integral(PiAllocator* pia, Range range) {
    PiType* uvar = call_alloc(sizeof(PiType), pia);
    uvar->sort = TUVar; 

    uvar->uvar = call_alloc(sizeof(UVarType), pia);
    *uvar->uvar = (UVarType) {
        .subst = NULL,
        .constraints = mk_constraint_list(4, pia),
        .default_behaviour = Integral,
    };

    Constraint con = (Constraint) {
        .type = ConInt,
        .range = range
    };
    push_constraint(con, &uvar->uvar->constraints);
    
    return uvar;
}

PiType* mk_uvar_floating(PiAllocator* pia, Range range) {
    PiType* uvar = call_alloc(sizeof(PiType), pia);
    uvar->sort = TUVar; 

    uvar->uvar = call_alloc(sizeof(UVarType), pia);
    *uvar->uvar = (UVarType) {
        .subst = NULL,
        .constraints = mk_constraint_list(4, pia),
        .default_behaviour = Floating,
    };

    Constraint con = (Constraint) {
        .type = ConFloat,
        .range = range
    };
    push_constraint(con, &uvar->uvar->constraints);
    
    return uvar;
}

Dimension mk_dim_uvar(PiAllocator *a) {
    return (Dimension) {
      .is_uvar = true,
      .uvar = (UVarDim) { .target = NULL },
    };
}

UnifyResult add_field_constraint(UVarType *uvar, Range range, Symbol field, PiType *field_ty, UnifyContext ctx) {
    while (true) {
        if ((uvar->default_behaviour == NoDefault) | (uvar->default_behaviour == Struct)) {
            uvar->default_behaviour = Struct;
            bool append = true;
            for (size_t i = 0; i < uvar->constraints.len; i++) {
                if (uvar->constraints.data[i].type != ConField) {
                    return (UnifyResult) {
                        .type = USimpleError,
                        .message = mv_cstr_doc("incompatible uvar constraints!", ctx.a),
                    };
                } else {
                    if (symbol_eq(uvar->constraints.data[i].has_field.name, field)) {
                        UnifyResult out = unify(uvar->constraints.data[i].has_field.type, field_ty, ctx);
                        if (out.type != UOk) return out; 
                        append = false;
                    }
                }
            }
            if (append) {
                Constraint con = (Constraint) {
                    .type = ConField,
                    .range = range,
                    .has_field.name = field,
                    .has_field.type = field_ty,
                };
                push_constraint(con, &uvar->constraints);
            }
        } else {
            return (UnifyResult) {
                .type = USimpleError,
                .message = mv_cstr_doc("incompatible uvar types!", ctx.a),
            };
        }

        if (uvar->subst && uvar->subst->sort == TUVar) {
            uvar = uvar->subst->uvar;
        } else {
            break; // stop the loop
        }
    }
    return (UnifyResult){.type = UOk};
}

UnifyResult add_variant_constraint(UVarType *uvar, Range range, Symbol variant, AddrPiList variant_types, UnifyContext ctx) {
    Allocator* a = ctx.a;
    while (true) {
        if ((uvar->default_behaviour == NoDefault) | (uvar->default_behaviour == Enum)) {
            uvar->default_behaviour = Enum;
            bool append = true;
            for (size_t i = 0; i < uvar->constraints.len; i++) {
                if (uvar->constraints.data[i].type != ConVariant) {
                    return (UnifyResult) {
                        .type = USimpleError,
                        .message = mv_cstr_doc("incompatible uvar constraints!", a),
                    };
                } else {
                    if (symbol_eq(uvar->constraints.data[i].has_field.name, variant)) {

                        SymPairArray renames = mk_sym_pair_array(8, a);
                        UnifyResult out = unify_variant(variant, variant_types,
                                                        uvar->constraints.data[i].has_variant.name,
                                                        *uvar->constraints.data[i].has_variant.types,
                                                        &renames, ctx);
                        sdelete_sym_pair_array(renames);
                        if (out.type != UOk) return out; 
                        append = false;
                    }
                }
            }
            if (append) {
                AddrPiList* types = mem_alloc(sizeof(AddrPiList), a);
                *types = variant_types;
                Constraint con = (Constraint) {
                    .type = ConVariant,
                    .range = range,
                    .has_variant.name = variant,
                    .has_variant.types = types,
                };
                push_constraint(con, &uvar->constraints);
            }
        } else {
            return (UnifyResult) {
                .type = USimpleError,
                .message = mv_cstr_doc("incompatible uvar types!", a),
            };
        }

        if (uvar->subst && uvar->subst->sort == TUVar) {
            uvar = uvar->subst->uvar;
        } else {
            break; // stop the loop
        }
    }
    return (UnifyResult){.type = UOk};
}

UnifyResult add_constraint(Constraint con, UVarType *uvar, UnifyContext ctx) {
    Allocator* a = ctx.a;
    switch (con.type) {
    case ConInt:
        for (size_t i = 0; i < uvar->constraints.len; i++) {
            if (uvar->constraints.data[i].type != ConInt) {
                // TODO (BUG) ensure constraints are compatibel
                return (UnifyResult) {
                    .type = USimpleError,
                    .message = mv_cstr_doc("Incompatible constraints!", a),
                };
            }
        }
        return (UnifyResult){.type = UOk};
    case ConFloat:
        for (size_t i = 0; i < uvar->constraints.len; i++) {
            if (uvar->constraints.data[i].type != ConFloat) {
                // TODO (BUG) ensure constraints are compatibel
                return (UnifyResult) {
                    .type = USimpleError,
                    .message = mv_cstr_doc("Incompatible constraints!", a),
                };
            }
        }
        return (UnifyResult){.type = UOk};
    case ConField:
        return add_field_constraint(uvar, con.range, con.has_field.name, con.has_field.type, ctx);
    case ConVariant:
        return add_variant_constraint(uvar, con.range, con.has_variant.name, *con.has_variant.types, ctx);
    }

    panic(mv_string("Invalid constraint provided to add_constraint"));
}

Document *pretty_uvar_type(UVarType *uvar, Allocator *a) {
    if (uvar->subst) {
        return pretty_type(uvar->subst, default_ptp, a);
    } else {
        return mv_str_doc(string_ptr(uvar, a), a);
    }
    //ConstraintPiList constraints;
    //UVarDefault default_behaviour;
    //AddrPiList substitutions;
}
