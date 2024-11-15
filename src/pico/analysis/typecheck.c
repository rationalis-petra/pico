#include "pico/analysis/typecheck.h"

#include "platform/signals.h"
#include "data/string.h"
#include "pretty/string_printer.h"

#include "pico/binding/environment.h"
#include "pico/binding/type_env.h"
#include "pico/analysis/unify.h"

// forward declarations
void type_check_expr(Syntax* untyped, PiType type, TypeEnv* env, UVarGenerator* gen, Allocator* a, ErrorPoint* point);
void type_infer_expr(Syntax* untyped, TypeEnv* env, UVarGenerator* gen, Allocator* a, ErrorPoint* point);

// Check a toplevel expression
void type_check(TopLevel* top, Environment* env, Allocator* a, ErrorPoint* point) {
    // If this is a definition, lookup the type to check against 
    TypeEnv *t_env = mk_type_env(env, a);
    UVarGenerator* gen = mk_gen(a);

    switch (top->type) {
    case TLDef: {
        PiType* check_against;
        EnvEntry e = env_lookup(top->def.bind, env);
        Syntax* term = top->def.value;

        if (e.success == Ok) {
            check_against = e.type;
        } else {
            check_against = mk_uvar(gen, a);
        }
        type_var(top->def.bind, check_against, t_env);
        type_check_expr(term, *check_against, t_env, gen, a, point);
        pop_type(t_env);
        break;
    }
    case TLExpr: {
        Syntax* term = top->expr;
        type_infer_expr(term, t_env, gen, a, point);
        break;
    }
    }
}

// Forward declarations for implementation (internal declarations)
void type_infer_i(Syntax* untyped, TypeEnv* env, UVarGenerator* gen, Allocator* a, ErrorPoint* point);
void type_check_i(Syntax* untyped, PiType* type, TypeEnv* env, UVarGenerator* gen, Allocator* a, ErrorPoint* point);
void eval_type(Syntax* untyped, TypeEnv* env, Allocator* a, ErrorPoint* point);
void squash_types(Syntax* untyped, Allocator* a, ErrorPoint* point);
PiType* get_head(PiType* type, PiType_t expected_sort);
PiType* reduce_type(PiType* type, Allocator* a);

// -----------------------------------------------------------------------------
// Interface
// -----------------------------------------------------------------------------
void type_infer_expr(Syntax* untyped, TypeEnv* env, UVarGenerator* gen, Allocator* a, ErrorPoint* point) {
    type_infer_i (untyped, env, gen, a, point);
    squash_types(untyped, a, point);
}

void type_check_expr(Syntax* untyped, PiType type, TypeEnv* env, UVarGenerator* gen, Allocator* a, ErrorPoint* point) {
    type_check_i (untyped, &type, env, gen, a, point);
    squash_types(untyped, a, point);
}

// -----------------------------------------------------------------------------
// Implementation & Notes
// 
// Type Evaluation 
// ---------------
// Types are evaluated at typechecking time
// Some basic evaluation & checking needs to be done to determine types:
// • Type-families instantiated with types need to be reduced.
// • Variables need to be looked up.
// • Any compound types (struct, enum, etc.) need to make sure any computations
//   are propagated 
// This primitive computation occurs at compile-time. 
// 
// 
// Typechecking strategy for Polymorphic Types
// -------------------------------------------
// It is important to consider that after typechecking, all type variables
// datatypes (enum, struct, proc) will be fully applied, with all variables
// either being
// 1. Local (runtime) type arguments to functions
// 2. Concrete types
// 
// Hence, upon completion of the typechecking phase, no types should have
// 'variables' introduced by type families. Further as (for now) types are
// structural and reduce, typechecking is relatively easy.
// 
//
// -----------------------------------------------------------------------------

void type_check_i(Syntax* untyped, PiType* type, TypeEnv* env, UVarGenerator* gen, Allocator* a, ErrorPoint* point) {
    type_infer_i(untyped, env, gen, a, point);
    Result out = unify(type, untyped->ptype, a);
    if (out.type == Err)
        throw_error(point, out.error_message);
}

// "internal" type inference. Destructively mutates types.
void type_infer_i(Syntax* untyped, TypeEnv* env, UVarGenerator* gen, Allocator* a, ErrorPoint* point) {
    switch (untyped->type) {
    case SLitUntypedIntegral:
        untyped->type = SLitTypedIntegral;
        untyped->ptype = mk_uvar_with_default(gen, a);
        break;
    case SLitTypedIntegral:
        untyped->ptype = mem_alloc(sizeof(PiType), a);
        *untyped->ptype = (PiType) {.sort = TPrim, .prim = untyped->integral.type,};
        break;
    case SLitBool:
        untyped->ptype = mem_alloc(sizeof(PiType), a);
        *untyped->ptype = (PiType) {.sort = TPrim, .prim = Bool,};
        break;
    case SLitString:
        untyped->ptype = mk_string_type(a);
        break;
    case SVariable: {
        TypeEntry te = type_env_lookup(untyped->variable, env);
        if (te.type != TENotFound) {
            untyped->ptype = te.ptype;
            if (te.value) {
                untyped->type = SCheckedType;
                untyped->type_val = te.value;
            }
        } else {
            String* sym = symbol_to_string(untyped->variable);
            String msg = mv_string("Couldn't find type of variable: ");
            throw_error(point, string_cat(msg, *sym, a));
        }
        break;
    }
    case SProcedure: {
        // give each arg a unification variable type. 
        PiType* proc_ty = mem_alloc(sizeof(PiType), a);
        proc_ty->sort = TProc;
        proc_ty->proc.args = mk_ptr_array(untyped->procedure.args.len, a);
        untyped->ptype = proc_ty;

        for (size_t i = 0; i < untyped->procedure.args.len; i++) {
            SymPtrACell arg = untyped->procedure.args.data[i];
            PiType* aty;
            if (arg.val) {
                eval_type(arg.val, env, a, point);
                aty = ((Syntax*)arg.val)->type_val;
            } else  {
                aty = mk_uvar(gen, a);
            }
            type_var(arg.key, aty, env);
            push_ptr(aty, &proc_ty->proc.args);
        }

        type_infer_i(untyped->procedure.body, env, gen, a, point); 
        pop_types(env, untyped->procedure.args.len);
        proc_ty->proc.ret = untyped->procedure.body->ptype;
        break;
    }
    case SAll: {
        // give each arg type kind.

        PiType* all_ty = mem_alloc(sizeof(PiType), a);
        untyped->ptype = all_ty;
        all_ty->sort = TAll;
        all_ty->binder.vars = mk_u64_array(untyped->all.args.len, a);

        for (size_t i = 0; i < untyped->all.args.len; i++) {
            Symbol arg = untyped->all.args.data[i];

            PiType* arg_ty = mem_alloc(sizeof(PiType), a);
            *arg_ty = (PiType) {.sort = TVar, .var = arg,};

            type_qvar(arg, arg_ty, env);
            push_u64(arg, &all_ty->binder.vars);
        }

        type_infer_i(untyped->all.body, env, gen, a, point); 
        pop_types(env, untyped->all.args.len);
        all_ty->binder.body = untyped->all.body->ptype;
        break;
    }
    case SApplication: {
        type_infer_i(untyped->application.function, env, gen, a, point);
        PiType fn_type = *untyped->application.function->ptype;
        if (fn_type.sort == TUVar) {
            // fill in structure 
            PiType* ret = mk_uvar(gen, a);
            PtrArray args = mk_ptr_array(16, a);
            for (size_t i = 0; i < untyped->application.args.len; i++) {
                push_ptr(mk_uvar(gen, a), &args);
            };

            fn_type.sort = TProc;
            fn_type.proc.args = args;
            fn_type.proc.ret = ret;
            *untyped->application.function->ptype = fn_type;
        }
        else if (fn_type.sort != TProc) {
            throw_error(point, mk_string("Expected LHS of application to be a proc", a));
        }
        
        if (fn_type.proc.args.len != untyped->application.args.len) {
            throw_error(point, mk_string("Incorrect number of function arguments", a));
        }

        for (size_t i = 0; i < fn_type.proc.args.len; i++) {
            type_check_i(untyped->application.args.data[i],
                         (PiType*)fn_type.proc.args.data[i],
                         env, gen, a, point);
        }

        untyped->ptype = fn_type.proc.ret;
        break;
    }
    case SAllApplication: {
        type_infer_i(untyped->all_application.function, env, gen, a, point);

        PiType all_type = *untyped->all_application.function->ptype;
        if (all_type.sort == TUVar) {
            throw_error(point, mk_string("Expected LHS of all application must be known (not currently inferrable)", a));
        }
        else if (all_type.sort != TAll) {
            throw_error(point, mk_string("Expected LHS of all application to be an all", a));
        }
        
        if (all_type.binder.vars.len != untyped->all_application.types.len) {
            throw_error(point, mk_string("Incorrect number of all type function arguments", a));
        }

        if (all_type.binder.body->sort != TProc) {
            throw_error(point, mk_string("Currently, can only typecheck application of all-proc, not arbitrary forall!", a));
        }

        // Check that all type args are actually types!
        SymPtrAssoc type_binds = mk_sym_ptr_assoc(all_type.binder.vars.len, a);
        for (size_t i = 0; i < all_type.binder.vars.len; i++) {
            Syntax* type = untyped->all_application.types.data[i];
            eval_type(type, env, a, point);
            sym_ptr_bind(all_type.binder.vars.data[i], type->type_val, &type_binds);
        }
        
        // Bind the vars in the all type to specific types!
        PiType* proc_type = pi_type_subst(all_type.binder.body, type_binds, a);

        for (size_t i = 0; i < proc_type->proc.args.len; i++) {
            type_check_i(untyped->all_application.args.data[i],
                         (PiType*)proc_type->proc.args.data[i],
                         env, gen, a, point);
        }

        untyped->ptype = proc_type->proc.ret;
        break;
    }
    case SConstructor: {
        // Typecheck variant
        eval_type (untyped->variant.enum_type, env, a, point);

        PiType* enum_type = untyped->variant.enum_type->type_val;
        if (enum_type->sort != TEnum) {
            throw_error(point, mv_string("Variant must be of enum type."));
        }

        bool found_variant = false;
        for (size_t i = 0; i < enum_type->enumeration.variants.len; i++) {
            if (enum_type->enumeration.variants.data[i].key == untyped->variant.tagname) {
                untyped->variant.tag = i;
                found_variant = true;

                // Generate variant has no args
                PtrArray* args = enum_type->enumeration.variants.data[i].val;
                if (args->len != 0) {
                    throw_error(point, mv_string("Incorrect number of args to variant constructor"));
                }
                untyped->ptype = enum_type;
                break;
            }
        }

        if (!found_variant) {
            throw_error(point, mv_string("Could not find variant tag."));
        }
        break;
    }
    case SVariant: {
        // Typecheck variant
        eval_type(untyped->variant.enum_type, env, a, point);

        // Typecheck is pretty simple: ensure that the tag is present in the
        PiType* enum_type = untyped->variant.enum_type->type_val;
        if (enum_type->sort != TEnum) {
            throw_error(point, mv_string("Variant must be of enum type."));
        }

        bool found_variant = false;
        for (size_t i = 0; i < enum_type->enumeration.variants.len; i++) {
            if (enum_type->enumeration.variants.data[i].key == untyped->variant.tagname) {
                untyped->variant.tag = i;
                found_variant = true;

                // Generate variant has no args
                PtrArray* args = enum_type->enumeration.variants.data[i].val;
                if (args->len != untyped->variant.args.len) {
                    throw_error(point, mv_string("Incorrect number of args to variant constructor"));
                }

                for (size_t i = 0; i < args->len; i++) {
                    type_check_i(untyped->variant.args.data[i], args->data[i], env, gen, a, point);
                }
                
                untyped->ptype = enum_type;
                break;
            }
        }

        if (!found_variant) {
            throw_error(point, mv_string("Could not find variant tag."));
        }
        break;
    }
    case SMatch: {
        // Typecheck the input 
        type_infer_i(untyped->match.val, env, gen, a, point);

        PiType* enum_type = untyped->match.val->ptype;
        if (enum_type->sort != TEnum) {
            throw_error(point, mv_string("Match expects value to have an enum type!"));
        }

        // Typecheck each variant, ensure they are the same
        PiType* out_ty = mk_uvar(gen, a);
        untyped->ptype = out_ty;
        U8Array used_indices = mk_u8_array(untyped->match.clauses.len, a);
        for (size_t i = 0; i < untyped->match.clauses.len; i++)
            push_u8(0, &used_indices);

        for (size_t i = 0; i < untyped->match.clauses.len; i++) {
            SynClause* clause = untyped->match.clauses.data[i];
            bool found_tag = false;
            for (size_t j = 0; j < enum_type->enumeration.variants.len; j++) {
                if (clause->tagname == enum_type->enumeration.variants.data[j].key) {
                    found_tag = true;
                    clause->tag = j;
                    used_indices.data[j] = 1;
                }
            }

            if (!found_tag) {
                throw_error(point, mv_string("Unable to find variant tag in match"));
            }

            // Now we've found the tag, typecheck the body
            PtrArray* types_to_bind = enum_type->enumeration.variants.data[i].val;
            if (types_to_bind->len != clause->vars.len) {
                throw_error(point,  mv_string("Bad number of binds!"));
            }

            for (size_t j = 0; j < types_to_bind->len; j++) {
                Symbol arg = clause->vars.data[j];
                PiType* aty = types_to_bind->data[j];
                type_var(arg, aty, env);
            }

            type_check_i(clause->body, out_ty, env, gen, a, point);
            pop_types(env, types_to_bind->len);
        }

        // Finally, check that all indices are accounted for;
        bool all_indices = true;
        for (size_t i = 0; i < used_indices.len; i++) all_indices &= used_indices.data[i];
        
        if (!all_indices) {
            throw_error(point, mv_string("Not all enumerations used in match expression"));
        }
        break;
    }
    case SStructure: {
        PiType* ty = mem_alloc(sizeof(PiType), a);
        untyped->ptype = ty; 
        eval_type(untyped->structure.ptype, env, a, point);
        *ty = *untyped->structure.ptype->type_val;

        if (ty->sort != TStruct) {
            throw_error(point, mv_string("Structure type invalid"));
        }

        if (untyped->structure.fields.len != ty->structure.fields.len) {
            throw_error(point, mv_string("Structure must have exactly n fields."));
        }

        for (size_t i = 0; i < ty->structure.fields.len; i++) {
            Syntax** field_syn = (Syntax**)sym_ptr_lookup(ty->structure.fields.data[i].key, untyped->structure.fields);
            if (field_syn) {
                PiType* field_ty = ty->structure.fields.data[i].val;
                type_check_i(*field_syn, field_ty, env, gen, a, point);
            } else {
                throw_error(point, mv_string("Structure is missing a field"));
            }
        }
        break;
    }
    case SProjector: {
        type_infer_i(untyped->projector.val, env, gen, a, point);
        PiType struct_type = *untyped->projector.val->ptype;
        if (struct_type.sort != TStruct) {
            throw_error(point, mv_string("Projection only works on structs."));
        }

        // search for field
        PiType* ret_ty = NULL;
        for (size_t i = 0; i < struct_type.structure.fields.len; i++) {
            if (struct_type.structure.fields.data[i].key == untyped->projector.field) {
                ret_ty = struct_type.structure.fields.data[i].val;
            }
        }
        if (ret_ty == NULL) {
            throw_error(point, mv_string("Projection only works on structs."));
        }
        untyped->ptype = ret_ty;
        break;
    }
    case SLet:
        for (size_t i = 0; i < untyped->let_expr.bindings.len; i++) {
            Symbol arg = untyped->let_expr.bindings.data[i].key;
            Syntax* val = untyped->let_expr.bindings.data[i].val;
            PiType* ty = mk_uvar(gen, a);

            type_check_i(val, ty, env, gen, a, point);
            // TODO: recursive bindings?
            type_var(arg, ty, env);
        }
        type_infer_i(untyped->let_expr.body, env, gen, a, point);
        untyped->ptype = untyped->let_expr.body->ptype;
        break;
    case SIf: {
        PiType* t = mem_alloc(sizeof(PiType), a);
        *t = (PiType) {.sort = TPrim,.prim = Bool};
        type_check_i(untyped->if_expr.condition,
                           t, env, gen, a, point);

        type_infer_i(untyped->if_expr.true_branch, env, gen, a, point);

        type_check_i(untyped->if_expr.false_branch,
                     untyped->if_expr.true_branch->ptype,
                     env, gen, a, point);
        untyped->ptype = untyped->if_expr.false_branch->ptype;
        break;
    }
    case SGoTo:
        if (label_present(untyped->go_to.label, env)) {
            PiType* t = mk_uvar(gen, a);
            untyped->ptype = t;
        } else {
            throw_error(point, mv_string("Error in go-to: Label Not found!"));
        }
        break;
    case SWithReset: {
        // A = expression type
        // in = reset (argument) type 
        // out = continuation (argument) type 
        PiType* tya = mk_uvar(gen, a);

        PiType* tyin = mk_uvar(gen, a);
        PiType* tyout = mk_uvar(gen, a);

        untyped->ptype = tya;
        untyped->with_reset.in_arg_ty = tyin;
        untyped->with_reset.cont_arg_ty = tyout;
        PiType* reset_ty = mem_alloc(sizeof(PiType), a);
        *reset_ty = (PiType) {.sort = TReset, .reset.in = tyin, .reset.out = tyout};

        type_var(untyped->with_reset.point_sym, reset_ty, env);
        type_check_i(untyped->with_reset.expr, tya, env, gen, a, point);
        pop_type(env);

        PiType* mark_ty = mem_alloc(sizeof(PiType), a);
        *mark_ty = (PiType) {.sort = TResumeMark};

        // continuation 
        type_var(untyped->with_reset.in_sym, tyin, env);
        type_var(untyped->with_reset.cont_sym, mark_ty, env);
        type_check_i(untyped->with_reset.handler, tya, env, gen, a, point);
        pop_types(env, 2);
        break;
    }
    case SResetTo: {
        PiType* tyin = mk_uvar(gen, a);
        PiType* tyout = mk_uvar(gen, a);
        untyped->ptype = tyout;

        PiType* reset_ty = mem_alloc(sizeof(PiType), a);
        *reset_ty = (PiType) {.sort = TReset, .reset.in = tyin, .reset.out = tyout};

        type_check_i(untyped->reset_to.point, reset_ty, env, gen, a, point);
        type_check_i(untyped->reset_to.arg, tyin, env, gen, a, point);
        break;
    }
    case SLabels: {
        PiType* ty = mk_uvar(gen, a);
        untyped->ptype = ty;
        SymbolArray labels = mk_u64_array(untyped->labels.terms.len, a);
        for (size_t i = 0;i < untyped->labels.terms.len; i++) {
            push_u64(untyped->labels.terms.data[i].key, &labels);
        }

        add_labels(labels, env);
        type_check_i(untyped->labels.entry, ty, env, gen, a, point);
        for (size_t i = 0 ; i < untyped->labels.terms.len; i++) {
            type_check_i(untyped->labels.terms.data[i].val, ty, env, gen, a, point);
        }
        pop_labels(env, labels.len);
        
        break;
    }
    case SSequence: {
        size_t num_binds = 0;
        for (size_t i = 0; i < untyped->sequence.elements.len; i++) {
            SeqElt* elt = untyped->sequence.elements.data[i];
            if (elt->is_binding) {
                PiType* type = mk_uvar(gen, a);
                type_check_i(elt->expr, type, env, gen, a, point);
                type_var (elt->symbol, type, env);
                num_binds++;
            } else {
                type_infer_i(elt->expr, env, gen, a, point);
            }
        }

        pop_types(env, num_binds);
        if (untyped->sequence.elements.len == 0) {
            PiType* t = mem_alloc(sizeof(PiType), a);
            *t = (PiType) {.sort = TPrim, .prim = Unit};
            untyped->ptype = t;
        } else {
            untyped->ptype = ((SeqElt*)untyped->sequence.elements.data[untyped->sequence.elements.len - 1])->expr->ptype;
        }

        break;
    }
    case SIs:
        eval_type(untyped->is.type, env, a, point);
        type_check_i(untyped->is.val,
                     untyped->is.type->type_val,
                     env, gen, a, point);
        untyped->ptype = untyped->is.type->type_val; 
        break;
    case SProcType:
    case SStructType:
    case SEnumType:
    case SResetType:
        eval_type(untyped, env, a, point);
        break;
    default:
        panic(mv_string("Internal Error: invalid syntax provided to (type_infer_i)"));
        break;
    }
}

void squash_types(Syntax* typed, Allocator* a, ErrorPoint* point) {
    switch (typed->type) {
    case SLitUntypedIntegral:
    case SLitTypedIntegral:
    case SLitBool:
    case SLitString:
    case SVariable:
        break;
    case SProcedure: {
        // squash body
        squash_types(typed->procedure.body, a, point);
        break;
    }
    case SAll: {
        // TODO (TAGS: FUTURE BUG): need to squash args when HKTs are allowed 
        squash_types(typed->all.body, a, point);
        break;
    }
    case SApplication: {
        squash_types(typed->application.function, a, point);
        
        for (size_t i = 0; i < typed->procedure.args.len; i++) {
            squash_types(typed->application.args.data[i], a, point);
        }
        break;
    }
    case SAllApplication: {
        squash_types(typed->application.function, a, point);

        for (size_t i = 0; i < typed->all_application.types.len; i++) {
            squash_types(typed->all_application.types.data[i], a, point);
        }
        
        for (size_t i = 0; i < typed->all_application.args.len; i++) {
            squash_types(typed->all_application.args.data[i], a, point);
        }
        break;
    }
    case SConstructor: {
        squash_types(typed->variant.enum_type, a, point);
        break;
    }
    case SVariant: {
        squash_types(typed->variant.enum_type, a, point);
        
        for (size_t i = 0; i < typed->variant.args.len; i++) {
            squash_types(typed->variant.args.data[i], a, point);
        }
        break;
    }
    case SMatch: {
        squash_types(typed->match.val, a, point);

        for (size_t i = 0; i < typed->match.clauses.len; i++) {
            SynClause* clause = (SynClause*)typed->match.clauses.data[i];
            squash_types(clause->body, a, point);
        }
        break;
    }
    case SStructure: {
        // Loop for each element in the 
        for (size_t i = 0; i < typed->structure.fields.len; i++) {
            Syntax* syn = typed->structure.fields.data[i].val;
            squash_types(syn, a, point);
        }
        break;
    }
    case SProjector:
        squash_types(typed->projector.val, a, point);
        break;
    case SLet:
        for (size_t i = 0; i < typed->let_expr.bindings.len; i++) {
            squash_types(typed->let_expr.bindings.data[i].val, a, point);
        }
        squash_types(typed->let_expr.body, a, point);
        break;
    case SIf: {
        squash_types(typed->if_expr.condition, a, point);
        squash_types(typed->if_expr.true_branch, a, point);
        squash_types(typed->if_expr.false_branch, a, point);
        break;
    }
    case SLabels:
        squash_types(typed->labels.entry, a, point);
        for (size_t i = 0; i < typed->labels.terms.len; i++) {
            squash_types(typed->labels.terms.data[i].val, a, point);
        }
        break;
    case SGoTo:
        break;
    case SWithReset:
        squash_types(typed->with_reset.expr, a, point);
        squash_types(typed->with_reset.handler, a, point);

        if (!has_unification_vars_p(*typed->with_reset.in_arg_ty)) {
            squash_type(typed->with_reset.in_arg_ty);
        } else {
            throw_error(point, mv_string("reset argument type not instantiated"));
        }
        if (!has_unification_vars_p(*typed->with_reset.cont_arg_ty)) {
            squash_type(typed->with_reset.cont_arg_ty);
        } else {
            throw_error(point, mv_string("resume argument type not instantiated"));
        }
        break;
    case SResetTo:
        squash_types(typed->reset_to.point, a, point);
        squash_types(typed->reset_to.arg, a, point);
        break;
    case SSequence:
        for (size_t i = 0; i < typed->sequence.elements.len; i++) {
            SeqElt* elt = typed->sequence.elements.data[i];
            squash_types(elt->expr, a, point);
        }
        break;
    case SIs:
        squash_type(typed->is.type->type_val);
        squash_types(typed->is.val, a, point);
        break;
    case SCheckedType:
        squash_type(typed->type_val);
        break;
    default:
        panic(mv_string("Internal Error: invalid syntactic form provided to (squash_types)"));
        break;
    }

    if (!has_unification_vars_p(*typed->ptype)) {
        squash_type(typed->ptype);
    }
    else {
        squash_type(typed->ptype);
        Document* doc = pretty_type(typed->ptype, a);
        String str = doc_to_str(doc, a);
        throw_error(point,
            string_cat(
                       string_cat(mv_string("Typechecking error: not all unification vars were instantiated. Term:\n"), 
                                  str, a),
                       string_cat(mv_string("\nType:\n"),
                                  doc_to_str(pretty_type(typed->ptype, a), a),
                                  a),
                       a));
    }
}

void eval_type(Syntax* untyped, TypeEnv* env, Allocator* a, ErrorPoint* point) {
    switch (untyped->type) {
    case SVariable: {
        TypeEntry e = type_env_lookup(untyped->variable, env);
        if (e.type == TENotFound) {
            String msg = mv_string("Variable not found: ");
            String sym = *symbol_to_string(untyped->variable);
            throw_error(point, string_cat(msg, sym, a));
        }
        if (e.value) {
            untyped->type = SCheckedType;
            untyped->ptype = e.ptype;
            untyped->type_val = e.value;
        } else {
            throw_error(point, mv_string("Variable expected to be type, was not!"));
        }
        break;
    }

    // Types & Type formers
    case SProcType: {
        PtrArray args = mk_ptr_array(untyped->proc_type.args.len, a);
        for (size_t i = 0; i < untyped->proc_type.args.len; i++) {
            Syntax* syn = untyped->proc_type.args.data[i];
            eval_type(syn, env, a, point);
            push_ptr(syn->type_val, &args);
        }
        Syntax* ret = untyped->proc_type.return_type;
        eval_type(untyped->proc_type.return_type, env, a, point);
        PiType* ret_ty = ret->type_val;

        PiType* out_type = mem_alloc(sizeof(PiType), a);
        *out_type = (PiType) {
            .sort = TProc,
            .proc.ret = ret_ty,
            .proc.args = args,
        };
        untyped->type_val = out_type;
        break;
    }
    case SStructType: {
        SymPtrAMap fields = mk_sym_ptr_amap(untyped->struct_type.fields.len, a);
        for (size_t i = 0; i < untyped->struct_type.fields.len; i++) {
            Syntax* syn = untyped->struct_type.fields.data[i].val;
            eval_type(syn, env, a, point);
            sym_ptr_insert(untyped->struct_type.fields.data[i].key, syn->type_val, &fields);
        }

        PiType* out_type = mem_alloc(sizeof(PiType), a);
        *out_type = (PiType) {
            .sort = TStruct,
            .structure.fields = fields,
        };
        untyped->type_val = out_type;
        break;
    }
    case SEnumType: {
        SymPtrAMap variants = mk_sym_ptr_amap(untyped->enum_type.variants.len, a);
        for (size_t i = 0; i < untyped->enum_type.variants.len; i++) {
            PtrArray* args = untyped->enum_type.variants.data[i].val;

            PtrArray* ty_args = mem_alloc(sizeof(PtrArray), a);
            *ty_args = mk_ptr_array(args->len, a);

            for (size_t j = 0; j < args->len; j++) {
                Syntax* syn = args->data[j];
                eval_type(syn, env, a, point);
                push_ptr(syn->type_val, ty_args);
            }
            
            sym_ptr_insert(untyped->enum_type.variants.data[i].key, ty_args, &variants);
        }

        PiType* out_type = mem_alloc(sizeof(PiType), a);
        *out_type = (PiType) {
            .sort = TEnum,
            .enumeration.variants = variants,
        };
        untyped->type_val = out_type;
        break;
    }
    case SResetType: {
        eval_type(untyped->reset_type.in, env, a, point);
        PiType* in = untyped->reset_type.in->type_val;

        eval_type(untyped->reset_type.out, env, a, point);
        PiType* out = untyped->reset_type.out->type_val;
        
        PiType* out_type = mem_alloc(sizeof(PiType), a);
        *out_type = (PiType) {
            .sort = TReset,
            .reset.in = in,
            .reset.out = out,
        };
        untyped->type_val = out_type;
        break;
    }
    case SForallType: {
        break;
    }
    case SExistsType: {
        break;
    }
    case STypeFamily: {
        break;
    }
    case SCheckedType: break; // Can leave blank
    default:
        throw_error(point, mv_string("Expected a type former - got a term former"));
    };

    untyped->type = SCheckedType;
    PiType* kind = mem_alloc(sizeof(PiType), a);
    *kind = (PiType) {
        .sort = TKind,
        .kind.nargs = 0,
    };
    untyped->ptype = kind;
}
