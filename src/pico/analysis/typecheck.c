#include "platform/error.h"
#include "platform/signals.h"

#include "data/string.h"
#include "components/pretty/standard_types.h"

#include "pico/binding/environment.h"
#include "pico/binding/type_env.h"
#include "pico/analysis/unify.h"
#include "pico/analysis/typecheck.h"
#include "pico/values/ctypes.h"
#include "pico/values/types.h"
#include "pico/codegen/codegen.h"
#include "pico/eval/call.h"
#include "pico/stdlib/core.h"
#include "pico/stdlib/meta/meta.h"
#include "pico/stdlib/foreign.h"

// forward declarations
void type_check_expr(Syntax* untyped, PiType type, TypeEnv* env, TypeCheckContext ctx);
void type_infer_expr(Syntax* untyped, TypeEnv* env, TypeCheckContext ctx);
void post_unify(Syntax* untyped, TypeEnv* env, PiAllocator* pia, Allocator* a, PiErrorPoint* point);
PiType* eval_type(Syntax* untyped, TypeEnv* env, TypeCheckContext ctx);

// Check a toplevel expression
void type_check(TopLevel* top, Environment* env, TypeCheckContext ctx) {
    // If this is a definition, lookup the type to check against 
    TypeEnv *t_env = mk_type_env(env, ctx.a);
    switch (top->type) {
    case TLDef: {
        PiType* check_against;
        PiType* ty = env_lookup_tydecl(top->def.bind, env);
        Syntax* term = top->def.value;

        if (ty != NULL) {
            check_against = ty;
        } else {
            check_against = mk_uvar(ctx.pia);
        }
        type_var(top->def.bind, check_against, t_env);
        type_check_expr(term, *check_against, t_env, ctx);
        post_unify(term, t_env, ctx.pia, ctx.a, ctx.point);
        pop_type(t_env);
        break;
    }
    case TLDecl: {
        PtrArray declarations = mk_ptr_array(2, ctx.a);
        for (size_t i = 0; i < top->decl.properties.len; i++) {
            SymPtrCell cell = top->decl.properties.data[i];
            if (symbol_eq(cell.key, string_to_symbol(mv_string("type")))) {
                PiType* ty = eval_type(cell.val, t_env, ctx);
                ModuleDecl* decl = mem_alloc(sizeof(ModuleDecl), ctx.a);
                *decl = (ModuleDecl) {
                    .sort = DeclType,
                    .type = ty,
                };
                push_ptr(decl, &declarations);
            } else {
                PicoError err = (PicoError) {
                    .range = ((Syntax*)cell.val)->range,
                    .message = mv_cstr_doc("unrecognized declaration", ctx.a),
                };
                throw_pi_error(ctx.point, err);
            }
        }
        top->decl.decls = declarations;
        break;
    }
    case TLImport: {
        for (size_t i = 0; i < top->import.clauses.len; i++) {
            ImportClause clause = top->import.clauses.data[i];
            if (!import_clause_valid(env, clause)) {
                PtrArray nodes = mk_ptr_array(2, ctx.a);
                push_ptr(mv_cstr_doc("Invalid import path: ", ctx.a), &nodes);
                push_ptr(pretty_import_clause(clause, ctx.a), &nodes);

                PicoError err = (PicoError) {
                    .range = top->import.range,
                    .message = mv_sep_doc(nodes, ctx.a),
                };
                throw_pi_error(ctx.point, err);
            }
        }
        break;
    }
    case TLExpr: {
        Syntax* term = top->expr;
        type_infer_expr(term, t_env, ctx);
        post_unify(term, t_env, ctx.pia, ctx.a, ctx.point);
        break;
    }
    }
}

// Forward declarations for implementation (internal declarations)
void type_infer_i(Syntax* untyped, TypeEnv* env, TypeCheckContext ctx);
void type_check_i(Syntax* untyped, PiType* type, TypeEnv* env, TypeCheckContext ctx);
void* eval_expr(Syntax* untyped, TypeEnv* env, TypeCheckContext ctx);
void* eval_typed_expr(Syntax* typed, TypeEnv* env, TypeCheckContext ctx);
void squash_types(Syntax* untyped, TypeCheckContext ctx);
PiType* get_head(PiType* type, PiType_t expected_sort);
PiType* reduce_type(PiType* type, Allocator* a);
void check_result_out(UnifyResult out, Range range, Allocator* a, PiErrorPoint* point);

// -----------------------------------------------------------------------------
// Interface
// -----------------------------------------------------------------------------
void type_infer_expr(Syntax* untyped, TypeEnv* env, TypeCheckContext ctx) {
    type_infer_i (untyped, env, ctx);
    squash_types(untyped, ctx);
}

void type_check_expr(Syntax* untyped, PiType type, TypeEnv* env, TypeCheckContext ctx) {
    type_check_i (untyped, &type, env, ctx);
    squash_types(untyped, ctx);
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

void type_check_i(Syntax* untyped, PiType* type, TypeEnv* env, TypeCheckContext ctx) {
    type_infer_i(untyped, env, ctx);
    UnifyContext uctx = (UnifyContext) {
        .a = ctx.a,
        .pia = ctx.pia,
        .logger = ctx.logger,
    };
    UnifyResult out = unify(type, untyped->ptype, uctx);
    check_result_out(out, untyped->range, ctx.a, ctx.point);
}

// "internal" type inference. Destructively mutates types.
void type_infer_i(Syntax* untyped, TypeEnv* env, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    PiErrorPoint* point = ctx.point;
    PicoError err;
    err.range = untyped->range;
    // Sometimes we go back and typecheck a term again, e.g. checking an
    // Application to an All generates a new term and typecheckes that.
    if (untyped->ptype) return;
    if (ctx.logger) {
        String str = string_cat(mv_string("Inferring type for: "), syntax_type_to_string(untyped->type), a);
        start_section(str, ctx.logger);
    }

    switch (untyped->type) {
    case SLitUntypedIntegral:
        untyped->type = SLitTypedIntegral;
        untyped->ptype = mk_uvar_integral(ctx.pia, untyped->range);
        break;
    case SLitTypedIntegral:
        untyped->ptype = mk_prim_type(ctx.pia, untyped->integral.type);
        break;
    case SLitUntypedFloating:
        untyped->type = SLitTypedFloating;
        untyped->ptype = mk_uvar_floating(ctx.pia, untyped->range);
        break;
    case SLitTypedFloating:
        untyped->ptype = mk_prim_type(ctx.pia, untyped->integral.type);
        break;
    case SLitArray: {
        PiType* element_type = mk_uvar(ctx.pia);
        for (size_t i = 0; i < untyped->array_lit.subterms.len; i++) {
            type_check_i(untyped->array_lit.subterms.data[i], element_type, env, ctx);
        }
        AddrPiList dimensions = mk_addr_list(untyped->array_lit.shape.len, ctx.pia);
        for (size_t i = 0; i < untyped->array_lit.shape.len; i++) {
            ArrayDimType* dim = call_alloc(sizeof(ArrayDimType), ctx.pia);
            *dim = (ArrayDimType) {
                .is_any = false,
                .value = untyped->array_lit.shape.data[i],
            };
            push_addr(dim, &dimensions);

        }

        PiType* array_type = call_alloc(sizeof(PiType), ctx.pia);
        *array_type = (PiType) {
            .sort = TArray,
            .array.sort = Fixed,
            .array.dimensions = dimensions,
            .array.element_type = element_type,
        };
        untyped->ptype = array_type;
        break;
    }
    case SLitBool:
        untyped->ptype = mk_prim_type(ctx.pia, Bool);
        break;
    case SLitUnit:
        untyped->ptype = mk_prim_type(ctx.pia, Unit);
        break;
    case SLitString:
        untyped->ptype = mk_string_type(ctx.pia);
        break;
    case SVariable: {
        TypeEntry te = type_env_lookup(untyped->variable, env);
        if (te.type != TENotFound) {
            if (te.is_module) {
                err.message = mv_cstr_doc("Unexpected module.", a);
                throw_pi_error(point, err);
            }

            // Note: if type is unification var...
            // then the kind is NULL? 
            untyped->ptype = te.ptype;
            if (te.value) {
                untyped->type = SCheckedType;
                untyped->type_val = te.value;
            }
        } else {
            String sym = symbol_to_string(untyped->variable, a);
            err.message = mv_str_doc(string_cat(mv_string("Couldn't find type of variable: "), sym, a), a);
            throw_pi_error(point, err);
        }
        break;
    }
    case SAbsVariable: {
        if (untyped->ptype == NULL) {
            panic(mv_string("Expect abolute variables being typechecked to have types!"));
        }
        break;
    }
    case SProcedure: {
        // TODO (BUG): ensure that we appropriately check for captures, 
        // as 'proc' does NOT support closures!

        // give each arg a unification variable type. 
        PiType* proc_ty = call_alloc(sizeof(PiType), ctx.pia);
        proc_ty->sort = TProc;
        proc_ty->proc.implicits = mk_addr_list(untyped->procedure.implicits.len, ctx.pia);
        proc_ty->proc.args = mk_addr_list(untyped->procedure.args.len, ctx.pia);
        untyped->ptype = proc_ty;

        for (size_t i = 0; i < untyped->procedure.implicits.len; i++) {
            SymPtrACell arg = untyped->procedure.implicits.data[i];
            PiType* aty;
            if (arg.val) {
                aty = eval_type(arg.val, env, ctx);
                if (aty->sort != TTraitInstance) {
                    err.message = mv_cstr_doc("Instance procedure argument does not have instance type.", a);
                    throw_pi_error(point, err);
                }
            } else  {
                aty = mk_uvar(ctx.pia);
            }
            type_var(arg.key, aty, env);
            push_addr(aty, &proc_ty->proc.implicits);
        }

        for (size_t i = 0; i < untyped->procedure.args.len; i++) {
            SymPtrACell arg = untyped->procedure.args.data[i];
            PiType* aty;
            if (arg.val) {
                aty = eval_type(arg.val, env, ctx);
            } else  {
                aty = mk_uvar(ctx.pia);
            }
            type_var(arg.key, aty, env);
            push_addr(aty, &proc_ty->proc.args);
        }

        type_infer_i(untyped->procedure.body, env, ctx); 
        pop_types(env, untyped->procedure.args.len + untyped->procedure.implicits.len);
        proc_ty->proc.ret = untyped->procedure.body->ptype;
        break;
    }
    case SAll: {
        // Give each arg type Type.
        PiType* all_ty = call_alloc(sizeof(PiType), ctx.pia);
        untyped->ptype = all_ty;
        all_ty->sort = TAll;
        all_ty->binder.vars = mk_sym_list(untyped->all.args.len, ctx.pia);

        for (size_t i = 0; i < untyped->all.args.len; i++) {
            Symbol arg = untyped->all.args.data[i];

            PiType* arg_ty = mem_alloc(sizeof(PiType), a);
            *arg_ty = (PiType) {.sort = TVar, .var = arg,};

            type_qvar(arg, arg_ty, env);
            push_sym(arg, &all_ty->binder.vars);
        }

        type_infer_i(untyped->all.body, env, ctx); 
        pop_types(env, untyped->all.args.len);
        all_ty->binder.body = untyped->all.body->ptype;
        break;
    }
    case SMacro: {
        // Macro inner type: 
        // proc [Array Syntax] Syntax
        // where syntax = ...
        PiType* syntax_array = mk_app_type(ctx.pia, get_list_type(), get_syntax_type());
        PiType* transformer_proc = mk_proc_type(ctx.pia, 1, syntax_array, get_macro_result_type());

        type_check_i(untyped->transformer, transformer_proc, env, ctx);
        PiType* t = call_alloc(sizeof(PiType), ctx.pia);
        *t = (PiType) {
            .sort = TPrim,
            .prim = TMacro,
        };
        untyped->ptype = t;
        break;
    }
    case SApplication: {
        type_infer_i(untyped->application.function, env, ctx);
        PiType fn_type = *untyped->application.function->ptype;
        if (fn_type.sort == TUVar) {
            // fill in structure 
            PiType* ret = mk_uvar(ctx.pia);
            AddrPiList args = mk_addr_list(16, ctx.pia);
            for (size_t i = 0; i < untyped->application.args.len; i++) {
                push_addr(mk_uvar(ctx.pia), &args);
            };

            fn_type.sort = TProc;
            fn_type.proc.args = args;
            fn_type.proc.ret = ret;
            *untyped->application.function->ptype = fn_type;
            untyped->ptype = ret;

        } else if (fn_type.sort == TProc) {
            if (fn_type.proc.args.len != untyped->application.args.len) {
                PtrArray nodes = mk_ptr_array(4, a);
                push_ptr(mv_cstr_doc("Incorrect number of function arguments - expected ", a), &nodes);
                push_ptr(pretty_u64(fn_type.proc.args.len, a), &nodes);
                push_ptr(mv_cstr_doc("but got ", a), &nodes);
                push_ptr(pretty_u64(untyped->application.args.len, a), &nodes);
                err.message = mv_hsep_doc(nodes, a);
                throw_pi_error(point, err);
            }

            for (size_t i = 0; i < fn_type.proc.args.len; i++) {
                type_check_i(untyped->application.args.data[i],
                             (PiType*)fn_type.proc.args.data[i],
                             env, ctx);
            }

            untyped->ptype = fn_type.proc.ret;

        } else if (fn_type.sort == TAll) {
            SynArray types = mk_ptr_array(fn_type.binder.vars.len, a);
            // TODO (FUTURE BUG): When HKTs are allowed, this will need to be
            // replaced with the correct kind!
            PiType* kind = mem_alloc(sizeof(PiType), a);
            *kind = (PiType){.sort = TKind, .kind.nargs = 0};
            for (size_t i = 0; i < fn_type.binder.vars.len; i++) {
                Syntax* syn = mem_alloc(sizeof(Syntax), a);
                *syn = (Syntax) {.type = SCheckedType, .ptype = kind, .type_val = mk_uvar(ctx.pia),};
                push_ptr(syn, &types);
            }

            SynAllApp new_app = (SynAllApp) {
                .function = untyped->application.function,
                .types = types,
                .implicits = untyped->application.implicits,
                .args = untyped->application.args
            };

            if (ctx.logger) {
                log_str(mv_string("function is 'all', substituting app for all-app"), ctx.logger);
            }
            untyped->type = SAllApplication;
            untyped->all_application = new_app;
            type_infer_i(untyped, env, ctx);
        } else if (fn_type.sort == TKind || fn_type.sort == TConstraint) {
            if (fn_type.kind.nargs != untyped->application.args.len) {
                err.message = mv_cstr_doc("Incorrect number of family arguments", a);
                throw_pi_error(point, err);
            }

            PiType* kind = mem_alloc(sizeof(PiType), a);
            *kind = (PiType) {.sort = TKind, .kind.nargs = 0};
            for (size_t i = 0; i < fn_type.kind.nargs; i++) {
                type_check_i(untyped->application.args.data[i],
                             kind, env, ctx);
            }
            *kind = (PiType) {.sort = fn_type.sort, .kind.nargs = 0};
            untyped->ptype = kind;
        } else {
            PtrArray arr = mk_ptr_array(2, a);
            push_ptr(mv_str_doc(mv_string("Expected LHS of application to be a function or kind. Was actually:"), a), &arr);
            push_ptr(pretty_type(&fn_type, a), &arr);
            err.message = mv_sep_doc(arr, a);
            throw_pi_error(point, err);
        }
        
        break;
    }
    case SAllApplication: {
        type_infer_i(untyped->all_application.function, env, ctx);

        PiType all_type = *untyped->all_application.function->ptype;
        if (all_type.sort == TUVar) {
            err.message = mv_cstr_doc("Expected LHS of all application must be known (not currently inferrable)", a);
            throw_pi_error(point, err);
        }
        else if (all_type.sort != TAll) {
            err.message = mv_cstr_doc("Expected LHS of all application to be an all", a);
            throw_pi_error(point, err);
        }
        
        if (all_type.binder.vars.len != untyped->all_application.types.len) {
            PtrArray nodes = mk_ptr_array(4, a);
            push_ptr(mk_str_doc(mv_string("Incorrect number of type arguments to all function - expected: "), a), &nodes);
            push_ptr(pretty_u64(all_type.binder.vars.len, a), &nodes);
            push_ptr(mk_str_doc(mv_string(", got: "), a), &nodes);
            push_ptr(pretty_u64(untyped->all_application.types.len, a), &nodes);
            err.message = mv_cat_doc(nodes, a);
            throw_pi_error(point, err);
        }

        if (all_type.binder.body->sort != TProc) {
            // Check that all type args are actually types!
            SymPtrAssoc type_binds = mk_sym_ptr_assoc(all_type.binder.vars.len, a);
            for (size_t i = 0; i < all_type.binder.vars.len; i++) {
                Syntax* type = untyped->all_application.types.data[i];
                eval_type(type, env, ctx);
                sym_ptr_bind(all_type.binder.vars.data[i], type->type_val, &type_binds);
            }

            if (all_type.binder.vars.len != untyped->all_application.types.len) {
                PtrArray nodes = mk_ptr_array(4, a);
                push_ptr(mk_str_doc(mv_string("Incorrect number of type arguments to all function - expected: "), a), &nodes);
                push_ptr(pretty_u64(all_type.binder.vars.len, a), &nodes);
                push_ptr(mk_str_doc(mv_string(", got: "), a), &nodes);
                push_ptr(pretty_u64(untyped->all_application.types.len, a), &nodes);
                err.message = mv_cat_doc(nodes, a);
                throw_pi_error(point, err);
            }

            // Now, check that args + implicits are empty
            if (untyped->all_application.implicits.len != 0) {
                PtrArray nodes = mk_ptr_array(4, a);
                PtrArray implicits = untyped->all_application.implicits;
                err.range.start = ((Syntax*)implicits.data[0])->range.start;
                err.range.end = ((Syntax*)implicits.data[implicits.len - 1])->range.end;
                push_ptr(mk_str_doc(mv_string("Incorrect number of implicit arguments to all function - expected: 0, got: "), a), &nodes);
                push_ptr(pretty_u64(implicits.len, a), &nodes);
                err.message = mv_cat_doc(nodes, a);
                throw_pi_error(point, err);
            }

            if (untyped->all_application.args.len != 0) {
                PtrArray nodes = mk_ptr_array(4, a);
                PtrArray arguments = untyped->all_application.args;
                err.range.start = ((Syntax*)arguments.data[0])->range.start;
                err.range.end = ((Syntax*)arguments.data[arguments.len - 1])->range.end;
                push_ptr(mk_str_doc(mv_string("Incorrect number of arguments to all function - expected: 0, got: "), a), &nodes);
                push_ptr(pretty_u64(arguments.len, a), &nodes);
                err.message = mv_cat_doc(nodes, a);
                throw_pi_error(point, err);
            }
            
            PiType* ret_type = pi_type_subst(all_type.binder.body, type_binds, ctx.pia, a);
            untyped->ptype = ret_type;
        } else {
            // Check that all type args are actually types!
            SymPtrAssoc type_binds = mk_sym_ptr_assoc(all_type.binder.vars.len, a);
            for (size_t i = 0; i < all_type.binder.vars.len; i++) {
                Syntax* type = untyped->all_application.types.data[i];
                eval_type(type, env, ctx);
                sym_ptr_bind(all_type.binder.vars.data[i], type->type_val, &type_binds);
            }
        
            // Bind the vars in the all type to specific types!
            PiType* proc_type = pi_type_subst(all_type.binder.body, type_binds, ctx.pia, a);
            if (proc_type->proc.args.len != untyped->all_application.args.len) {
                PtrArray nodes = mk_ptr_array(4, a);
                push_ptr(mk_str_doc(mv_string("Incorrect number of value arguments to all function - expected: "), a), &nodes);
                push_ptr(pretty_u64(proc_type->proc.args.len, a), &nodes);
                push_ptr(mk_str_doc(mv_string(", got: "), a), &nodes);
                push_ptr(pretty_u64(untyped->all_application.args.len, a), &nodes);
                err.message = mv_cat_doc(nodes, a);
                throw_pi_error(point, err);
            }

            for (size_t i = 0; i < proc_type->proc.args.len; i++) {
                type_check_i(untyped->all_application.args.data[i],
                             (PiType*)proc_type->proc.args.data[i],
                             env, ctx);
            }

            // Note: substitution has already happened above, so no need to do
            //   it again! 
            untyped->ptype = proc_type->proc.ret;
        }
        break;
    }
    case SSeal: {
        untyped->ptype = eval_type(untyped->seal.type, env, ctx);
        PiType* sealed_type = unwrap_type(untyped->ptype, ctx.pia, a);

        if (sealed_type->sort != TSealed) {
            err.range = untyped->seal.type->range;
            err.message = mv_cstr_doc("Must use sealed type to seal a value.", a);
            throw_pi_error(point, err);
        }

        if (sealed_type->sealed.vars.len != untyped->seal.types.len) {
            err.message = mv_cstr_doc("Incorrect number of type arguments provided to a seal.", a);
            throw_pi_error(point, err);
        }

        // Substitute a uvar in in for the sealed type:
        SymPtrAssoc type_binds = mk_sym_ptr_assoc(sealed_type->sealed.vars.len, a);
        for (size_t i = 0; i < sealed_type->sealed.vars.len; i++) {
            PiType* ty = eval_type(untyped->seal.types.data[i], env, ctx);
            sym_ptr_bind(sealed_type->sealed.vars.data[i], ty, &type_binds);
        }

        if (sealed_type->sealed.implicits.len > 0) {
            not_implemented(mv_string("Sealing with implicits."));
        }
        PiType* body_type = pi_type_subst(sealed_type->sealed.body, type_binds, ctx.pia, a);

        type_check_i(untyped->seal.body, body_type, env, ctx);
        break;
    }
    case SUnseal: {
        type_infer_i(untyped->unseal.sealed, env, ctx);
        PiType* sealed_type = unwrap_type(untyped->unseal.sealed->ptype, ctx.pia, a);
        if (sealed_type->sort != TSealed) {
            err.range = untyped->unseal.sealed->range;
            err.message = mv_cstr_doc("When unsealing, the unsealed value must have a 'Sealed' type.", a);
            throw_pi_error(point, err);
        }

        if (sealed_type->sealed.vars.len != untyped->unseal.types.len) {
            err.message = mv_cstr_doc("Number of type parameeters in unseal does not match those in the sealed type.", a);
            throw_pi_error(point, err);
        }

        SymPtrAssoc type_binds = mk_sym_ptr_assoc(sealed_type->sealed.vars.len, a);
        for (size_t i = 0; i < sealed_type->sealed.vars.len; i++) {
            PiType* ty = call_alloc(sizeof(PiType), ctx.pia);
            *ty = (PiType) {.sort = TVar, .var = untyped->unseal.types.data[i]};
            sym_ptr_bind(sealed_type->sealed.vars.data[i], ty, &type_binds);

            type_qvar(untyped->unseal.types.data[i], ty, env);
        }
        PiType* var_ty = pi_type_subst(sealed_type->sealed.body, type_binds, ctx.pia, a);
        type_var(untyped->unseal.binder, var_ty, env);
        type_infer_i(untyped->unseal.body, env, ctx);
        untyped->ptype = untyped->unseal.body->ptype;

        pop_types(env, 1 + sealed_type->sealed.vars.len);
        break;
    }
    case SConstructor: {
        // Typecheck variant
        if (untyped->variant.enum_type) {
            untyped->ptype = eval_type(untyped->variant.enum_type, env, ctx);
            PiType* enum_type = unwrap_type(untyped->ptype, ctx.pia, a);

            if (enum_type->sort != TEnum) {
                err.message = mv_cstr_doc("Variant must be of enum type.", a);
                throw_pi_error(point, err);
            }

            bool found_variant = false;
            for (size_t i = 0; i < enum_type->enumeration.variants.len; i++) {
                if (symbol_eq(enum_type->enumeration.variants.data[i].key, untyped->variant.tagname)) {
                    untyped->variant.tag = i;
                    found_variant = true;

                    // Generate variant has no args
                    PtrArray* args = enum_type->enumeration.variants.data[i].val;
                    if (args->len != 0) {
                        PtrArray arr = mk_ptr_array(4, a);
                        push_ptr(mv_cstr_doc("Incorrect number of args to variant constructor - expected", a), &arr);
                        push_ptr(pretty_u64(args->len, a), &arr);
                        push_ptr(mv_cstr_doc("but got", a), &arr);
                        push_ptr(pretty_u64(0, a), &arr);
                        err.message = mv_hsep_doc(arr, a);
                        throw_pi_error(point, err);
                    }
                    break;
                }
            }

            if (!found_variant) {
                String msg_origin = mv_string("Could not find variant tag: ");
                String data = symbol_to_string(untyped->variant.tagname, a);
                err.message = mv_str_doc(string_cat(msg_origin, data, a), a);
                throw_pi_error(point, err);
            }
        } else {
            untyped->ptype = mk_uvar(ctx.pia);
            AddrPiList types = mk_addr_list(0, ctx.pia);

            UnifyContext uctx = {
                .a = a,
                .pia = ctx.pia,
                .logger = ctx.logger,
            };
            UnifyResult out = add_variant_constraint(untyped->ptype->uvar, untyped->range, untyped->variant.tagname, types, uctx);
            check_result_out(out, untyped->range, a, point);
        }
        break;
    }
    case SVariant: {
        // Typecheck variant
        if (untyped->variant.enum_type) {
            untyped->ptype = eval_type(untyped->variant.enum_type, env, ctx);
            PiType* enum_type = unwrap_type(untyped->ptype, ctx.pia, a);

            if (enum_type->sort != TEnum) {
                err.message = mv_cstr_doc("Variant must be of enum type.", a);
                throw_pi_error(point, err);
            }

            bool found_variant = false;
            for (size_t i = 0; i < enum_type->enumeration.variants.len; i++) {
                if (symbol_eq(enum_type->enumeration.variants.data[i].key, untyped->variant.tagname)) {
                    untyped->variant.tag = i;
                    found_variant = true;

                    // Generate variant has no args
                    PtrArray* args = enum_type->enumeration.variants.data[i].val;
                    if (args->len != untyped->variant.args.len) {
                        PtrArray arr = mk_ptr_array(4, a);
                        push_ptr(mv_cstr_doc("Incorrect number of args to variant constructor - expected", a), &arr);
                        push_ptr(pretty_u64(args->len, a), &arr);
                        push_ptr(mv_cstr_doc("but got", a), &arr);
                        push_ptr(pretty_u64(untyped->variant.args.len, a), &arr);
                        err.message = mv_hsep_doc(arr, a);
                        throw_pi_error(point, err);
                    }

                    for (size_t i = 0; i < args->len; i++) {
                        type_check_i(untyped->variant.args.data[i], args->data[i], env, ctx);
                    }
                    break;
                }
            }

            if (!found_variant) {
                String msg_origin = mv_string("Could not find variant tag: ");
                String data = symbol_to_string(untyped->variant.tagname, a);
                err.message = mv_str_doc(string_cat(msg_origin, data, a), a);
                throw_pi_error(point, err);
            }
        } else {
            untyped->ptype = mk_uvar(ctx.pia);
            AddrPiList types = mk_addr_list(untyped->variant.args.len, ctx.pia);

            for (size_t i = 0; i < untyped->variant.args.len; i++) {
                type_infer_i(untyped->variant.args.data[i], env, ctx);
                push_addr(((Syntax*)untyped->variant.args.data[i])->ptype, &types);
            }
            UnifyContext uctx = {
                .a = a,
                .pia = ctx.pia,
                .logger = ctx.logger,
            };
            UnifyResult out = add_variant_constraint(untyped->ptype->uvar, untyped->range, untyped->variant.tagname, types, uctx);
            check_result_out(out, untyped->range, a, point);
        }
        break;
    }
    case SMatch: {
        // Typecheck the input 
        type_infer_i(untyped->match.val, env, ctx);

        PiType* enum_type = unwrap_type(untyped->match.val->ptype, ctx.pia, a); 
        if (enum_type->sort == TEnum) {
            // Typecheck each variant, ensure they are the same
            PiType* out_ty = mk_uvar(ctx.pia);
            untyped->ptype = out_ty;
            U8Array used_indices = mk_u8_array(untyped->match.clauses.len, a);
            for (size_t i = 0; i < untyped->match.clauses.len; i++)
                push_u8(0, &used_indices);

            for (size_t i = 0; i < untyped->match.clauses.len; i++) {
                SynClause* clause = untyped->match.clauses.data[i];
                bool found_tag = false;
                for (size_t j = 0; j < enum_type->enumeration.variants.len; j++) {
                    if (symbol_eq(clause->tagname, enum_type->enumeration.variants.data[j].key)) {
                        found_tag = true;
                        clause->tag = j;
                        if (used_indices.data[j] != 0) {
                            err.message = mv_cstr_doc("Same tag occurs twice in match body.", a);
                            throw_pi_error(point, err);
                        }
                        used_indices.data[j] = 1;
                    }
                }

                if (!found_tag) {
                    PtrArray nodes = mk_ptr_array(4, a);
                    push_ptr(mv_cstr_doc("The tag", a), &nodes);
                    push_ptr(mk_str_doc(symbol_to_string(clause->tagname, a), a), &nodes);
                    push_ptr(mv_cstr_doc("does not correspond to any tag in the enum type.", a), &nodes);
                    err.message = mv_sep_doc(nodes, a);
                    throw_pi_error(point, err);
                }

                // Now we've found the tag, typecheck the body
                PtrArray* types_to_bind = enum_type->enumeration.variants.data[clause->tag].val;
                if (types_to_bind->len != clause->vars.len) {
                    err.message = mv_cstr_doc("Bad number of binds!", a);
                    throw_pi_error(point, err);
                }

                for (size_t j = 0; j < types_to_bind->len; j++) {
                    Symbol arg = clause->vars.data[j];
                    PiType* aty = types_to_bind->data[j];
                    type_var(arg, aty, env);
                }

                type_check_i(clause->body, out_ty, env, ctx);
                pop_types(env, types_to_bind->len);
            }

            // Finally, check that all indices are accounted for;
            bool all_indices = true;
            for (size_t i = 0; i < used_indices.len; i++) all_indices &= used_indices.data[i];

            if (!all_indices) {
                err.message = mv_cstr_doc("Not all enumerations used in match expression", a);
                throw_pi_error(point, err);
            }
        } else if (enum_type->sort == TUVar) {
            // TODO (FEAT): Adjust this so there is room for re-ordering! 
            //   possibly placing constraints on the enum, but constraining it
            //   to only have the provided fields?? a special unification??
            PiType* enum_type = call_alloc(sizeof(PiType), ctx.pia);
            *enum_type = (PiType) {
                .sort = TEnum,
                .enumeration.tag_size = 64,
                .enumeration.variants = mk_sym_addr_piamap(untyped->match.clauses.len, ctx.pia),
            };

            PiType* out_ty = mk_uvar(ctx.pia);
            untyped->ptype = out_ty;
            for (size_t i = 0; i < untyped->match.clauses.len; i++) {
                SynClause* clause = untyped->match.clauses.data[i];

                AddrPiList types = mk_addr_list(clause->vars.len, ctx.pia);
                for (size_t j = 0; j < clause->vars.len; j++) {
                    Symbol arg = clause->vars.data[j];
                    PiType* aty = mk_uvar(ctx.pia);
                    push_addr(aty, &types);
                    type_var(arg, aty, env);
                }

                type_check_i(clause->body, out_ty, env, ctx);
                pop_types(env, types.len);

                AddrPiList* to_insert = mem_alloc(sizeof(AddrPiList), a);
                *to_insert = types;
                sym_addr_insert(clause->tagname, to_insert, &enum_type->enumeration.variants);
            }
            type_check_i(untyped->match.val, enum_type, env, ctx);
        } else {
            err.range = untyped->match.val->range;
            PtrArray nodes;
            push_ptr(mv_cstr_doc("Unexpected type provided to match. Expected an enum type, but got:", a),  &nodes);
            push_ptr(pretty_type(untyped->match.val->ptype, a), &nodes);
            err.message = mv_hsep_doc(nodes, a);
            throw_pi_error(point, err);
        }
        break;
    }
    case SStructure: {
        if (untyped->structure.base) {
            bool all_fields_required;
            type_infer_i(untyped->structure.base, env, ctx);
            if (untyped->structure.base->ptype->sort == TKind) {
                untyped->ptype = eval_type(untyped->structure.base, env, ctx);
                all_fields_required = true;
            } else {
                untyped->ptype = untyped->structure.base->ptype;
                all_fields_required = false;
            }

            PiType* struct_type = unwrap_type(untyped->ptype, ctx.pia, ctx.a);

            if (struct_type->sort != TStruct) {
                PtrArray nodes = mk_ptr_array(2, a);
                push_ptr(mv_cstr_doc("Structure provided/based off of non-structure type:", a), &nodes);
                push_ptr(pretty_type(struct_type, a), &nodes);
                err.message = mv_sep_doc(nodes, a);
                throw_pi_error(point, err);
            }

            // Check if any fields are missing when a new struct is being created
            // error message:
            if (untyped->structure.base->ptype->sort == TKind) {
                PtrArray docs = mk_ptr_array(4, a);
                docs.len++;
                bool missing_fields = false;
                for (size_t i = 0; i < struct_type->structure.fields.len; i++) {
                    Symbol field = struct_type->structure.fields.data[i].key;
                    bool has_field = false;
                    for (size_t j = 0; j < untyped->structure.fields.len; j++) {
                        if (symbol_eq(field, untyped->structure.fields.data[j].key))
                            has_field = true;
                    }
                    if (!has_field) {
                        push_ptr(mk_str_doc(symbol_to_string(field, a), a), &docs);
                        missing_fields = true;
                    }
                }
                
                if (missing_fields) {
                    docs.data[0] = mv_cstr_doc("Structure value definition is missing the field(s):", a);
                    err.message = mv_hsep_doc(docs, a);
                    throw_pi_error(point, err);
                }
            }

            for (size_t i = 0; i < struct_type->structure.fields.len; i++) {
                Syntax** field_syn = (Syntax**)sym_ptr_lookup(struct_type->structure.fields.data[i].key, untyped->structure.fields);
                if (field_syn) {
                    PiType* field_ty = struct_type->structure.fields.data[i].val;
                    type_check_i(*field_syn, field_ty, env, ctx);
                } else if (all_fields_required) {
                    panic(mv_string("An earlier typechecking step failed to ensure all fields were present in a structure"));
                    throw_pi_error(point, err);
                }
            }
        } else {
            PiType struct_type = (PiType) {
                .sort = TStruct,
                .structure.fields = mk_sym_addr_piamap(untyped->structure.fields.len, ctx.pia),
            };
            for (size_t i = 0; i < untyped->structure.fields.len; i++) {
                // TODO (FEATURE): allow the inference algorithm to later
                //                 reorder the fields!
                SymPtrCell cell = untyped->structure.fields.data[i];
                type_infer_i(cell.val, env, ctx);
                sym_addr_insert(cell.key, ((Syntax*)cell.val)->ptype, &struct_type.structure.fields);
            }

            untyped->ptype = call_alloc(sizeof(PiType), ctx.pia);
            *untyped->ptype = struct_type;
        }
        break;
    }
    case SProjector: {
        type_infer_i(untyped->projector.val, env, ctx);
        PiType source_type = *unwrap_type(untyped->projector.val->ptype, ctx.pia, a);

        if (source_type.sort == TStruct) {
            // search for field
            PiType* ret_ty = NULL;
            for (size_t i = 0; i < source_type.structure.fields.len; i++) {
                if (symbol_eq(source_type.structure.fields.data[i].key, untyped->projector.field)) {
                    ret_ty = source_type.structure.fields.data[i].val;
                }
            }
            if (ret_ty == NULL) {
                err.message = mv_cstr_doc("Field not found in struct!", a);
                throw_pi_error(point, err);
            }
            untyped->ptype = ret_ty;

        } else if (source_type.sort == TTraitInstance) {
            // search for field
            PiType* ret_ty = NULL;
            for (size_t i = 0; i < source_type.instance.fields.len; i++) {
                if (symbol_eq(source_type.instance.fields.data[i].key, untyped->projector.field)) {
                    ret_ty = source_type.instance.fields.data[i].val;
                }
            }
            if (ret_ty == NULL) {
                err.message = mv_cstr_doc("Field not found in instance!", a);
                throw_pi_error(point, err);
            }
            untyped->ptype = ret_ty;

        } else if (source_type.sort == TUVar) {
            untyped->ptype = mk_uvar(ctx.pia);
            UnifyContext uctx = {
                .a = a,
                .pia = ctx.pia,
                .logger = ctx.logger,
            };
            UnifyResult out = add_field_constraint(source_type.uvar, untyped->range, untyped->projector.field, untyped->ptype, uctx);
            check_result_out(out, untyped->range, a, point);
        } else {
            PtrArray nodes = mk_ptr_array(2, a);
            push_ptr(mv_cstr_doc("Projection only works on structs and traits. Instead got:", a), &nodes);
            push_ptr(pretty_type(untyped->projector.val->ptype, a),  &nodes);
            err.message = mk_sep_doc(nodes, a);
            throw_pi_error(point, err);
        }
        break;
    }
    case SInstance: {
        // Output type
        PiType* ty = call_alloc(sizeof(PiType), ctx.pia);
        untyped->ptype = ty;

        PiType* constraint_ty = call_alloc(sizeof(PiType), ctx.pia);
        *constraint_ty = (PiType) {.sort = TConstraint, .constraint.nargs = 0};

        PiType* ty_ty = call_alloc(sizeof(PiType), ctx.pia);
        *ty_ty = (PiType) {.sort = TKind, .constraint.nargs = 0};

        for (size_t i = 0; i < untyped->instance.params.len; i++) {
            Symbol arg = untyped->instance.params.data[i];
            type_var(arg, ty_ty, env);
        }

        for (size_t i = 0; i < untyped->instance.implicits.len; i++) {
            SymPtrACell arg = untyped->instance.implicits.data[i];
            PiType* aty;
            if (arg.val) {
                aty = eval_type(arg.val, env, ctx);
            } else  {
                aty = mk_uvar(ctx.pia);
            }
            type_var(arg.key, aty, env);
        }

        *ty = *eval_type(untyped->instance.constraint, env, ctx);

        if (ty->sort != TTraitInstance) {
            err.message = mv_cstr_doc("Instance type invalid", a);
            throw_pi_error(point, err);
        }

        if (untyped->instance.fields.len != ty->trait.fields.len) {
            err.message = mv_cstr_doc("Instance must have exactly n fields.", a);
            throw_pi_error(point, err);
        }

        for (size_t i = 0; i < ty->instance.fields.len; i++) {
            Syntax** field_syn = (Syntax**)sym_ptr_lookup(ty->instance.fields.data[i].key, untyped->instance.fields);
            if (field_syn) {
                PiType* field_ty = ty->instance.fields.data[i].val;
                type_check_i(*field_syn, field_ty, env, ctx);
            } else {
                err.message = mv_cstr_doc("Trait instance is missing a field", a);
                throw_pi_error(point, err);
            }
        }

        pop_types(env, untyped->instance.params.len + untyped->instance.implicits.len);
        break;
    }
    case SDynamic: {
        type_infer_i(untyped->dynamic, env, ctx);
        PiType* inner_type = untyped->dynamic->ptype; 
        PiType* t = call_alloc(sizeof(PiType), ctx.pia);
        *t = (PiType) {
            .sort = TDynamic,
            .dynamic = inner_type,
        };
        untyped->ptype = t;
        break;
    }
    case SDynamicUse: {
        type_infer_i(untyped->dynamic, env, ctx);
        PiType* dyn_type = untyped->dynamic->ptype; 
        if (dyn_type->sort != TDynamic) {
            err.message = mv_cstr_doc("use on non-dynamic type!", a);
            throw_pi_error(point, err);
        }
        untyped->ptype = dyn_type->dynamic;
        break;
    }
    case SDynamicSet: {
        type_infer_i(untyped->dynamic_set.dynamic, env, ctx);
        PiType* dyn_type = untyped->dynamic_set.dynamic->ptype; 
        if (dyn_type->sort != TDynamic) {
            err.message = mv_cstr_doc("set on non-dynamic type!", a);
            throw_pi_error(point, err);
        }
        type_check_i(untyped->dynamic_set.new_val, dyn_type->dynamic, env, ctx);
        untyped->ptype = mk_prim_type(ctx.pia, Unit);
        break;
    }
    case SDynamicLet: {
        for (size_t i = 0; i < untyped->dyn_let_expr.bindings.len; i++) {
            DynBinding* dbind = untyped->dyn_let_expr.bindings.data[i];

            PiType* dyn_ty = call_alloc(sizeof(PiType), ctx.pia);
            PiType* val_ty = mk_uvar(ctx.pia);
            *dyn_ty = (PiType) {
                .sort = TDynamic,
                .dynamic = val_ty,
            };

            type_check_i(dbind->var, dyn_ty, env, ctx);
            type_check_i(dbind->expr, val_ty, env, ctx);
        }
        type_infer_i(untyped->dyn_let_expr.body, env, ctx);
        untyped->ptype = untyped->dyn_let_expr.body->ptype;
        break;
    }
    case SLet: {
        for (size_t i = 0; i < untyped->let_expr.bindings.len; i++) {
            Symbol arg = untyped->let_expr.bindings.data[i].key;
            Syntax* val = untyped->let_expr.bindings.data[i].val;

            type_infer_i(val, env, ctx);

            // TODO (FEAT): add support for recursive bindings (e.g. procedures)
            type_var(arg, val->ptype, env);
        }
        type_infer_i(untyped->let_expr.body, env, ctx);
        untyped->ptype = untyped->let_expr.body->ptype;
        pop_types(env, untyped->let_expr.bindings.len);
        break;
    }
    case SIf: {
        PiType* t = call_alloc(sizeof(PiType), ctx.pia);
        *t = (PiType) {.sort = TPrim,.prim = Bool};
        type_check_i(untyped->if_expr.condition, t, env, ctx);

        PiType* out_type = mk_uvar(ctx.pia);
        type_check_i(untyped->if_expr.true_branch, out_type, env, ctx);

        type_check_i(untyped->if_expr.false_branch, out_type, env, ctx);
        untyped->ptype = out_type;
        break;
    }
    case SGoTo: {
        PtrArray* args = lookup_label(untyped->go_to.label, env);
        if (args) {
            if (args->len != untyped->go_to.args.len) {
                err.message = mv_cstr_doc("Error in go-to: wrong number of args!", a);
                throw_pi_error(point, err);
            }

            for (size_t i = 0; i < args->len; i++) {
                type_check_i(untyped->go_to.args.data[i], args->data[i], env, ctx);
            }

            // TODO (FEATURE): another type of defaulted uvar - unit!
            PiType* t = mk_uvar(ctx.pia);
            untyped->ptype = t;
        } else {
            PtrArray nodes = mk_ptr_array(2, a);
            push_ptr(mv_cstr_doc("Error in go-to: label not found:", a), &nodes);
            push_ptr(mk_str_doc(symbol_to_string(untyped->go_to.label, a), a), &nodes);
            err.message = mv_sep_doc(nodes, a);
            throw_pi_error(point, err);
        }
        break;
    }
    case SWithReset: {
        // A = expression type
        // in = reset (argument) type 
        // out = continuation (argument) type 
        PiType* tya = mk_uvar(ctx.pia);

        PiType* tyin = mk_uvar(ctx.pia);
        PiType* tyout = mk_uvar(ctx.pia);

        untyped->ptype = tya;
        untyped->with_reset.in_arg_ty = tyin;
        untyped->with_reset.cont_arg_ty = tyout;
        PiType* reset_ty = call_alloc(sizeof(PiType), ctx.pia);
        *reset_ty = (PiType) {.sort = TReset, .reset.in = tyin, .reset.out = tyout};

        type_var(untyped->with_reset.point_sym, reset_ty, env);
        type_check_i(untyped->with_reset.expr, tya, env, ctx);
        pop_type(env);

        PiType* mark_ty = call_alloc(sizeof(PiType), ctx.pia);
        *mark_ty = (PiType) {.sort = TResumeMark};

        // continuation 
        type_var(untyped->with_reset.in_sym, tyin, env);
        type_var(untyped->with_reset.cont_sym, mark_ty, env);
        type_check_i(untyped->with_reset.handler, tya, env, ctx);
        pop_types(env, 2);
        break;
    }
    case SResetTo: {
        PiType* tyin = mk_uvar(ctx.pia);
        PiType* tyout = mk_uvar(ctx.pia);
        untyped->ptype = tyout;

        PiType* reset_ty = call_alloc(sizeof(PiType), ctx.pia);
        *reset_ty = (PiType) {.sort = TReset, .reset.in = tyin, .reset.out = tyout};

        type_check_i(untyped->reset_to.point, reset_ty, env, ctx);
        type_check_i(untyped->reset_to.arg, tyin, env, ctx);
        break;
    }
    case SLabels: {
        // Recall that a lables looks like
        // (labels (expr)
        //  [l1 [arg1 arg2] body1]
        //  [l2 body2]
        //  [l3 [arg1 arg2 arg3] body3])
        //
        // The labels (along with their argument types) can be jumped to from
        // within the expr and within any body. Therefore, we must bind all
        // label symbols with their argument types into the environment before
        // checking any sub expression. This is what ethe below loop does.
        PiType* ty = mk_uvar(ctx.pia);
        untyped->ptype = ty;
        SymPtrAssoc labels = mk_sym_ptr_assoc(untyped->labels.terms.len, a);
        for (size_t i = 0; i < untyped->labels.terms.len; i++) {
            SynLabelBranch* branch = untyped->labels.terms.data[i].val;
            PtrArray* arr = mem_alloc(sizeof(PtrArray), a);
            *arr = mk_ptr_array(branch->args.len, a);
            for (size_t i = 0; i < branch->args.len; i++) {
                SymPtrACell arg = branch->args.data[i];
                PiType* aty;
                if (arg.val) {
                    aty = eval_type(arg.val, env, ctx);
                } else  {
                    aty = mk_uvar(ctx.pia);
                    branch->args.data[i].val = aty;
                }
                push_ptr(aty, arr);
            }
            sym_ptr_bind(untyped->labels.terms.data[i].key, arr, &labels);
        }
        add_labels(labels, env);

        // Now that the environment is setup, we typecheck the expression
        type_check_i(untyped->labels.entry, ty, env, ctx);

        // Then typecheck all label bodies, with arguments appropriately bound in the environment
        for (size_t i = 0 ; i < untyped->labels.terms.len; i++) {
            SynLabelBranch* branch = untyped->labels.terms.data[i].val;
            for (size_t i = 0; i < branch->args.len; i++) {
                SymPtrACell arg = branch->args.data[i];
                type_var(arg.key, arg.val, env);
            }
            type_check_i(branch->body, ty, env, ctx);
            pop_types(env, branch->args.len);
        }
        pop_labels(env, labels.len);
        
        break;
    }
    case SSequence: {
        size_t num_binds = 0;
        for (size_t i = 0; i < untyped->sequence.elements.len; i++) {
            SeqElt* elt = untyped->sequence.elements.data[i];
            if (elt->is_binding) {
                //PiType* type = mk_uvar(a);
                type_infer_i(elt->expr, env, ctx);
                type_var (elt->symbol, elt->expr->ptype, env);
                num_binds++;
            } else {
                type_infer_i(elt->expr, env, ctx);
            }
        }

        pop_types(env, num_binds);
        if (untyped->sequence.elements.len == 0) {
            PiType* t = call_alloc(sizeof(PiType), ctx.pia);
            *t = (PiType) {.sort = TPrim, .prim = Unit};
            untyped->ptype = t;
        } else {
            untyped->ptype = ((SeqElt*)untyped->sequence.elements.data[untyped->sequence.elements.len - 1])->expr->ptype;
        }

        break;
    }
    case SIs: {
        PiType* should_be = eval_type(untyped->is.type, env, ctx);
        type_check_i(untyped->is.val, should_be, env, ctx);
        untyped->ptype = untyped->is.type->type_val; 
        break;
    }
    case SInTo: {
        PiType* distinct_type = eval_type(untyped->is.type, env, ctx);
        if (distinct_type->sort != TDistinct) {
            err.message = mv_cstr_doc("into must move a value into a distinct type!", a);
            throw_pi_error(point, err);
        }
        Module* current = get_std_current_module();
        if ((distinct_type->distinct.source_module != NULL) && (distinct_type->distinct.source_module != current)) {
            err.message = mv_cstr_doc("into for opaque types can only be used in the same module!", a);
            throw_pi_error(point, err);
        }

        type_check_i(untyped->into.val, distinct_type->distinct.type, env, ctx);
        untyped->ptype = distinct_type; 
        break;
    }
    case SOutOf: {
        PiType* distinct_type = eval_type(untyped->out_of.type, env, ctx);
        if (distinct_type->sort != TDistinct) {
            err.message = mv_cstr_doc("out-of must move a value out of a distinct type!", a);
            throw_pi_error(point, err);
        }
        Module* current = get_std_current_module();
        if ((distinct_type->distinct.source_module != NULL) && (distinct_type->distinct.source_module != current)) {
            err.message = mv_cstr_doc("out-of for opaque types can only be used in the same module!", a);
            throw_pi_error(point, err);
        }

        type_check_i(untyped->is.val, distinct_type, env, ctx);
        untyped->ptype = distinct_type->distinct.type; 
        break;
    }
    case SName: {
        PiType* named_type = eval_type(untyped->name.type, env, ctx);
        if (named_type->sort != TNamed) {
            err.message = mv_cstr_doc("name must name a value with a named type!", a);
            throw_pi_error(point, err);
        }

        type_check_i(untyped->name.val, named_type->named.type, env, ctx);
        untyped->ptype = named_type; 
        break;
    }
    case SUnName: {
        type_infer_i(untyped->unname, env, ctx);

        if (untyped->unname->ptype->sort != TNamed) {
            err.message = mv_cstr_doc("Unname expects inner term to be named.", a);
            throw_pi_error(point, err);
        }
        untyped->ptype = untyped->unname->ptype->named.type;
        break;
    }
    case SWiden: {
        PiType* wide_type = eval_type(untyped->widen.type, env, ctx);
        type_infer_i(untyped->widen.val, env, ctx);
        untyped->ptype = wide_type; 
        break;
    }
    case SNarrow: {
        PiType* narrow_type = eval_type(untyped->narrow.type, env, ctx);
        type_infer_i(untyped->narrow.val, env, ctx);
        untyped->ptype = narrow_type; 
        break;
    }
    case SSizeOf: {
        eval_type(untyped->size, env, ctx);
        PiType* out = call_alloc(sizeof(PiType), ctx.pia);
        *out = (PiType){.sort = TPrim, .prim = UInt_64};
        untyped->ptype = out; 
        break;
    }
    case SAlignOf: {
        eval_type(untyped->size, env, ctx);
        PiType* out = call_alloc(sizeof(PiType), ctx.pia);
        *out = (PiType){.sort = TPrim, .prim = UInt_64};
        untyped->ptype = out; 
        break;
    }
    case SOffsetOf: {
        eval_type(untyped->offset_of.body, env, ctx);
        PiType* out = call_alloc(sizeof(PiType), ctx.pia);

        PiType* struct_type = unwrap_type(untyped->offset_of.body->type_val, ctx.pia, ctx.a);
        if (struct_type->sort != TStruct) {
            err.range = untyped->offset_of.body->range;
            err.message = mv_cstr_doc("Unsupported operand: taking offset of non-struct type.",a);
            throw_pi_error(point, err); 
        }

        bool found_field = false;
        for (size_t i = 0; i < struct_type->structure.fields.len; i++) {
            if (symbol_eq(untyped->offset_of.field, struct_type->structure.fields.data[i].key))
                found_field = true;
        }

        if (!found_field) {
            err.range = untyped->offset_of.body->range;
            err.message = mv_cstr_doc("Field missing in provided structure type.",a);
            throw_pi_error(point, err); 
        }

        *out = (PiType){.sort = TPrim, .prim = UInt_64};
        untyped->ptype = out; 
        break;
    }
    case SModule: {
        panic(mv_string("Unsupported operation: inferring type of module"));
    }
    case SProcType: {
        PiType* t = call_alloc(sizeof(PiType), ctx.pia);
        *t = (PiType){.sort = TKind, .kind.nargs = 0};
        untyped->ptype = t;

        for (size_t i = 0; i < untyped->proc_type.args.len; i++) {
            Syntax* syn = untyped->proc_type.args.data[i];
            type_check_i(syn, t, env, ctx);
        }

        Syntax* ret = untyped->proc_type.return_type;
        type_check_i(ret, t, env, ctx);
        break;
    }
    case SStructType: {
        PiType* t = call_alloc(sizeof(PiType), ctx.pia);
        *t = (PiType){.sort = TKind, .kind.nargs = 0};
        untyped->ptype = t;

        for (size_t i = 0; i < untyped->struct_type.fields.len; i++) {
            Syntax* syn = untyped->struct_type.fields.data[i].val;
            type_check_i(syn, t, env, ctx);
        }
        break;
    }
    case SEnumType: {
        PiType* t = call_alloc(sizeof(PiType), ctx.pia);
        *t = (PiType){.sort = TKind, .kind.nargs = 0};
        untyped->ptype = t;

        for (size_t i = 0; i < untyped->enum_type.variants.len; i++) {
            PtrArray* args = untyped->enum_type.variants.data[i].val;

            for (size_t j = 0; j < args->len; j++) {
                Syntax* syn = args->data[j];
                type_check_i(syn, t, env, ctx);
            }
        }
        break;
    }
    case SResetType: {
        PiType* t = call_alloc(sizeof(PiType), ctx.pia);
        *t = (PiType){.sort = TKind, .kind.nargs = 0};
        untyped->ptype = t;

        type_check_i(untyped->reset_type.in, t, env, ctx);
        type_check_i(untyped->reset_type.out, t, env, ctx);
        break;
    }
    case SDynamicType: {
        PiType* t = call_alloc(sizeof(PiType), ctx.pia);
        *t = (PiType){.sort = TKind, .kind.nargs = 0};
        untyped->ptype = t;

        type_check_i(untyped->dynamic_type, t, env, ctx);
        break;
    }
    case SAllType: {
        // For now, assume that each type has the kind Type (i.e. is not a family)
        PiType* ty = call_alloc(sizeof(PiType), ctx.pia);
        *ty = (PiType) {.sort = TKind, .kind.nargs = 0};
        untyped->ptype = ty;

        for (size_t i = 0; i < untyped->bind_type.bindings.len; i++) {
            Symbol arg = untyped->bind_type.bindings.data[i];
            type_var(arg, ty, env);
        }

        type_check_i(untyped->bind_type.body, ty, env, ctx);
        pop_types(env, untyped->bind_type.bindings.len);
        break;
    }
    case SSealedType: {
        PiType* ty = call_alloc(sizeof(PiType), ctx.pia);
        *ty = (PiType) {.sort = TKind, .kind.nargs = 0};
        untyped->ptype = ty;

        for (size_t i = 0; i < untyped->sealed_type.vars.len; i++) {
            Symbol arg = untyped->sealed_type.vars.data[i];
            type_var(arg, ty, env);
        }

        PiType* t = call_alloc(sizeof(PiType), ctx.pia);
        *t = (PiType){.sort = TConstraint, .kind.nargs = 0};
        untyped->ptype = t;
        for (size_t i = 0; i < untyped->sealed_type.implicits.len; i++) {
            Syntax* implicit = untyped->sealed_type.implicits.data[i];
            type_check_i(implicit, t, env, ctx);
        }

        type_check_i(untyped->sealed_type.body, ty, env, ctx);
        pop_types(env, untyped->sealed_type.vars.len);
        break;
    }
    case STypeFamily: {
        // For now, assume that each type has the kind Type (i.e. is not a family)
        PiType* ty = call_alloc(sizeof(PiType), ctx.pia);
        *ty = (PiType) {.sort = TKind, .kind.nargs = untyped->bind_type.bindings.len};
        untyped->ptype = ty;

        PiType* aty = call_alloc(sizeof(PiType), ctx.pia);
        *aty = (PiType) {.sort = TKind, .kind.nargs = 0};
        for (size_t i = 0; i < untyped->bind_type.bindings.len; i++) {
            Symbol arg = untyped->bind_type.bindings.data[i];
            type_var(arg, aty, env);
        }

        type_check_i(untyped->bind_type.body, aty, env, ctx);
        pop_types(env, untyped->bind_type.bindings.len);
        break;
    }
    case SLiftCType: {
        // Get c type
        PiType* c_type = get_c_type();
        type_check_i(untyped->c_type, c_type, env, ctx);

        PiType* ty = call_alloc(sizeof(PiType), ctx.pia);
        *ty = (PiType) {.sort = TKind, .kind.nargs = 0};
        untyped->ptype = ty;
        break;
    }
    case SNamedType: {
        PiType* self_type = mk_uvar(ctx.pia);
        
        type_var(untyped->named_type.name, self_type, env);
        type_check_i(untyped->named_type.body, self_type, env, ctx);
        pop_type(env);

        untyped->ptype = untyped->named_type.body->ptype;
        if (untyped->ptype->sort != TKind) {
            err.message = mv_cstr_doc("Named expects types and families as arguments!", a);
            throw_pi_error(point, err);
        }
        break;
    }
    case SDistinctType: {
        type_infer_i(untyped->distinct_type, env, ctx);
        untyped->ptype= untyped->distinct_type->ptype;
        if (untyped->ptype->sort != TKind) {
            err.message = mv_cstr_doc("Distinct expects types and families as arguments!", a);
            throw_pi_error(point, err);
        }
        break;
    }
    case STraitType: {
        PiType* ty = call_alloc(sizeof(PiType), ctx.pia);
        *ty = (PiType) {.sort = TConstraint, .constraint.nargs = untyped->trait.vars.len};
        untyped->ptype = ty;

        PiType* aty = call_alloc(sizeof(PiType), ctx.pia);
        *aty = (PiType) {.sort = TKind, .kind.nargs = 0};
        for (size_t i = 0; i < untyped->trait.vars.len; i++) {
            Symbol arg = untyped->trait.vars.data[i];
            type_var(arg, aty, env);
        }

        for (size_t i = 0; i < untyped->trait.fields.len; i++) {
            Syntax* s = untyped->trait.fields.data[i].val;
            type_check_i(s, aty, env, ctx);
        }
        pop_types(env, untyped->bind_type.bindings.len);
        break;
    }
    case SOpaqueType: {
        type_infer_i(untyped->distinct_type, env, ctx);
        untyped->ptype= untyped->distinct_type->ptype;
        if (untyped->ptype->sort != TKind) {
            err.message = mv_cstr_doc("Opaque expects types and families as arguments!", a);
            throw_pi_error(point, err);
        }
        break;
    }
    case SCheckedType:
        // In the case of a checked type, the typechecking is already complete,
        // so we can do nothing.
        break;
    case SAnnotation: {
        panic(mv_string("Annotations are only supported at the top level."));
    }
    case SReinterpret: {
        type_infer_i(untyped->reinterpret.type, env, ctx);
        type_infer_i(untyped->reinterpret.body, env, ctx);
        if (untyped->reinterpret.from_native) {
            // • Expect the body to be a c value
            // • expect the type to be a pico type
            if (untyped->reinterpret.body->ptype->sort != TCType) {
                err.range = untyped->reinterpret.body->range;
                err.message = mv_cstr_doc("reinterpret-native input value to be a c value, was not!", a);
                throw_pi_error(point, err);
            }
            if (untyped->reinterpret.type->ptype->sort != TKind) {
                err.range = untyped->reinterpret.type->range;
                err.message = mv_cstr_doc("reinterpret-native expected output type to be a relic type, was not!", a);
                throw_pi_error(point, err);
            }

            // Evaluate the output type, check that can reinterpret.
            PiType* pico_type = *(PiType**)eval_typed_expr(untyped->reinterpret.type, env, ctx);
            CType* c_type = &untyped->reinterpret.body->ptype->c_type;

            if (!can_reinterpret(c_type, pico_type)) {
                err.message = mv_cstr_doc("Cannot reinterpret c value as relic value.", a);
                throw_pi_error(point, err);
            }
            untyped->ptype = pico_type;
        } else {
            // • Expect the body to be a pico value (which all values are, so skip check)
            // • Expect the type to be a c type
            if (untyped->reinterpret.type->ptype->sort != TKind) {
                err.range = untyped->reinterpret.type->range;
                err.message = mv_cstr_doc("reinterpret-relic expected output type to be a type, was not!", a);
                throw_pi_error(point, err);
            }

            // Evaluate the output type, check that can reinterpret.
            PiType* type_val = *(PiType**)eval_typed_expr(untyped->reinterpret.type, env, ctx);
            if (type_val->sort != TCType) {
                err.range = untyped->reinterpret.type->range;
                err.message = mv_cstr_doc("reinterpret-relic expected output type to be a c type, was not!", a);
                throw_pi_error(point, err);
            }

            CType* c_type = &type_val->c_type;
            PiType* pico_type = untyped->reinterpret.body->ptype;

            if (!can_reinterpret(c_type, pico_type)) {
                err.message = mv_cstr_doc("Cannot reinterpret relic value as c value.", a);
                throw_pi_error(point, err);
            }
            untyped->ptype = type_val;
        }

        break;
    }
    case SConvert: {
        type_infer_i(untyped->convert.type, env, ctx);
        type_infer_i(untyped->convert.body, env, ctx);
        if (untyped->convert.from_native) {
            // • Expect the body to be a c value
            // • expect the type to be a pico type
            if (untyped->convert.body->ptype->sort != TCType) {
                err.message = mv_cstr_doc("convert-native input value to be a c value, was not!", a);
                throw_pi_error(point, err);
            }
            if (untyped->convert.type->ptype->sort != TKind) {
                err.message = mv_cstr_doc("convert-native expected output type to be a relic type, was not!", a);
                throw_pi_error(point, err);
            }

            // Evaluate the output type, check that can convert.
            PiType* pico_type = *(PiType**)eval_typed_expr(untyped->convert.type, env, ctx);
            CType* c_type = &untyped->convert.body->ptype->c_type;

            if (!can_convert(c_type, pico_type)) {
                PtrArray nodes = mk_ptr_array(4, a);
                push_ptr(mv_str_doc(mv_string("Cannot convert between C Type: "), a), &nodes);
                push_ptr(pretty_ctype(c_type, a), &nodes);
                push_ptr(mv_str_doc(mv_string("\nand Relic Type: "), a), &nodes);
                push_ptr(pretty_type(pico_type, a), &nodes);
                err.message = mv_cat_doc(nodes, a);
                throw_pi_error(point, err);
            }
            untyped->ptype = pico_type;
        } else {
            // • Expect the body to be a pico value (which all values are, so skip check)
            // • Expect the type to be a c type
            if (untyped->convert.type->ptype->sort != TKind) {
                err.message = mv_cstr_doc("convert-relic expected output type to be a type, was not!", a);
                throw_pi_error(point, err);
            }

            // Evaluate the output type, check that can convert.
            PiType* type_val = *(PiType**)eval_typed_expr(untyped->convert.type, env, ctx);
            if (type_val->sort != TCType) {
                err.message = mv_cstr_doc("convert-relic expected output type to be a c type, was not!", a);
                throw_pi_error(point, err);
            }

            CType* c_type = &type_val->c_type;
            PiType* pico_type = untyped->convert.body->ptype;

            if (!can_convert(c_type, pico_type)) {
                err.message = mv_cstr_doc("Cannot convert relic value as c value.", a);
                throw_pi_error(point, err);
            }
            untyped->ptype = type_val;
        }

        break;
    }
    case STypeOf: {
        type_infer_i(untyped->type_of, env, ctx);
        untyped->ptype = call_alloc(sizeof(PiType), ctx.pia);
        *untyped->ptype = (PiType) {.sort = TKind, .kind.nargs = 0};
        break;
    }
    case SDescribe: {
        TypeEntry entry = type_env_lookup(untyped->to_describe.data[0], env);
        //EnvEntry entry = env_lookup(syn.to_describe.data[0], base);
        for (size_t i = 1; i < untyped->to_describe.len; i++) {
            if (entry.type == TENotFound) {
                err.message = mv_cstr_doc("Unknown symbol in path to describe.", a);
                throw_pi_error(point, err);
            }

            if (entry.is_module) {
                ModuleEntry* mentry = get_def(untyped->to_describe.data[i], entry.module);
                if (mentry) {
                    entry.is_module = mentry->is_module;
                    entry.module = mentry->value;
                    entry.ptype = &mentry->type;
                } else {
                    err.message = mv_cstr_doc("Unknown symbol in path to describe.", a);
                    throw_pi_error(point, err);
                }
            } else {
                err.message = mv_cstr_doc("Expected module when describing.", a);
                throw_pi_error(point, err);
            }
        }
        if (entry.type == TENotFound) {
            err.message = mv_cstr_doc("Unknown symbol in path to describe.", a);
            throw_pi_error(point, err);
        }
        untyped->ptype = mk_string_type(ctx.pia);
        break;
    }
    case SQuote: {
        untyped->ptype = get_syntax_type();
        break;
    }
    case SCapture: {
        untyped->ptype = get_syntax_type();
        break;
    }

    }

    if (ctx.logger) {
        PtrArray docs = mk_ptr_array(2, a);
        push_ptr(mv_str_doc(mv_string("Inferred type:"), a), &docs);
        push_ptr(pretty_type(untyped->ptype, a), &docs);
        log_doc(mv_sep_doc(docs, a), ctx.logger);
        end_section(ctx.logger);
    }
    if (untyped->ptype == NULL) {
        panic(mv_string("Internal Error: typecheck failed to infer type."));
    }
}

// Post Unify: perform actions which require types to be resolved,
// i.e. no uvars!
// This includes
// - Instantiating implicits
// - Checking that widen and narrow widen and narrow their arguments, respectively
void post_unify(Syntax* syn, TypeEnv* env, PiAllocator* pia, Allocator* a, PiErrorPoint* point) {
    PicoError err;
    err.range = syn->range;
    switch (syn->type) {
    case SLitUntypedIntegral:
    case SLitTypedIntegral:
    case SLitUntypedFloating:
    case SLitTypedFloating:
    case SLitString:
    case SLitBool:
    case SLitUnit:
    case SVariable:
    case SAbsVariable:
        break;
    case SLitArray: {
        for (size_t i = 0; i < syn->array_lit.subterms.len; i++) {
            post_unify(syn->array_lit.subterms.data[i], env, pia, a, point);
        }
        break;
    }

    // Terms & term formers
    case SProcedure: {
        for (size_t i = 0; i < syn->procedure.implicits.len; i++) {
            SymPtrACell arg = syn->procedure.implicits.data[i];
            type_var(arg.key, arg.val, env);
        }
        for (size_t i = 0; i < syn->procedure.args.len; i++) {
            SymPtrACell arg = syn->procedure.args.data[i];
            type_var(arg.key, arg.val, env);
        }
        post_unify(syn->procedure.body, env, pia, a, point);
        pop_types(env, syn->procedure.args.len + syn->procedure.implicits.len);
        break;
    }
    case SAll: {
        for (size_t i = 0; i < syn->all.args.len; i++) {
            Symbol arg = syn->all.args.data[i];

            PiType* arg_ty = call_alloc(sizeof(PiType), pia);
            *arg_ty = (PiType) {.sort = TVar, .var = arg,};

            type_qvar(arg, arg_ty, env);
        }
        post_unify(syn->all.body, env, pia, a, point);
        pop_types(env, syn->all.args.len);
        break;
    }
    case SMacro: {
        post_unify(syn->transformer, env, pia, a, point);
        break;
    }
    case SApplication: {
        post_unify(syn->application.function, env, pia, a, point);
        for (size_t i = 0; i < syn->application.args.len; i++) {
            post_unify(syn->application.args.data[i], env, pia, a, point);
        }

        if (syn->application.implicits.len != 0) {
            err.message = mv_cstr_doc("Implicit instantiation assumes no implicits are already present!", a);
            throw_pi_error(point, err);
        }
        PiType fn_type = *syn->application.function->ptype;
        if (fn_type.sort == TProc) {
            for (size_t i = 0; i < fn_type.proc.implicits.len; i++) {
                PiType* arg_ty = fn_type.proc.implicits.data[i];
                if (arg_ty->sort != TTraitInstance) {
                    err.message = mv_cstr_doc("Implicit arguments must have type trait instance!", a);
                    throw_pi_error(point, err);
                }

                InstanceEntry e = type_instance_lookup(arg_ty->instance.instance_of, arg_ty->instance.args, env);
                switch (e.type) {
                case IEAbsSymbol: {
                    Syntax* new_impl = mem_alloc(sizeof(Syntax), a);
                    *new_impl = (Syntax) {
                        .type = SAbsVariable,
                        .abvar = e.abvar,
                        .ptype = arg_ty,
                    };
                    push_ptr(new_impl, &syn->application.implicits);
                    break;
                }
                case IENotFound:
                    err.message = mv_cstr_doc("Implicit argument cannot be instantiated - instance not found!", a);
                    throw_pi_error(point, err);
                case IEAmbiguous:
                    err.message = mv_cstr_doc("Implicit argument cannot be instantiated - ambiguous instances!", a);
                    throw_pi_error(point, err);
                default:
                    panic(mv_string("Invalid instance entry type!"));
                }
            }
        } else if (fn_type.sort != TKind && fn_type.sort != TConstraint ) {
            panic(mv_string("Invalid lhs in application in post_unify: not Proc or Kind"));
        } 
        break;
    }
    case SAllApplication: {
        post_unify(syn->all_application.function, env, pia, a, point);
        for (size_t i = 0; i < syn->all_application.args.len; i++) {
            post_unify(syn->all_application.args.data[i], env, pia, a, point);
        }

        if (syn->all_application.implicits.len != 0) {
            err.message = mv_cstr_doc("Implicit instantiation assumes no implicits are already present!", a);
            throw_pi_error(point, err);
        }

        // Given, e.g. an application (a {I64 Bool} x y), with a : All [A B] t, perform a
        // beta-reduction to get t[A/I64, B/Bool]. This will be used to
        // instantiate implicits if t is a Proc.
        PiType all_type = *syn->all_application.function->ptype;
        SymPtrAssoc type_binds = mk_sym_ptr_assoc(all_type.binder.vars.len, a);
        for (size_t i = 0; i < all_type.binder.vars.len; i++) {
            Syntax* type = syn->all_application.types.data[i];
            sym_ptr_bind(all_type.binder.vars.data[i], type->type_val, &type_binds);
        }
        // TODO (BUG): this type probably wants unwrapping
        PiType* proc_type = pi_type_subst(all_type.binder.body, type_binds, pia, a);

        // Early exit if we don't need to do any instantiation.
        if (proc_type->sort != TProc) return;

        for (size_t i = 0; i < proc_type->proc.implicits.len; i++) {
            PiType* arg_ty = proc_type->proc.implicits.data[i];
            if (arg_ty->sort != TTraitInstance) {
                err.message = mv_cstr_doc("Implicit arguments must have type trait instance!", a);
                throw_pi_error(point, err);
            }

            InstanceEntry e = type_instance_lookup(arg_ty->instance.instance_of, arg_ty->instance.args, env);
            switch (e.type) {
            case IEAbsSymbol: {
                Syntax* new_impl = mem_alloc(sizeof(Syntax), a);
                *new_impl = (Syntax) {
                    .type = SAbsVariable,
                    .abvar = e.abvar,
                    .ptype = arg_ty,
                };
                push_ptr(new_impl, &syn->all_application.implicits);
                break;
            }
            case IENotFound:
                err.message = mv_cstr_doc("Implicit argument cannot be instantiated - instance not found!", a);
                throw_pi_error(point, err);
            case IEAmbiguous:
                err.message = mv_cstr_doc("Implicit argument cannot be instantiated - ambiguous instances!", a);
                throw_pi_error(point, err);
            default:
                panic(mv_string("Invalid instance entry type!"));
            }
        }
        break;
    }
    case SSeal: {
        if (syn->seal.implicits.len > 0) {
            panic(mv_string("not implemented: post-unify for seal with > 0 implicits"));
        }
        post_unify(syn->seal.body, env, pia, a, point);
        break;
    }
    case SUnseal: {
        SymPtrAssoc type_binds = mk_sym_ptr_assoc(syn->unseal.types.len, a);
        for (size_t i = 0; i < syn->unseal.types.len; i++) {
            Symbol arg = syn->unseal.types.data[i];

            PiType* arg_ty = call_alloc(sizeof(PiType), pia);
            *arg_ty = (PiType) {.sort = TVar, .var = arg,};

            sym_ptr_bind(arg, arg_ty, &type_binds);
            type_qvar(arg, arg_ty, env);
        }
        PiType* var_ty = pi_type_subst(syn->unseal.sealed->ptype->sealed.body, type_binds, pia, a);
        type_var(syn->unseal.binder, var_ty, env);
        post_unify(syn->unseal.body, env, pia, a, point);
        pop_types(env, syn->all.args.len + 1);
        break;
    }
    case SConstructor:  {
        // Resolve the variant tag
        PiType* enum_type = unwrap_type(syn->ptype, pia, a);

        bool found_variant = false;
        for (size_t i = 0; i < enum_type->enumeration.variants.len; i++) {
            SymAddrPiCell cell = enum_type->enumeration.variants.data[i];
            if (symbol_eq(cell.key, syn->variant.tagname)) {
                found_variant = true;
                syn->variant.tag = i;
                break;
            }
        }
        if (!found_variant) {
            panic(mv_string("Unable to find constructor tag in post-unify."));
        }
        break;
    }
    case SVariant: {
        // Resolve the variant tag
        PiType* enum_type = unwrap_type(syn->ptype, pia, a);

        bool found_variant = false;
        for (size_t i = 0; i < enum_type->enumeration.variants.len; i++) {
            SymAddrPiCell cell = enum_type->enumeration.variants.data[i];
            if (symbol_eq(cell.key, syn->variant.tagname)) {
                found_variant = true;
                syn->variant.tag = i;
                break;
            }
        }
        if (!found_variant) {
            panic(mv_string("Unable to find variant tag in post-unify."));
        }
        for (size_t i = 0; i < syn->variant.args.len; i++) {
            post_unify(syn->variant.args.data[i], env, pia, a, point);
        }
        break;
    }
    case SMatch: {
        post_unify(syn->match.val, env, pia, a, point);

        for (size_t i = 0; i < syn->match.clauses.len; i++) {
            SynClause* clause = syn->match.clauses.data[i];

            // TODO BUG: bind types/vars from clause
            post_unify(clause->body, env, pia, a, point);
        }
        break;
    }
    case SStructure: {
        if (syn->structure.base) {
            post_unify(syn->structure.base, env, pia, a, point);
        }
        for (size_t i = 0; i < syn->structure.fields.len; i++) {
            post_unify(syn->structure.fields.data[i].val, env, pia, a, point);
        }
        break;
    }
    case SProjector: {
        post_unify(syn->projector.val, env, pia, a, point);
        break;
    }
    case SInstance: {
        PiType* ty_ty = call_alloc(sizeof(PiType), pia);
        *ty_ty = (PiType) {.sort = TKind, .constraint.nargs = 0};

        for (size_t i = 0; i < syn->instance.params.len; i++) {
            Symbol arg = syn->instance.params.data[i];
            type_var(arg, ty_ty, env);
        }

        for (size_t i = 0; i < syn->instance.implicits.len; i++) {
            SymPtrACell arg = syn->instance.implicits.data[i];
            type_var(arg.key, arg.val, env);
        }

        for (size_t i = 0; i < syn->ptype->instance.fields.len; i++) {
            Syntax** field_syn = (Syntax**)sym_ptr_lookup(syn->ptype->instance.fields.data[i].key, syn->instance.fields);
            if (field_syn) {
                post_unify(*field_syn, env, pia, a, point);
            } else {
                err.message = mv_cstr_doc("Trait instance is missing a field", a);
                throw_pi_error(point, err);
            }
        }

        pop_types(env, syn->instance.params.len + syn->instance.implicits.len);
        break;
    }
    case SDynamic:
        post_unify(syn->dynamic, env, pia, a, point);
        break;
    case SDynamicUse:
        post_unify(syn->use, env, pia, a, point);
        break;
    case SDynamicSet:
        post_unify(syn->dynamic_set.dynamic, env, pia, a, point);
        post_unify(syn->dynamic_set.new_val, env, pia, a, point);
        break;

    // Control Flow & Binding
    case SDynamicLet:
        for (size_t i = 0; i < syn->dyn_let_expr.bindings.len; i++) {
            DynBinding* b = syn->dyn_let_expr.bindings.data[i];
            post_unify(b->var, env, pia, a, point);
            post_unify(b->expr, env, pia, a, point);
        }
        post_unify(syn->dyn_let_expr.body, env, pia, a, point);
        break;
    case SLet:
        // TODO BUG: update environment 
        for (size_t i = 0; i < syn->let_expr.bindings.len; i++) {
            post_unify(syn->let_expr.bindings.data[i].val, env, pia, a, point);
        }
        post_unify(syn->let_expr.body, env, pia, a, point);
        break;
    case SIf:
        post_unify(syn->if_expr.condition, env, pia, a, point);
        post_unify(syn->if_expr.true_branch, env, pia, a, point);
        post_unify(syn->if_expr.false_branch, env, pia, a, point);
        break;
    case SLabels:
        post_unify(syn->labels.entry, env, pia, a, point);
        for (size_t i = 0; i < syn->labels.terms.len; i++) {
            SynLabelBranch* branch = syn->labels.terms.data[i].val;
            post_unify(branch->body, env, pia, a, point);
        }
        break;
    case SGoTo:
        for (size_t i = 0; i < syn->go_to.args.len; i++) {
            post_unify(syn->go_to.args.data[i], env, pia, a, point);
        }
        break;
    case SSequence:
        for (size_t i = 0; i < syn->sequence.elements.len; i++) {
            SeqElt* elt = syn->sequence.elements.data[i];
            post_unify(elt->expr, env, pia, a, point);
        }
        break;
    case SWithReset:
        // TODO BUG: update environment 
        post_unify(syn->with_reset.expr, env, pia, a, point);
        post_unify(syn->with_reset.handler, env, pia, a, point);
        break;
    case SResetTo:
        post_unify(syn->reset_to.point, env, pia, a, point);
        post_unify(syn->reset_to.arg, env, pia, a, point);
        break;

    // Special
    case SIs:
        post_unify(syn->is.val, env, pia, a, point);
        post_unify(syn->is.type, env, pia, a, point);
        break;
    case SInTo:
        post_unify(syn->into.val, env, pia, a, point);
        post_unify(syn->into.type, env, pia, a, point);
        break;
    case SOutOf:
        post_unify(syn->out_of.val, env, pia, a, point);
        post_unify(syn->out_of.type, env, pia, a, point);
        break;
    case SName:
        post_unify(syn->name.val, env, pia, a, point);
        post_unify(syn->name.type, env, pia, a, point);
        break;
    case SWiden:
        post_unify(syn->widen.val, env, pia, a, point);
        post_unify(syn->widen.type, env, pia, a, point);
        if (!is_wider(syn->widen.val->ptype, syn->widen.type->type_val)) {
            err.message = mv_cstr_doc("This widening is invalid", a);
            throw_pi_error(point, err);
        }
        break;
    case SNarrow:
        post_unify(syn->narrow.val, env, pia, a, point);
        post_unify(syn->narrow.type, env, pia, a, point);
        if (!is_narrower(syn->narrow.val->ptype, syn->narrow.type->type_val)) {
            PtrArray docs = mk_ptr_array(2, a);
            push_ptr(mv_cstr_doc("This narrowing is invalid - cannot narrow from type:", a), &docs);
            push_ptr(pretty_type(syn->narrow.val->ptype, a), &docs);
            push_ptr(mv_cstr_doc("to type:", a), &docs);
            push_ptr(pretty_type(syn->narrow.type->type_val, a), &docs);
            err.message = mv_hsep_doc(docs, a);
            throw_pi_error(point, err);
        }
        break;
    case SUnName:
        post_unify(syn->unname, env, pia, a, point);
        break;
    case SSizeOf:
    case SAlignOf:
        post_unify(syn->size, env, pia, a, point);
        break;
    case SOffsetOf:
        post_unify(syn->offset_of.body, env, pia, a, point);
        break;
    case SModule:
        panic(mv_string("instantiate implicits not implemented for module"));

    // Types & Type formers
    case SProcType:
    case SStructType:
    case SEnumType:
    case SResetType:
    case SDynamicType:
    case SNamedType:
    case SDistinctType:
    case SOpaqueType:
    case STraitType:
    case SAllType:
    case SSealedType:
    case STypeFamily:
        break;
    case SLiftCType:
        post_unify(syn->c_type, env, pia, a, point);
        break;
    case SCheckedType:
        // TODO (INVESTIGATE FEATURE): check that it is OK to do nothing? (no implicits in types, right?)
        //      what if we have types produced by procedures, i.e. Proc [...] Type)?
        break;

    case SAnnotation:
        panic(mv_string("instantiate implicits not implemented for a annotation"));
    case SReinterpret:
        post_unify(syn->reinterpret.type, env, pia, a, point);
        post_unify(syn->reinterpret.body, env, pia, a, point);
        break;
    case SConvert:
        post_unify(syn->convert.type, env, pia, a, point);
        post_unify(syn->convert.body, env, pia, a, point);
        break;
    case STypeOf: {
        post_unify(syn->type_of, env, pia, a, point);
        break;
    }
    case SDescribe: 
    case SQuote: 
    case SCapture: 
        break;
    }
}

// This function recursively descends into a term and squashes all types.
// In this case, to squash a type removes all unification vars from 
// the type. (see squash_type in unify.h)
void squash_types(Syntax* typed, TypeCheckContext ctx) {
    UnifyContext uctx = {
        .a = ctx.a,
        .pia = ctx.pia,
        .logger = ctx.logger,
    };
    PicoError err = {.range = typed->range};
    switch (typed->type) {
    case SLitUntypedIntegral:
    case SLitTypedIntegral:
    case SLitUntypedFloating:
    case SLitTypedFloating:
    case SLitBool:
    case SLitUnit:
    case SLitString:
    case SVariable:
    case SAbsVariable:
        break;
    case SLitArray: {
        for (size_t i = 0; i < typed->array_lit.subterms.len; i++) {
            squash_types(typed->array_lit.subterms.data[i], ctx);
        }
        break;
    }
    case SProcedure: {
        squash_types(typed->procedure.body, ctx);
        break;
    }
    case SAll: {
        // TODO (FUTURE BUG): need to squash args when HKTs are allowed 
        squash_types(typed->all.body, ctx);
        break;
    }
    case SMacro: { 
        squash_types(typed->transformer, ctx);
        break;
    }
    case SApplication: {
        squash_types(typed->application.function, ctx);
        
        for (size_t i = 0; i < typed->application.implicits.len; i++) {
            squash_types(typed->application.implicits.data[i], ctx);
        }

        for (size_t i = 0; i < typed->application.args.len; i++) {
            squash_types(typed->application.args.data[i], ctx);
        }
        break;
    }
    case SAllApplication: {
        squash_types(typed->application.function, ctx);

        for (size_t i = 0; i < typed->all_application.types.len; i++) {
            squash_types(typed->all_application.types.data[i], ctx);
            Syntax* checked = typed->all_application.types.data[i];
            if (checked->type_val->sort == TUVar) {
                PtrArray nodes = mk_ptr_array(4, ctx.a);
                push_ptr(mk_str_doc(mv_string("Typechecking error: All application type not instantiated"), ctx.a), &nodes);
                push_ptr(pretty_type(checked->type_val, ctx.a), &nodes);

                err.message = mv_vsep_doc(nodes, ctx.a);
                throw_pi_error(ctx.point, err);
            }
        }

        for (size_t i = 0; i < typed->all_application.implicits.len; i++) {
            squash_types(typed->all_application.implicits.data[i], ctx);
        }
        
        for (size_t i = 0; i < typed->all_application.args.len; i++) {
            squash_types(typed->all_application.args.data[i], ctx);
        }
        break;
    }
    case SSeal: {
        squash_types(typed->seal.body, ctx);

        for (size_t i = 0; i < typed->seal.types.len; i++) {
            squash_types(typed->seal.types.data[i], ctx);
        }

        for (size_t i = 0; i < typed->seal.implicits.len; i++) {
            squash_types(typed->seal.implicits.data[i], ctx);
        }
        break;
    }
    case SUnseal: {
        squash_types(typed->unseal.sealed, ctx);

        // TODO (BUG!): ensure that there are no free types in typed->unseal.body->ptype
        squash_types(typed->unseal.body, ctx);
        break;
    }
    case SConstructor: {
        if (typed->variant.enum_type) {
            squash_types(typed->variant.enum_type, ctx);
        }
        break;
    }
    case SVariant: {
        if (typed->variant.enum_type) {
            squash_types(typed->variant.enum_type, ctx);
        }
        
        for (size_t i = 0; i < typed->variant.args.len; i++) {
            squash_types(typed->variant.args.data[i], ctx);
        }
        break;
    }
    case SMatch: {
        squash_types(typed->match.val, ctx);

        for (size_t i = 0; i < typed->match.clauses.len; i++) {
            SynClause* clause = (SynClause*)typed->match.clauses.data[i];
            squash_types(clause->body, ctx);
        }
        break;
    }
    case SStructure: {
        if (typed->structure.base) {
            squash_types(typed->structure.base, ctx);
        }
        for (size_t i = 0; i < typed->structure.fields.len; i++) {
            Syntax* syn = typed->structure.fields.data[i].val;
            squash_types(syn, ctx);
        }
        break;
    }
    case SProjector:
        squash_types(typed->projector.val, ctx);
        break;
    case SInstance:
        squash_types(typed->instance.constraint, ctx);

        for (size_t i = 0; i < typed->instance.implicits.len; i++) {
            Syntax* syn = typed->instance.fields.data[i].val;
            squash_types(syn, ctx);
        }

        for (size_t i = 0; i < typed->instance.fields.len; i++) {
            Syntax* syn = typed->instance.fields.data[i].val;
            squash_types(syn, ctx);
        }
        break;
    case SDynamic:
        squash_types(typed->dynamic, ctx);
        break;
    case SDynamicUse:
        squash_types(typed->use, ctx);
        break;
    case SDynamicSet:
        squash_types(typed->dynamic_set.dynamic, ctx);
        squash_types(typed->dynamic_set.new_val, ctx);
        break;
    case SDynamicLet:
        for (size_t i = 0; i < typed->dyn_let_expr.bindings.len; i++) {
            DynBinding* dbind = typed->dyn_let_expr.bindings.data[i];
            squash_types(dbind->var, ctx);
            squash_types(dbind->expr, ctx);
        }
        squash_types(typed->dyn_let_expr.body, ctx);
        break;
    case SLet:
        for (size_t i = 0; i < typed->let_expr.bindings.len; i++) {
            squash_types(typed->let_expr.bindings.data[i].val, ctx);
        }
        squash_types(typed->let_expr.body, ctx);
        break;
    case SIf: {
        squash_types(typed->if_expr.condition, ctx);
        squash_types(typed->if_expr.true_branch, ctx);
        squash_types(typed->if_expr.false_branch, ctx);
        break;
    }
    case SLabels:
        squash_types(typed->labels.entry, ctx);
        for (size_t i = 0; i < typed->labels.terms.len; i++) {
            SynLabelBranch* branch = typed->labels.terms.data[i].val;
            squash_types(branch->body, ctx);
        }
        break;
    case SGoTo:
        for (size_t i = 0; i < typed->go_to.args.len; i++) {
            squash_types(typed->go_to.args.data[i], ctx);
        }
        break;
    case SWithReset:
        squash_types(typed->with_reset.expr, ctx);
        squash_types(typed->with_reset.handler, ctx);

        if (!has_unification_vars_p(*typed->with_reset.in_arg_ty)) {
            squash_type(typed->with_reset.in_arg_ty, uctx);
        } else {
            err.message = mv_cstr_doc("reset argument type not instantiated", ctx.a);
            throw_pi_error(ctx.point, err);
        }
        if (!has_unification_vars_p(*typed->with_reset.cont_arg_ty)) {
            squash_type(typed->with_reset.cont_arg_ty, uctx);
        } else {
            err.message = mv_cstr_doc("resume argument type not instantiated", ctx.a);
            throw_pi_error(ctx.point, err);
        }
        break;
    case SResetTo:
        squash_types(typed->reset_to.point, ctx);
        squash_types(typed->reset_to.arg, ctx);
        break;
    case SSequence:
        for (size_t i = 0; i < typed->sequence.elements.len; i++) {
            SeqElt* elt = typed->sequence.elements.data[i];
            squash_types(elt->expr, ctx);
        }
        break;
    case SIs:
        squash_type(typed->is.type->type_val, uctx);
        squash_types(typed->is.val, ctx);
        break;
    case SInTo:
        squash_type(typed->into.type->type_val, uctx);
        squash_types(typed->into.val, ctx);
        break;
    case SOutOf:
        squash_type(typed->out_of.type->type_val, uctx);
        squash_types(typed->out_of.val, ctx);
        break;
    case SName:
        squash_type(typed->name.type->type_val, uctx);
        squash_types(typed->name.val, ctx);
        break;
    case SUnName:
        squash_types(typed->unname, ctx);
        break;
    case SWiden:
        squash_type(typed->widen.type->type_val, uctx);
        squash_types(typed->widen.val, ctx);
        break;
    case SNarrow:
        squash_type(typed->narrow.type->type_val, uctx);
        squash_types(typed->narrow.val, ctx);
        break;
    case SSizeOf:
    case SAlignOf:
        squash_types(typed->size, ctx);
        break;
    case SOffsetOf:
        squash_types(typed->offset_of.body, ctx);
        break;
    case SProcType: {
        for (size_t i = 0; i < typed->proc_type.args.len; i++) {
            squash_types(typed->proc_type.args.data[i], ctx);
        }

        squash_types(typed->proc_type.return_type, ctx);
        break;
    }
    case SStructType: {
        for (size_t i = 0; i < typed->struct_type.fields.len; i++) {
            squash_types(typed->struct_type.fields.data[i].val, ctx);
        }
        break;
    }
    case SEnumType: {
        for (size_t i = 0; i < typed->enum_type.variants.len; i++) {
            PtrArray* args = typed->enum_type.variants.data[i].val;

            for (size_t j = 0; j < args->len; j++) {
                squash_types(args->data[j], ctx);
            }
        }
        break;
    }
    case SResetType: {
        squash_types(typed->reset_type.in, ctx);
        squash_types(typed->reset_type.out, ctx);
        break;
    }
    case SDynamicType: {
        squash_types(typed->dynamic_type, ctx);
        break;
    }
    case SAllType:
        squash_types(typed->bind_type.body, ctx);
        break;
    case SSealedType:
        squash_types(typed->sealed_type.body, ctx);
        break;
    case STypeFamily:
        squash_types(typed->bind_type.body, ctx);
        break;
    case SLiftCType:
        squash_types(typed->c_type, ctx);
        break;
    case SNamedType:
        squash_types(typed->named_type.body, ctx);
        break;
    case SDistinctType:
        squash_types(typed->distinct_type, ctx);
        break;
    case SOpaqueType:
        squash_types(typed->opaque_type, ctx);
        break;
    case STraitType:
        for (size_t i = 0; i < typed->trait.fields.len; i++) {
            squash_types(typed->trait.fields.data[i].val, ctx);
        }
        break;
    case SCheckedType: {
        // TODO: it seems like *sometimes* we want for this to be allowed as a
        //   uvar (notably when calling eval_type), but otherwise don't want
        //   to throw an error here.
        squash_type(typed->type_val, uctx);
        break;
    }
    case SReinterpret:
        squash_types(typed->reinterpret.type, ctx);
        squash_types(typed->reinterpret.body, ctx);
        break;
    case SConvert:
        squash_types(typed->convert.type, ctx);
        squash_types(typed->convert.body, ctx);
        break;
    case STypeOf: {
        squash_types(typed->type_of, ctx);
        break;
    }
    case SDescribe:
    case SQuote:
    case SCapture: 
        break;
    default:
        panic(mv_string("Internal Error: invalid syntactic form provided to squash_types"));
        break;
    }

    // has_unification_vars only returns true if those vars don't go anywhere!
    if (!has_unification_vars_p(*typed->ptype)) {
        squash_type(typed->ptype, uctx);
    }
    else {
        squash_type(typed->ptype, uctx);

        PtrArray nodes = mk_ptr_array(4, ctx.a);
        push_ptr(mk_str_doc(mv_string("Typechecking error: not all unification vars were instantiated. Term:"), ctx.a), &nodes);
        push_ptr(pretty_syntax(typed, ctx.a), &nodes);
        push_ptr(mk_str_doc(mv_string("Type:"), ctx.a), &nodes);
        push_ptr(pretty_type(typed->ptype, ctx.a), &nodes);

        err.message = mv_vsep_doc(nodes, ctx.a);
        throw_pi_error(ctx.point, err);
    }
}

void* eval_typed_expr(Syntax* typed, TypeEnv* env, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    PiErrorPoint* point = ctx.point;
    Target gen_target = ctx.target;
    clear_target(gen_target);

    if (ctx.logger) {
        String str = string_cat(mv_string("evaluating: "), syntax_type_to_string(typed->type), a);
        start_section(str, ctx.logger);

        PtrArray docs = mk_ptr_array(2, a);
        push_ptr(mk_str_doc(mv_string("syntax:"), a), &docs);
        push_ptr(mv_nest_doc(2, pretty_syntax(typed, a), a), &docs);
        log_doc(mv_sep_doc(docs, ctx.a), ctx.logger);
    }

    squash_types(typed, ctx);
    post_unify(typed, env, ctx.pia, a, point);
    
    // Catch error here; so can cleanup after self before further unwinding.
    ErrorPoint cleanup_point;
    if (catch_error(cleanup_point)) goto on_error;

    // TODO (INVESTIGATE): is LinkData needed
    generate_type_expr(typed, env, gen_target, a, &cleanup_point);

    void* result = pico_run_expr(gen_target, pi_size_of(*typed->ptype), a, &cleanup_point);

    if (ctx.logger) {
        PiType** ptype = result;

        PtrArray docs = mk_ptr_array(2, a);
        push_ptr(mk_str_doc(mv_string("evaluated to:"), a), &docs);
        push_ptr(mv_nest_doc(2, pretty_type(*ptype, a), a), &docs);
        log_doc(mv_sep_doc(docs, ctx.a), ctx.logger);

        end_section(ctx.logger);
    }
    return result;

 on_error: {
        PicoError err;
        err.range = typed->range;
        err.message = mv_str_doc(cleanup_point.error_message, a);
        throw_pi_error(point, err);
    }
}

void* eval_expr(Syntax* untyped, TypeEnv* env, TypeCheckContext ctx) {
    type_infer_i(untyped, env, ctx);
    return eval_typed_expr(untyped, env, ctx);
}

// TODO (BUG LOGIC UB): evaluation may produce a function pointer (or an object
// with a function pointer) that points to generated code. This method currently
// provides no means to capture that assembly.
PiType* eval_type(Syntax* untyped, TypeEnv* env, TypeCheckContext ctx) {
    type_infer_i(untyped, env, ctx);

    if (untyped->ptype->sort != TKind && untyped->ptype->sort != TConstraint) {
        PicoError err = (PicoError) {
            .range = untyped->range,
            .message = mv_cstr_doc("Value expected to be type, was not!", ctx.a),
        };
        throw_pi_error(ctx.point, err);
    }

    PiType** result = eval_typed_expr(untyped, env, ctx);
    untyped->type = SCheckedType;
    untyped->type_val = *result;

    return *result;
}

void check_result_out(UnifyResult out, Range range, Allocator* a, PiErrorPoint* point) {
    if (out.type == USimpleError) {
        PicoError err = (PicoError) {
            .range = range,
            .message = out.message, 
        };
        throw_pi_error(point, err);
    }
    if (out.type == UConstraintError) {
        PtrArray errs = mk_ptr_array(2, a);
        PicoError* err_main = mem_alloc(sizeof(PicoError), a);
        *err_main = (PicoError) {
            .range = range,
            .message = out.message,
        };

        PicoError* err_src = mem_alloc(sizeof(PicoError), a);
        *err_src = (PicoError) {
            .range = out.initial,
            .message = mv_cstr_doc("Constraint was introduced here", a)
        };
        push_ptr(err_main, &errs);
        push_ptr(err_src, &errs);

        throw_pi_errors(point, errs);
    }
}
