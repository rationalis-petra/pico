#include "platform/error.h"
#include "platform/signals.h"

#include "data/string.h"

#include "pico/binding/environment.h"
#include "pico/binding/type_env.h"
#include "pico/typecheck/unify.h"
#include "pico/typecheck/typecheck.h"
#include "pico/typecheck/type_errors.h"
#include "pico/values/ctypes.h"
#include "pico/values/types.h"
#include "pico/codegen/codegen.h"
#include "pico/eval/call.h"
#include "pico/stdlib/core.h"
#include "pico/stdlib/meta/meta.h"
#include "pico/stdlib/foreign.h"

// forward declarations
void type_check_expr(SynRef untyped, PiType type, TypeEnv* env, TypeCheckContext ctx);
void type_infer_expr(SynRef untyped, TypeEnv* env, TypeCheckContext ctx);
void post_unify(SynRef untyped, TypeEnv* env, TypeCheckContext ctx);
PiType* eval_type(SynRef untyped, TypeEnv* env, TypeCheckContext ctx);

// Check a toplevel expression
void type_check(TopLevel* top, Environment* env, TypeCheckContext ctx) {
    // If this is a definition, lookup the type to check against 
    TypeEnv *t_env = mk_type_env(env, ctx.a);
    switch (top->type) {
    case TLDef: {
        PiType* check_against;
        PiType* ty = env_lookup_tydecl(top->def.bind, env);
        SynRef term = top->def.value;

        if (ctx.logger) {
            log_str(mv_string("\n--------------------------------------------------------------------------------\n"), ctx.logger);
            PtrArray nodes = mk_ptr_array(4, ctx.a);
            push_ptr(mv_cstr_doc("                    TYPECHECK FOR:", ctx.a), &nodes);
            push_ptr(mv_str_doc(view_symbol_string(top->def.bind), ctx.a), &nodes);
            log_doc(mv_hsep_doc(nodes, ctx.a), ctx.logger);
            log_str(mv_string("\n--------------------------------------------------------------------------------\n"), ctx.logger);
        }

        if (ty != NULL) {
            check_against = ty;
        } else {
            check_against = mk_uvar(ctx.pia);
        }
        // TODO (BUG): only bind self if procedure/all etc.
        type_var(top->def.bind, check_against, t_env);
        type_check_expr(term, *check_against, t_env, ctx);
        post_unify(term, t_env, ctx);
        pop_type(t_env);
        break;
    }
    case TLDecl: {
        PtrArray declarations = mk_ptr_array(top->decl.properties.len, ctx.a);
        for (size_t i = 0; i < top->decl.properties.len; i++) {
            SymSynCell cell = top->decl.properties.data[i];
            if (symbol_eq(cell.key, string_to_symbol(mv_string("type")))) {
                PiType* ty = eval_type(cell.val, t_env, ctx);
                ModuleDecl* decl = mem_alloc(sizeof(ModuleDecl), ctx.a);
                *decl = (ModuleDecl) {
                    .sort = DeclType,
                    .type = ty,
                };
                push_ptr(decl, &declarations);
            } else {
                type_error_invalid_declaration(cell.key, cell.val, ctx);
            }
        }
        top->decl.decls = declarations;
        break;
    }
    case TLImport: {
        for (size_t i = 0; i < top->import.clauses.len; i++) {
            ImportClause clause = top->import.clauses.data[i];
            if (!import_clause_valid(env, clause)) {
                type_error_invalid_import(clause, top->import.range, ctx);
            }
        }
        break;
    }
    case TLExpr: {
        SynRef term = top->expr;
        type_infer_expr(term, t_env, ctx);
        post_unify(term, t_env, ctx);
        break;
    }
    }
}

// Forward declarations for implementation (internal declarations)
void type_infer_i(SynRef untyped, TypeEnv* env, TypeCheckContext ctx);
void type_check_i(SynRef untyped, PiType* type, Range tysrc, TypeEnv* env, TypeCheckContext ctx);
void* eval_expr(SynRef untyped, TypeEnv* env, TypeCheckContext ctx);
void* eval_typed_expr(SynRef typed, TypeEnv* env, TypeCheckContext ctx);
void squash_types(SynRef untyped, TypeEnv* env, TypeCheckContext ctx);
PiType* get_head(PiType* type, PiType_t expected_sort);
PiType* reduce_type(PiType* type, Allocator* a);

// -----------------------------------------------------------------------------
// Interface
// -----------------------------------------------------------------------------
void type_infer_expr(SynRef untyped, TypeEnv* env, TypeCheckContext ctx) {
    type_infer_i (untyped, env, ctx);
    squash_types(untyped, env, ctx);
}

void type_check_expr(SynRef untyped, PiType type, TypeEnv* env, TypeCheckContext ctx) {
    type_check_i (untyped, &type, (Range){}, env, ctx);
    squash_types(untyped, env, ctx);
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

void type_check_i(SynRef ref, PiType* type, Range tysrc, TypeEnv* env, TypeCheckContext ctx) {
    Syntax untyped = get_syntax(ref, ctx.tape);

    // If possible, traverse into the structure and type. This is not necessary,
    // but lets us produce better error messages.
    UnifyContext uctx = (UnifyContext) {
        .a = ctx.a,
        .pia = ctx.pia,
        .current_module = type_env_module(env),
        .logger = ctx.logger,
    };

    if (type->sort == TProc && untyped.type == SProcedure) {
        if (untyped.procedure.implicits.len != type->proc.implicits.len) {
            type_error_proc_incorrect_num_implicits(ref, type, ctx);
        }
        if (untyped.procedure.args.len != type->proc.args.len) {
            type_error_proc_incorrect_num_args(ref, type, ctx);
        }

        PiType* kind0 = call_alloc(sizeof(PiType), ctx.pia);
        *kind0 = (PiType){.sort = TKind, .kind.nargs = 0};
        for (size_t i = 0; i < type->proc.implicits.len; i++) {
            PiType* ann = type->proc.implicits.data[i];
            SymPtrCell cell = untyped.procedure.implicits.data[i];
            if (cell.val) {
                PiType* aty = eval_type(*(SynRef*)cell.val, env, ctx);
                UnifyResult out = unify(ann, aty, uctx);
                // TODO: add extra data to the 'check' reason that lets us infer
                //       that a specific argument is being checked!
                UnifyReason reason = {
                  .type = URCheck,
                  .check.range = get_range(*(SynRef*)cell.val, ctx.tape).term,
                  .check.expected = ann,
                  .check.actual = aty,
                };
                check_result_out(out, get_range(ref, ctx.tape).term, reason, ctx.a, ctx.point);
            } else {
                // TODO: replace proc annotated argument lists with SymSynAMaps,
                // which will provide better memory layout chatacteristics. Use
                // a 'none' syntax if not filled in. 
                SynRef* aty = mem_alloc(sizeof(SynRef), ctx.a);
                *aty = new_syntax(ctx.tape);
                set_syntax(*aty,
                           (Syntax){
                               .type = SCheckedType,
                               .type_val = ann,
                           },
                           ctx.tape);

                set_type(*aty, kind0, ctx.tape);
                untyped.procedure.implicits.data[i].val = aty;
            }
        }
        for (size_t i = 0; i < type->proc.args.len; i++) {
            PiType* ann = type->proc.args.data[i];
            SymPtrCell cell = untyped.procedure.args.data[i];
            if (cell.val) {
                PiType* aty = eval_type(*(SynRef*)cell.val, env, ctx);
                UnifyResult out = unify(ann, aty, uctx);
                UnifyReason reason = {
                  .type = URCheck,
                  .check.range= get_range(*(SynRef*)cell.val, ctx.tape).term,
                  .check.expected = ann,
                  .check.actual = aty,
                };
                check_result_out(out, get_range(ref, ctx.tape).term, reason, ctx.a, ctx.point);
            } else {
                SynRef* aty = call_alloc(sizeof(SynRef), ctx.pia);
                *aty = new_syntax(ctx.tape);
                set_syntax(*aty,
                           (Syntax) {
                               .type = SCheckedType,
                               .type_val = ann,
                           },
                           ctx.tape);
                set_type(*aty, kind0, ctx.tape);
                untyped.procedure.args.data[i].val = aty;
            }
        }

        type_infer_i(ref, env, ctx);
        PiType* inferred = get_type(ref, ctx.tape);
        UnifyResult out = unify(type, inferred, uctx);
        UnifyReason reason = {
          .type = URCheck,
          .check.range = get_range(untyped.procedure.body, ctx.tape).term,
          .check.expected = type->proc.ret,
          .check.actual = get_type(untyped.procedure.body, ctx.tape),
        };
        check_result_out(out, get_range(ref, ctx.tape).term, reason, ctx.a, ctx.point);
        
    } else {
        // If we can't easily traverse into the structure/type, ten 
        type_infer_i(ref, env, ctx);
        PiType* inferred = get_type(ref, ctx.tape);
        UnifyResult out = unify(type, inferred, uctx);
        UnifyReason reason = {
          .type = URCheck,
          .check.range = get_range(ref, ctx.tape).term,
          .check.expected = type,
          .check.actual = inferred,
        };
        check_result_out(out, get_range(ref, ctx.tape).term, reason, ctx.a, ctx.point);
    }
}

// "internal" type inference. Destructively mutates types.
void type_infer_i(SynRef ref, TypeEnv* env, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    PiErrorPoint* point = ctx.point;
    PicoError err;
    Syntax untyped = get_syntax(ref, ctx.tape);
    err.range = get_range(ref, ctx.tape).term;
    Range term_range = get_range(ref, ctx.tape).term;
    // Sometimes we go back and typecheck a term again, e.g. checking an
    // Application to an All generates a new term and typecheckes that.
    if (get_type(ref, ctx.tape)) return;
    if (ctx.logger) {
        String str = string_cat(mv_string("Inferring type for: "), syntax_type_to_string(untyped.type), a);
        start_section(str, ctx.logger);
    }

    switch (untyped.type) {
    case SLitUntypedIntegral:
        untyped.type = SLitTypedIntegral;
        set_type(ref, mk_uvar_integral(ctx.pia, term_range), ctx.tape);
        set_syntax(ref, untyped, ctx.tape);
        break;
    case SLitTypedIntegral:
        set_type(ref, mk_prim_type(ctx.pia, untyped.integral.type), ctx.tape);
        break;
    case SLitUntypedFloating:
        untyped.type = SLitTypedFloating;
        set_type(ref, mk_uvar_floating(ctx.pia, term_range), ctx.tape);
        set_syntax(ref, untyped, ctx.tape);
        break;
    case SLitTypedFloating:
        set_type(ref, mk_prim_type(ctx.pia, untyped.integral.type), ctx.tape);
        break;
    case SLitBool:
        set_type(ref, mk_prim_type(ctx.pia, Bool), ctx.tape);
        break;
    case SLitUnit:
        set_type(ref, mk_prim_type(ctx.pia, Unit), ctx.tape);
        break;
    case SLitString:
        set_type(ref, mk_string_type(ctx.pia), ctx.tape);
        break;
    case SVariable: {
        TypeEntry te = type_env_lookup(untyped.variable, env);
        if (ctx.logger) {
            String str = string_cat(mv_string("variable: "), view_symbol_string(untyped.variable), a);
            log_str(str, ctx.logger);
        }

        if (te.type != TENotFound) {
            if (te.is_module) {
                // An expression cannot evaluate to a module. Paths involving 
                // modules, e.g. num.u64 are resolved during abstraction.
                type_error_unexpected_module(ref, te.module, ctx);
            }

            set_type(ref, te.ptype, ctx.tape);;
            if (te.value) {
                set_syntax(ref, (Syntax){.type = SCheckedType, .type_val = te.value}, ctx.tape);
            }
        } else {
            type_error_unknown_var(ref, ctx);
        }
        break;
    }
    case SAbsVariable: {
        set_type(ref, untyped.abvar.type, ctx.tape);;
        break;
    }
    case SProcedure: {
        // TODO (BUG): ensure that we appropriately check for captures, 
        // as 'proc' does NOT support closures!

        // give each arg a unification variable type. 
        PiType* proc_ty = call_alloc(sizeof(PiType), ctx.pia);
        *proc_ty = (PiType) {
            .sort = TProc,
            .proc.implicits = mk_addr_list(untyped.procedure.implicits.len, ctx.pia),
            .proc.args = mk_addr_list(untyped.procedure.args.len, ctx.pia),
        };
        set_type(ref, proc_ty, ctx.tape);;

        for (size_t i = 0; i < untyped.procedure.implicits.len; i++) {
            SymPtrCell arg = untyped.procedure.implicits.data[i];
            PiType* aty;
            if (arg.val) {
                aty = eval_type(*(SynRef*)arg.val, env, ctx);
                if (aty->sort != TTraitInstance) {
                    type_error_expecting_instance_arg(i, ref, ctx);
                }
            } else  {
                aty = mk_uvar(ctx.pia);
            }
            type_var(arg.key, aty, env);
            push_addr(aty, &proc_ty->proc.implicits);
        }

        for (size_t i = 0; i < untyped.procedure.args.len; i++) {
            SymPtrCell arg = untyped.procedure.args.data[i];
            PiType* aty;
            if (arg.val) {
                aty = eval_type(*(SynRef*)arg.val, env, ctx);
            } else  {
                aty = mk_uvar(ctx.pia);
            }
            type_var(arg.key, aty, env);
            push_addr(aty, &proc_ty->proc.args);
        }

        type_infer_i(untyped.procedure.body, env, ctx); 
        pop_types(env, untyped.procedure.args.len + untyped.procedure.implicits.len);
        proc_ty->proc.ret = get_type(untyped.procedure.body, ctx.tape);
        break;
    }
    case SAll: {
        // Give each arg type Type.
        PiType* all_ty = call_alloc(sizeof(PiType), ctx.pia);
        set_type(ref, all_ty, ctx.tape);;
        all_ty->sort = TAll;
        all_ty->binder.vars = mk_sym_list(untyped.all.args.len, ctx.pia);

        for (size_t i = 0; i < untyped.all.args.len; i++) {
            Symbol arg = untyped.all.args.data[i];

            PiType* arg_ty = mem_alloc(sizeof(PiType), a);
            *arg_ty = (PiType) {.sort = TVar, .var = arg,};

            type_qvar(arg, arg_ty, env);
            push_sym(arg, &all_ty->binder.vars);
        }

        type_infer_i(untyped.all.body, env, ctx); 
        pop_types(env, untyped.all.args.len);
        all_ty->binder.body = get_type(untyped.all.body, ctx.tape);
        break;
    }
    case SMacro: {
        // Macro inner type: 
        // proc [Array Syntax] Syntax
        // where syntax = ...
        PiType* syntax_array = mk_app_type(ctx.pia, get_list_type(), get_syntax_type());
        PiType* transformer_proc = mk_proc_type(ctx.pia, 1, syntax_array, get_macro_result_type());

        Range tysrc = (Range){};
        type_check_i(untyped.transformer, transformer_proc, tysrc, env, ctx);
        PiType* t = call_alloc(sizeof(PiType), ctx.pia);
        *t = (PiType) {
            .sort = TPrim,
            .prim = TMacro,
        };
        set_type(ref, t, ctx.tape);;
        break;
    }
    case SApplication: {
        type_infer_i(untyped.application.function, env, ctx);
        PiType* fn_type = get_type(untyped.application.function, ctx.tape);
        if (fn_type->sort == TUVar) {
            // fill in structure 
            PiType* ret = mk_uvar(ctx.pia);
            AddrPiList args = mk_addr_list(16, ctx.pia);
            for (size_t i = 0; i < untyped.application.args.len; i++) {
                type_infer_i(untyped.application.args.data[i], env, ctx);
                push_addr(mk_uvar(ctx.pia), &args);
            };
            
            PiType* new_fn = mem_alloc(sizeof(PiType), a);
            *new_fn = (PiType) {
                .sort = TProc,
                .proc.args = args,
                .proc.ret = ret,
            };

            UnifyContext uctx = (UnifyContext) {
                .a = ctx.a,
                .pia = ctx.pia,
                .current_module = type_env_module(env),
                .logger = ctx.logger,
            };
            UnifyResult out = unify(new_fn, fn_type, uctx);
            UnifyReason reason = {
                .type = URCheck,
                .check.range = get_range(ref, ctx.tape).term,
                .check.expected = new_fn,
                .check.actual = fn_type,
            };
            check_result_out(out, get_range(ref, ctx.tape).term, reason, ctx.a, ctx.point);

        } else if (fn_type->sort == TProc) {
            if (fn_type->proc.args.len != untyped.application.args.len) {
                type_error_incorrect_num_args(fn_type, ref, InvValues, ctx);
            }

            Range tysrc = get_range(untyped.application.function, ctx.tape).term;
            for (size_t i = 0; i < fn_type->proc.args.len; i++) {
                SynRef arg = untyped.application.args.data[i]; 
                type_check_i(arg,
                             (PiType*)fn_type->proc.args.data[i],
                             tysrc, env, ctx);
            }

            set_type(ref, fn_type->proc.ret, ctx.tape);;

        } else if (fn_type->sort == TAll) {
            SynArray types = mk_syn_array(fn_type->binder.vars.len, a);
            // TODO (FUTURE BUG): When HKTs are allowed, this will need to be
            // replaced with the correct kind!
            PiType* kind = mem_alloc(sizeof(PiType), a);
            *kind = (PiType){.sort = TKind, .kind.nargs = 0};
            for (size_t i = 0; i < fn_type->binder.vars.len; i++) {
                SynRef syn = new_syntax(ctx.tape);
                set_syntax(syn, (Syntax) {.type = SCheckedType, .type_val = mk_uvar(ctx.pia),}, ctx.tape);
                set_type(syn, kind, ctx.tape);
                push_syn(syn, &types);
            }

            SynAllApp new_app = (SynAllApp) {
                .function = untyped.application.function,
                .types = types,
                .implicits = untyped.application.implicits,
                .args = untyped.application.args
            };

            if (ctx.logger) {
                log_str(mv_string("function is 'all', substituting app for all-app"), ctx.logger);
            }

            set_syntax(ref, (Syntax) {.type = SAllApplication, .all_application = new_app}, ctx.tape);
            type_infer_i(ref, env, ctx);
        } else if (fn_type->sort == TKind || fn_type->sort == TConstraint) {
            if (fn_type->kind.nargs != untyped.application.args.len) {
                type_error_incorrect_num_args(fn_type, ref, InvValues, ctx);
            }
            if (fn_type->kind.nargs == 0) {
                type_error_app_not_family(fn_type, ref, ctx);
            }

            PiType* kind = mem_alloc(sizeof(PiType), a);
            *kind = (PiType) {.sort = TKind, .kind.nargs = 0};
            Range tysrc = get_range(untyped.application.function, ctx.tape).term;
            for (size_t i = 0; i < fn_type->kind.nargs; i++) {
              type_check_i(untyped.application.args.data[i], kind,
                           tysrc, env, ctx);
            }
            *kind = (PiType) {.sort = fn_type->sort, .kind.nargs = 0};
            set_type(ref, kind, ctx.tape);;
        } else {
            type_error_invalid_application_target(fn_type, ref, ctx);
        }
        
        break;
    }
    case SAllApplication: {
        type_infer_i(untyped.all_application.function, env, ctx);

        PiType* all_type = unwrap_type(get_type(untyped.all_application.function, ctx.tape), type_env_module(env), ctx.pia, a);
        if (all_type->sort == TUVar) {
            type_error_invalid_application_target(all_type, ref, ctx);
        }
        else if (all_type->sort != TAll) {
            type_error_invalid_application_target(all_type, ref, ctx);
        }
        
        if (all_type->binder.vars.len != untyped.all_application.types.len) {
            type_error_incorrect_num_args(all_type, ref, InvTypes, ctx);
        }

        if (all_type->binder.body->sort != TProc) {
            // Check that all type args are actually types!
            SymPtrAssoc type_binds = mk_sym_ptr_assoc(all_type->binder.vars.len, a);
            for (size_t i = 0; i < all_type->binder.vars.len; i++) {
                SynRef type = untyped.all_application.types.data[i];
                eval_type(type, env, ctx);
                sym_ptr_bind(all_type->binder.vars.data[i], get_syntax(type, ctx.tape).type_val, &type_binds);
            }

            if (all_type->binder.vars.len != untyped.all_application.types.len) {
                type_error_incorrect_num_args(all_type, ref, InvTypes, ctx);
            }

            // Now, check that implicits are empty
            if (untyped.all_application.implicits.len != 0) {
                type_error_incorrect_num_args_all_noproc(all_type, ref, true, ctx);
            }

            if (untyped.all_application.args.len != 0) {
                type_error_incorrect_num_args_all_noproc(all_type, ref, false, ctx);
            }
            
            PiType* ret_type = pi_type_subst(all_type->binder.body, type_binds, ctx.pia, a);
            set_type(ref, ret_type, ctx.tape);;
        } else {
            // Check that all type args are actually types!
            SymPtrAssoc type_binds = mk_sym_ptr_assoc(all_type->binder.vars.len, a);
            for (size_t i = 0; i < all_type->binder.vars.len; i++) {
                SynRef type = untyped.all_application.types.data[i];
                eval_type(type, env, ctx);
                sym_ptr_bind(all_type->binder.vars.data[i], get_syntax(type, ctx.tape).type_val, &type_binds);
            }
        
            // Bind the vars in the all type to specific types!
            PiType* proc_type = pi_type_subst(all_type->binder.body, type_binds, ctx.pia, a);
            if (proc_type->proc.args.len != untyped.all_application.args.len) {
                type_error_incorrect_num_args(all_type, ref, InvValues, ctx);
            }

            Range tysrc = get_range(untyped.all_application.function, ctx.tape).term;
            for (size_t i = 0; i < proc_type->proc.args.len; i++) {
                type_check_i(untyped.all_application.args.data[i],
                             (PiType*)proc_type->proc.args.data[i],
                             tysrc, env, ctx);
            }

            // Note: substitution has already happened above, so no need to do
            //   it again! 
            set_type(ref, proc_type->proc.ret, ctx.tape);;
        }
        break;
    }
    case SSeal: {
        set_type(ref, eval_type(untyped.seal.type, env, ctx), ctx.tape);;
        PiType* sealed_type = unwrap_type(get_type(ref, ctx.tape), type_env_module(env), ctx.pia, a);

        if (sealed_type->sort != TSealed) {
            type_error_invalid_seal_type(sealed_type, ref, ctx);
        }

        if (sealed_type->sealed.vars.len != untyped.seal.types.len) {
            type_error_incorrect_num_seal_args(sealed_type, ref, ctx);
        }

        // Substitute a uvar in in for the sealed type:
        SymPtrAssoc type_binds = mk_sym_ptr_assoc(sealed_type->sealed.vars.len, a);
        for (size_t i = 0; i < sealed_type->sealed.vars.len; i++) {
            PiType* ty = eval_type(untyped.seal.types.data[i], env, ctx);
            sym_ptr_bind(sealed_type->sealed.vars.data[i], ty, &type_binds);
        }

        if (sealed_type->sealed.implicits.len > 0) {
            not_implemented(mv_string("Sealing with implicits."));
        }
        PiType* body_type = pi_type_subst(sealed_type->sealed.body, type_binds, ctx.pia, a);

        type_check_i(untyped.seal.body, body_type, get_range(untyped.seal.type, ctx.tape).term, env, ctx);
        break;
    }
    case SUnseal: {
        type_infer_i(untyped.unseal.sealed, env, ctx);
        PiType* sealed_type = unwrap_type(get_type(untyped.unseal.sealed, ctx.tape), type_env_module(env), ctx.pia, a);
        if (sealed_type->sort != TSealed) {
            type_error_invalid_unseal_type(sealed_type, ref, ctx);
        }

        if (sealed_type->sealed.vars.len != untyped.unseal.types.len) {
            type_error_incorrect_num_unseal_binds(sealed_type, ref, ctx);
        }

        SymPtrAssoc type_binds = mk_sym_ptr_assoc(sealed_type->sealed.vars.len, a);
        for (size_t i = 0; i < sealed_type->sealed.vars.len; i++) {
            PiType* ty = call_alloc(sizeof(PiType), ctx.pia);
            *ty = (PiType) {.sort = TVar, .var = untyped.unseal.types.data[i]};
            sym_ptr_bind(sealed_type->sealed.vars.data[i], ty, &type_binds);

            type_qvar(untyped.unseal.types.data[i], ty, env);
        }
        PiType* var_ty = pi_type_subst(sealed_type->sealed.body, type_binds, ctx.pia, a);
        type_var(untyped.unseal.binder, var_ty, env);
        type_infer_i(untyped.unseal.body, env, ctx);
        set_type(ref, get_type(untyped.unseal.body, ctx.tape), ctx.tape);;

        pop_types(env, 1 + sealed_type->sealed.vars.len);
        break;
    }
    case SConstructor: {
        // Typecheck variant
        if (untyped.variant.has_enum_type == Some) {
            set_type(ref, eval_type(untyped.variant.enum_type, env, ctx), ctx.tape);;
            PiType* enum_type = unwrap_type(get_type(ref, ctx.tape), type_env_module(env), ctx.pia, a);

            if (enum_type->sort != TEnum) {
                type_error_invalid_variant_type(enum_type, ref, ctx);
            }

            bool found_variant = false;
            for (size_t i = 0; i < enum_type->enumeration.variants.len; i++) {
                if (symbol_eq(enum_type->enumeration.variants.data[i].key, untyped.variant.tagname)) {
                    untyped.variant.tag = i;
                    found_variant = true;

                    // Generate variant has no args
                    PtrArray* args = enum_type->enumeration.variants.data[i].val;
                    if (args->len != 0) {
                        type_error_incorrect_num_variant_args(enum_type, ref, i, ctx);
                    }
                    break;
                }
            }

            if (!found_variant) {
                type_error_missing_variant_tag(enum_type, ref, ctx);
            }
        } else {
            set_type(ref, mk_uvar(ctx.pia), ctx.tape);;
            AddrPiList types = mk_addr_list(0, ctx.pia);

            UnifyContext uctx = {
                .a = a,
                .pia = ctx.pia,
                .current_module = type_env_module(env),
                .logger = ctx.logger,
            };
            Range range = get_range(ref, ctx.tape).term;
            UnifyResult out = add_variant_constraint(get_type(ref, ctx.tape)->uvar, range, untyped.variant.tagname, types, uctx);
            check_result_out(out, range, (UnifyReason){.type = URNone}, a, point);
        }
        break;
    }
    case SVariant: {
        // Typecheck variant
        if (untyped.variant.has_enum_type == Some) {
            set_type(ref, eval_type(untyped.variant.enum_type, env, ctx), ctx.tape);;
            PiType* enum_type = unwrap_type(get_type(ref, ctx.tape), type_env_module(env), ctx.pia, a);

            if (enum_type->sort != TEnum) {
                type_error_invalid_variant_type(enum_type, ref, ctx);
            }

            bool found_variant = false;
            for (size_t i = 0; i < enum_type->enumeration.variants.len; i++) {
                if (symbol_eq(enum_type->enumeration.variants.data[i].key, untyped.variant.tagname)) {
                    untyped.variant.tag = i;
                    found_variant = true;

                    // Generate variant has no args
                    PtrArray* args = enum_type->enumeration.variants.data[i].val;
                    if (args->len != untyped.variant.args.len) {
                        type_error_incorrect_num_variant_args(enum_type, ref, i, ctx);
                    }

                    for (size_t i = 0; i < args->len; i++) {
                        Range tysrc = get_range(untyped.variant.enum_type, ctx.tape).term;
                        type_check_i(untyped.variant.args.data[i], args->data[i], tysrc, env, ctx);
                    }
                    break;
                }
            }

            if (!found_variant) {
                type_error_missing_variant_tag(enum_type, ref, ctx);
            }
        } else {
            set_type(ref, mk_uvar(ctx.pia), ctx.tape);;
            AddrPiList types = mk_addr_list(untyped.variant.args.len, ctx.pia);

            for (size_t i = 0; i < untyped.variant.args.len; i++) {
                type_infer_i(untyped.variant.args.data[i], env, ctx);
                push_addr(get_type(untyped.variant.args.data[i], ctx.tape), &types);
            }
            UnifyContext uctx = {
                .a = a,
                .pia = ctx.pia,
                .current_module = type_env_module(env),
                .logger = ctx.logger,
            };
            Range range = get_range(ref, ctx.tape).term;
            UnifyResult out = add_variant_constraint(get_type(ref, ctx.tape)->uvar, range, untyped.variant.tagname, types, uctx);
            check_result_out(out, range, (UnifyReason){.type = URNone}, a, point);
        }
        break;
    }
    case SMatch: {
        // Typecheck the input 
        type_infer_i(untyped.match.val, env, ctx);

        PiType* enum_type = unwrap_type(get_type(untyped.match.val, ctx.tape), type_env_module(env), ctx.pia, a); 
        if (enum_type->sort == TEnum) {
            // Typecheck each variant, ensure they are the same
            PiType* out_ty = mk_uvar(ctx.pia);
            set_type(ref, out_ty, ctx.tape);;
            U8Array used_indices = mk_u8_array(enum_type->enumeration.variants.len, a);
            for (size_t i = 0; i < enum_type->enumeration.variants.len; i++)
                push_u8(0, &used_indices);

            for (size_t i = 0; i < untyped.match.clauses.len; i++) {
                SynClause* clause = untyped.match.clauses.data[i];
                if (clause->is_wildcard) {
                    // Use all indices
                    for (size_t i = 0; i < used_indices.len; i++) {
                        used_indices.data[i] = 1;
                    }
                    type_check_i(clause->body, out_ty, (Range){}, env, ctx);
                } else {
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

                    type_check_i(clause->body, out_ty, (Range){}, env, ctx);
                    pop_types(env, types_to_bind->len);
                }
            }

            // Finally, check that all indices are accounted for;
            bool all_indices = true;
            for (size_t i = 0; i < used_indices.len; i++) all_indices &= used_indices.data[i];

            if (!all_indices) {
                type_error_match_missing_variants(enum_type, ref, used_indices, ctx);
            }
        } else if (enum_type->sort == TUVar) {
            // TODO (FEAT): Adjust this so there is room for re-ordering! 
            //   possibly placing constraints on the enum, but constraining it
            //   to only have the provided fields?? a special unification??
            PiType* new_enum_type = call_alloc(sizeof(PiType), ctx.pia);
            *new_enum_type = (PiType) {
                .sort = TEnum,
                .enumeration.tag_size = 64,
                .enumeration.variants = mk_sym_addr_piamap(untyped.match.clauses.len, ctx.pia),
            };

            PiType* out_ty = mk_uvar(ctx.pia);
            set_type(ref, out_ty, ctx.tape);;
            bool was_wildcard = false;
            for (size_t i = 0; i < untyped.match.clauses.len; i++) {
                SynClause* clause = untyped.match.clauses.data[i];

                if (clause->is_wildcard) {
                    type_check_i(clause->body, out_ty, (Range){}, env, ctx);
                    was_wildcard = true;
                } else {
                    AddrPiList types = mk_addr_list(clause->vars.len, ctx.pia);
                    for (size_t j = 0; j < clause->vars.len; j++) {
                        Symbol arg = clause->vars.data[j];
                        PiType* aty = mk_uvar(ctx.pia);
                        push_addr(aty, &types);
                        type_var(arg, aty, env);
                    }

                    type_check_i(clause->body, out_ty, (Range){}, env, ctx);
                    pop_types(env, types.len);

                    AddrPiList* to_insert = mem_alloc(sizeof(AddrPiList), a);
                    *to_insert = types;
                    sym_addr_insert(clause->tagname, to_insert, &new_enum_type->enumeration.variants);
                }
            }
            if (was_wildcard) {
                // was_wildcard: Each branch in the match clause imposes a
                //   constraint, not
                for (size_t i = 0; i < new_enum_type->enumeration.variants.len; i++) {
                    UnifyContext uctx = {
                        .a = a,
                        .pia = ctx.pia,
                        .current_module = type_env_module(env),
                        .logger = ctx.logger,
                    };
                    SymAddrPiCell cell = new_enum_type->enumeration.variants.data[i];

                    Range range = get_range(ref, ctx.tape).term;
                    UnifyResult res = add_variant_constraint(enum_type->uvar, range, cell.key, (*(AddrPiList*)cell.val), uctx);
                    check_result_out(res, range, (UnifyReason){.type = URNone}, a, point);
                }
            } else {
                // No wildcard: the match enumerates all possibilities
                type_check_i(untyped.match.val, new_enum_type, (Range){}, env, ctx);
            }
        } else {
            type_error_match_invalid_type(get_type(untyped.match.val, ctx.tape), ref, ctx);
        }
        break;
    }
    case SArray: {
        PiType* type = mk_uvar(ctx.pia);
        for (size_t i = 0; i < untyped.array.elements.len; i++) {
            type_check_i(untyped.array.elements.data[i], type, (Range){}, env, ctx);
        }
        PiType* arr_type = mem_alloc(sizeof(PiType), a);
        DimPiList dims = mk_dim_list(untyped.array.dimensions.len, ctx.pia);
        for (size_t i = 0; i < untyped.array.dimensions.len; i++) {
            Dimension dim = {
                .is_uvar = false,
                .val = untyped.array.dimensions.data[i],
            };
            push_dim(dim, &dims);
        }
        *arr_type = (PiType) {
            .sort = TArray,
            .array.dimensions = dims,
            .array.element = type,
        };
        set_type(ref, arr_type, ctx.tape);
        break;
    }
    case SArrayElt: {
        PiType* array_type = call_alloc(sizeof(PiType), ctx.pia); 
        PiType* elt_type = mk_uvar(ctx.pia);
        DimPiList dimensions = mk_dim_list(untyped.array.dimensions.len, ctx.pia);
        for (size_t i = 0; i < untyped.array.dimensions.len; i++) {
            Dimension dim = mk_dim_uvar(ctx.pia);
            push_dim(dim, &dimensions);
        }
        *array_type = (PiType) {
            .sort = TArray, 
            .array.dimensions = dimensions,
            .array.element = elt_type,
        };

        Range arr_src = get_range(ref, ctx.tape).term;
        type_check_i(untyped.array_elt.array, array_type, arr_src, env, ctx);

        for (size_t i = 0; i < untyped.array_elt.index.len; i++) {
            PiType* index_type = call_alloc(sizeof(PiType), ctx.pia); 
            *index_type = (PiType) {
                .sort = TPrim, 
                .prim = UInt_64,
            };
            Range range = get_range(untyped.array_elt.index.data[i], ctx.tape).term;
            type_check_i(untyped.array_elt.index.data[i], index_type, range, env, ctx);
        }

        set_type(ref, elt_type, ctx.tape);
        break;
    }
    case SStructure: {
        if (untyped.structure.has_base == Some) {
            bool all_fields_required;
            type_infer_i(untyped.structure.base, env, ctx);
            if (get_type(untyped.structure.base, ctx.tape)->sort == TKind) {
                set_type(ref, eval_type(untyped.structure.base, env, ctx), ctx.tape);;
                all_fields_required = true;
            } else {
                set_type(ref, get_type(untyped.structure.base, ctx.tape), ctx.tape);;
                all_fields_required = false;
            }

            PiType* struct_type = unwrap_type(get_type(ref, ctx.tape), type_env_module(env), ctx.pia, ctx.a);

            if (struct_type->sort != TStruct) {
                if (struct_type->sort == TDistinct) {
                    PtrArray nodes = mk_ptr_array(2, a);
                    push_ptr(mv_cstr_doc(
                        "Attempting to create a structure "
                        "based off of an opaque type. "
                        "Note that instances of opaque types can only be "
                        "created in the module that the opaque type is defined."
                        , a), &nodes);
                    push_ptr(pretty_type(struct_type, default_ptp, a), &nodes);
                    err.message = mv_sep_doc(nodes, a);
                } else {
                    PtrArray nodes = mk_ptr_array(2, a);
                    push_ptr(mv_cstr_doc("Structure provided/based off of non-structure type:", a), &nodes);
                    push_ptr(pretty_type(struct_type, default_ptp, a), &nodes);
                    err.message = mv_sep_doc(nodes, a);
                }
                throw_pi_error(point, err);
            }

            // Check if any fields are missing when a new struct is being created
            // error message:
            if (get_type(untyped.structure.base, ctx.tape)->sort == TKind) {
                SymbolArray missing_fields = mk_symbol_array(4, a);
                for (size_t i = 0; i < struct_type->structure.fields.len; i++) {
                    Symbol field = struct_type->structure.fields.data[i].key;
                    bool has_field = false;
                    for (size_t j = 0; j < untyped.structure.fields.len; j++) {
                        if (symbol_eq(field, untyped.structure.fields.data[j].key))
                            has_field = true;
                    }
                    if (!has_field) {
                        push_symbol(field, &missing_fields);
                    }
                }
                
                if (missing_fields.len > 0) {
                    PtrArray docs = mk_ptr_array(2 + missing_fields.len, a);
                    if (missing_fields.len == 1) {
                        push_ptr(mv_cstr_doc("Structure value definition is missing the field:", a), &docs);
                        push_ptr(mk_str_doc(view_symbol_string(missing_fields.data[0]), a), &docs);
                    } else {
                        push_ptr(mv_cstr_doc("Structure value definition is missing the fields:", a), &docs);
                        for (size_t i = 0; i < missing_fields.len; i++) {
                            if (i < missing_fields.len - 2) {
                                push_ptr(mv_str_doc(string_cat(view_symbol_string(missing_fields.data[i]), mv_string(","), a), a), &docs);
                            } else if (i == missing_fields.len - 2) {
                                push_ptr(mk_str_doc(view_symbol_string(missing_fields.data[i]), a), &docs);
                                push_ptr(mv_cstr_doc("and", a), &docs);
                            } else {
                                push_ptr(mk_str_doc(view_symbol_string(missing_fields.data[i]), a), &docs);
                            }
                        }
                    }
                    err.message = mv_hsep_doc(docs, a);
                    throw_pi_error(point, err);
                }
            }

            for (size_t i = 0; i < struct_type->structure.fields.len; i++) {
                SynRef* field_syn = sym_syn_lookup(struct_type->structure.fields.data[i].key, untyped.structure.fields);
                if (field_syn) {
                    PiType* field_ty = struct_type->structure.fields.data[i].val;
                    Range tysrc = get_range(untyped.structure.base, ctx.tape).term;
                    type_check_i(*field_syn, field_ty, tysrc, env, ctx);
                } else if (all_fields_required) {
                    panic(mv_string("An earlier typechecking step failed to ensure all fields were present in a structure"));
                }
            }
        } else {
            PiType struct_type = (PiType) {
                .sort = TStruct,
                .structure.fields = mk_sym_addr_piamap(untyped.structure.fields.len, ctx.pia),
            };
            for (size_t i = 0; i < untyped.structure.fields.len; i++) {
                // TODO (FEATURE): allow the inference algorithm to later
                //                 reorder the fields!
                SymSynCell cell = untyped.structure.fields.data[i];
                type_infer_i(cell.val, env, ctx);
                sym_addr_insert(cell.key, get_type((SynRef)cell.val, ctx.tape), &struct_type.structure.fields);
            }

            PiType* type = call_alloc(sizeof(PiType), ctx.pia);
            *type = struct_type;
            set_type(ref, type, ctx.tape);;
        }
        break;
    }
    case SProjector: {
        type_infer_i(untyped.projector.val, env, ctx);
        PiType source_type = *unwrap_type(get_type(untyped.projector.val, ctx.tape), type_env_module(env), ctx.pia, a);

        if (source_type.sort == TStruct) {
            // search for field
            PiType* ret_ty = NULL;
            for (size_t i = 0; i < source_type.structure.fields.len; i++) {
                if (symbol_eq(source_type.structure.fields.data[i].key, untyped.projector.field)) {
                    ret_ty = source_type.structure.fields.data[i].val;
                }
            }
            if (ret_ty == NULL) {
                err.message = mv_cstr_doc("Field not found in struct!", a);
                throw_pi_error(point, err);
            }
            set_type(ref, ret_ty, ctx.tape);;

        } else if (source_type.sort == TTraitInstance) {
            // search for field
            PiType* ret_ty = NULL;
            for (size_t i = 0; i < source_type.instance.fields.len; i++) {
                if (symbol_eq(source_type.instance.fields.data[i].key, untyped.projector.field)) {
                    ret_ty = source_type.instance.fields.data[i].val;
                }
            }
            if (ret_ty == NULL) {
                err.message = mv_cstr_doc("Field not found in instance!", a);
                throw_pi_error(point, err);
            }
            set_type(ref, ret_ty, ctx.tape);;

        } else if (source_type.sort == TUVar) {
            set_type(ref, mk_uvar(ctx.pia), ctx.tape);;
            UnifyContext uctx = {
                .a = a,
                .pia = ctx.pia,
                .current_module = type_env_module(env),
                .logger = ctx.logger,
            };
            Range range = get_range(ref, ctx.tape).term;
            UnifyResult out = add_field_constraint(source_type.uvar, range, untyped.projector.field, get_type(ref, ctx.tape), uctx);
            check_result_out(out, range, (UnifyReason){.type = URNone}, a, point);
        } else {
            type_error_proj_invalid_type(&source_type, ref, ctx);
        }
        break;
    }
    case SInstance: {
        // Output type
        PiType* ty = call_alloc(sizeof(PiType), ctx.pia);
        set_type(ref, ty, ctx.tape);;

        PiType* constraint_ty = call_alloc(sizeof(PiType), ctx.pia);
        *constraint_ty = (PiType) {.sort = TConstraint, .constraint.nargs = 0};

        PiType* ty_ty = call_alloc(sizeof(PiType), ctx.pia);
        *ty_ty = (PiType) {.sort = TKind, .constraint.nargs = 0};

        for (size_t i = 0; i < untyped.instance.params.len; i++) {
            Symbol arg = untyped.instance.params.data[i];
            type_var(arg, ty_ty, env);
        }

        for (size_t i = 0; i < untyped.instance.implicits.len; i++) {
            SymPtrCell arg = untyped.instance.implicits.data[i];
            PiType* aty;
            if (arg.val) {
                aty = eval_type(*(SynRef*)arg.val, env, ctx);
            } else  {
                aty = mk_uvar(ctx.pia);
            }
            type_var(arg.key, aty, env);
        }

        *ty = *eval_type(untyped.instance.constraint, env, ctx);

        if (ty->sort != TTraitInstance) {
            type_error_instance_invalid_type(ty, get_range(untyped.instance.constraint, ctx.tape).term, ctx);
        }

        if (untyped.instance.fields.len != ty->instance.fields.len) {
            type_error_instance_wrong_nfields(get_range(ref, ctx.tape).term, ty->instance.fields.len, untyped.instance.fields.len, ctx);
        }

        for (size_t i = 0; i < ty->instance.fields.len; i++) {
            SynRef* field_syn = (SynRef*)sym_syn_lookup(ty->instance.fields.data[i].key, untyped.instance.fields);
            if (field_syn) {
                PiType* field_ty = ty->instance.fields.data[i].val;
                Range tysrc = get_range(untyped.instance.constraint, ctx.tape).term;
                type_check_i(*field_syn, field_ty, tysrc, env, ctx);
            } else {
                type_error_instance_missing_field(get_range(ref, ctx.tape).term, ty->instance.fields.data[i].key, ctx);
            }
        }

        pop_types(env, untyped.instance.params.len + untyped.instance.implicits.len);
        break;
    }
    case SDynamic: {
        type_infer_i(untyped.dynamic, env, ctx);
        PiType* inner_type = get_type(untyped.dynamic, ctx.tape); 
        PiType* t = call_alloc(sizeof(PiType), ctx.pia);
        *t = (PiType) {
            .sort = TDynamic,
            .dynamic = inner_type,
        };
        set_type(ref, t, ctx.tape);;
        break;
    }
    case SDynamicUse: {
        type_infer_i(untyped.dynamic, env, ctx);
        PiType* dyn_type = get_type(untyped.dynamic, ctx.tape); 
        if (dyn_type->sort != TDynamic) {
            err.message = mv_cstr_doc("use on non-dynamic type!", a);
            throw_pi_error(point, err);
        }
        set_type(ref, dyn_type->dynamic, ctx.tape);;
        break;
    }
    case SDynamicSet: {
        type_infer_i(untyped.dynamic_set.dynamic, env, ctx);
        PiType* dyn_type = get_type(untyped.dynamic_set.dynamic, ctx.tape); 
        if (dyn_type->sort != TDynamic) {
            err.message = mv_cstr_doc("set on non-dynamic type!", a);
            throw_pi_error(point, err);
        }
        Range tysrc = get_range(untyped.dynamic_set.dynamic, ctx.tape).term;
        type_check_i(untyped.dynamic_set.new_val, dyn_type->dynamic, tysrc, env, ctx);
        set_type(ref, mk_prim_type(ctx.pia, Unit), ctx.tape);;
        break;
    }
    case SDynamicLet: {
        for (size_t i = 0; i < untyped.dyn_let_expr.bindings.len; i++) {
            DynBinding* dbind = untyped.dyn_let_expr.bindings.data[i];

            PiType* dyn_ty = call_alloc(sizeof(PiType), ctx.pia);
            PiType* val_ty = mk_uvar(ctx.pia);
            *dyn_ty = (PiType) {
                .sort = TDynamic,
                .dynamic = val_ty,
            };

            Range dyn_tysrc = get_range(dbind->expr, ctx.tape).term;
            type_check_i(dbind->var, dyn_ty, dyn_tysrc, env, ctx);
            Range val_tysrc = get_range(dbind->var, ctx.tape).term;
            type_check_i(dbind->expr, val_ty, val_tysrc, env, ctx);
        }
        type_infer_i(untyped.dyn_let_expr.body, env, ctx);
        set_type(ref, get_type(untyped.dyn_let_expr.body, ctx.tape), ctx.tape);;
        break;
    }
    case SLet: {
        for (size_t i = 0; i < untyped.let_expr.bindings.len; i++) {
            Symbol arg = untyped.let_expr.bindings.data[i].key;
            SynRef val = untyped.let_expr.bindings.data[i].val;

            type_infer_i(val, env, ctx);

            // TODO (FEAT): add support for recursive bindings (e.g. procedures)
            type_var(arg, get_type(val, ctx.tape), env);
        }
        type_infer_i(untyped.let_expr.body, env, ctx);
        set_type(ref, get_type(untyped.let_expr.body, ctx.tape), ctx.tape);;
        pop_types(env, untyped.let_expr.bindings.len);
        break;
    }
    case SIf: {
        PiType* t = call_alloc(sizeof(PiType), ctx.pia);
        *t = (PiType) {.sort = TPrim,.prim = Bool};
        type_check_i(untyped.if_expr.condition, t, (Range){}, env, ctx);

        PiType* out_type = mk_uvar(ctx.pia);
        type_check_i(untyped.if_expr.true_branch, out_type, (Range){}, env, ctx);
        Range tysrc = get_range(untyped.if_expr.true_branch, ctx.tape).term;
        type_check_i(untyped.if_expr.false_branch, out_type, tysrc, env, ctx);
        set_type(ref, out_type, ctx.tape);;
        break;
    }
    case SCond: {
        PiType* boolty = call_alloc(sizeof(PiType), ctx.pia);
        *boolty = (PiType) {.sort = TPrim,.prim = Bool};

        PiType* out_type = mk_uvar(ctx.pia);
        for (size_t i = 0; i < untyped.cond.clauses.len; i++) {
            CondClause* clause = untyped.cond.clauses.data[i];
            type_check_i(clause->condition, boolty, (Range){}, env, ctx);
            type_check_i(clause->branch, out_type, (Range){}, env, ctx);
        }
        type_check_i(untyped.cond.otherwise, out_type, (Range){}, env, ctx);
        set_type(ref, out_type, ctx.tape);;
        break;
    }
    case SGoTo: {
        PtrArray* args = lookup_label(untyped.go_to.label, env);
        if (args) {
            if (args->len != untyped.go_to.args.len) {
                err.message = mv_cstr_doc("Error in go-to: wrong number of args!", a);
                throw_pi_error(point, err);
            }

            for (size_t i = 0; i < args->len; i++) {
                type_check_i(untyped.go_to.args.data[i], args->data[i], (Range){}, env, ctx);
            }

            // TODO (FEATURE): another type of defaulted uvar - unit!
            PiType* t = mk_uvar(ctx.pia);
            set_type(ref, t, ctx.tape);;
        } else {
            PtrArray nodes = mk_ptr_array(2, a);
            push_ptr(mv_cstr_doc("Error in go-to: label not found:", a), &nodes);
            push_ptr(mk_str_doc(symbol_to_string(untyped.go_to.label, a), a), &nodes);
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

        set_type(ref, tya, ctx.tape);;
        untyped.with_reset.in_arg_ty = tyin;
        untyped.with_reset.cont_arg_ty = tyout;
        PiType* reset_ty = call_alloc(sizeof(PiType), ctx.pia);
        *reset_ty = (PiType) {.sort = TReset, .reset.in = tyin, .reset.out = tyout};

        type_var(untyped.with_reset.point_sym, reset_ty, env);
        type_check_i(untyped.with_reset.expr, tya, (Range){}, env, ctx);
        pop_type(env);

        PiType* mark_ty = call_alloc(sizeof(PiType), ctx.pia);
        *mark_ty = (PiType) {.sort = TResumeMark};

        // continuation 
        type_var(untyped.with_reset.in_sym, tyin, env);
        type_var(untyped.with_reset.cont_sym, mark_ty, env);
        type_check_i(untyped.with_reset.handler, tya, (Range){}, env, ctx);
        pop_types(env, 2);
        break;
    }
    case SResetTo: {
        PiType* tyin = mk_uvar(ctx.pia);
        PiType* tyout = mk_uvar(ctx.pia);
        set_type(ref, tyout, ctx.tape);;

        PiType* reset_ty = call_alloc(sizeof(PiType), ctx.pia);
        *reset_ty = (PiType) {.sort = TReset, .reset.in = tyin, .reset.out = tyout};

        type_check_i(untyped.reset_to.point, reset_ty, (Range){}, env, ctx);
        type_check_i(untyped.reset_to.arg, tyin, (Range){}, env, ctx);
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
        set_type(ref, ty, ctx.tape);;
        SymPtrAssoc labels = mk_sym_ptr_assoc(untyped.labels.terms.len, a);
        for (size_t i = 0; i < untyped.labels.terms.len; i++) {
            SynLabelBranch* branch = untyped.labels.terms.data[i].val;
            PtrArray* arr = mem_alloc(sizeof(PtrArray), a);
            *arr = mk_ptr_array(branch->args.len, a);
            for (size_t i = 0; i < branch->args.len; i++) {
                SymPtrCell arg = branch->args.data[i];
                PiType* aty;
                if (arg.val) {
                    aty = eval_type(*(SynRef*)arg.val, env, ctx);
                } else  {
                    aty = mk_uvar(ctx.pia);
                    branch->args.data[i].val = aty;
                }
                push_ptr(aty, arr);
            }
            sym_ptr_bind(untyped.labels.terms.data[i].key, arr, &labels);
        }
        add_labels(labels, env);

        // Now that the environment is setup, we typecheck the expression
        type_check_i(untyped.labels.entry, ty, (Range){}, env, ctx);

        // Then typecheck all label bodies, with arguments appropriately bound in the environment
        for (size_t i = 0 ; i < untyped.labels.terms.len; i++) {
            SynLabelBranch* branch = untyped.labels.terms.data[i].val;
            for (size_t i = 0; i < branch->args.len; i++) {
                SymPtrCell arg = branch->args.data[i];
                type_var(arg.key, arg.val, env);
            }
            type_check_i(branch->body, ty, (Range){}, env, ctx);
            pop_types(env, branch->args.len);
        }
        pop_labels(env, labels.len);
        
        break;
    }
    case SSequence: {
        size_t num_binds = 0;
        for (size_t i = 0; i < untyped.sequence.elements.len; i++) {
            SeqElt* elt = untyped.sequence.elements.data[i];
            if (elt->is_binding) {
                //PiType* type = mk_uvar(a);
                type_infer_i(elt->expr, env, ctx);
                type_var (elt->symbol, get_type(elt->expr, ctx.tape), env);
                num_binds++;
            } else {
                type_infer_i(elt->expr, env, ctx);
            }
        }

        pop_types(env, num_binds);
        if (untyped.sequence.elements.len == 0) {
            PiType* t = call_alloc(sizeof(PiType), ctx.pia);
            *t = (PiType) {.sort = TPrim, .prim = Unit};
            set_type(ref, t, ctx.tape);;
        } else {
            SeqElt* seq = untyped.sequence.elements.data[untyped.sequence.elements.len - 1];
            set_type(ref, get_type(seq->expr, ctx.tape), ctx.tape);;
        }

        break;
    }
    case SIs: {
        PiType* should_be = eval_type(untyped.is.type, env, ctx);
        Range tysrc = get_range(untyped.is.type, ctx.tape).term;
        type_check_i(untyped.is.val, should_be, tysrc, env, ctx);
        set_type(ref, get_syntax(untyped.is.type, ctx.tape).type_val, ctx.tape);; 
        break;
    }
    case SInTo: {
        PiType* distinct_type = eval_type(untyped.is.type, env, ctx);
        if (distinct_type->sort != TDistinct) {
            err.message = mv_cstr_doc("into must move a value into a distinct type!", a);
            throw_pi_error(point, err);
        }
        Module* current = get_std_current_module();
        if ((distinct_type->distinct.source_module != NULL) && (distinct_type->distinct.source_module != current)) {
            err.message = mv_cstr_doc("into for opaque types can only be used in the same module!", a);
            throw_pi_error(point, err);
        }

        Range tysrc = get_range(untyped.is.type, ctx.tape).term;
        type_check_i(untyped.into.val, distinct_type->distinct.type, tysrc, env, ctx);
        set_type(ref, distinct_type, ctx.tape);; 
        break;
    }
    case SOutOf: {
        PiType* distinct_type = eval_type(untyped.out_of.type, env, ctx);
        if (distinct_type->sort != TDistinct) {
            err.message = mv_cstr_doc("out-of must move a value out of a distinct type!", a);
            throw_pi_error(point, err);
        }
        Module* current = get_std_current_module();
        if ((distinct_type->distinct.source_module != NULL) && (distinct_type->distinct.source_module != current)) {
            err.message = mv_cstr_doc("out-of for opaque types can only be used in the same module!", a);
            throw_pi_error(point, err);
        }

        Range tysrc = get_range(untyped.is.type, ctx.tape).term;
        type_check_i(untyped.is.val, distinct_type, tysrc, env, ctx);
        set_type(ref, distinct_type->distinct.type, ctx.tape);; 
        break;
    }
    case SName: {
      AddrPiList* args = NULL;
      if (untyped.name.args.len > 0) {
        args = call_alloc(sizeof(AddrPiList), ctx.pia);
        *args = mk_addr_list(untyped.name.args.len - 1, ctx.pia);
        for (size_t i = 0; i < untyped.name.args.len; i++) {
          push_addr(eval_type(untyped.name.args.data[i], env, ctx), args);
        }
      }
      type_infer_i(untyped.name.body, env, ctx);

      PiType* named = mem_alloc(sizeof(PiType), a);
      *named = (PiType) {
        .sort = TNamed,
        .named.name = untyped.name.name,
        .named.type = get_type(untyped.name.body, ctx.tape),
        .named.args = args,
      };
      set_type(ref, named, ctx.tape);; 
      break;
    }
    case SUnName: {
        type_infer_i(untyped.unname, env, ctx);

        // TODO (BUG) : move this to a post-unification step
        if (get_type(untyped.unname, ctx.tape)->sort != TNamed) {
            err.message = mv_cstr_doc("Unname expects inner term to be named.", a);
            throw_pi_error(point, err);
        }
        set_type(ref, get_type(untyped.unname, ctx.tape)->named.type, ctx.tape);;
        break;
    }
    case SWiden: {
        PiType* wide_type = eval_type(untyped.widen.type, env, ctx);
        type_infer_i(untyped.widen.val, env, ctx);
        set_type(ref, wide_type, ctx.tape);; 
        break;
    }
    case SNarrow: {
        PiType* narrow_type = eval_type(untyped.narrow.type, env, ctx);
        type_infer_i(untyped.narrow.val, env, ctx);
        set_type(ref, narrow_type, ctx.tape);; 
        break;
    }
    case SSizeOf: {
        eval_type(untyped.size, env, ctx);
        PiType* out = call_alloc(sizeof(PiType), ctx.pia);
        *out = (PiType){.sort = TPrim, .prim = UInt_64};
        set_type(ref, out, ctx.tape);; 
        break;
    }
    case SAlignOf: {
        eval_type(untyped.size, env, ctx);
        PiType* out = call_alloc(sizeof(PiType), ctx.pia);
        *out = (PiType){.sort = TPrim, .prim = UInt_64};
        set_type(ref, out, ctx.tape);; 
        break;
    }
    case SOffsetOf: {
        eval_type(untyped.offset_of.body, env, ctx);

        PiType* struct_type = unwrap_type(get_syntax(untyped.offset_of.body, ctx.tape).type_val, type_env_module(env), ctx.pia, ctx.a);
        if (struct_type->sort != TStruct) {
            err.range = get_range(untyped.offset_of.body, ctx.tape).term;
            err.message = mv_cstr_doc("Unsupported operand: taking offset of non-struct type.",a);
            throw_pi_error(point, err); 
        }

        bool found_field = false;
        for (size_t i = 0; i < struct_type->structure.fields.len; i++) {
            if (symbol_eq(untyped.offset_of.field, struct_type->structure.fields.data[i].key))
                found_field = true;
        }

        if (!found_field) {
            err.range = get_range(untyped.offset_of.body, ctx.tape).term;
            err.message = mv_cstr_doc("Field missing in provided structure type.",a);
            throw_pi_error(point, err); 
        }

        PiType* out = mk_prim_type(ctx.pia, UInt_64);
        set_type(ref, out, ctx.tape);; 
        break;
    }
    case SDynAlloc: {
        PiType* expected = mk_prim_type(ctx.pia, UInt_64);
        type_check_i(untyped.size, expected, (Range){}, env, ctx);
        PiType* out = mk_prim_type(ctx.pia, Address);
        set_type(ref, out, ctx.tape);; 
        break;
    }
    case SModule: {
        panic(mv_string("Unsupported operation: inferring type of module"));
    }
    case SProcType: {
        PiType* t = call_alloc(sizeof(PiType), ctx.pia);
        *t = (PiType){.sort = TKind, .kind.nargs = 0};
        set_type(ref, t, ctx.tape);;

        for (size_t i = 0; i < untyped.proc_type.args.len; i++) {
            SynRef syn = untyped.proc_type.args.data[i];
            type_check_i(syn, t, (Range){}, env, ctx);
        }

        SynRef ret = untyped.proc_type.return_type;
        type_check_i(ret, t, (Range){}, env, ctx);
        break;
    }
    case SArrayType: {
        PiType* t = call_alloc(sizeof(PiType), ctx.pia);
        *t = (PiType){.sort = TKind, .kind.nargs = 0};
        set_type(ref, t, ctx.tape);;

        type_check_i(untyped.array_type.element, t, (Range){}, env, ctx);
        break;
    }
    case SStructType: {
        PiType* t = call_alloc(sizeof(PiType), ctx.pia);
        *t = (PiType){.sort = TKind, .kind.nargs = 0};
        set_type(ref, t, ctx.tape);;

        for (size_t i = 0; i < untyped.struct_type.fields.len; i++) {
            SynRef syn = untyped.struct_type.fields.data[i].val;
            type_check_i(syn, t, (Range){}, env, ctx);
        }
        break;
    }
    case SEnumType: {
        PiType* t = call_alloc(sizeof(PiType), ctx.pia);
        *t = (PiType){.sort = TKind, .kind.nargs = 0};
        set_type(ref, t, ctx.tape);;

        for (size_t i = 0; i < untyped.enum_type.variants.len; i++) {
            SynArray* args = untyped.enum_type.variants.data[i].val;

            for (size_t j = 0; j < args->len; j++) {
                SynRef syn = args->data[j];
                type_check_i(syn, t, (Range){}, env, ctx);
            }
        }
        break;
    }
    case SResetType: {
        PiType* t = call_alloc(sizeof(PiType), ctx.pia);
        *t = (PiType){.sort = TKind, .kind.nargs = 0};
        set_type(ref, t, ctx.tape);;

        type_check_i(untyped.reset_type.in, t, (Range){}, env, ctx);
        type_check_i(untyped.reset_type.out, t, (Range){}, env, ctx);
        break;
    }
    case SDynamicType: {
        PiType* t = call_alloc(sizeof(PiType), ctx.pia);
        *t = (PiType){.sort = TKind, .kind.nargs = 0};
        set_type(ref, t, ctx.tape);;

        type_check_i(untyped.dynamic_type, t, (Range){}, env, ctx);
        break;
    }
    case SAllType: {
        // For now, assume that each type has the kind Type (i.e. is not a family)
        PiType* ty = call_alloc(sizeof(PiType), ctx.pia);
        *ty = (PiType) {.sort = TKind, .kind.nargs = 0};
        set_type(ref, ty, ctx.tape);;

        for (size_t i = 0; i < untyped.bind_type.bindings.len; i++) {
            Symbol arg = untyped.bind_type.bindings.data[i];
            type_var(arg, ty, env);
        }

        type_check_i(untyped.bind_type.body, ty, (Range){}, env, ctx);
        pop_types(env, untyped.bind_type.bindings.len);
        break;
    }
    case SSealedType: {
        PiType* ty = call_alloc(sizeof(PiType), ctx.pia);
        *ty = (PiType) {.sort = TKind, .kind.nargs = 0};
        set_type(ref, ty, ctx.tape);;

        for (size_t i = 0; i < untyped.sealed_type.vars.len; i++) {
            Symbol arg = untyped.sealed_type.vars.data[i];
            type_var(arg, ty, env);
        }

        PiType* t = call_alloc(sizeof(PiType), ctx.pia);
        *t = (PiType){.sort = TConstraint, .kind.nargs = 0};
        set_type(ref, t, ctx.tape);;
        for (size_t i = 0; i < untyped.sealed_type.implicits.len; i++) {
            SynRef implicit = untyped.sealed_type.implicits.data[i];
            type_check_i(implicit, t, (Range){}, env, ctx);
        }

        type_check_i(untyped.sealed_type.body, ty, (Range){}, env, ctx);
        pop_types(env, untyped.sealed_type.vars.len);
        break;
    }
    case STypeFamily: {
        // For now, assume that each type has the kind Type (i.e. is not a family)
        PiType* ty = call_alloc(sizeof(PiType), ctx.pia);
        *ty = (PiType) {.sort = TKind, .kind.nargs = untyped.bind_type.bindings.len};
        set_type(ref, ty, ctx.tape);;

        if (untyped.bind_type.bindings.len == 0) {
            type_error_family_must_have_args(ref, ctx);
        }

        PiType* aty = call_alloc(sizeof(PiType), ctx.pia);
        *aty = (PiType) {.sort = TKind, .kind.nargs = 0};
        for (size_t i = 0; i < untyped.bind_type.bindings.len; i++) {
            Symbol arg = untyped.bind_type.bindings.data[i];
            type_var(arg, aty, env);
        }

        type_check_i(untyped.bind_type.body, aty, (Range){}, env, ctx);
        pop_types(env, untyped.bind_type.bindings.len);
        break;
    }
    case SLiftCType: {
        // Get c type
        PiType* c_type = get_c_type();
        type_check_i(untyped.c_type, c_type, (Range){}, env, ctx);

        PiType* ty = call_alloc(sizeof(PiType), ctx.pia);
        *ty = (PiType) {.sort = TKind, .kind.nargs = 0};
        set_type(ref, ty, ctx.tape);;
        break;
    }
    case SNamedType: {
        PiType* self_type = mk_uvar(ctx.pia);
        
        type_var(untyped.named_type.name, self_type, env);
        type_check_i(untyped.named_type.body, self_type, (Range){}, env, ctx);
        pop_type(env);

        PiType* type = get_type(untyped.named_type.body, ctx.tape);
        set_type(ref, type, ctx.tape);;
        if (type->sort != TKind) {
            err.message = mv_cstr_doc("Named expects types and families as arguments!", a);
            throw_pi_error(point, err);
        }
        break;
    }
    case SDistinctType: {
        type_infer_i(untyped.distinct_type.body, env, ctx);
        PiType* type = get_type(untyped.distinct_type.body, ctx.tape);
        set_type(ref, type, ctx.tape);;
        if (type->sort != TKind) {
            err.message = mv_cstr_doc("Distinct expects types and families as arguments!", a);
            throw_pi_error(point, err);
        }
        break;
    }
    case SOpaqueType: {
        type_infer_i(untyped.opaque_type.body, env, ctx);
        PiType* type = get_type(untyped.opaque_type.body, ctx.tape);
        set_type(ref, type, ctx.tape);;
        if (type->sort != TKind) {
            err.message = mv_cstr_doc("Opaque expects types and families as arguments!", a);
            throw_pi_error(point, err);
        }
        break;
    }
    case STraitType: {
        PiType* ty = call_alloc(sizeof(PiType), ctx.pia);
        *ty = (PiType) {.sort = TConstraint, .constraint.nargs = untyped.trait.vars.len};
        set_type(ref, ty, ctx.tape);;

        PiType* aty = call_alloc(sizeof(PiType), ctx.pia);
        *aty = (PiType) {.sort = TKind, .kind.nargs = 0};
        for (size_t i = 0; i < untyped.trait.vars.len; i++) {
            Symbol arg = untyped.trait.vars.data[i];
            type_var(arg, aty, env);
        }

        for (size_t i = 0; i < untyped.trait.fields.len; i++) {
            SynRef s = untyped.trait.fields.data[i].val;
            type_check_i(s, aty, (Range){}, env, ctx);
        }
        pop_types(env, untyped.bind_type.bindings.len);
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
        type_infer_i(untyped.reinterpret.type, env, ctx);
        type_infer_i(untyped.reinterpret.body, env, ctx);
        if (untyped.reinterpret.from_native) {
            // • Expect the body to be a c value
            // • expect the type to be a pico type
            PiType* ctype = get_type(untyped.reinterpret.body, ctx.tape);
            if (ctype->sort != TCType) {
                err.range = get_range(untyped.reinterpret.body, ctx.tape).term;
                err.message = mv_cstr_doc("reinterpret-native input value to be a c value, was not!", a);
                throw_pi_error(point, err);
            }
            PiType* pitype = get_type(untyped.reinterpret.type, ctx.tape);
            if (pitype->sort != TKind) {
                err.range = get_range(untyped.reinterpret.type, ctx.tape).term;
                err.message = mv_cstr_doc("reinterpret-native expected output type to be a relic type, was not!", a);
                throw_pi_error(point, err);
            }

            // Evaluate the output type, check that can reinterpret.
            PiType* pico_type = *(PiType**)eval_typed_expr(untyped.reinterpret.type, env, ctx);
            CType* c_type = &ctype->c_type;

            if (!can_reinterpret(c_type, pico_type)) {
                err.message = mv_cstr_doc("Cannot reinterpret c value as relic value.", a);
                throw_pi_error(point, err);
            }
            set_type(ref, pico_type, ctx.tape);;
        } else {
            // • Expect the body to be a pico value (which all values are, so skip check)
            // • Expect the type to be a c type
            PiType* csort = get_type(untyped.reinterpret.type, ctx.tape);
            if (csort->sort != TKind) {
                err.range = get_range(untyped.reinterpret.type, ctx.tape).term;
                err.message = mv_cstr_doc("reinterpret-relic expected output type to be a type, was not!", a);
                throw_pi_error(point, err);
            }

            // Evaluate the output type, check that can reinterpret.
            PiType* type_val = *(PiType**)eval_typed_expr(untyped.reinterpret.type, env, ctx);
            if (type_val->sort != TCType) {
                err.range = get_range(untyped.reinterpret.type, ctx.tape).term;
                err.message = mv_cstr_doc("reinterpret-relic expected output type to be a c type, was not!", a);
                throw_pi_error(point, err);
            }

            CType* c_type = &type_val->c_type;
            PiType* pico_type = get_type(untyped.reinterpret.body, ctx.tape);

            if (!can_reinterpret(c_type, pico_type)) {
                err.message = mv_cstr_doc("Cannot reinterpret relic value as c value.", a);
                throw_pi_error(point, err);
            }
            set_type(ref, type_val, ctx.tape);;
        }

        break;
    }
    case SConvert: {
        type_infer_i(untyped.convert.type, env, ctx);
        type_infer_i(untyped.convert.body, env, ctx);
        if (untyped.convert.from_native) {
            // • Expect the body to be a c value
            // • expect the type to be a pico type
            PiType* srcty = get_type(untyped.convert.body, ctx.tape);
            if (srcty->sort != TCType) {
                err.message = mv_cstr_doc("convert-native input value to be a c value, was not!", a);
                throw_pi_error(point, err);
            }
            if (get_type(untyped.convert.type, ctx.tape)->sort != TKind) {
                err.message = mv_cstr_doc("convert-native expected output type to be a relic type, was not!", a);
                throw_pi_error(point, err);
            }

            // Evaluate the output type, check that can convert.
            PiType* pico_type = *(PiType**)eval_typed_expr(untyped.convert.type, env, ctx);
            CType* c_type = &srcty->c_type;

            if (!can_convert(c_type, pico_type)) {
                PtrArray nodes = mk_ptr_array(4, a);
                push_ptr(mv_str_doc(mv_string("Cannot convert between C Type: "), a), &nodes);
                push_ptr(pretty_ctype(c_type, a), &nodes);
                push_ptr(mv_str_doc(mv_string("\nand Relic Type: "), a), &nodes);
                push_ptr(pretty_type(pico_type, default_ptp, a), &nodes);
                err.message = mv_cat_doc(nodes, a);
                throw_pi_error(point, err);
            }
            set_type(ref, pico_type, ctx.tape);;
        } else {
            // • Expect the body to be a pico value (which all values are, so skip check)
            // • Expect the type to be a c type
            PiType* destty = get_type(untyped.convert.type, ctx.tape);
            if (destty->sort != TKind) {
                err.message = mv_cstr_doc("convert-relic expected output type to be a type, was not!", a);
                throw_pi_error(point, err);
            }

            // Evaluate the output type, check that can convert.
            PiType* type_val = *(PiType**)eval_typed_expr(untyped.convert.type, env, ctx);
            if (type_val->sort != TCType) {
                err.message = mv_cstr_doc("convert-relic expected output type to be a c type, was not!", a);
                throw_pi_error(point, err);
            }

            CType* c_type = &type_val->c_type;
            PiType* pico_type = get_type(untyped.convert.body, ctx.tape);

            if (!can_convert(c_type, pico_type)) {
                err.message = mv_cstr_doc("Cannot convert relic value as c value.", a);
                throw_pi_error(point, err);
            }
            set_type(ref, type_val, ctx.tape);;
        }

        break;
    }
    case STypeOf: {
        type_infer_i(untyped.type_of, env, ctx);
        PiType* type = call_alloc(sizeof(PiType), ctx.pia);
        *type = (PiType) {.sort = TKind, .kind.nargs = 0};
        set_type(ref, type, ctx.tape);
        break;
    }
    case SDescribe: {
        TypeEntry entry = type_env_lookup(untyped.to_describe.data[0], env);
        //EnvEntry entry = env_lookup(syn.to_describe.data[0], base);
        for (size_t i = 1; i < untyped.to_describe.len; i++) {
            if (entry.type == TENotFound) {
                err.message = mv_cstr_doc("Unknown symbol in path to describe.", a);
                throw_pi_error(point, err);
            }

            if (entry.is_module) {
                ModuleEntry* mentry = get_def(untyped.to_describe.data[i], entry.module);
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
        set_type(ref, mk_string_type(ctx.pia), ctx.tape);;
        break;
    }
    case SQuote: {
        set_type(ref, get_syntax_type(), ctx.tape);;
        break;
    }
    case SCapture: {
        set_type(ref, get_syntax_type(), ctx.tape);;
        break;
    }
    case SDevAnnotation: {
        if (untyped.dev.flags & DBTypecheck)
            debug_break();
        if (untyped.dev.flags & DPTypecheck)
            panic(mv_string("not implemented: developer-print typecheck."));

        type_infer_i(untyped.dev.inner, env, ctx);
        set_type(ref, get_type(untyped.dev.inner, ctx.tape), ctx.tape);; 
        break;
    }

    }

    if (ctx.logger) {
        PtrArray docs = mk_ptr_array(2, a);
        push_ptr(mv_str_doc(mv_string("Inferred type:"), a), &docs);
        push_ptr(pretty_type(get_type(ref, ctx.tape), default_ptp, a), &docs);
        log_doc(mv_sep_doc(docs, a), ctx.logger);
        end_section(ctx.logger);
    }
    if (get_type(ref, ctx.tape) == NULL) {
        panic(mv_string("Internal Error: typecheck failed to infer type."));
    }
}

// Post Unify: perform actions which require types to be resolved,
// i.e. no uvars!
// This includes
// - Instantiating implicits
// - Checking that widen and narrow widen and narrow their arguments, respectively
void post_unify(SynRef ref, TypeEnv* env, TypeCheckContext ctx) {
    PicoError err;
    Syntax syn = get_syntax(ref, ctx.tape);
    err.range = get_range(ref, ctx.tape).term;

    switch (syn.type) {
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

    // Terms & term formers
    case SProcedure: {
        for (size_t i = 0; i < syn.procedure.implicits.len; i++) {
            SymPtrCell arg = syn.procedure.implicits.data[i];
            type_var(arg.key, arg.val, env);
        }
        for (size_t i = 0; i < syn.procedure.args.len; i++) {
            SymPtrCell arg = syn.procedure.args.data[i];
            type_var(arg.key, arg.val, env);
        }
        post_unify(syn.procedure.body, env, ctx);
        pop_types(env, syn.procedure.args.len + syn.procedure.implicits.len);
        break;
    }
    case SAll: {
        for (size_t i = 0; i < syn.all.args.len; i++) {
            Symbol arg = syn.all.args.data[i];

            PiType* arg_ty = call_alloc(sizeof(PiType), ctx.pia);
            *arg_ty = (PiType) {.sort = TVar, .var = arg,};

            type_qvar(arg, arg_ty, env);
        }
        post_unify(syn.all.body, env, ctx);
        pop_types(env, syn.all.args.len);
        break;
    }
    case SMacro: {
        post_unify(syn.transformer, env, ctx);
        break;
    }
    case SApplication: {
        post_unify(syn.application.function, env, ctx);
        for (size_t i = 0; i < syn.application.args.len; i++) {
            post_unify(syn.application.args.data[i], env, ctx);
        }

        if (syn.application.implicits.len != 0) {
            err.message = mv_cstr_doc("Implicit instantiation assumes no implicits are already present!", ctx.a);
            throw_pi_error(ctx.point, err);
        }
        PiType* fn_type = get_type(syn.application.function, ctx.tape);
        if (fn_type->sort == TProc) {
            for (size_t i = 0; i < fn_type->proc.implicits.len; i++) {
                PiType* arg_ty = fn_type->proc.implicits.data[i];
                if (arg_ty->sort != TTraitInstance) {
                    err.message = mv_cstr_doc("Implicit arguments must have type trait instance!", ctx.a);
                    throw_pi_error(ctx.point, err);
                }

                InstanceEntry e = type_instance_lookup(arg_ty->instance.instance_of, arg_ty->instance.args, env);
                switch (e.type) {
                case IEAbsSymbol: {
                    SynRef new_impl = new_syntax(ctx.tape);
                    set_syntax(new_impl,
                               (Syntax){
                                   .type = SAbsVariable,
                                   .abvar = e.abvar,
                               },
                               ctx.tape);
                    set_type(new_impl, arg_ty, ctx.tape);
                    push_syn(new_impl, &syn.application.implicits);
                    set_syntax(ref, syn, ctx.tape);
                    break;
                }
                case IENotFound:
                    err.message = mv_cstr_doc("Implicit argument cannot be instantiated - instance not found!", ctx.a);
                    throw_pi_error(ctx.point, err);
                case IEAmbiguous:
                    err.message = mv_cstr_doc("Implicit argument cannot be instantiated - ambiguous instances!", ctx.a);
                    throw_pi_error(ctx.point, err);
                default:
                    panic(mv_string("Invalid instance entry type!"));
                }
            }
        } else if (fn_type->sort != TKind && fn_type->sort != TConstraint ) {
            panic(mv_string("Invalid lhs in application in post_unify: not Proc or Kind"));
        } 
        break;
    }
    case SAllApplication: {
        post_unify(syn.all_application.function, env, ctx);
        for (size_t i = 0; i < syn.all_application.args.len; i++) {
            post_unify(syn.all_application.args.data[i], env, ctx);
        }

        if (syn.all_application.implicits.len != 0) {
            err.message = mv_cstr_doc("Implicit instantiation assumes no implicits are already present!", ctx.a);
            throw_pi_error(ctx.point, err);
        }

        // Given, e.g. an application (a {I64 Bool} x y), with a : All [A B] t, perform a
        // beta-reduction to get t[A/I64, B/Bool]. This will be used to
        // instantiate implicits if t is a Proc.
        PiType* all_type = get_type(syn.all_application.function, ctx.tape);
        SymPtrAssoc type_binds = mk_sym_ptr_assoc(all_type->binder.vars.len, ctx.a);
        for (size_t i = 0; i < all_type->binder.vars.len; i++) {
            SynRef type = syn.all_application.types.data[i];
            sym_ptr_bind(all_type->binder.vars.data[i], get_syntax(type, ctx.tape).type_val, &type_binds);
        }
        // TODO (BUG): this type probably wants unwrapping
        PiType* proc_type = pi_type_subst(all_type->binder.body, type_binds, ctx.pia, ctx.a);

        // Early exit if we don't need to do any instantiation.
        if (proc_type->sort != TProc) return;

        for (size_t i = 0; i < proc_type->proc.implicits.len; i++) {
            PiType* arg_ty = proc_type->proc.implicits.data[i];
            if (arg_ty->sort != TTraitInstance) {
                err.message = mv_cstr_doc("Implicit arguments must have type trait instance!", ctx.a);
                throw_pi_error(ctx.point, err);
            }

            InstanceEntry e = type_instance_lookup(arg_ty->instance.instance_of, arg_ty->instance.args, env);
            switch (e.type) {
            case IEAbsSymbol: {
                SynRef new_impl = new_syntax(ctx.tape);//mem_alloc(sizeof(Syntax), ctx.a);
                set_syntax(new_impl,
                           (Syntax) {
                               .type = SAbsVariable,
                               .abvar = e.abvar,
                           },
                           ctx.tape);
                set_type(new_impl, arg_ty, ctx.tape);
                push_syn(new_impl, &syn.all_application.implicits);
                set_syntax(ref, syn, ctx.tape);
                break;
            }
            case IENotFound:
                err.message = mv_cstr_doc("Implicit argument cannot be instantiated - instance not found!", ctx.a);
                throw_pi_error(ctx.point, err);
            case IEAmbiguous:
                err.message = mv_cstr_doc("Implicit argument cannot be instantiated - ambiguous instances!", ctx.a);
                throw_pi_error(ctx.point, err);
            default:
                panic(mv_string("Invalid instance entry type!"));
            }
        }
        break;
    }
    case SSeal: {
        if (syn.seal.implicits.len > 0) {
            panic(mv_string("not implemented: post-unify for seal with > 0 implicits"));
        }
        post_unify(syn.seal.body, env, ctx);
        break;
    }
    case SUnseal: {
        SymPtrAssoc type_binds = mk_sym_ptr_assoc(syn.unseal.types.len, ctx.a);
        for (size_t i = 0; i < syn.unseal.types.len; i++) {
            Symbol arg = syn.unseal.types.data[i];

            PiType* arg_ty = call_alloc(sizeof(PiType), ctx.pia);
            *arg_ty = (PiType) {.sort = TVar, .var = arg,};

            sym_ptr_bind(arg, arg_ty, &type_binds);
            type_qvar(arg, arg_ty, env);
        }
        PiType* src_ty = get_type(syn.unseal.sealed, ctx.tape);
        PiType* var_ty = pi_type_subst(src_ty->sealed.body, type_binds, ctx.pia, ctx.a);
        type_var(syn.unseal.binder, var_ty, env);
        post_unify(syn.unseal.body, env, ctx);
        pop_types(env, syn.all.args.len + 1);
        break;
    }
    case SConstructor:  {
        // Resolve the variant tag
        PiType* enum_type = unwrap_type(get_type(ref, ctx.tape), type_env_module(env), ctx.pia, ctx.a);

        bool found_variant = false;
        for (size_t i = 0; i < enum_type->enumeration.variants.len; i++) {
            SymAddrPiCell cell = enum_type->enumeration.variants.data[i];
            if (symbol_eq(cell.key, syn.variant.tagname)) {
                found_variant = true;
                syn.variant.tag = i;
                set_syntax(ref, syn, ctx.tape);
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
        PiType* enum_type = unwrap_type(get_type(ref, ctx.tape), type_env_module(env), ctx.pia, ctx.a);

        bool found_variant = false;
        for (size_t i = 0; i < enum_type->enumeration.variants.len; i++) {
            SymAddrPiCell cell = enum_type->enumeration.variants.data[i];
            if (symbol_eq(cell.key, syn.variant.tagname)) {
                found_variant = true;
                syn.variant.tag = i;
                set_syntax(ref, syn, ctx.tape);
                break;
            }
        }
        if (!found_variant) {
            panic(mv_string("Unable to find variant tag in post-unify."));
        }
        for (size_t i = 0; i < syn.variant.args.len; i++) {
            post_unify(syn.variant.args.data[i], env, ctx);
        }
        break;
    }
    case SMatch: {
        post_unify(syn.match.val, env, ctx);
        PiType* enum_type = unwrap_type(get_type(syn.match.val, ctx.tape), type_env_module(env), ctx.pia, ctx.a); 

        for (size_t i = 0; i < syn.match.clauses.len; i++) {
            SynClause* clause = syn.match.clauses.data[i];
            bool found_tag = false;
            for (size_t j = 0; j < enum_type->enumeration.variants.len; j++) {
                if (symbol_eq(clause->tagname, enum_type->enumeration.variants.data[j].key)) {
                    found_tag = true;
                    clause->tag = j;
                }
                if (clause->is_wildcard) {
                    found_tag = true;
                }
            }

            if (!found_tag) {
                PtrArray nodes = mk_ptr_array(4, ctx.a);
                push_ptr(mv_cstr_doc("The clause", ctx.a), &nodes);
                push_ptr(mk_str_doc(symbol_to_string(clause->tagname, ctx.a), ctx.a), &nodes);
                push_ptr(mv_cstr_doc("does not correspond to any variant in the enum type.", ctx.a), &nodes);
                err.message = mv_sep_doc(nodes, ctx.a);
                throw_pi_error(ctx.point, err);
            }

            // TODO BUG: bind types/vars from clause
            post_unify(clause->body, env, ctx);
        }
        break;
    }
    case SArray: {
        for (size_t i = 0; i < syn.array.elements.len; i++) {
            post_unify(syn.array.elements.data[i], env, ctx);
        }
        break;
    }
    case SArrayElt: {
        for (size_t i = 0; i < syn.array_elt.index.len; i++) {
            post_unify(syn.array_elt.index.data[i], env, ctx);
        }
        post_unify(syn.array_elt.array, env, ctx);
        break;
    }
    case SStructure: {
        if (syn.structure.has_base == Some) {
            post_unify(syn.structure.base, env, ctx);
        }
        for (size_t i = 0; i < syn.structure.fields.len; i++) {
            post_unify(syn.structure.fields.data[i].val, env, ctx);
        }
        break;
    }
    case SProjector: {
        post_unify(syn.projector.val, env, ctx);
        break;
    }
    case SInstance: {
        PiType* ty_ty = call_alloc(sizeof(PiType), ctx.pia);
        *ty_ty = (PiType) {.sort = TKind, .constraint.nargs = 0};

        for (size_t i = 0; i < syn.instance.params.len; i++) {
            Symbol arg = syn.instance.params.data[i];
            type_var(arg, ty_ty, env);
        }

        for (size_t i = 0; i < syn.instance.implicits.len; i++) {
            SymPtrCell arg = syn.instance.implicits.data[i];
            type_var(arg.key, arg.val, env);
        }

        PiType* type = get_type(ref, ctx.tape);
        for (size_t i = 0; i < type->instance.fields.len; i++) {
            SynRef* field_syn = (SynRef*)sym_syn_lookup(type->instance.fields.data[i].key, syn.instance.fields);
            if (field_syn) {
                post_unify(*field_syn, env, ctx);
            } else {
                err.message = mv_cstr_doc("Trait instance is missing a field", ctx.a);
                throw_pi_error(ctx.point, err);
            }
        }

        pop_types(env, syn.instance.params.len + syn.instance.implicits.len);
        break;
    }
    case SDynamic:
        post_unify(syn.dynamic, env, ctx);
        break;
    case SDynamicUse:
        post_unify(syn.use, env, ctx);
        break;
    case SDynamicSet:
        post_unify(syn.dynamic_set.dynamic, env, ctx);
        post_unify(syn.dynamic_set.new_val, env, ctx);
        break;

    // Control Flow & Binding
    case SDynamicLet:
        for (size_t i = 0; i < syn.dyn_let_expr.bindings.len; i++) {
            DynBinding* b = syn.dyn_let_expr.bindings.data[i];
            post_unify(b->var, env, ctx);
            post_unify(b->expr, env, ctx);
        }
        post_unify(syn.dyn_let_expr.body, env, ctx);
        break;
    case SLet:
        // TODO BUG: update environment 
        for (size_t i = 0; i < syn.let_expr.bindings.len; i++) {
            post_unify(syn.let_expr.bindings.data[i].val, env, ctx);
        }
        post_unify(syn.let_expr.body, env, ctx);
        break;
    case SIf:
        post_unify(syn.if_expr.condition, env, ctx);
        post_unify(syn.if_expr.true_branch, env, ctx);
        post_unify(syn.if_expr.false_branch, env, ctx);
        break;
    case SCond: {
        for (size_t i = 0; i < syn.cond.clauses.len; i++) {
            CondClause* clause = syn.cond.clauses.data[i];
            post_unify(clause->condition, env, ctx);
            post_unify(clause->branch, env, ctx);
        }
        post_unify(syn.cond.otherwise, env, ctx);
        break;
    }
    case SLabels:
        post_unify(syn.labels.entry, env, ctx);
        for (size_t i = 0; i < syn.labels.terms.len; i++) {
            SynLabelBranch* branch = syn.labels.terms.data[i].val;
            post_unify(branch->body, env, ctx);
        }
        break;
    case SGoTo:
        for (size_t i = 0; i < syn.go_to.args.len; i++) {
            post_unify(syn.go_to.args.data[i], env, ctx);
        }
        break;
    case SSequence:
        for (size_t i = 0; i < syn.sequence.elements.len; i++) {
            SeqElt* elt = syn.sequence.elements.data[i];
            post_unify(elt->expr, env, ctx);
        }
        break;
    case SWithReset:
        // TODO BUG: update environment 
        post_unify(syn.with_reset.expr, env, ctx);
        post_unify(syn.with_reset.handler, env, ctx);
        break;
    case SResetTo:
        post_unify(syn.reset_to.point, env, ctx);
        post_unify(syn.reset_to.arg, env, ctx);
        break;

    // Special
    case SIs:
        post_unify(syn.is.val, env, ctx);
        post_unify(syn.is.type, env, ctx);
        break;
    case SInTo:
        post_unify(syn.into.val, env, ctx);
        post_unify(syn.into.type, env, ctx);
        break;
    case SOutOf:
        post_unify(syn.out_of.val, env, ctx);
        post_unify(syn.out_of.type, env, ctx);
        break;
    case SName:
        post_unify(syn.name.body, env, ctx);
        break;
    case SWiden: {
        post_unify(syn.widen.val, env, ctx);
        post_unify(syn.widen.type, env, ctx);
        PiType* src_type = get_type(syn.widen.val, ctx.tape);
        PiType* dest_type = get_syntax(syn.widen.type, ctx.tape).type_val;
        if (!is_wider(src_type, dest_type)) {
            err.message = mv_cstr_doc("This widening is invalid", ctx.a);
            throw_pi_error(ctx.point, err);
        }
        break;
    }
    case SNarrow:
        post_unify(syn.narrow.val, env, ctx);
        post_unify(syn.narrow.type, env, ctx);
        PiType* src_type = get_type(syn.narrow.val, ctx.tape);
        PiType* dest_type = get_syntax(syn.narrow.type, ctx.tape).type_val;
        if (!is_narrower(src_type, dest_type)) {
            PtrArray docs = mk_ptr_array(2, ctx.a);
            push_ptr(mv_cstr_doc("This narrowing is invalid - cannot narrow from type:", ctx.a), &docs);
            push_ptr(pretty_type(src_type, default_ptp, ctx.a), &docs);
            push_ptr(mv_cstr_doc("to type:", ctx.a), &docs);
            push_ptr(pretty_type(dest_type, default_ptp, ctx.a), &docs);
            err.message = mv_hsep_doc(docs, ctx.a);
            throw_pi_error(ctx.point, err);
        }
        break;
    case SUnName:
        post_unify(syn.unname, env, ctx);
        break;
    case SSizeOf:
    case SAlignOf:
        post_unify(syn.size, env, ctx);
        break;
    case SOffsetOf:
        post_unify(syn.offset_of.body, env, ctx);
        break;
    case SDynAlloc:
        post_unify(syn.size, env, ctx);
        break;
    case SModule:
        panic(mv_string("instantiate implicits not implemented for module"));

    // Types & Type formers
    case SProcType:
    case SArrayType:
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
        post_unify(syn.c_type, env, ctx);
        break;
    case SCheckedType:
        // TODO (INVESTIGATE FEATURE): check that it is OK to do nothing? (no implicits in types, right?)
        //      what if we have types produced by procedures, i.e. Proc [...] Type)?
        break;

    case SAnnotation:
        panic(mv_string("instantiate implicits not implemented for a annotation"));
    case SReinterpret:
        post_unify(syn.reinterpret.type, env, ctx);
        post_unify(syn.reinterpret.body, env, ctx);
        break;
    case SConvert:
        post_unify(syn.convert.type, env, ctx);
        post_unify(syn.convert.body, env, ctx);
        break;
    case STypeOf: {
        post_unify(syn.type_of, env, ctx);
        break;
    }
    case SDescribe: 
    case SQuote: 
    case SCapture: 
        break;
    case SDevAnnotation:
        post_unify(syn.dev.inner, env, ctx);
    }
}

// This function recursively descends into a term and squashes all types.
// In this case, to squash a type removes all unification vars from 
// the type. (see squash_type in unify.h)
void squash_types(SynRef ref, TypeEnv* env, TypeCheckContext ctx) {
    Syntax typed = get_syntax(ref, ctx.tape); 
    UnifyContext uctx = {
        .a = ctx.a,
        .pia = ctx.pia,
        .current_module = type_env_module(env),
        .logger = ctx.logger,
    };
    PicoError err = {.range = get_range(ref, ctx.tape).term};
    switch (typed.type) {
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
    case SProcedure: {
        squash_types(typed.procedure.body, env, ctx);
        break;
    }
    case SAll: {
        // TODO (FUTURE BUG): need to squash args when HKTs are allowed 
        squash_types(typed.all.body, env, ctx);
        break;
    }
    case SMacro: { 
        squash_types(typed.transformer, env, ctx);
        break;
    }
    case SApplication: {
        squash_types(typed.application.function, env, ctx);
        
        for (size_t i = 0; i < typed.application.implicits.len; i++) {
            squash_types(typed.application.implicits.data[i], env, ctx);
        }

        for (size_t i = 0; i < typed.application.args.len; i++) {
            squash_types(typed.application.args.data[i], env, ctx);
        }
        break;
    }
    case SAllApplication: {
        squash_types(typed.application.function, env, ctx);

        for (size_t i = 0; i < typed.all_application.types.len; i++) {
            squash_types(typed.all_application.types.data[i], env, ctx);
            SynRef checked = typed.all_application.types.data[i];
            if (get_syntax(checked, ctx.tape).type_val->sort == TUVar) {
                type_error_all_app_couldnt_deduce_types(i, ref, ctx);
            }
        }

        for (size_t i = 0; i < typed.all_application.implicits.len; i++) {
            squash_types(typed.all_application.implicits.data[i], env, ctx);
        }
        
        for (size_t i = 0; i < typed.all_application.args.len; i++) {
            squash_types(typed.all_application.args.data[i], env, ctx);
        }
        break;
    }
    case SSeal: {
        squash_types(typed.seal.body, env, ctx);

        for (size_t i = 0; i < typed.seal.types.len; i++) {
            squash_types(typed.seal.types.data[i], env, ctx);
        }

        for (size_t i = 0; i < typed.seal.implicits.len; i++) {
            squash_types(typed.seal.implicits.data[i], env, ctx);
        }
        break;
    }
    case SUnseal: {
        squash_types(typed.unseal.sealed, env, ctx);

        // TODO (BUG!): ensure that there are no free types in typed.unseal.body->ptype
        squash_types(typed.unseal.body, env, ctx);
        break;
    }
    case SConstructor: {
        if (typed.variant.has_enum_type == Some) {
            squash_types(typed.variant.enum_type, env, ctx);
        }
        break;
    }
    case SVariant: {
        if (typed.variant.has_enum_type == Some) {
            squash_types(typed.variant.enum_type, env, ctx);
        }
        
        for (size_t i = 0; i < typed.variant.args.len; i++) {
            squash_types(typed.variant.args.data[i], env, ctx);
        }
        break;
    }
    case SMatch: {
        squash_types(typed.match.val, env, ctx);

        for (size_t i = 0; i < typed.match.clauses.len; i++) {
            SynClause* clause = (SynClause*)typed.match.clauses.data[i];
            squash_types(clause->body, env, ctx);
        }
        break;
    }
    case SArray: {
        for (size_t i = 0; i < typed.array.elements.len; i++) {
            squash_types(typed.array.elements.data[i], env, ctx);
        }
        break;
    }
    case SArrayElt: {
        for (size_t i = 0; i < typed.array_elt.index.len; i++) {
            squash_types(typed.array_elt.index.data[i], env, ctx);
        }
        squash_types(typed.array_elt.array, env, ctx);
        break;
    }
    case SStructure: {
        if (typed.structure.has_base == Some) {
            squash_types(typed.structure.base, env, ctx);
        }
        for (size_t i = 0; i < typed.structure.fields.len; i++) {
            SynRef syn = typed.structure.fields.data[i].val;
            squash_types(syn, env, ctx);
        }
        break;
    }
    case SProjector:
        squash_types(typed.projector.val, env, ctx);
        break;
    case SInstance:
        squash_types(typed.instance.constraint, env, ctx);

        for (size_t i = 0; i < typed.instance.implicits.len; i++) {
            SynRef syn = typed.instance.fields.data[i].val;
            squash_types(syn, env, ctx);
        }

        for (size_t i = 0; i < typed.instance.fields.len; i++) {
            SynRef syn = typed.instance.fields.data[i].val;
            squash_types(syn, env, ctx);
        }
        break;
    case SDynamic:
        squash_types(typed.dynamic, env, ctx);
        break;
    case SDynamicUse:
        squash_types(typed.use, env, ctx);
        break;
    case SDynamicSet:
        squash_types(typed.dynamic_set.dynamic, env, ctx);
        squash_types(typed.dynamic_set.new_val, env, ctx);
        break;
    case SDynamicLet:
        for (size_t i = 0; i < typed.dyn_let_expr.bindings.len; i++) {
            DynBinding* dbind = typed.dyn_let_expr.bindings.data[i];
            squash_types(dbind->var, env, ctx);
            squash_types(dbind->expr, env, ctx);
        }
        squash_types(typed.dyn_let_expr.body, env, ctx);
        break;
    case SLet:
        for (size_t i = 0; i < typed.let_expr.bindings.len; i++) {
            squash_types(typed.let_expr.bindings.data[i].val, env, ctx);
        }
        squash_types(typed.let_expr.body, env, ctx);
        break;
    case SIf: {
        squash_types(typed.if_expr.condition, env, ctx);
        squash_types(typed.if_expr.true_branch, env, ctx);
        squash_types(typed.if_expr.false_branch, env, ctx);
        break;
    }
    case SCond: {
        for (size_t i = 0; i < typed.cond.clauses.len; i++) {
            CondClause* clause = typed.cond.clauses.data[i];
            squash_types(clause->condition, env, ctx);
            squash_types(clause->branch, env, ctx);
        }
        squash_types(typed.cond.otherwise, env, ctx);
        break;
    }
    case SLabels:
        squash_types(typed.labels.entry, env, ctx);
        for (size_t i = 0; i < typed.labels.terms.len; i++) {
            SynLabelBranch* branch = typed.labels.terms.data[i].val;
            squash_types(branch->body, env, ctx);
        }
        break;
    case SGoTo:
        for (size_t i = 0; i < typed.go_to.args.len; i++) {
            squash_types(typed.go_to.args.data[i], env, ctx);
        }
        break;
    case SWithReset:
        squash_types(typed.with_reset.expr, env, ctx);
        squash_types(typed.with_reset.handler, env, ctx);

        if (!has_unification_vars_p(*typed.with_reset.in_arg_ty)) {
            squash_type(typed.with_reset.in_arg_ty, uctx);
        } else {
            err.message = mv_cstr_doc("reset argument type not instantiated", ctx.a);
            throw_pi_error(ctx.point, err);
        }
        if (!has_unification_vars_p(*typed.with_reset.cont_arg_ty)) {
            squash_type(typed.with_reset.cont_arg_ty, uctx);
        } else {
            err.message = mv_cstr_doc("resume argument type not instantiated", ctx.a);
            throw_pi_error(ctx.point, err);
        }
        break;
    case SResetTo:
        squash_types(typed.reset_to.point, env, ctx);
        squash_types(typed.reset_to.arg, env, ctx);
        break;
    case SSequence:
        for (size_t i = 0; i < typed.sequence.elements.len; i++) {
            SeqElt* elt = typed.sequence.elements.data[i];
            squash_types(elt->expr, env, ctx);
        }
        break;
    case SIs:
        squash_type(get_syntax(typed.is.type, ctx.tape).type_val, uctx);
        squash_types(typed.is.val, env, ctx);
        break;
    case SInTo:
        squash_type(get_syntax(typed.into.type, ctx.tape).type_val, uctx);
        squash_types(typed.into.val, env, ctx);
        break;
    case SOutOf:
        squash_type(get_syntax(typed.out_of.type, ctx.tape).type_val, uctx);
        squash_types(typed.out_of.val, env, ctx);
        break;
    case SName:
        squash_types(typed.name.body, env, ctx);
        break;
    case SUnName:
        squash_types(typed.unname, env, ctx);
        break;
    case SWiden:
        squash_type(get_syntax(typed.widen.type, ctx.tape).type_val, uctx);
        squash_types(typed.widen.val, env, ctx);
        break;
    case SNarrow:
        squash_type(get_syntax(typed.narrow.type, ctx.tape).type_val, uctx);
        squash_types(typed.narrow.val, env, ctx);
        break;
    case SSizeOf:
    case SAlignOf:
        squash_types(typed.size, env, ctx);
        break;
    case SOffsetOf:
        squash_types(typed.offset_of.body, env, ctx);
        break;
    case SDynAlloc:
        squash_types(typed.size, env, ctx);
        break;
    case SProcType: {
        for (size_t i = 0; i < typed.proc_type.args.len; i++) {
            squash_types(typed.proc_type.args.data[i], env, ctx);
        }

        squash_types(typed.proc_type.return_type, env, ctx);
        break;
    }
    case SArrayType: {
        squash_types(typed.array_type.element, env, ctx);
        break;
    }
    case SStructType: {
        for (size_t i = 0; i < typed.struct_type.fields.len; i++) {
            squash_types(typed.struct_type.fields.data[i].val, env, ctx);
        }
        break;
    }
    case SEnumType: {
        for (size_t i = 0; i < typed.enum_type.variants.len; i++) {
            SynArray* args = typed.enum_type.variants.data[i].val;

            for (size_t j = 0; j < args->len; j++) {
                squash_types(args->data[j], env, ctx);
            }
        }
        break;
    }
    case SResetType: {
        squash_types(typed.reset_type.in, env, ctx);
        squash_types(typed.reset_type.out, env, ctx);
        break;
    }
    case SDynamicType: {
        squash_types(typed.dynamic_type, env, ctx);
        break;
    }
    case SAllType:
        squash_types(typed.bind_type.body, env, ctx);
        break;
    case SSealedType:
        squash_types(typed.sealed_type.body, env, ctx);
        break;
    case STypeFamily:
        squash_types(typed.bind_type.body, env, ctx);
        break;
    case SLiftCType:
        squash_types(typed.c_type, env, ctx);
        break;
    case SNamedType:
        squash_types(typed.named_type.body, env, ctx);
        break;
    case SDistinctType:
        squash_types(typed.distinct_type.body, env, ctx);
        break;
    case SOpaqueType:
        squash_types(typed.opaque_type.body, env, ctx);
        break;
    case STraitType:
        for (size_t i = 0; i < typed.trait.fields.len; i++) {
            squash_types(typed.trait.fields.data[i].val, env, ctx);
        }
        break;
    case SCheckedType: {
        // TODO: it seems like *sometimes* we want for this to be allowed as a
        //   uvar (notably when calling eval_type), but otherwise don't want
        //   to throw an error here.
        squash_type(typed.type_val, uctx);
        break;
    }
    case SReinterpret:
        squash_types(typed.reinterpret.type, env, ctx);
        squash_types(typed.reinterpret.body, env, ctx);
        break;
    case SConvert:
        squash_types(typed.convert.type, env, ctx);
        squash_types(typed.convert.body, env, ctx);
        break;
    case STypeOf: {
        squash_types(typed.type_of, env, ctx);
        break;
    }
    case SDescribe:
    case SQuote:
    case SCapture: 
        break;
    case SDevAnnotation: 
        squash_types(typed.dev.inner, env, ctx);
        break;
    default:
        panic(mv_string("Internal Error: invalid syntactic form provided to squash_types"));
        break;
    }

    // has_unification_vars only returns true if those vars don't go anywhere!
    if (!has_unification_vars_p(*get_type(ref, ctx.tape))) {
        squash_type(get_type(ref, ctx.tape), uctx);
    }
    else {
        squash_type(get_type(ref, ctx.tape), uctx);

        PtrArray nodes = mk_ptr_array(4, ctx.a);
        push_ptr(mk_str_doc(mv_string("Typechecking error: not all unification vars were instantiated. Term:"), ctx.a), &nodes);
        push_ptr(pretty_syntax(ref, ctx.tape, ctx.a), &nodes);
        push_ptr(mk_str_doc(mv_string("Type:"), ctx.a), &nodes);
        push_ptr(pretty_type(get_type(ref, ctx.tape), default_ptp, ctx.a), &nodes);

        err.message = mv_vsep_doc(nodes, ctx.a);
        throw_pi_error(ctx.point, err);
    }
}

void* eval_typed_expr(SynRef ref, TypeEnv* env, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    PiErrorPoint* point = ctx.point;
    Target gen_target = ctx.target;
    clear_target(gen_target);

    if (ctx.logger) {
        String str = string_cat(mv_string("evaluating: "), syntax_type_to_string(get_syntax(ref, ctx.tape).type), a);
        start_section(str, ctx.logger);

        PtrArray docs = mk_ptr_array(2, a);
        push_ptr(mk_str_doc(mv_string("syntax:"), a), &docs);
        push_ptr(mv_nest_doc(2, pretty_syntax(ref, ctx.tape, a), a), &docs);
        log_doc(mv_sep_doc(docs, ctx.a), ctx.logger);
    }

    squash_types(ref, env, ctx);
    post_unify(ref, env, ctx);
    
    // Catch error here; so can cleanup after self before further unwinding.
    ErrorPoint cleanup_point;
    if (catch_error(cleanup_point)) goto on_error;

    // TODO (INVESTIGATE): is LinkData needed
    CodegenContext cg_ctx = {
        .tape = ctx.tape, .a = a, .point = &cleanup_point, .target = gen_target, 
    };
    generate_type_expr(ref, env, cg_ctx);

    PiType type = *get_type(ref, ctx.tape);
    void* result = pico_run_expr(gen_target, pi_size_of(type), a, &cleanup_point);

    if (ctx.logger) {
        PiType** ptype = result;

        PtrArray docs = mk_ptr_array(2, a);
        push_ptr(mk_str_doc(mv_string("evaluated to:"), a), &docs);
        push_ptr(mv_nest_doc(2, pretty_type(*ptype, default_ptp, a), a), &docs);
        log_doc(mv_sep_doc(docs, ctx.a), ctx.logger);

        end_section(ctx.logger);
    }
    return result;

 on_error: {
        PicoError err;
        err.range = get_range(ref, ctx.tape).term;
        err.message = cleanup_point.error_message;
        throw_pi_error(point, err);
    }
}

void* eval_expr(SynRef untyped, TypeEnv* env, TypeCheckContext ctx) {
    type_infer_i(untyped, env, ctx);
    return eval_typed_expr(untyped, env, ctx);
}

// TODO (BUG LOGIC UB): evaluation may produce a function pointer (or an object
// with a function pointer) that points to generated code. This method currently
// provides no means to capture that assembly.
PiType* eval_type(SynRef ref, TypeEnv* env, TypeCheckContext ctx) {
    Syntax syn = get_syntax(ref, ctx.tape);
    if (syn.type == SCheckedType) return syn.type_val;
    type_infer_i(ref, env, ctx);

    PiType* type = get_type(ref, ctx.tape);
    if (type->sort != TKind && type->sort != TConstraint) {
        PicoError err = (PicoError) {
            .range = get_range(ref, ctx.tape).term,
            .message = mv_cstr_doc("Value expected to be type, was not!", ctx.a),
        };
        throw_pi_error(ctx.point, err);
    }

    PiType** result = eval_typed_expr(ref, env, ctx);
    syn = (Syntax) {
        .type = SCheckedType,
        .type_val = *result,
    };
    set_syntax(ref, syn, ctx.tape);

    return *result;
}

