#include "platform/error.h"
#include "platform/memory/executable.h"
#include "platform/signals.h"

#include "data/string.h"
#include "pretty/string_printer.h"

#include "pico/binding/environment.h"
#include "pico/binding/type_env.h"
#include "pico/analysis/unify.h"
#include "pico/analysis/typecheck.h"
#include "pico/stdlib/core.h"
#include "pico/stdlib/extra.h"
#include "pico/stdlib/foreign.h"
#include "pico/values/ctypes.h"
#include "pico/codegen/codegen.h"
#include "pico/codegen/foreign_adapters.h"
#include "pico/eval/call.h"

// forward declarations
void type_check_expr(Syntax* untyped, PiType type, TypeEnv* env, UVarGenerator* gen, Allocator* a, ErrorPoint* point);
void type_infer_expr(Syntax* untyped, TypeEnv* env, UVarGenerator* gen, Allocator* a, ErrorPoint* point);
void instantiate_implicits(Syntax* untyped, TypeEnv* env, Allocator* a, ErrorPoint* point);

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
        instantiate_implicits(term, t_env, a, point);
        pop_type(t_env);
        break;
    }
    case TLExpr: {
        Syntax* term = top->expr;
        type_infer_expr(term, t_env, gen, a, point);
        instantiate_implicits(term, t_env, a, point);
        break;
    }
    }
}

// Forward declarations for implementation (internal declarations)
void type_infer_i(Syntax* untyped, TypeEnv* env, UVarGenerator* gen, Allocator* a, ErrorPoint* point);
void type_check_i(Syntax* untyped, PiType* type, TypeEnv* env, UVarGenerator* gen, Allocator* a, ErrorPoint* point);
PiType* eval_type(Syntax* untyped, TypeEnv* env, Allocator* a, UVarGenerator* gen, ErrorPoint* point);
void* eval_expr(Syntax* untyped, TypeEnv* env, Allocator* a, UVarGenerator* gen, ErrorPoint* point);
void* eval_typed_expr(Syntax* typed, TypeEnv* env, Allocator* a, ErrorPoint* point);
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
    // TODO (INVESTIGATE): consider checking if ptype is null or not to save
    // duplicate work?
    switch (untyped->type) {
    case SLitUntypedIntegral:
        untyped->type = SLitTypedIntegral;
        untyped->ptype = mk_uvar_integral(gen, a);
        break;
    case SLitTypedIntegral:
        untyped->ptype = mk_prim_type(a, untyped->integral.type);
        break;
    case SLitUntypedFloating:
        untyped->type = SLitTypedFloating;
        untyped->ptype = mk_uvar_floating(gen, a);
        break;
    case SLitTypedFloating:
        untyped->ptype = mk_prim_type(a, untyped->integral.type);
        break;
    case SLitBool:
        untyped->ptype = mk_prim_type(a, Bool);
        break;
    case SLitString:
        untyped->ptype = mk_string_type(a);
        break;
    case SVariable: {
        TypeEntry te = type_env_lookup(untyped->variable, env);
        if (te.type != TENotFound) {
            if (te.is_module) {
                throw_error(point, mv_string("Unexpected module."));
            }

            untyped->ptype = te.ptype;
            if (te.value) {
                untyped->type = SCheckedType;
                untyped->type_val = te.value;
            }
        } else {
            String* sym = symbol_to_string(untyped->variable);
            String msg = string_cat(mv_string("Couldn't find type of variable: "), *sym, a);
            throw_error(point, msg);
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
        // give each arg a unification variable type. 
        PiType* proc_ty = mem_alloc(sizeof(PiType), a);
        proc_ty->sort = TProc;
        proc_ty->proc.implicits = mk_ptr_array(untyped->procedure.implicits.len, a);
        proc_ty->proc.args = mk_ptr_array(untyped->procedure.args.len, a);
        untyped->ptype = proc_ty;

        for (size_t i = 0; i < untyped->procedure.implicits.len; i++) {
            SymPtrACell arg = untyped->procedure.implicits.data[i];
            PiType* aty;
            if (arg.val) {
                aty = eval_type(arg.val, env, a, gen, point);
                if (aty->sort != TTraitInstance) {
                    throw_error(point, mv_string("Instance procedure argument does not have instance type."));
                }
            } else  {
                aty = mk_uvar(gen, a);
            }
            type_var(arg.key, aty, env);
            push_ptr(aty, &proc_ty->proc.implicits);
        }

        for (size_t i = 0; i < untyped->procedure.args.len; i++) {
            SymPtrACell arg = untyped->procedure.args.data[i];
            PiType* aty;
            if (arg.val) {
                aty = eval_type(arg.val, env, a, gen, point);
            } else  {
                aty = mk_uvar(gen, a);
            }
            type_var(arg.key, aty, env);
            push_ptr(aty, &proc_ty->proc.args);
        }

        type_infer_i(untyped->procedure.body, env, gen, a, point); 
        pop_types(env, untyped->procedure.args.len + untyped->procedure.implicits.len);
        proc_ty->proc.ret = untyped->procedure.body->ptype;
        break;
    }
    case SAll: {
        // Give each arg type kind.
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
    case SMacro: {
        // Macro inner type: 
        // proc [Array Syntax] Syntax
        // where syntax = ...
        PiType* syntax_array = mk_app_type(a, get_array_type(), get_syntax_type());
        PiType* transformer_proc = mk_proc_type(a, 1, syntax_array, get_syntax_type());

        type_check_i(untyped->transformer, transformer_proc, env, gen, a, point);
        PiType* t = mem_alloc(sizeof(PiType), a);
        *t = (PiType) {
            .sort = TPrim,
            .prim = TMacro,
        };
        untyped->ptype = t;
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

        } else if (fn_type.sort == TProc) {
            if (fn_type.proc.args.len != untyped->application.args.len) {
                throw_error(point, mk_string("Incorrect number of function arguments", a));
            }

            for (size_t i = 0; i < fn_type.proc.args.len; i++) {
                type_check_i(untyped->application.args.data[i],
                             (PiType*)fn_type.proc.args.data[i],
                             env, gen, a, point);
            }

            untyped->ptype = fn_type.proc.ret;

        } else if (fn_type.sort == TAll) {
            SynArray types = mk_ptr_array(fn_type.binder.vars.len, a);
            for (size_t i = 0; i < fn_type.binder.vars.len; i++) {
                Syntax* syn = mem_alloc(sizeof(Syntax), a);
                *syn = (Syntax) {.type = SCheckedType, .type_val = mk_uvar(gen, a),};
                push_ptr(syn, &types);
            }

            SynAllApp new_app = (SynAllApp) {
                .function = untyped->application.function,
                .types = types,
                .implicits = untyped->application.implicits,
                .args = untyped->application.args
            };
            untyped->type = SAllApplication;
            untyped->all_application = new_app;
            type_infer_i(untyped, env, gen, a, point);

        } else if (fn_type.sort == TKind) {
            if (fn_type.kind.nargs != untyped->application.args.len) {
                throw_error(point, mk_string("Incorrect number of family arguments", a));
            }

            PiType* kind = mem_alloc(sizeof(PiType), a);
            *kind = (PiType) {.sort = TKind, .kind.nargs = 0};
            for (size_t i = 0; i < fn_type.kind.nargs; i++) {
                type_check_i(untyped->application.args.data[i],
                             kind, env, gen, a, point);
            }
            untyped->ptype = kind;
        } else {
            PtrArray arr = mk_ptr_array(2, a);
            push_ptr(mv_str_doc(mv_string("Expected LHS of application to be a function or kind. Was actually:"), a), &arr);
            push_ptr(pretty_type(&fn_type, a), &arr);
            throw_error(point, doc_to_str(mv_sep_doc(arr, a), a));
        }
        
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
            // Check that all type args are actually types!
            SymPtrAssoc type_binds = mk_sym_ptr_assoc(all_type.binder.vars.len, a);
            for (size_t i = 0; i < all_type.binder.vars.len; i++) {
                Syntax* type = untyped->all_application.types.data[i];
                eval_type(type, env, a, gen, point);
                sym_ptr_bind(all_type.binder.vars.data[i], type->type_val, &type_binds);
            }
            
            PiType* ret_type = pi_type_subst(all_type.binder.body, type_binds, a);
            untyped->ptype = ret_type;
        } else {

            // Check that all type args are actually types!
            SymPtrAssoc type_binds = mk_sym_ptr_assoc(all_type.binder.vars.len, a);
            for (size_t i = 0; i < all_type.binder.vars.len; i++) {
                Syntax* type = untyped->all_application.types.data[i];
                eval_type(type, env, a, gen, point);
                sym_ptr_bind(all_type.binder.vars.data[i], type->type_val, &type_binds);
            }
        
            // Bind the vars in the all type to specific types!
            PiType* proc_type = pi_type_subst(all_type.binder.body, type_binds, a);
            if (proc_type->proc.args.len != untyped->all_application.args.len) {
                const char* msg = proc_type->proc.args.len < untyped->all_application.args.len
                    ? "Too many args to procedure in all-application"
                    : "Too few args to procedure in all-application";
                throw_error(point, mk_string(msg, a));
            }

            for (size_t i = 0; i < proc_type->proc.args.len; i++) {
                type_check_i(untyped->all_application.args.data[i],
                             (PiType*)proc_type->proc.args.data[i],
                             env, gen, a, point);
            }

            untyped->ptype = proc_type->proc.ret;
        }
        break;
    }
    case SConstructor: {
        // Typecheck variant
        PiType* enum_type = eval_type(untyped->variant.enum_type, env, a, gen, point);

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
            String msg_origin = mv_string("Could not find variant tag: ");
            String data = *symbol_to_string(untyped->variant.tagname);
            throw_error(point, string_cat(msg_origin, data, a));
        }
        break;
    }
    case SVariant: {
        // Typecheck variant
        PiType* enum_type = eval_type(untyped->variant.enum_type, env, a, gen, point);

        // Typecheck is pretty simple: ensure that the tag is present in the
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
            String msg_origin = mv_string("Could not find variant tag: ");
            String data = *symbol_to_string(untyped->variant.tagname);
            throw_error(point, string_cat(msg_origin, data, a));
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
        PiType* ty = eval_type(untyped->structure.ptype, env, a, gen, point);
        untyped->ptype = ty; 

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
        PiType source_type = *untyped->projector.val->ptype;
        if (source_type.sort == TStruct) {
            // search for field
            PiType* ret_ty = NULL;
            for (size_t i = 0; i < source_type.structure.fields.len; i++) {
                if (source_type.structure.fields.data[i].key == untyped->projector.field) {
                    ret_ty = source_type.structure.fields.data[i].val;
                }
            }
            if (ret_ty == NULL) {
                throw_error(point, mv_string("Field not found in struct!"));
            }
            untyped->ptype = ret_ty;

        } else if (source_type.sort == TTraitInstance) {
            // search for field
            PiType* ret_ty = NULL;
            for (size_t i = 0; i < source_type.instance.fields.len; i++) {
                if (source_type.instance.fields.data[i].key == untyped->projector.field) {
                    ret_ty = source_type.instance.fields.data[i].val;
                }
            }
            if (ret_ty == NULL) {
                throw_error(point, mv_string("Field not found in instance!"));
            }
            untyped->ptype = ret_ty;

        } else {
            throw_error(point, mv_string("Projection only works on structs and traits."));
        }
        break;
    }
    case SInstance: {
        // Output type
        PiType* ty = mem_alloc(sizeof(PiType), a);
        untyped->ptype = ty;

        PiType* constraint_ty = mem_alloc(sizeof(PiType), a);
        *constraint_ty = (PiType) {.sort = TConstraint, .constraint.nargs = 0};

        PiType* ty_ty = mem_alloc(sizeof(PiType), a);
        *ty_ty = (PiType) {.sort = TKind, .constraint.nargs = 0};

        for (size_t i = 0; i < untyped->instance.params.len; i++) {
            Symbol arg = untyped->instance.params.data[i];
            type_var(arg, ty_ty, env);
        }

        for (size_t i = 0; i < untyped->instance.implicits.len; i++) {
            SymPtrACell arg = untyped->instance.implicits.data[i];
            PiType* aty;
            if (arg.val) {
                aty = eval_type(arg.val, env, a, gen, point);
            } else  {
                aty = mk_uvar(gen, a);
            }
            type_var(arg.key, aty, env);
        }

        *ty = *eval_type(untyped->instance.constraint, env, a, gen, point);

        if (ty->sort != TTraitInstance) {
            throw_error(point, mv_string("Instance type invalid"));
        }

        if (untyped->instance.fields.len != ty->trait.fields.len) {
            throw_error(point, mv_string("Instance must have exactly n fields."));
        }

        for (size_t i = 0; i < ty->instance.fields.len; i++) {
            Syntax** field_syn = (Syntax**)sym_ptr_lookup(ty->instance.fields.data[i].key, untyped->instance.fields);
            if (field_syn) {
                PiType* field_ty = ty->instance.fields.data[i].val;
                type_check_i(*field_syn, field_ty, env, gen, a, point);
            } else {
                throw_error(point, mv_string("Trait instance is missing a field"));
            }
        }

        pop_types(env, untyped->instance.params.len + untyped->instance.implicits.len);
        break;
    }
    case SDynamic: {
        type_infer_i(untyped->dynamic, env, gen, a, point);
        PiType* inner_type = untyped->dynamic->ptype; 
        PiType* t = mem_alloc(sizeof(PiType), a);
        *t = (PiType) {
            .sort = TDynamic,
            .dynamic = inner_type,
        };
        untyped->ptype = t;
        break;
    }
    case SDynamicUse: {
        type_infer_i(untyped->dynamic, env, gen, a, point);
        PiType* dyn_type = untyped->dynamic->ptype; 
        if (dyn_type->sort != TDynamic) {
            throw_error(point, mv_string("use on non-dynamic type!"));
        }
        untyped->ptype = dyn_type->dynamic;
        break;
    }
    case SDynamicLet: {
        for (size_t i = 0; i < untyped->dyn_let_expr.bindings.len; i++) {
            DynBinding* dbind = untyped->dyn_let_expr.bindings.data[i];

            PiType* dyn_ty = mem_alloc(sizeof(PiType), a);
            PiType* val_ty = mk_uvar(gen, a);
            *dyn_ty = (PiType) {
                .sort = TDynamic,
                .dynamic = val_ty,
            };

            type_check_i(dbind->var, dyn_ty, env, gen, a, point);
            type_check_i(dbind->expr, val_ty, env, gen, a, point);
        }
        type_infer_i(untyped->dyn_let_expr.body, env, gen, a, point);
        untyped->ptype = untyped->dyn_let_expr.body->ptype;
        break;
    }
    case SLet: {
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
        pop_types(env, untyped->let_expr.bindings.len);
        break;
    }
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
    case SGoTo: {
        if (label_present(untyped->go_to.label, env)) {
            PiType* t = mk_uvar(gen, a);
            untyped->ptype = t;
        } else {
            throw_error(point, mv_string("Error in go-to: Label Not found!"));
        }
        break;
    }
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
    case SIs: {
        PiType* should_be = eval_type(untyped->is.type, env, a, gen, point);
        type_check_i(untyped->is.val, should_be, env, gen, a, point);
        untyped->ptype = untyped->is.type->type_val; 
        break;
    }
    case SInTo: {
        PiType* distinct_type = eval_type(untyped->is.type, env, a, gen, point);
        if (distinct_type->sort != TDistinct) {
            throw_error(point, mv_string("into must move a value into a distinct type!"));
        }
        Module* current = get_std_current_module();
        if ((distinct_type->distinct.source_module != NULL) && (distinct_type->distinct.source_module != current)) {
            throw_error(point, mv_string("into for opaque types can only be used in the same module!"));
        }

        type_check_i(untyped->into.val,
                     distinct_type->distinct.type,
                     env, gen, a, point);
        untyped->ptype = distinct_type; 
        break;
    }
    case SOutOf: {
        PiType* distinct_type = eval_type(untyped->out_of.type, env, a, gen, point);
        if (distinct_type->sort != TDistinct) {
            throw_error(point, mv_string("out-of must move a value out of a distinct type!"));
        }
        Module* current = get_std_current_module();
        if ((distinct_type->distinct.source_module != NULL) && (distinct_type->distinct.source_module != current)) {
            throw_error(point, mv_string("out-of for opaque types can only be used in the same module!"));
        }

        type_check_i(untyped->is.val,
                     distinct_type,
                     env, gen, a, point);
        untyped->ptype = distinct_type->distinct.type; 
        break;
    }
    case SDynAlloc: {
        PiType* t = mem_alloc(sizeof(PiType), a);
        *t = (PiType){.sort = TPrim, .prim = UInt_64};
        type_check_i(untyped->size, t, env, gen, a, point);

        PiType* out = mem_alloc(sizeof(PiType), a);
        *out = (PiType){.sort = TPrim, .prim = Address};
        untyped->ptype = out; 
        break;
    }
    case SSizeOf: {
        // TODO: this is sus. 
        eval_type(untyped->size, env, a, gen, point);
        PiType* out = mem_alloc(sizeof(PiType), a);
        *out = (PiType){.sort = TPrim, .prim = UInt_64};
        untyped->ptype = out; 
        break;
    }
    case SAlignOf: {
        eval_type(untyped->size, env, a, gen, point);
        PiType* out = mem_alloc(sizeof(PiType), a);
        *out = (PiType){.sort = TPrim, .prim = UInt_64};
        untyped->ptype = out; 
        break;
    }
    case SModule: {
        panic(mv_string("Unsupported operation: inferring type of module"));
    }
    case SProcType: {
        PiType* t = mem_alloc(sizeof(PiType), a);
        *t = (PiType){.sort = TKind, .kind.nargs = 0};
        untyped->ptype = t;

        for (size_t i = 0; i < untyped->proc_type.args.len; i++) {
            Syntax* syn = untyped->proc_type.args.data[i];
            type_check_i(syn, t, env, gen, a, point);
        }

        Syntax* ret = untyped->proc_type.return_type;
        type_check_i(ret, t, env, gen, a, point);
        break;
    }
    case SStructType: {
        PiType* t = mem_alloc(sizeof(PiType), a);
        *t = (PiType){.sort = TKind, .kind.nargs = 0};
        untyped->ptype = t;

        for (size_t i = 0; i < untyped->struct_type.fields.len; i++) {
            Syntax* syn = untyped->struct_type.fields.data[i].val;
            type_check_i(syn, t, env, gen, a, point);
        }
        break;
    }
    case SEnumType: {
        PiType* t = mem_alloc(sizeof(PiType), a);
        *t = (PiType){.sort = TKind, .kind.nargs = 0};
        untyped->ptype = t;

        for (size_t i = 0; i < untyped->enum_type.variants.len; i++) {
            PtrArray* args = untyped->enum_type.variants.data[i].val;

            for (size_t j = 0; j < args->len; j++) {
                Syntax* syn = args->data[j];
                type_check_i(syn, t, env, gen, a, point);
            }
        }
        break;
    }
    case SResetType: {
        PiType* t = mem_alloc(sizeof(PiType), a);
        *t = (PiType){.sort = TKind, .kind.nargs = 0};
        untyped->ptype = t;

        type_check_i(untyped->reset_type.in, t, env, gen, a, point);
        type_check_i(untyped->reset_type.out, t, env, gen, a, point);
        break;
    }
    case SDynamicType: {
        PiType* t = mem_alloc(sizeof(PiType), a);
        *t = (PiType){.sort = TKind, .kind.nargs = 0};
        untyped->ptype = t;

        type_check_i(untyped->dynamic_type, t, env, gen, a, point);
        break;
    }
    case SAllType: {
        // For now, assume that each type has the kind Type (i.e. is not a family)
        PiType* ty = mem_alloc(sizeof(PiType), a);
        *ty = (PiType) {.sort = TKind, .kind.nargs = 0};
        untyped->ptype = ty;

        for (size_t i = 0; i < untyped->bind_type.bindings.len; i++) {
            Symbol arg = untyped->bind_type.bindings.data[i];
            type_var(arg, ty, env);
        }

        type_check_i(untyped->bind_type.body, ty, env, gen, a, point);
        pop_types(env, untyped->bind_type.bindings.len);
        break;
    }
    case SExistsType: {
        panic(mv_string("Unsupported operation: inferring type of existential type"));
    }
    case STypeFamily: {
        // For now, assume that each type has the kind Type (i.e. is not a family)
        PiType* ty = mem_alloc(sizeof(PiType), a);
        *ty = (PiType) {.sort = TKind, .kind.nargs = untyped->bind_type.bindings.len};
        untyped->ptype = ty;

        PiType* aty = mem_alloc(sizeof(PiType), a);
        *aty = (PiType) {.sort = TKind, .kind.nargs = 0};
        for (size_t i = 0; i < untyped->bind_type.bindings.len; i++) {
            Symbol arg = untyped->bind_type.bindings.data[i];
            type_var(arg, aty, env);
        }

        type_check_i(untyped->bind_type.body, aty, env, gen, a, point);
        pop_types(env, untyped->bind_type.bindings.len);
        break;
    }
    case SCType: {
        // Get c type
        PiType* c_type = get_c_type();
        type_check_i(untyped->c_type, c_type, env, gen, a, point);

        PiType* ty = mem_alloc(sizeof(PiType), a);
        *ty = (PiType) {.sort = TKind, .kind.nargs = 0};
        untyped->ptype = ty;
        break;
    }
    case SNamedType: {
        PiType* self_type = mk_uvar(gen, a);
        
        type_var(untyped->named_type.name, self_type, env);
        type_check_i(untyped->named_type.body, self_type, env, gen, a, point);
        pop_type(env);

        untyped->ptype = untyped->named_type.body->ptype;
        if (untyped->ptype->sort != TKind) {
            throw_error(point, mv_string("Named expects types and families as arguments!"));
        }
        break;
    }
    case SDistinctType: {
        type_infer_i(untyped->distinct_type, env, gen, a, point);
        untyped->ptype= untyped->distinct_type->ptype;
        if (untyped->ptype->sort != TKind) {
            throw_error(point, mv_string("Distinct expects types and families as arguments!"));
        }
        break;
    }
    case STraitType: {
        PiType* ty = mem_alloc(sizeof(PiType), a);
        *ty = (PiType) {.sort = TConstraint, .constraint.nargs = untyped->trait.vars.len};
        untyped->ptype = ty;

        PiType* aty = mem_alloc(sizeof(PiType), a);
        *aty = (PiType) {.sort = TKind, .kind.nargs = 0};
        for (size_t i = 0; i < untyped->trait.vars.len; i++) {
            Symbol arg = untyped->trait.vars.data[i];
            type_var(arg, aty, env);
        }

        for (size_t i = 0; i < untyped->trait.fields.len; i++) {
            Syntax* s = untyped->trait.fields.data[i].val;
            type_check_i(s, aty, env, gen, a, point);
        }
        pop_types(env, untyped->bind_type.bindings.len);
        break;
    }
    case SOpaqueType: {
        type_infer_i(untyped->distinct_type, env, gen, a, point);
        untyped->ptype= untyped->distinct_type->ptype;
        if (untyped->ptype->sort != TKind) {
            throw_error(point, mv_string("Opaque expects types and families as arguments!"));
        }
        break;
    }
    case SCheckedType: {
        panic(mv_string("Inferring type of checked type. This is unexpected"));
    }
    case SAnnotation: {
        panic(mv_string("Annotations are only supported at the top level."));
    }
    case SReinterpret: {
        type_infer_i(untyped->reinterpret.type, env, gen, a, point);
        type_infer_i(untyped->reinterpret.body, env, gen, a, point);
        if (untyped->reinterpret.from_native) {
            // • Expect the body to be a c value
            // • expect the type to be a pico type
            if (untyped->reinterpret.body->ptype->sort != TCType) {
                throw_error(point, mv_string("reinterpret-native input value to be a c value, was not!"));
            }
            if (untyped->reinterpret.type->ptype->sort != TKind) {
                throw_error(point, mv_string("reinterpret-native expected output type to be a relic type, was not!"));
            }

            // Evaluate the output type, check that can reinterpret.
            PiType* pico_type = *(PiType**)eval_typed_expr(untyped->reinterpret.type, env, a, point);
            CType* c_type = &untyped->reinterpret.body->ptype->c_type;

            if (!can_reinterpret(c_type, pico_type)) {
                throw_error(point, mv_string("Cannot reinterpret c value as relic value."));
            }
            untyped->ptype = pico_type;
        } else {
            // • Expect the body to be a pico value (which all values are, so skip check)
            // • Expect the type to be a c type
            if (untyped->reinterpret.type->ptype->sort != TKind) {
                throw_error(point, mv_string("reinterpret-relic expected output type to be a type, was not!"));
            }

            // Evaluate the output type, check that can reinterpret.
            PiType* type_val = *(PiType**)eval_typed_expr(untyped->reinterpret.type, env, a, point);
            if (type_val->sort != TCType) {
                throw_error(point, mv_string("reinterpret-relic expected output type to be a c type, was not!"));
            }

            CType* c_type = &type_val->c_type;
            PiType* pico_type = untyped->reinterpret.body->ptype;

            if (!can_reinterpret(c_type, pico_type)) {
                throw_error(point, mv_string("Cannot reinterpret relic value as c value."));
            }
            untyped->ptype = type_val;
        }

        break;
    }
    case SConvert: {
        type_infer_i(untyped->convert.type, env, gen, a, point);
        type_infer_i(untyped->convert.body, env, gen, a, point);
        if (untyped->convert.from_native) {
            // • Expect the body to be a c value
            // • expect the type to be a pico type
            if (untyped->convert.body->ptype->sort != TCType) {
                throw_error(point, mv_string("convert-native input value to be a c value, was not!"));
            }
            if (untyped->convert.type->ptype->sort != TKind) {
                throw_error(point, mv_string("convert-native expected output type to be a relic type, was not!"));
            }

            // Evaluate the output type, check that can convert.
            PiType* pico_type = *(PiType**)eval_typed_expr(untyped->convert.type, env, a, point);
            CType* c_type = &untyped->convert.body->ptype->c_type;

            if (!can_convert(c_type, pico_type)) {
                throw_error(point, mv_string("Cannot convert c value as relic value."));
            }
            untyped->ptype = pico_type;
        } else {
            // • Expect the body to be a pico value (which all values are, so skip check)
            // • Expect the type to be a c type
            if (untyped->convert.type->ptype->sort != TKind) {
                throw_error(point, mv_string("convert-relic expected output type to be a type, was not!"));
            }

            // Evaluate the output type, check that can convert.
            PiType* type_val = *(PiType**)eval_typed_expr(untyped->convert.type, env, a, point);
            if (type_val->sort != TCType) {
                throw_error(point, mv_string("convert-relic expected output type to be a c type, was not!"));
            }

            CType* c_type = &type_val->c_type;
            PiType* pico_type = untyped->convert.body->ptype;

            if (!can_convert(c_type, pico_type)) {
                throw_error(point, mv_string("Cannot convert relic value as c value."));
            }
            untyped->ptype = type_val;
        }

        break;
    }

    }
    if (untyped->ptype == NULL) {
        panic(mv_string("Internal Error: typecheck failed to instantiate type."));
    }
}

void instantiate_implicits(Syntax* syn, TypeEnv* env, Allocator* a, ErrorPoint* point) {
    switch (syn->type) {
    case SLitUntypedIntegral:
    case SLitTypedIntegral:
    case SLitUntypedFloating:
    case SLitTypedFloating:
    case SLitString:
    case SLitBool:
    case SVariable:
    case SAbsVariable:
        break;

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
        instantiate_implicits(syn->procedure.body, env, a, point);
        pop_types(env, syn->procedure.args.len + syn->procedure.implicits.len);
        break;
    }
    case SAll: {
        for (size_t i = 0; i < syn->all.args.len; i++) {
            Symbol arg = syn->all.args.data[i];

            // TODO INVESTIGATE: could this cause issues??
            PiType* arg_ty = mem_alloc(sizeof(PiType), a);
            *arg_ty = (PiType) {.sort = TVar, .var = arg,};

            type_qvar(arg, arg_ty, env);
        }
        instantiate_implicits(syn->all.body, env, a, point);
        pop_types(env, syn->all.args.len);
        break;
    }
    case SMacro: {
        instantiate_implicits(syn->transformer, env, a, point);
        break;
    }
    case SApplication: {
        instantiate_implicits(syn->application.function, env, a, point);
        for (size_t i = 0; i < syn->application.args.len; i++) {
            instantiate_implicits(syn->application.args.data[i], env, a, point);
        }

        if (syn->application.implicits.len != 0) {
            throw_error(point, mk_string("Implicit instantiation assumes no implicits are already present!", a));
        }
        PiType fn_type = *syn->application.function->ptype;
        for (size_t i = 0; i < fn_type.proc.implicits.len; i++) {
            PiType* arg_ty = fn_type.proc.implicits.data[i];
            if (arg_ty->sort != TTraitInstance) {
                throw_error(point, mk_string("Implicit arguments must have type trait instance!", a));
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
                throw_error(point, mk_string("Implicit argument cannot be instantiated - instance not found!", a));
            case IEAmbiguous:
                throw_error(point, mk_string("Implicit argument cannot be instantiated - ambiguous instances!", a));
            default:
                panic(mv_string("Invalid instance entry type!"));
            }
        }
        break;
    }
    case SAllApplication: {
        instantiate_implicits(syn->all_application.function, env, a, point);
        for (size_t i = 0; i < syn->all_application.args.len; i++) {
            instantiate_implicits(syn->all_application.args.data[i], env, a, point);
        }

        if (syn->all_application.implicits.len != 0) {
            throw_error(point, mk_string("Implicit instantiation assumes no implicits are already present!", a));
        }

        PiType all_type = *syn->all_application.function->ptype;
        SymPtrAssoc type_binds = mk_sym_ptr_assoc(all_type.binder.vars.len, a);
        for (size_t i = 0; i < all_type.binder.vars.len; i++) {
            Syntax* type = syn->all_application.types.data[i];
            sym_ptr_bind(all_type.binder.vars.data[i], type->type_val, &type_binds);
        }
        
        // Bind the vars in the all type to specific types!
        PiType* proc_type = pi_type_subst(all_type.binder.body, type_binds, a);

        // Early exit if we don't need to do any instantiation.
        if (proc_type->sort != TProc) return;

        for (size_t i = 0; i < proc_type->proc.implicits.len; i++) {
            PiType* arg_ty = proc_type->proc.implicits.data[i];
            if (arg_ty->sort != TTraitInstance) {
                throw_error(point, mk_string("Implicit arguments must have type trait instance!", a));
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
                throw_error(point, mk_string("Implicit argument cannot be instantiated - instance not found!", a));
            case IEAmbiguous:
                throw_error(point, mk_string("Implicit argument cannot be instantiated - ambiguous instances!", a));
            default:
                panic(mv_string("Invalid instance entry type!"));
            }
        }
        break;
    }
    case SConstructor: break;
    case SVariant: {
        for (size_t i = 0; i < syn->variant.args.len; i++) {
            instantiate_implicits(syn->variant.args.data[i], env, a, point);
        }
        break;
    }
    case SMatch: {
        instantiate_implicits(syn->match.val, env, a, point);

        for (size_t i = 0; i < syn->match.clauses.len; i++) {
            SynClause* clause = syn->match.clauses.data[i];

            // TODO BUG: bind types from clause
            instantiate_implicits(clause->body, env, a, point);
        }
        break;
    }
    case SStructure: {
        for (size_t i = 0; i < syn->structure.fields.len; i++) {
            instantiate_implicits(syn->structure.fields.data[i].val, env, a, point);
        }
        break;
    }
    case SProjector: {
        instantiate_implicits(syn->projector.val, env, a, point);
        break;
    }
    case SInstance: {
        PiType* ty_ty = mem_alloc(sizeof(PiType), a);
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
                instantiate_implicits(*field_syn, env, a, point);
            } else {
                throw_error(point, mv_string("Trait instance is missing a field"));
            }
        }

        pop_types(env, syn->instance.params.len + syn->instance.implicits.len);
        break;
    }
    case SDynamic:
        instantiate_implicits(syn->dynamic, env, a, point);
        break;
    case SDynamicUse:
        instantiate_implicits(syn->use, env, a, point);
        break;

    // Control Flow & Binding
    case SDynamicLet:
        for (size_t i = 0; i < syn->dyn_let_expr.bindings.len; i++) {
            DynBinding* b = syn->dyn_let_expr.bindings.data[i];
            instantiate_implicits(b->var, env, a, point);
            instantiate_implicits(b->expr, env, a, point);
        }
        instantiate_implicits(syn->dyn_let_expr.body, env, a, point);
        break;
    case SLet:
        // TODO BUG: update environment 
        for (size_t i = 0; i < syn->let_expr.bindings.len; i++) {
            instantiate_implicits(syn->let_expr.bindings.data[i].val, env, a, point);
        }
        instantiate_implicits(syn->let_expr.body, env, a, point);
        break;
    case SIf:
        instantiate_implicits(syn->if_expr.condition, env, a, point);
        instantiate_implicits(syn->if_expr.true_branch, env, a, point);
        instantiate_implicits(syn->if_expr.false_branch, env, a, point);
        break;
    case SLabels:
        panic(mv_string("instantiate implicits not implemented for labels"));
    case SGoTo:
        break;
    case SSequence:
        for (size_t i = 0; i < syn->sequence.elements.len; i++) {
            instantiate_implicits(syn->sequence.elements.data[i], env, a, point);
        }
        break;
    case SWithReset:
        // TODO BUG: update environment 
        instantiate_implicits(syn->with_reset.expr, env, a, point);
        instantiate_implicits(syn->with_reset.handler, env, a, point);
        break;
    case SResetTo:
        instantiate_implicits(syn->reset_to.point, env, a, point);
        instantiate_implicits(syn->reset_to.arg, env, a, point);
        break;

    // Special
    case SIs:
        instantiate_implicits(syn->is.val, env, a, point);
        instantiate_implicits(syn->is.type, env, a, point);
        break;
    case SInTo:
        instantiate_implicits(syn->into.val, env, a, point);
        instantiate_implicits(syn->into.type, env, a, point);
        break;
    case SOutOf:
        instantiate_implicits(syn->out_of.val, env, a, point);
        instantiate_implicits(syn->out_of.type, env, a, point);
        break;
    case SDynAlloc:
    case SSizeOf:
    case SAlignOf:
        instantiate_implicits(syn->size, env, a, point);
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
    case SExistsType:
    case STypeFamily:
        break;
    case SCType:
        instantiate_implicits(syn->c_type, env, a, point);
        break;
    case SCheckedType:
        // TODO: check that it is OK to do nothing? (no implicits in types, right?)
        break;

    case SAnnotation:
        panic(mv_string("instantiate implicits not implemented for a annotation"));
    case SReinterpret:
        instantiate_implicits(syn->reinterpret.type, env, a, point);
        instantiate_implicits(syn->reinterpret.body, env, a, point);
        break;
    case SConvert:
        instantiate_implicits(syn->convert.type, env, a, point);
        instantiate_implicits(syn->convert.body, env, a, point);
        break;
    }
}

void squash_types(Syntax* typed, Allocator* a, ErrorPoint* point) {
    switch (typed->type) {
    case SLitUntypedIntegral:
    case SLitTypedIntegral:
    case SLitUntypedFloating:
    case SLitTypedFloating:
    case SLitBool:
    case SLitString:
    case SVariable:
    case SAbsVariable:
        break;
    case SProcedure: {
        // squash body
        // TODO (FUTURE BUG): Need to squash types of annotated arguments!
        squash_types(typed->procedure.body, a, point);
        break;
    }
    case SAll: {
        // TODO (FUTURE BUG): need to squash args when HKTs are allowed 
        squash_types(typed->all.body, a, point);
        break;
    }
    case SMacro: { 
        squash_types(typed->transformer, a, point);
        break;
    }
    case SApplication: {
        squash_types(typed->application.function, a, point);
        
        for (size_t i = 0; i < typed->application.implicits.len; i++) {
            squash_types(typed->application.implicits.data[i], a, point);
        }

        for (size_t i = 0; i < typed->application.args.len; i++) {
            squash_types(typed->application.args.data[i], a, point);
        }
        break;
    }
    case SAllApplication: {
        squash_types(typed->application.function, a, point);

        for (size_t i = 0; i < typed->all_application.types.len; i++) {
            squash_types(typed->all_application.types.data[i], a, point);
        }

        for (size_t i = 0; i < typed->all_application.implicits.len; i++) {
            squash_types(typed->all_application.implicits.data[i], a, point);
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
        for (size_t i = 0; i < typed->structure.fields.len; i++) {
            Syntax* syn = typed->structure.fields.data[i].val;
            squash_types(syn, a, point);
        }
        break;
    }
    case SProjector:
        squash_types(typed->projector.val, a, point);
        break;
    case SInstance:
        squash_types(typed->instance.constraint, a, point);

        for (size_t i = 0; i < typed->instance.implicits.len; i++) {
            Syntax* syn = typed->instance.fields.data[i].val;
            squash_types(syn, a, point);
        }

        for (size_t i = 0; i < typed->instance.fields.len; i++) {
            Syntax* syn = typed->instance.fields.data[i].val;
            squash_types(syn, a, point);
        }
        break;
    case SDynamic:
        squash_types(typed->dynamic, a, point);
        break;
    case SDynamicUse:
        squash_types(typed->use, a, point);
        break;
    case SDynamicLet:
        for (size_t i = 0; i < typed->dyn_let_expr.bindings.len; i++) {
            DynBinding* dbind = typed->dyn_let_expr.bindings.data[i];
            squash_types(dbind->var, a, point);
            squash_types(dbind->expr, a, point);
        }
        squash_types(typed->dyn_let_expr.body, a, point);
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
    case SInTo:
        squash_type(typed->into.type->type_val);
        squash_types(typed->into.val, a, point);
        break;
    case SOutOf:
        squash_type(typed->out_of.type->type_val);
        squash_types(typed->out_of.val, a, point);
        break;
    case SDynAlloc:
    case SSizeOf:
    case SAlignOf:
        squash_types(typed->size, a, point);
        break;
    case SProcType: {
        for (size_t i = 0; i < typed->proc_type.args.len; i++) {
            squash_types(typed->proc_type.args.data[i], a, point);
        }

        squash_types(typed->proc_type.return_type, a, point);
        break;
    }
    case SStructType: {
        for (size_t i = 0; i < typed->struct_type.fields.len; i++) {
            squash_types(typed->struct_type.fields.data[i].val, a, point);
        }
        break;
    }
    case SEnumType: {
        for (size_t i = 0; i < typed->enum_type.variants.len; i++) {
            PtrArray* args = typed->enum_type.variants.data[i].val;

            for (size_t j = 0; j < args->len; j++) {
                squash_types(args->data[j], a, point);
            }
        }
        break;
    }
    case SResetType: {
        squash_types(typed->reset_type.in, a, point);
        squash_types(typed->reset_type.out, a, point);
        break;
    }
    case SDynamicType: {
        squash_types(typed->dynamic_type, a, point);
        break;
    }
    case SAllType:
        squash_types(typed->bind_type.body, a, point);
        break;
    case STypeFamily:
        squash_types(typed->bind_type.body, a, point);
        break;
    case SCType:
        squash_types(typed->c_type, a, point);
        break;
    case SNamedType:
        squash_types(typed->named_type.body, a, point);
        break;
    case SDistinctType:
        squash_types(typed->distinct_type, a, point);
        break;
    case SOpaqueType:
        squash_types(typed->opaque_type, a, point);
        break;
    case STraitType:
        for (size_t i = 0; i < typed->trait.fields.len; i++) {
            squash_types(typed->trait.fields.data[i].val, a, point);
        }
        break;
    case SCheckedType:
        squash_type(typed->type_val);
        break;
    case SReinterpret:
        squash_types(typed->reinterpret.type, a, point);
        squash_types(typed->reinterpret.body, a, point);
        break;
    case SConvert:
        squash_types(typed->convert.type, a, point);
        squash_types(typed->convert.body, a, point);
        break;
    default:
        panic(mv_string("Internal Error: invalid syntactic form provided to squash_types"));
        break;
    }

    if (!has_unification_vars_p(*typed->ptype)) {
        squash_type(typed->ptype);
    }
    else {
        squash_type(typed->ptype);

        PtrArray nodes = mk_ptr_array(4, a);
        push_ptr(mk_str_doc(mv_string("Typechecking error: not all unification vars were instantiated. Term:"), a), &nodes);
        push_ptr(pretty_syntax(typed, a), &nodes);
        push_ptr(mk_str_doc(mv_string("Type:"), a), &nodes);
        push_ptr(pretty_type(typed->ptype, a), &nodes);

        throw_error(point, doc_to_str(mv_vsep_doc(nodes, a), a));
    }
}

void* eval_typed_expr(Syntax* typed, TypeEnv* env, Allocator* a, ErrorPoint* point) {
    Allocator exalloc = mk_executable_allocator(a);
    
    // Catch error here; so can cleanup after self before further unwinding.
    ErrorPoint cleanup_point;
    if (catch_error(cleanup_point)) goto on_error;

    Target gen_target = {
        .target = mk_assembler(&exalloc),
        .code_aux = mk_assembler(&exalloc),
        .data_aux = mem_alloc(sizeof(U8Array), a)
    };
    *gen_target.data_aux = mk_u8_array(128, a);

    // TODO (INVESTIGATE): is LinkData needed
    generate_type_expr(typed, env, gen_target, a, &cleanup_point);

    void* result = pico_run_expr(gen_target, pi_size_of(*typed->ptype), a, &cleanup_point);

    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    release_executable_allocator(exalloc);
    return result;

 on_error:
    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    release_executable_allocator(exalloc);
    throw_error(point, cleanup_point.error_message);
}

void* eval_expr(Syntax* untyped, TypeEnv* env, Allocator* a, UVarGenerator* gen, ErrorPoint* point) {
    type_infer_i(untyped, env, gen, a, point);
    return eval_typed_expr(untyped, env, a, point);
}

// TODO (BUG LOGIC UB): evaluation may produce a function pointer (or an object
// with a function pointer) that points to generated code. This method currently
// provides no means to capture that assembly.
PiType* eval_type(Syntax* untyped, TypeEnv* env, Allocator* a, UVarGenerator* gen, ErrorPoint* point) {
    type_infer_i(untyped, env, gen, a, point);

    if (untyped->ptype->sort != TKind) {
        throw_error(point, mv_string("Value expected to be type, was not!"));
    }

    PiType** result = eval_typed_expr(untyped, env, a, point);
    untyped->type = SCheckedType;
    untyped->type_val = *result;

    return *result;
}
