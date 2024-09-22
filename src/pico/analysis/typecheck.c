#include "pico/analysis/typecheck.h"

#include "data/string.h"
#include "pretty/string_printer.h"

#include "pico/binding/environment.h"
#include "pico/binding/type_env.h"
#include "pico/analysis/unify.h"

// forward declarations
Result type_check_expr(Syntax* untyped, PiType type, TypeEnv* env, UVarGenerator* gen, Allocator* a);
Result type_infer_expr(Syntax* untyped, TypeEnv* env, UVarGenerator* gen, Allocator* a);

// Check a toplevel expression
Result type_check(TopLevel* top, Environment* env, Allocator* a) {
    // If this is a definition, lookup the type to check against 
    Result out;
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
        out = type_check_expr(term, *check_against, t_env, gen, a);
        pop_type(t_env);
        break;
    }
    case TLExpr: {
        Syntax* term = &top->expr;
        out = type_infer_expr(term, t_env, gen, a);
        break;
    }
    }

    // TODO: delete_type_env (ok for now as we arena allocate!)
    delete_gen(gen, a);
    return out;
}

// Forward declarations for implementation (internal declarations)
Result type_infer_i(Syntax* untyped, TypeEnv* env, UVarGenerator* gen, Allocator* a);
Result type_check_i(Syntax* untyped, PiType* type, TypeEnv* env, UVarGenerator* gen, Allocator* a);
Result eval_type(Syntax* untyped, TypeEnv* env, Allocator* a);
Result squash_types(Syntax* untyped, Allocator* a);
PiType* get_head(PiType* type, PiType_t expected_sort);
PiType* reduce_type(PiType* type, Allocator* a);

// -----------------------------------------------------------------------------
// Interface
// -----------------------------------------------------------------------------
Result type_infer_expr(Syntax* untyped, TypeEnv* env, UVarGenerator* gen, Allocator* a) {
    // Arena allocator to place types
    Result impl = type_infer_i (untyped, env, gen, a);

    Result out;
    if (impl.type == Ok) {
        out.type = Ok;
    } else {
        out.type = Err;
        // The error message is in the arena allocator's memory, so we need to
        // copy it to grand extra persistance.
        out.error_message = copy_string(impl.error_message, a);
    }

    // now, squash all types
    if (out.type == Ok) {
        Result sqres = squash_types(untyped, a);
        if (sqres.type == Err) {
            out.type = Err;
            // The error message is in the arena allocator's memory, so we need to
            // copy it to grant extra persistance.
            out.error_message = copy_string(sqres.error_message, a);
        }
    }

    return out;
}

Result type_check_expr(Syntax* untyped, PiType type, TypeEnv* env, UVarGenerator* gen, Allocator* a) {
    // TODO copy type, use in type_check_i
    Result impl = type_check_i (untyped, &type, env, gen, a);

    Result out;
    out.type = Ok;
    if (impl.type == Err) {
        out.type = Err;
        // The error message is in the arena allocator's memory, so we need to
        // copy it to grant extra persistance.
        out.error_message = copy_string(impl.error_message, a);
    }

    // Now, squash all types
    if (out.type == Ok) {
        Result sqres = squash_types(untyped, a);
        if (sqres.type == Err) {
            out.type = Err;
            // The error message is in the arena allocator's memory, so we need to
            // copy it to grant extra persistance.
            out.error_message = copy_string(sqres.error_message, a);
        }
    }

    return out;
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

Result type_check_i(Syntax* untyped, PiType* type, TypeEnv* env, UVarGenerator* gen, Allocator* a) {
    Result out;
    out = type_infer_i(untyped, env, gen, a);
    if (out.type == Err) return out;
    out = unify(type, untyped->ptype, a);
    return out;
}

// "internal" type inference. Destructively mutates types
Result type_infer_i(Syntax* untyped, TypeEnv* env, UVarGenerator* gen, Allocator* a) {
    Result out;
    switch (untyped->type) {
    case SLitI64:
        out.type = Ok;
        untyped->ptype = mem_alloc(sizeof(PiType),a);
        untyped->ptype->sort = TPrim; 
        untyped->ptype->prim = Int_64;
        break;
    case SLitBool:
        out.type = Ok;
        untyped->ptype = mem_alloc(sizeof(PiType),a);
        untyped->ptype->sort = TPrim; 
        untyped->ptype->prim = Bool;
        break;
    case SVariable: {
        TypeEntry te = type_env_lookup(untyped->variable, env);
        if (te.type == TELocal || te.type == TEGlobal) {
            out.type = Ok;
            untyped->ptype = te.ptype;
            if (te.value) {
                untyped->type = SCheckedType;
                untyped->type_val = te.value;
            }
        } else {
            out.type = Err;
            String* sym = symbol_to_string(untyped->variable);
            String msg = mv_string("Couldn't find type of variable: ");
            out.error_message = string_cat(msg, *sym, a);
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
            Symbol arg = untyped->procedure.args.data[i];
            PiType* aty = mk_uvar(gen, a);
            type_var(arg, aty, env);
            push_ptr(aty, &proc_ty->proc.args);
        }

        out = type_infer_i(untyped->procedure.body, env, gen, a); 
        pop_types(env, untyped->procedure.args.len);
        proc_ty->proc.ret = untyped->procedure.body->ptype;
        break;
    }
    case SApplication: {
        out = type_infer_i(untyped->application.function, env, gen, a);
        if (out.type == Err) return out;
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
            out.type = Err;
            out.error_message = mk_string("Expected LHS of application to be a proc", a);
            return out;
        }
        
        if (fn_type.proc.args.len != untyped->application.args.len) {
            out.type = Err;
            out.error_message = mk_string("Incorrect number of function arguments", a);
            return out;
        }

        for (size_t i = 0; i < fn_type.proc.args.len; i++) {
            out = type_check_i(untyped->application.args.data[i],
                               (PiType*)fn_type.proc.args.data[i],
                               env, gen, a);
            if (out.type == Err) return out;
        }

        untyped->ptype = fn_type.proc.ret;
        break;
    }
    case SConstructor: {
        // Typecheck variant
        out = eval_type (untyped->variant.enum_type, env, a);
        if (out.type == Err) return out;

        PiType* enum_type = untyped->variant.enum_type->type_val;
        if (enum_type->sort != TEnum) {
            return (Result) {
                .type = Err,
                .error_message = mv_string("Variant must be of enum type."),
            };
        }

        bool found_variant = false;
        for (size_t i = 0; i < enum_type->enumeration.variants.len; i++) {
            if (enum_type->enumeration.variants.data[i].key == untyped->variant.tagname) {
                untyped->variant.tag = i;
                found_variant = true;

                // Generate variant has no args
                PtrArray* args = enum_type->enumeration.variants.data[i].val;
                if (args->len != 0) {
                    return (Result) {
                        .type = Err,
                        .error_message = mv_string("Incorrect number of args to variant constructor"),
                    };
                }
                untyped->ptype = enum_type;
                break;
            }
        }

        if (!found_variant) {
            return (Result) {
                .type = Err,
                .error_message = mv_string("Could not find variant tag."),
            };
        }

        out.type = Ok;
        break;
    }
    case SVariant: {
        // Typecheck variant
        out = eval_type(untyped->variant.enum_type, env, a);
        if (out.type == Err) return out;

        // Typecheck is pretty simple: ensure that the tag is present in the

        PiType* enum_type = untyped->variant.enum_type->type_val;
        if (enum_type->sort != TEnum) {
            return (Result) {
                .type = Err,
                .error_message = mv_string("Variant must be of enum type."),
            };
        }

        bool found_variant = false;
        for (size_t i = 0; i < enum_type->enumeration.variants.len; i++) {
            if (enum_type->enumeration.variants.data[i].key == untyped->variant.tagname) {
                untyped->variant.tag = i;
                found_variant = true;

                // Generate variant has no args
                PtrArray* args = enum_type->enumeration.variants.data[i].val;
                if (args->len != untyped->variant.args.len) {
                    return (Result) {
                        .type = Err,
                        .error_message = mv_string("Incorrect number of args to variant constructor"),
                    };
                }

                for (size_t i = 0; i < args->len; i++) {
                    out = type_check_i(untyped->variant.args.data[i], args->data[i], env, gen, a);
                    if (out.type == Err) return out;
                }
                
                untyped->ptype = enum_type;
                break;
            }
        }

        if (!found_variant) {
            return (Result) {
                .type = Err,
                .error_message = mv_string("Could not find variant tag."),
            };
        }

        out.type = Ok;
        break;
    }
    case SMatch: {
        // Typecheck the input 
        out = type_infer_i(untyped->match.val, env, gen, a);
        if (out.type == Err) return out;

        PiType* enum_type = untyped->match.val->ptype;
        if (enum_type->sort != TEnum) {
            return (Result) {
                .type = Err,
                .error_message = mv_string("Match expects value to have an enum type!"),
            };
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
                return (Result) {
                    .type = Err,
                    .error_message = mv_string("Unable to find variant tag in match"),
                };
            }

            // Now we've found the tag, typecheck the body
            PtrArray* types_to_bind = enum_type->enumeration.variants.data[i].val;
            if (types_to_bind->len != clause->vars.len) {
                return (Result) {
                    .type = Err,
                    .error_message = mv_string("Bad number of binds!"),
                };
            }

            for (size_t j = 0; j < types_to_bind->len; j++) {
                Symbol arg = clause->vars.data[j];
                PiType* aty = types_to_bind->data[j];
                type_var(arg, aty, env);
            }

            out = type_check_i(clause->body, out_ty, env, gen, a);
            pop_types(env, types_to_bind->len);
            if (out.type == Err) return out;
        }

        // Finally, check that all indices are accounted for;
        bool all_indices = true;
        for (size_t i = 0; i < used_indices.len; i++) all_indices &= used_indices.data[i];
        
        if (!all_indices) {
            return (Result) {
                .type = Err,
                .error_message = mv_string("Not all enumerations used in match expression"),
            };
        }

        return out;
        break;
    }
    case SStructure: {
        PiType* ty = mem_alloc(sizeof(PiType), a);
        untyped->ptype = ty; 
        out = eval_type(untyped->structure.ptype, env, a);
        if (out.type == Err) return out;
        *ty = *untyped->structure.ptype->type_val;

        if (ty->sort != TStruct) {
            return (Result) {
                .type = Err,
                .error_message = mv_string("Structure type invalid"),
            };
        }

        if (untyped->structure.fields.len != ty->structure.fields.len) {
            out.type = Err;
            out.error_message = mv_string("Structure must have exactly n fields.");
            return out;
        }

        for (size_t i = 0; i < ty->structure.fields.len; i++) {
            Syntax** field_syn = (Syntax**)sym_ptr_lookup(ty->structure.fields.data[i].key, untyped->structure.fields);
            if (field_syn) {
                PiType* field_ty = ty->structure.fields.data[i].val;
                out = type_check_i(*field_syn, field_ty, env, gen, a);
                if (out.type == Err) return out;
            } else {
                out.type = Err;
                out.error_message = mv_string("Structure is missing a field");
                return out;
            }
        }
        out.type = Ok;
        break;
    }
    case SProjector: {
        out = type_infer_i(untyped->projector.val, env, gen, a);
        if (out.type == Err) return out;
        PiType struct_type = *untyped->projector.val->ptype;
        if (struct_type.sort != TStruct) {
            return (Result) {
                .type = Err,
                .error_message = mv_string("Projection only works on structs."),
            };
        }

        // search for field
        PiType* ret_ty = NULL;
        for (size_t i = 0; i < struct_type.structure.fields.len; i++) {
            if (struct_type.structure.fields.data[i].key == untyped->projector.field) {
                ret_ty = struct_type.structure.fields.data[i].val;
            }
        }
        if (ret_ty == NULL) {
            return (Result) {
                .type = Err,
                .error_message = mv_string("Projection only works on structs."),
            };
        }
        untyped->ptype = ret_ty;
        break;
    }
    case SLet:
        out.type = Err;
        out.error_message = mv_string("Type inference not implemented for this syntactic form");
        break;
    case SIf: {
        PiType* t = mem_alloc(sizeof(PiType), a);
        t->sort = TPrim;
        t->prim = Bool;
        out = type_check_i(untyped->if_expr.condition,
                           t,
                           env, gen, a);
        if (out.type == Err) return out;

        out = type_infer_i(untyped->if_expr.true_branch, env, gen, a);

        if (out.type == Err) return out;
        out = type_check_i(untyped->if_expr.false_branch,
                           untyped->if_expr.true_branch->ptype,
                           env, gen, a);
        untyped->ptype = untyped->if_expr.false_branch->ptype;
        break;
    }
    case SProcType:
    case SStructType:
    case SEnumType:
        out = eval_type(untyped, env, a);
        break;
    default:
        out.type = Err;
        out.error_message = mk_string("Internal Error: unrecognized syntactic form (type_infer_i)", a);
        break;
    }
    return out;
}

Result squash_types(Syntax* typed, Allocator* a) {
    Result out;
    switch (typed->type) {
    case SLitI64:
    case SLitBool:
    case SVariable:
        out.type = Ok;
        break;
    case SProcedure: {
        // squash body
        out = squash_types(typed->procedure.body, a);
        break;
    }
    case SApplication: {
        out = squash_types(typed->application.function, a);
        if (out.type == Err) return out;
        
        for (size_t i = 0; i < typed->procedure.args.len; i++) {
            out = squash_types(typed->application.args.data[i], a);
            if (out.type == Err) return out;
        }
        break;
    }
    case SConstructor: {
        out = squash_types(typed->variant.enum_type, a);
        break;
    }
    case SVariant: {
        out = squash_types(typed->variant.enum_type, a);
        if (out.type == Err) return out;
        
        for (size_t i = 0; i < typed->variant.args.len; i++) {
            out = squash_types(typed->variant.args.data[i], a);
            if (out.type == Err) return out;
        }
        break;
    }
    case SMatch: {
        out = squash_types(typed->match.val, a);
        if (out.type == Err) return out;

        for (size_t i = 0; i < typed->match.clauses.len; i++) {
            SynClause* clause = (SynClause*)typed->match.clauses.data[i];
            out = squash_types(clause->body, a);
            if (out.type == Err) return out;
        }
        break;
    }
    case SStructure: {
        // Loop for each element in the 
        for (size_t i = 0; i < typed->structure.fields.len; i++) {
            Syntax* syn = typed->structure.fields.data[i].val;
            out = squash_types(syn, a);
            if (out.type == Err) return out;
        }
        break;
    }
    case SProjector:
        squash_types(typed->projector.val, a);
        break;
        
    case SLet:
        out.type = Err;
        out.error_message = mk_string("squash_types not implemented for let", a);
        break;
    case SIf: {
        out = squash_types(typed->if_expr.condition, a);
        if (out.type == Err) return out;
        out = squash_types(typed->if_expr.true_branch, a);
        if (out.type == Err) return out;
        out = squash_types(typed->if_expr.false_branch, a);
        break;
    }
    case SCheckedType:
        squash_type(typed->type_val);
        break;
    default:
        out.type = Err;
        out.error_message = mk_string("Internal Error: unrecognized syntactic form (squash_types)", a);
        break;
    }

    if (out.type == Ok) {
        if (!has_unification_vars_p(*typed->ptype)) {
            squash_type(typed->ptype);
        }
        else {
            out.type = Err;
            squash_type(typed->ptype);
            Document* doc = pretty_type(typed->ptype, a);
            String str = doc_to_str(doc, a);
            out.error_message =
                string_cat(
                           string_cat(mv_string("Typechecking error: not all unification vars were instantiated. Term:\n"), 
                                      str, a),
                           string_cat(mv_string("\nType:\n"),
                                      doc_to_str(pretty_type(typed->ptype, a), a),
                                      a),
                           a);
            delete_string(str, a);
        }
    }
    return out;
}

Result eval_type(Syntax* untyped, TypeEnv* env, Allocator* a) {
    Result out;

    switch (untyped->type) {
    case SVariable: {
        TypeEntry e = type_env_lookup(untyped->variable, env);
        if (e.value) {
            untyped->type = SCheckedType;
            untyped->type_val = e.value;
            out.type = Ok;
        } else {
            out = (Result) {
                .type = Err,
                .error_message = mv_string("Variable expected to be type, was not!"),
            };
        }
        break;
    }

    // Types & Type formers
    case SProcType: {
        PtrArray args = mk_ptr_array(untyped->proc_type.args.len, a);
        for (size_t i = 0; i < untyped->proc_type.args.len; i++) {
            Syntax* syn = untyped->proc_type.args.data[i];
            out = eval_type(syn, env, a);
            if (out.type == Err) return out;
            push_ptr(syn->type_val, &args);
        }
        Syntax* ret = untyped->proc_type.return_type;
        out = eval_type(untyped->proc_type.return_type, env, a);
        if (out.type == Err) return out;
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
            out = eval_type(syn, env, a);
            if (out.type == Err) return out;
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
                out = eval_type(syn, env, a);
                if (out.type == Err) return out;
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
        out = (Result) {
            .type = Err,
            .error_message = mv_string("Expected a type former - got a term former"),
        };
    };

    untyped->type = SCheckedType;
    PiType* kind = mem_alloc(sizeof(PiType), a);
    *kind = (PiType) {
        .sort = TKind,
        .kind.nargs = 0,
    };
    untyped->ptype = kind;

    return out;
}
