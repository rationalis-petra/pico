#include "pico/analysis/typecheck.h"

#include "data/string.h"
#include "pretty/string_printer.h"

#include "pico/binding/environment.h"
#include "pico/binding/type_env.h"
#include "pico/analysis/unify.h"

// forward declarations
result type_check_expr(syntax* untyped, pi_type type, type_env* env, uvar_generator* gen, allocator a);
result type_infer_expr(syntax* untyped, type_env* env, uvar_generator* gen, allocator a);

// Check a toplevel expression

result type_check(toplevel* top, environment* env, allocator a) {
    // If this is a definition, lookup the type to check against 
    result out;
    type_env *t_env = mk_type_env(env, a);
    uvar_generator* gen = mk_gen(a);

    switch (top->type) {
    case TLDef: {
        pi_type* check_against;
        env_entry e = env_lookup(top->def.bind, env);
        syntax* term = top->def.value;

        if (e.success == Ok) {
            check_against = e.type;
        } else {
            check_against = mk_uvar(gen, a);
        }
        type_var(top->def.bind, check_against, t_env, a);
        out = type_check_expr(term, *check_against, t_env, gen, a);
        pop_type(t_env);
        break;
    }
    case TLExpr: {
        syntax* term = &top->expr;
        out = type_infer_expr(term, t_env, gen, a);
        break;
    }
    }

    // TODO: delete_type_env (ok for now as we arena allocate!)
    delete_gen(gen, a);
    return out;
}

// Forward declarations for implementation
result type_infer_i(syntax* untyped, type_env* env, uvar_generator* gen, allocator a);
result type_check_i(syntax* untyped, pi_type* type, type_env* env, uvar_generator* gen, allocator a);
result squash_types(syntax* untyped, allocator a);
result get_type(syntax* type, pi_type* out);

// -----------------------------------------------------------------------------
// Interface
// -----------------------------------------------------------------------------
result type_infer_expr(syntax* untyped, type_env* env, uvar_generator* gen, allocator a) {
    // Arena allocator to place types
    result impl = type_infer_i (untyped, env, gen, a);

    result out;
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
        result sqres = squash_types(untyped, a);
        if (sqres.type == Err) {
            out.type = Err;
            // The error message is in the arena allocator's memory, so we need to
            // copy it to grant extra persistance.
            out.error_message = copy_string(sqres.error_message, a);
        }
    }

    return out;
}

result type_check_expr(syntax* untyped, pi_type type, type_env* env, uvar_generator* gen, allocator a) {
    // TODO copy type, use in type_check_i
    result impl = type_check_i (untyped, &type, env, gen, a);

    result out;
    out.type = Ok;
    if (impl.type == Err) {
        out.type = Err;
        // The error message is in the arena allocator's memory, so we need to
        // copy it to grant extra persistance.
        out.error_message = copy_string(impl.error_message, a);
    }

    // now, squash all types
    if (out.type == Ok) {
        result sqres = squash_types(untyped, a);
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
// Implementation
// -----------------------------------------------------------------------------

result type_check_i(syntax* untyped, pi_type* type, type_env* env, uvar_generator* gen, allocator a) {
    result out;
    out = type_infer_i(untyped, env, gen, a);
    if (out.type == Err) return out;
    out = unify(type, untyped->ptype, a);
    return out;
}

// "internal" type inference. Destructively mutates types
result type_infer_i(syntax* untyped, type_env* env, uvar_generator* gen, allocator a) {
    result out;
    switch (untyped->type) {
    case SLitI64:
        out.type = Ok;
        untyped->ptype = mem_alloc(sizeof(pi_type),a);
        untyped->ptype->sort = TPrim; 
        untyped->ptype->prim = Int_64;
        break;
    case SLitBool:
        out.type = Ok;
        untyped->ptype = mem_alloc(sizeof(pi_type),a);
        untyped->ptype->sort = TPrim; 
        untyped->ptype->prim = Bool;
        break;
    case SType: {
        pi_type* ty = mem_alloc(sizeof(pi_type), a);
        ty->sort = TPrim;
        ty->prim = TType; 
        untyped->ptype = ty;
        out.type = Ok;
        break;
    }
    case SVariable: {
        type_entry te = type_env_lookup(untyped->variable, env);
        if (te.type == TELocal || te.type == TEGlobal) {
            untyped->ptype = te.ptype;
            out.type = Ok;
        } else {
            out.type = Err;
            string* sym = symbol_to_string(untyped->variable);
            string msg = mv_string("Couldn't find type of variable: ");
            out.error_message = string_cat(msg, *sym, a);
        }
        break;
    }
    case SProcedure: {
        // give each arg a unification variable type. 
        pi_type* proc_ty = mem_alloc(sizeof(pi_type), a);
        proc_ty->sort = TProc;
        proc_ty->proc.args = mk_ptr_array(untyped->procedure.args.len, a);
        untyped->ptype = proc_ty;

        for (size_t i = 0; i < untyped->procedure.args.len; i++) {
            pi_symbol arg = untyped->procedure.args.data[i];
            pi_type* aty = mk_uvar(gen, a);
            type_var(arg, aty, env, a);
            push_ptr(aty, &proc_ty->proc.args, a);
        }

        out = type_infer_i(untyped->procedure.body, env, gen, a); 
        pop_types(env, untyped->procedure.args.len);
        proc_ty->proc.ret = untyped->procedure.body->ptype;
        break;
    }
    case SApplication: {
        out = type_infer_i(untyped->application.function, env, gen, a);
        if (out.type == Err) return out;
        pi_type fn_type = *untyped->application.function->ptype;
        if (fn_type.sort == TUVar) {
            // fill in structure 
            pi_type* ret = mk_uvar(gen, a);
            ptr_array args = mk_ptr_array(16, a);
            for (size_t i = 0; i < untyped->application.args.len; i++) {
                push_ptr(mk_uvar(gen, a), &args, a);
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
                               (pi_type*)fn_type.proc.args.data[i],
                               env, gen, a);
            if (out.type == Err) return out;
        }

        untyped->ptype = fn_type.proc.ret;
        break;
    }
    case SConstructor: {
        // Typecheck variant
        out = type_infer_i (untyped->variant.enum_type, env, gen, a);
        if (out.type == Err) return out;

        // Typecheck is pretty simple: ensure that the tag is present in the
        // variant type
        if (untyped->variant.enum_type->type != SType) {
            return (result) {
                .type = Err,
                .error_message = mv_string("Variant must be from a type."),
            };
        }

        pi_type* enum_type = untyped->variant.enum_type->type_val;
        if (enum_type->sort != TEnum) {
            return (result) {
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
                ptr_array* args = enum_type->enumeration.variants.data[i].val;
                if (args->len != 0) {
                    return (result) {
                        .type = Err,
                        .error_message = mv_string("Incorrect number of args to variant constructor"),
                    };
                }
                untyped->ptype = enum_type;
                break;
            }
        }

        if (!found_variant) {
            return (result) {
                .type = Err,
                .error_message = mv_string("Could not find variant tag."),
            };
        }

        out.type = Ok;
        break;
    }
    case SVariant: {
        // Typecheck variant
        out = type_infer_i (untyped->variant.enum_type, env, gen, a);
        if (out.type == Err) return out;

        // Typecheck is pretty simple: ensure that the tag is present in the
        // variant type
        if (untyped->variant.enum_type->type != SType) {
            return (result) {
                .type = Err,
                .error_message = mv_string("Variant must be from a type."),
            };
        }

        pi_type* enum_type = untyped->variant.enum_type->type_val;
        if (enum_type->sort != TEnum) {
            return (result) {
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
                ptr_array* args = enum_type->enumeration.variants.data[i].val;
                if (args->len != untyped->variant.args.len) {
                    return (result) {
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
            return (result) {
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

        pi_type* enum_type = untyped->match.val->ptype;
        if (enum_type->sort != TEnum) {
            return (result) {
                .type = Err,
                .error_message = mv_string("Match expects value to have an enum type!"),
            };
        }

        // Typecheck each variant, ensure they are the same
        pi_type* out_ty = mk_uvar(gen, a);
        untyped->ptype = out_ty;
        u8_array used_indices = mk_u8_array(untyped->match.clauses.len, a);
        for (size_t i = 0; i < untyped->match.clauses.len; i++)
            push_u8(0, &used_indices, a);

        for (size_t i = 0; i < untyped->match.clauses.len; i++) {
            syn_clause* clause = untyped->match.clauses.data[i];
            bool found_tag = false;
            for (size_t j = 0; j < enum_type->enumeration.variants.len; j++) {
                if (clause->tagname == enum_type->enumeration.variants.data[j].key) {
                    found_tag = true;
                    clause->tag = j;
                    used_indices.data[j] = 1;
                }
            }

            if (!found_tag) {
                return (result) {
                    .type = Err,
                    .error_message = mv_string("Unable to find variant tag in match"),
                };
            }

            // Now we've found the tag, typecheck the body
            ptr_array* types_to_bind = enum_type->enumeration.variants.data[i].val;
            if (types_to_bind->len != clause->vars.len) {
                return (result) {
                    .type = Err,
                    .error_message = mv_string("Bad number of binds!"),
                };
            }

            for (size_t j = 0; j < types_to_bind->len; j++) {
                pi_symbol arg = clause->vars.data[j];
                pi_type* aty = types_to_bind->data[j];
                type_var(arg, aty, env, a);
            }

            out = type_check_i(clause->body, out_ty, env, gen, a);
            pop_types(env, types_to_bind->len);
            if (out.type == Err) return out;
        }

        // Finally, check that all indices are accounted for;
        bool all_indices = true;
        for (size_t i = 0; i < used_indices.len; i++) all_indices &= used_indices.data[i];
        
        if (!all_indices) {
            return (result) {
                .type = Err,
                .error_message = mv_string("Not all enumerations used in match expression"),
            };
        }

        return out;
        break;
    }
    case SStructure: {
        pi_type* ty = mem_alloc(sizeof(pi_type), a);
        untyped->ptype = ty; 
        out = get_type(untyped->structure.ptype, ty);
        if (out.type == Err) return out;

        if (untyped->structure.fields.len != ty->structure.fields.len) {
            out.type = Err;
            out.error_message = mv_string("Structure must have exactly n fields.");
            return out;
        }

        for (size_t i = 0; i < ty->structure.fields.len; i++) {
            syntax** field_syn = (syntax**)sym_ptr_lookup(ty->structure.fields.data[i].key, untyped->structure.fields);
            if (field_syn) {
                pi_type* field_ty = ty->structure.fields.data[i].val;
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
        pi_type struct_type = *untyped->projector.val->ptype;
        if (struct_type.sort != TStruct) {
            return (result) {
                .type = Err,
                .error_message = mv_string("Projection only works on structs."),
            };
        }

        // search for field
        pi_type* ret_ty = NULL;
        for (size_t i = 0; i < struct_type.structure.fields.len; i++) {
            if (struct_type.structure.fields.data[i].key == untyped->projector.field) {
                ret_ty = struct_type.structure.fields.data[i].val;
            }
        }
        if (ret_ty == NULL) {
            return (result) {
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
        pi_type* t = mem_alloc(sizeof(pi_type), a);
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
    default:
        out.type = Err;
        out.error_message = mk_string("Internal Error: unrecognized syntactic form", a);
        break;
    }
    return out;
}

result squash_types(syntax* typed, allocator a) {
    result out;
    switch (typed->type) {
    case SLitI64:
    case SLitBool:
    case SType:
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
            syn_clause* clause = (syn_clause*)typed->match.clauses.data[i];
            out = squash_types(clause->body, a);
            if (out.type == Err) return out;
        }
        break;
    }
    case SStructure: {
        // Loop for each element in the 
        for (size_t i = 0; i < typed->structure.fields.len; i++) {
            syntax* syn = typed->structure.fields.data[i].val;
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
    default:
        out.type = Err;
        out.error_message = mk_string("Internal Error: unrecognized syntactic form", a);
        break;
    }

    if (out.type == Ok) {
        if (!has_unification_vars_p(*typed->ptype)) {
            squash_type(typed->ptype);
        }
        else {
            out.type = Err;
            squash_type(typed->ptype);
            document* doc = pretty_type(typed->ptype, a);
            string str = doc_to_str(doc, a);
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

result get_type(syntax* type, pi_type* out) {
    result ret;
    if (type->type == SType) {
        ret.type = Ok;
        *out = *type->type_val;
    } else {
        ret.type = Err;
        ret.error_message = mv_string("Typechecking error: expecting node to be a type.");
    }
    return ret;
}
