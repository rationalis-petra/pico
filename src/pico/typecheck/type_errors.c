#include "components/pretty/standard_types.h"

#include "pico/typecheck/type_errors.h"


// ---------------------------------------------------------------------- 
//
//                              Typechecking  
//
// ----------------------------------------------------------------------

// After unification, checkif typecheck was successful
void check_result_out(UnifyResult out, Range range, UnifyReason reason, Allocator* a, PiErrorPoint* point) {
    if (out.type == USimpleError) {
        PtrArray errs = mk_ptr_array(3, a);
        PicoError* err;

        if (reason.type == URCheck) {
            UReasonCheck check = reason.check;

            PtrArray nodes = mk_ptr_array(4, a);
            push_ptr(mv_cstr_doc("A typechecking error occurred here - the term does not have type", a), &nodes);
            push_ptr(pretty_type(check.expected, default_ptp, a), &nodes);
            push_ptr(mv_cstr_doc("But instead has type", a), &nodes);
            push_ptr(pretty_type(check.actual, default_ptp, a), &nodes);

            PicoError* err = mem_alloc(sizeof(PicoError), a);
            push_ptr(err, &errs);
            *err = (PicoError) {
                .range = range,
                .message = mv_sep_doc(nodes, a),
            };
        }


        err = mem_alloc(sizeof(PicoError), a);
        push_ptr(err, &errs);
        *err = (PicoError) {
            .range = reason.type == URCheck ? (Range){} : range,
            .message = out.message, 
        };

        throw_pi_errors(point, errs);
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

// Variables
_Noreturn void type_error_unexpected_module(SynRef syn, Module* module, TypeCheckContext ctx) {
  PicoError err = {
    .message = mv_cstr_doc(
        "This variable refers to a module. Please note that modules cannot be \n"
        "used as values directly in code, but must instead be used only to \n"
        "access their members, e.g. u64.to-string or platform.terminal.write-string \n",
        ctx.a),
    .range = get_range(syn, ctx.tape).term,
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_unknown_var(SynRef ref, TypeCheckContext ctx) {
    // TODO (Improvement): search the environment for symbols with a similar name
    Syntax syn = get_syntax(ref, ctx.tape);
    PtrArray nodes = mk_ptr_array(3, ctx.a);
    push_ptr(mv_cstr_doc("The variable", ctx.a), &nodes);
    push_ptr(mk_paren_doc("'", "'", mk_str_doc(view_symbol_string(syn.variable), ctx.a), ctx.a), &nodes);
    push_ptr(mv_cstr_doc("is not bound anywhere, and so cannot be used here.", ctx.a), &nodes);

    PicoError err = {
        .range = get_range(ref, ctx.tape).term,
        .message = mv_sep_doc(nodes, ctx.a),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_invalid_declaration(Symbol type, SynRef arg, TypeCheckContext ctx) {
    PtrArray nodes = mk_ptr_array(3, ctx.a);
    push_ptr(mv_cstr_doc("The declaration", ctx.a), &nodes);
    push_ptr(mk_paren_doc("'", "'", mk_str_doc(view_symbol_string(type), ctx.a), ctx.a), &nodes);
    push_ptr(mv_cstr_doc("is not a recognized declaration type. Valid declarations are 'type' and 'inline'.", ctx.a), &nodes);

    PicoError err = {
        .range = get_range(arg, ctx.tape).term,
        .message = mv_sep_doc(nodes, ctx.a),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_invalid_import(ImportClause clause, Range range, TypeCheckContext ctx) {
    PtrArray nodes = mk_ptr_array(3, ctx.a);
    push_ptr(mv_cstr_doc("The import clause", ctx.a), &nodes);
    push_ptr(mk_paren_doc("'", "'", pretty_import_clause(clause, ctx.a), ctx.a), &nodes);
    push_ptr(mv_cstr_doc("is malformed. This means that in the 'path', either "
                         "a module being referred to does not exist, or a "
                         "value that is not a module is being used as one."
                         , ctx.a), &nodes);

    PicoError err = {
        .range = range,
        .message = mv_sep_doc(nodes, ctx.a),
    };
    throw_pi_error(ctx.point, err);
}

// Proceudre
_Noreturn void type_error_expecting_instance_arg(size_t implicit_idx, SynRef ref, TypeCheckContext ctx) {
    Syntax proc = get_syntax(ref, ctx.tape);
    SymPtrCell arg = proc.procedure.implicits.data[implicit_idx];

    PtrArray nodes = mk_ptr_array(4, ctx.a);
    push_ptr(mv_cstr_doc("The argument", ctx.a), &nodes);
    push_ptr(mk_paren_doc("'", "'", 
                          mv_str_doc(view_symbol_string(arg.key), ctx.a), ctx.a), &nodes);
    push_ptr(mv_cstr_doc("is beign used as an instance argument, i.e. between '{' and '}'."
                         " As such, it is expected to have a trait (instance) type, but"
                         " it instead has type:" , ctx.a), &nodes);
    push_ptr(mv_nest_doc(2, pretty_type(get_type(ref, ctx.tape), default_ptp, ctx.a), ctx.a), &nodes);

    PicoError err = {
        .range = get_range(ref, ctx.tape).term,
        .message = mv_sep_doc(nodes, ctx.a),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_proc_incorrect_num_implicits(SynRef ref,
                                                       PiType *type,
                                                       TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    PtrArray nodes = mk_ptr_array(4, a);
    Syntax proc = get_syntax(ref, ctx.tape);

    push_ptr(mv_cstr_doc("The procedure here was previously inferred or declared to have", a), &nodes);
    push_ptr(pretty_u64(type->proc.implicits.len, a), &nodes);
    push_ptr(mv_cstr_doc("implicit arguments, but it actually has", a), &nodes);
    push_ptr(pretty_u64(proc.procedure.implicits.len, a), &nodes);

    PicoError err = {
        .range = get_range(ref, ctx.tape).term,
        .message = mv_hsep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}
_Noreturn void type_error_proc_incorrect_num_args(SynRef ref, PiType *type,
                                                  TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    PtrArray nodes = mk_ptr_array(4, a);
    Syntax proc = get_syntax(ref, ctx.tape);

    push_ptr(mv_cstr_doc("The procedure here was previously inferred or declared to have", a), &nodes);
    push_ptr(pretty_u64(type->proc.args.len, a), &nodes);
    push_ptr(mv_cstr_doc("arguments, but it actually has", a), &nodes);
    push_ptr(pretty_u64(proc.procedure.args.len, a), &nodes);

    PicoError err = {
        .range = get_range(ref, ctx.tape).term,
        .message = mv_hsep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

// Application
//---------------------------------------------------------------------- 

_Noreturn void type_error_incorrect_num_args(PiType* type, SynRef ref, InvalidArgType args_type, TypeCheckContext ctx) {
    Syntax app = get_syntax(ref, ctx.tape);
    Allocator* a = ctx.a;
    PtrArray err_nodes = mk_ptr_array(4, a);
    switch (args_type) {
    case InvTypes:
        push_ptr(mv_cstr_doc("Incorrect number of types applied to all - expected", a), &err_nodes);
        break;
    case InvImplicits:
        push_ptr(mv_cstr_doc("Incorrect number of implicits applied to proc - expected", a), &err_nodes);
        break;
    case InvValues:
        if (type->sort == TKind) {
            push_ptr(mv_cstr_doc("Incorrect number of types applied to Family - expected", a), &err_nodes);
        } else {
            push_ptr(mv_cstr_doc("Incorrect number of arguments applied to proc - expected", a), &err_nodes);
        }
        break;
    }

    switch (args_type) {
    case InvTypes:
        push_ptr(pretty_u64(type->binder.vars.len, a), &err_nodes);
        break;
    case InvImplicits:
        if (type->sort == TAll) {
            push_ptr(pretty_u64(type->binder.body->proc.implicits.len, a), &err_nodes);
        } else {
            push_ptr(pretty_u64(type->proc.implicits.len, a), &err_nodes);
        }
        break;
    case InvValues:
        if (type->sort == TAll) {
            push_ptr(pretty_u64(type->binder.body->proc.args.len, a), &err_nodes);
        } else if (type->sort == TKind) {
            push_ptr(pretty_u64(type->binder.vars.len, a), &err_nodes);
        } else {
            push_ptr(pretty_u64(type->proc.args.len, a), &err_nodes);
        }
        break;
    }
    push_ptr(mv_cstr_doc("but got", a), &err_nodes);

    PtrArray cat_nodes = mk_ptr_array(2, a);
    push_ptr(pretty_u64(app.application.args.len, a), &cat_nodes);
    push_ptr(mv_cstr_doc(".", a), &cat_nodes);
    push_ptr(mv_cat_doc(cat_nodes, a), &err_nodes);

    PtrArray supplement_nodes = mk_ptr_array(4, a);
    if (type->sort != TKind) {
        push_ptr(mv_cstr_doc("The function being applied has type:", a), &supplement_nodes);
    } else {
        push_ptr(mv_cstr_doc("The type being applied has kind:", a), &supplement_nodes);
    }
    push_ptr(mv_nest_doc(2, pretty_type(type, default_ptp, a), a), &supplement_nodes);

    PtrArray nodes = mk_ptr_array(2, a);
    push_ptr(mv_hsep_doc(err_nodes, a), &nodes);
    push_ptr(mv_hsep_doc(supplement_nodes, a), &nodes);

    PicoError err = {
        .range = get_range(ref, ctx.tape).term,
        .message = mv_vsep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_invalid_application_target(PiType *type, SynRef ref, TypeCheckContext ctx) {
    Syntax app = get_syntax(ref, ctx.tape);
    bool is_all = app.type == SAllApplication;

    Allocator* a = ctx.a;
    PtrArray nodes = mk_ptr_array(3, a);
    if (is_all) {
        push_ptr(mv_str_doc(mv_string("Attempting to apply this value to some types and arguments. However, it has type: "), a), &nodes);
    } else {
        push_ptr(mv_str_doc(mv_string("Attempting to apply this value to some arguments. However, it has type: "), a), &nodes);
    }
    push_ptr(mv_nest_doc(2, pretty_type(type, default_ptp, a), a), &nodes);
    if (is_all) {
        push_ptr(mv_str_doc(mv_string("which cannot be used this way. Only 'all' terms can be applied in this context."), a), &nodes);
    } else {
        push_ptr(mv_str_doc(mv_string("which cannot be used this way. Only 'proc', 'all' and 'Family' terms can be applied in this context."), a), &nodes);
    }
    PicoError err = {
        .range = is_all ? get_range(app.all_application.function, ctx.tape).term : get_range(app.application.function, ctx.tape).term,
        .message = mv_sep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_incorrect_num_args_all_noproc(PiType *type, SynRef ref, bool is_implicit_args, TypeCheckContext ctx) {
    Syntax app = get_syntax(ref, ctx.tape);
    Allocator* a = ctx.a;
    PtrArray nodes = mk_ptr_array(4, a);
    if (is_implicit_args) {
        push_ptr(mk_cstr_doc("Incorrect number of implicit arguments to all function - expected: 0, got: ", a), &nodes);
    } else {
        push_ptr(mk_cstr_doc("Incorrect number of arguments to all function - expected: 0, got: ", a), &nodes);
    }
    size_t len = is_implicit_args ? app.all_application.implicits.len : app.all_application.args.len;
    push_ptr(pretty_u64(len, a), &nodes);
    push_ptr(mk_cstr_doc("Note: the function being applied has type: ", a), &nodes);
    push_ptr(pretty_type(type, default_ptp, a), &nodes);

    PicoError err = {
        .range = get_range(app.all_application.function, ctx.tape).term,
        .message = mv_sep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_all_app_couldnt_deduce_types(size_t arg_idx, SynRef ref, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    Syntax app = get_syntax(ref, ctx.tape);
    PiType* fn_ty = get_type(app.all_application.function, ctx.tape);

    PtrArray nodes = mk_ptr_array(4, a);
    push_ptr(mv_cstr_doc("Typechecking error: When applying an all with type", a), &nodes);
    push_ptr(pretty_type(fn_ty, default_ptp, a), &nodes);
    push_ptr(mv_cstr_doc("not all types were able to be deduced. In particular, the type of ", a), &nodes);
    push_ptr(mk_paren_doc("'", "'", mv_str_doc(view_symbol_string(fn_ty->binder.vars.data[arg_idx]), a), a), &nodes);
    push_ptr(mv_cstr_doc("is ambiguous.", a), &nodes);

    PicoError err = {
        .range = get_range(app.all_application.function, ctx.tape).term,
        .message = mv_hsep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_app_not_family(PiType* type, SynRef ref, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    Syntax app = get_syntax(ref, ctx.tape);
    PtrArray nodes = mk_ptr_array(4, a);
    push_ptr(mk_cstr_doc("Attempting to apply the type:", a), &nodes);
    push_ptr(pretty_type(type, default_ptp, a), &nodes);
    push_ptr(mk_cstr_doc("As a type family of 0 arguments, but it is not a type family.", a), &nodes);

    PicoError err = {
        .range = get_range(app.application.function, ctx.tape).term,
        .message = mv_sep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}


//---------------------------------------------------------------------- 
//
// Sealing
//
//---------------------------------------------------------------------- 

_Noreturn void type_error_invalid_seal_type(PiType *type, SynRef seal, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    PtrArray nodes = mk_ptr_array(4, a);
    push_ptr(mv_cstr_doc("A seal is being constructed with type:", a), &nodes);
    push_ptr(mv_nest_doc(2, pretty_type(type, default_ptp, a), a), &nodes);
    push_ptr(mv_cstr_doc("However, only 'Sealed' types are allowed here.", a), &nodes);

    PicoError err = {
        .range = get_range(seal, ctx.tape).term,
        .message = mv_sep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_incorrect_num_seal_args(PiType* type, SynRef ref, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    Syntax seal = get_syntax(ref, ctx.tape);
    PtrArray nodes = mk_ptr_array(6, a);
    push_ptr(mv_cstr_doc("Attempting to seal away ", a), &nodes);
    push_ptr(pretty_u64(seal.seal.types.len, a), &nodes);
    push_ptr(mv_cstr_doc("types, however, the seal type was expecting ", a), &nodes);
    push_ptr(pretty_u64(type->sealed.vars.len, a), &nodes);
    push_ptr(mv_cstr_doc(" The type being sealed against is:", a), &nodes);
    push_ptr(pretty_type(type, default_ptp, a), &nodes);

    PicoError err = {
        .range = get_range(ref, ctx.tape).term,
        .message = mv_hsep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_invalid_unseal_type(PiType *type, SynRef ref, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    Syntax unseal = get_syntax(ref, ctx.tape);
    PtrArray nodes = mk_ptr_array(4, a);
    push_ptr(mv_cstr_doc("An unseal performed, however, the value being unsealed has type:", a), &nodes);
    push_ptr(mv_nest_doc(2, pretty_type(type, default_ptp, a), a), &nodes);
    push_ptr(mv_cstr_doc("Only 'Sealed' types are allowed here.", a), &nodes);

    PicoError err = {
        .range = get_range(unseal.unseal.sealed, ctx.tape).term,
        .message = mv_sep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_incorrect_num_unseal_binds(PiType* type, SynRef ref, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    Syntax unseal = get_syntax(ref, ctx.tape);
    PtrArray nodes = mk_ptr_array(6, a);
    push_ptr(mv_cstr_doc("Attempting to unseal and bind", a), &nodes);
    push_ptr(pretty_u64(unseal.unseal.types.len, a), &nodes);
    push_ptr(mv_cstr_doc("types, however, the seal type was expecting ", a), &nodes);
    push_ptr(pretty_u64(type->sealed.vars.len, a), &nodes);
    push_ptr(mv_cstr_doc("The type being unsealed is:", a), &nodes);
    push_ptr(mv_nest_doc(2, pretty_type(type, default_ptp, a), a), &nodes);

    PicoError err = {
        .range = get_range(ref, ctx.tape).term,
        .message = mv_hsep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_invalid_variant_type(PiType *type, SynRef variant, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    PtrArray nodes = mk_ptr_array(6, a);
    push_ptr(mv_cstr_doc("Contructing a variant from type", a), &nodes);
    push_ptr(mv_nest_doc(2, pretty_type(type, default_ptp, a), a), &nodes);
    push_ptr(mv_cstr_doc("which is not an enum type. Only Enum types can be used in this instance.", a), &nodes);

    PicoError err = {
        .range = get_range(variant, ctx.tape).term,
        .message = mv_sep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_incorrect_num_variant_args(PiType *type, SynRef ref, size_t variant_idx, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    Syntax variant = get_syntax(ref, ctx.tape);
    PtrArray nodes = mk_ptr_array(6, a);
    PtrArray* variant_args = type->enumeration.variants.data[variant_idx].val;

    push_ptr(mv_cstr_doc("Attempting constructing the variant ", a), &nodes);
    push_ptr(mv_str_doc(view_symbol_string(variant.variant.tagname), a), &nodes);
    push_ptr(mv_cstr_doc("with ", a), &nodes);
    push_ptr(pretty_u64(variant.variant.args.len, a), &nodes);
    push_ptr(mv_cstr_doc("args, however, this variant only accepts", a), &nodes);
    push_ptr(pretty_u64(variant_args->len, a), &nodes);
    push_ptr(mv_cstr_doc("args. The type being construted is:", a), &nodes);
    push_ptr(mv_nest_doc(2, pretty_type(type, default_ptp, a), a), &nodes);

    PicoError err = {
        .range = get_range(ref, ctx.tape).term,
        .message = mv_hsep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_missing_variant_tag(PiType* type, SynRef ref, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    Syntax variant = get_syntax(ref, ctx.tape);
    PtrArray nodes = mk_ptr_array(6, a);

    push_ptr(mv_cstr_doc("Attempting constructing the variant", a), &nodes);
    push_ptr(mk_paren_doc("'","'", mv_str_doc(view_symbol_string(variant.variant.tagname), a), a), &nodes);
    push_ptr(mv_cstr_doc("which does not exist in the inferred Enum type. The type this should have is:", a), &nodes);
    push_ptr(mv_nest_doc(2, pretty_type(type, default_ptp, a), a), &nodes);

    PicoError err = {
        .range = get_range(ref, ctx.tape).term,
        .message = mv_hsep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

// Match
_Noreturn void type_error_match_invalid_type(PiType *type, SynRef ref, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    Syntax match = get_syntax(ref, ctx.tape);
    PtrArray nodes = mk_ptr_array(2, a);
    push_ptr(mv_cstr_doc("Unexpected type provided to match. Expected an enum type, but got:", a),  &nodes);
    push_ptr(pretty_type(get_type(match.match.val, ctx.tape), default_ptp, a), &nodes);

    PicoError err = {
        .range = get_range(match.match.val, ctx.tape).term,
        .message = mv_hsep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_match_duplicate_tag(PiType* type, SynRef match, size_t variant_idx, TypeCheckContext ctx);
_Noreturn void type_error_match_incorrect_tag(PiType* type, SynRef match, size_t variant_idx, TypeCheckContext ctx);
_Noreturn void type_error_match_num_binds(PiType* type, SynRef match, size_t variant_idx, TypeCheckContext ctx);
_Noreturn void type_error_match_missing_variants(PiType* type, SynRef match, U8Array used_variants, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    PtrArray nodes = mk_ptr_array(6, a);

    push_ptr(mv_cstr_doc("This match is non-exhaustive. The following variants were not found in the match:", a), &nodes);
    for (size_t i = 0; i < used_variants.len; i++) {
        if (!used_variants.data[i]) {
            Symbol sym = type->enumeration.variants.data[i].key;
            push_ptr(mk_str_doc(view_symbol_string(sym), a), &nodes);
        }
    }

    PicoError err = {
        .range = get_range(match, ctx.tape).term,
        .message = mv_sep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

// Struct
_Noreturn void type_error_struct_invalid_type(PiType *type, SynRef strct, TypeCheckContext ctx); 
_Noreturn void type_error_struct_missing_field(PiType* type, SynRef strct, TypeCheckContext ctx);
_Noreturn void type_error_struct_dupliate_field(PiType* type, SynRef strct, TypeCheckContext ctx);
_Noreturn void type_error_struct_extra_field(PiType* type, SynRef strct, TypeCheckContext ctx);

// Projection
_Noreturn void type_error_proj_invalid_type(PiType* type, SynRef ref, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    Syntax proj = get_syntax(ref, ctx.tape);
    PtrArray nodes = mk_ptr_array(6, a);

    if (type->sort == TDistinct) {
        push_ptr(mv_cstr_doc("Attempting to access the field", a), &nodes);
        push_ptr(mk_paren_doc("'", "'", mv_str_doc(view_symbol_string(proj.projector.field), a), a), &nodes);
        push_ptr(mv_cstr_doc("however, this field cannot be accessed, as the source has an opaque type.", a), &nodes);
        push_ptr(mv_cstr_doc("This means that the type can only be used within the module it defined. Outside of its' home", a), &nodes);
        push_ptr(mv_cstr_doc("module, a value with opaque type can only be passed into functions. \n\nThe type is provided below for convenience.", a), &nodes);
        push_ptr(pretty_type(get_type(proj.projector.val, ctx.tape), default_ptp, a),  &nodes);
    
    } else {
        push_ptr(mv_cstr_doc("Attempting to access the field", a), &nodes);
        push_ptr(mk_paren_doc("'", "'", mv_str_doc(view_symbol_string(proj.projector.field), a), a), &nodes);
        push_ptr(mv_cstr_doc(" however, this field cannot be accessed, as the source has type", a), &nodes);
        push_ptr(mv_nest_doc(2, pretty_type(get_type(proj.projector.val, ctx.tape), default_ptp, a), a),  &nodes);
        push_ptr(mv_cstr_doc("which does not allow field access.", a), &nodes);
    }

    PicoError err = {
        .range = get_range(ref, ctx.tape).term,
        .message = mv_hsep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

// Instance
_Noreturn void type_error_instance_invalid_type(PiType *type, Range range, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    PtrArray nodes = mk_ptr_array(3, a);
    
    push_ptr(mv_cstr_doc("Attempting to create an instance of", a), &nodes);
    push_ptr(pretty_type(type, default_ptp, a),  &nodes);
    push_ptr(mv_cstr_doc("however, this type is not a trait. You can only create instances of trait types.", a), &nodes);

    PicoError err = {
        .range = range,
        .message = mv_hsep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_instance_wrong_nfields(Range range, size_t expected, size_t actual, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    PtrArray nodes = mk_ptr_array(4, a);

    push_ptr(mv_cstr_doc("Attempting to make a trait instance with", a), &nodes);
    push_ptr(pretty_u64(actual, a),  &nodes);
    push_ptr(mv_cstr_doc("fields. However, the trait the instace is being made from has", a), &nodes);
    push_ptr(pretty_u64(expected, a),  &nodes);

    PicoError err = {
        .range = range,
        .message = mv_hsep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_instance_missing_field(Range range, Symbol name, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    PtrArray nodes = mk_ptr_array(3, a);

    push_ptr(mv_cstr_doc("Attempting to creats an instance with field ", a), &nodes);
    push_ptr(mk_paren_doc("'", "'.", mv_str_doc(view_symbol_string(name), a), a), &nodes);
    push_ptr(mv_cstr_doc("However, the trait the instace is being made from does not have this field", a), &nodes);

    PicoError err = {
        .range = range,
        .message = mv_hsep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

//  Type Formers
// ------------------------------------------------------------
_Noreturn void type_error_family_must_have_args(SynRef family, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    const char *c_str =
        "Attempting to form a type Family with no type parameters. \n"
        "Type families require at least one type parameter.";

    PicoError err = {
        .range = get_range(family, ctx.tape).term,
        .message = mv_cstr_doc(c_str, a),
    };
    throw_pi_error(ctx.point, err);
}

// ---------------------------------------------------------------------- 
//
//                              Unifictaion  
//
// ----------------------------------------------------------------------

UnifyResult unify_error_variant_name_mismatch(Symbol lhs, Symbol rhs,
                                              UnifyContext ctx) {
    Allocator* a = ctx.a;
    PtrArray nodes = mk_ptr_array(6, a);

    push_ptr(mv_cstr_doc("Unification failed: RHS and LHS enums must have matching variant-names.",a ), &nodes);
    {
        PtrArray l1 = mk_ptr_array(8, a);
        push_ptr(mv_cstr_doc("    LHS has name: ", a) ,&l1);
        push_ptr(mv_str_doc(symbol_to_string(lhs, a), a), &l1);
        push_ptr(mv_cat_doc(l1, a), &nodes);
    }
    {
        PtrArray l2 = mk_ptr_array(8, a);
        push_ptr(mv_cstr_doc("    RHS has name: ", a) ,&l2);
        push_ptr(mv_str_doc(symbol_to_string(rhs, a), a), &l2);
        push_ptr(mv_cat_doc(l2, a), &nodes);
    }

    return (UnifyResult) {
        .type = USimpleError,
        .message = mv_vsep_doc(nodes, a),
    };
}


UnifyResult unify_error_name_has_args_match(PiType* lhs, PiType* rhs, Allocator* a) {
    PtrArray nodes = mk_ptr_array(6, a);
    push_ptr(mv_cstr_doc("Named type mismatch: two named types must both be instantiated with the same number of arguments.", a), &nodes);
    push_ptr(mv_cstr_doc("This error occurred when trying to unify types: ", a), &nodes);
    push_ptr(mv_nest_doc(2, pretty_type(lhs, default_ptp, a), a), &nodes);
    push_ptr(mv_cstr_doc("and", a), &nodes);
    push_ptr(mv_nest_doc(2, pretty_type(rhs, default_ptp, a), a), &nodes);
    return (UnifyResult) {
        .type = USimpleError,
        .message = mv_sep_doc(nodes, a),
    };
}
