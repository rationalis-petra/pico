#include "components/pretty/standard_types.h"

#include "pico/typecheck/type_errors.h"

_Noreturn void type_error_unexpected_module(Syntax* syn, Module* module, TypeCheckContext ctx) {
  PicoError err = {
    .message = mv_cstr_doc(
        "This variable refers to a module. Please note that modules cannot be \n"
        "used as values directly in code, but must instead be used only to \n"
        "access their members, e.g. u64.to-string or platform.terminal.write-string \n",
        ctx.a),
        .range = syn->range,
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_unknown_var(Syntax *syn, TypeCheckContext ctx) {
    // TODO (Improvement): search the environment for symbols with a similar name
    PtrArray nodes = mk_ptr_array(3, ctx.a);
    push_ptr(mv_cstr_doc("The variable", ctx.a), &nodes);
    push_ptr(mk_paren_doc("'", "'", mk_str_doc(view_symbol_string(syn->variable), ctx.a), ctx.a), &nodes);
    push_ptr(mv_cstr_doc("is not bound anywhere, and so cannot be used here.", ctx.a), &nodes);

    PicoError err = {
        .range = syn->range,
        .message = mv_sep_doc(nodes, ctx.a),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_invalid_declaration(Symbol type, Syntax *arg, TypeCheckContext ctx) {
    PtrArray nodes = mk_ptr_array(3, ctx.a);
    push_ptr(mv_cstr_doc("The declaration", ctx.a), &nodes);
    push_ptr(mk_paren_doc("'", "'", mk_str_doc(view_symbol_string(type), ctx.a), ctx.a), &nodes);
    push_ptr(mv_cstr_doc("is not a recognized declaration type. Valid declarations are 'type' and 'inline'.", ctx.a), &nodes);

    PicoError err = {
        .range = arg->range,
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
_Noreturn void type_error_expecting_instance_arg(size_t implicit_idx, Syntax *proc, TypeCheckContext ctx) {
    SymPtrACell arg = proc->procedure.implicits.data[implicit_idx];

    PtrArray nodes = mk_ptr_array(4, ctx.a);
    push_ptr(mv_cstr_doc("The argument", ctx.a), &nodes);
    push_ptr(mk_paren_doc("'", "'", 
                          mv_str_doc(view_symbol_string(arg.key), ctx.a), ctx.a), &nodes);
    push_ptr(mv_cstr_doc("is beign used as an instance argument, i.e. between '{' and '}'."
                         " As such, it is expected to have a trait (instance) type, but"
                         " it instead has type:" , ctx.a), &nodes);
    push_ptr(mv_nest_doc(2, pretty_type(proc->ptype, ctx.a), ctx.a), &nodes);

    PicoError err = {
        .range = proc->range,
        .message = mv_sep_doc(nodes, ctx.a),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_proc_incorrect_num_implicits(Syntax *proc,
                                                       PiType *type,
                                                       TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    PtrArray nodes = mk_ptr_array(4, a);

    push_ptr(mv_cstr_doc("The procedure here was previously inferred or declared to have", a), &nodes);
    push_ptr(pretty_u64(type->proc.implicits.len, a), &nodes);
    push_ptr(mv_cstr_doc("implicit arguments, but it actually has", a), &nodes);
    push_ptr(pretty_u64(proc->procedure.implicits.len, a), &nodes);

    PicoError err = {
        .range = proc->range,
        .message = mv_hsep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}
_Noreturn void type_error_proc_incorrect_num_args(Syntax *proc, PiType *type,
                                                  TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    PtrArray nodes = mk_ptr_array(4, a);

    push_ptr(mv_cstr_doc("The procedure here was previously inferred or declared to have", a), &nodes);
    push_ptr(pretty_u64(type->proc.args.len, a), &nodes);
    push_ptr(mv_cstr_doc("arguments, but it actually has", a), &nodes);
    push_ptr(pretty_u64(proc->procedure.args.len, a), &nodes);

    PicoError err = {
        .range = proc->range,
        .message = mv_hsep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

// Application
//---------------------------------------------------------------------- 

_Noreturn void type_error_incorrect_num_args(PiType* type, Syntax *app, InvalidArgType args_type, TypeCheckContext ctx) {
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
    push_ptr(pretty_u64(app->application.args.len, a), &cat_nodes);
    push_ptr(mv_cstr_doc(".", a), &cat_nodes);
    push_ptr(mv_cat_doc(cat_nodes, a), &err_nodes);

    PtrArray supplement_nodes = mk_ptr_array(4, a);
    if (type->sort != TKind) {
        push_ptr(mv_cstr_doc("The function being applied has type:", a), &supplement_nodes);
    } else {
        push_ptr(mv_cstr_doc("The type being applied has kind:", a), &supplement_nodes);
    }
    push_ptr(mv_nest_doc(2, pretty_type(type, a), a), &supplement_nodes);

    PtrArray nodes = mk_ptr_array(2, a);
    push_ptr(mv_hsep_doc(err_nodes, a), &nodes);
    push_ptr(mv_hsep_doc(supplement_nodes, a), &nodes);

    PicoError err = {
        .range = app->range,
        .message = mv_vsep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_invalid_application_target(PiType *type, Syntax *app, TypeCheckContext ctx) {
    bool is_all = app->type == SAllApplication;

    Allocator* a = ctx.a;
    PtrArray nodes = mk_ptr_array(3, a);
    if (is_all) {
        push_ptr(mv_str_doc(mv_string("Attempting to apply this value to some types and arguments. However, it has type: "), a), &nodes);
    } else {
        push_ptr(mv_str_doc(mv_string("Attempting to apply this value to some arguments. However, it has type: "), a), &nodes);
    }
    push_ptr(mv_nest_doc(2, pretty_type(type, a), a), &nodes);
    if (is_all) {
        push_ptr(mv_str_doc(mv_string("which cannot be used this way. Only 'all' terms can be applied in this context."), a), &nodes);
    } else {
        push_ptr(mv_str_doc(mv_string("which cannot be used this way. Only 'proc', 'all' and 'Family' terms can be applied in this context."), a), &nodes);
    }
    PicoError err = {
        .range = is_all ? app->all_application.function->range : app->application.function->range,
        .message = mv_sep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_incorrect_num_args_all_noproc(PiType *type, Syntax *app, bool is_implicit_args, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    PtrArray nodes = mk_ptr_array(4, a);
    if (is_implicit_args) {
        push_ptr(mk_cstr_doc("Incorrect number of implicit arguments to all function - expected: 0, got: ", a), &nodes);
    } else {
        push_ptr(mk_cstr_doc("Incorrect number of arguments to all function - expected: 0, got: ", a), &nodes);
    }
    size_t len = is_implicit_args ? app->all_application.implicits.len : app->all_application.args.len;
    push_ptr(pretty_u64(len, a), &nodes);
    push_ptr(mk_cstr_doc("Note: the function being applied has type: ", a), &nodes);
    push_ptr(pretty_type(type, a), &nodes);

    PicoError err = {
        .range = app->all_application.function->range,
        .message = mv_sep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_all_app_couldnt_deduce_types(size_t arg_idx, Syntax *app, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    PiType* fn_ty = app->all_application.function->ptype;

    PtrArray nodes = mk_ptr_array(4, a);
    push_ptr(mv_cstr_doc("Typechecking error: When applying an all with type", a), &nodes);
    push_ptr(pretty_type(fn_ty, a), &nodes);
    push_ptr(mv_cstr_doc("not all types were able to be deduced. In particular, the type of ", a), &nodes);
    push_ptr(mk_paren_doc("'", "'", mv_str_doc(view_symbol_string(fn_ty->binder.vars.data[arg_idx]), a), a), &nodes);
    push_ptr(mv_cstr_doc("is ambiguous.", a), &nodes);

    PicoError err = {
        .range = app->all_application.function->range,
        .message = mv_hsep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}


//---------------------------------------------------------------------- 
//
// Sealing
//
//---------------------------------------------------------------------- 

_Noreturn void type_error_invalid_seal_type(PiType *type, Syntax *seal, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    PtrArray nodes = mk_ptr_array(4, a);
    push_ptr(mv_cstr_doc("A seal is being constructed with type:", a), &nodes);
    push_ptr(mv_nest_doc(2, pretty_type(type, a), a), &nodes);
    push_ptr(mv_cstr_doc("However, only 'Sealed' types are allowed here.", a), &nodes);

    PicoError err = {
        .range = seal->range,
        .message = mv_sep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_incorrect_num_seal_args(PiType *type, Syntax *seal, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    PtrArray nodes = mk_ptr_array(6, a);
    push_ptr(mv_cstr_doc("Attempting to seal away ", a), &nodes);
    push_ptr(pretty_u64(seal->seal.types.len, a), &nodes);
    push_ptr(mv_cstr_doc("types, however, the seal type was expecting ", a), &nodes);
    push_ptr(pretty_u64(type->sealed.vars.len, a), &nodes);
    push_ptr(mv_cstr_doc(" The type being sealed against is:", a), &nodes);
    push_ptr(pretty_type(type, a), &nodes);

    PicoError err = {
        .range = seal->range,
        .message = mv_hsep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_invalid_unseal_type(PiType *type, Syntax *unseal, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    PtrArray nodes = mk_ptr_array(4, a);
    push_ptr(mv_cstr_doc("An unseal performed, however, the value being unsealed has type:", a), &nodes);
    push_ptr(mv_nest_doc(2, pretty_type(type, a), a), &nodes);
    push_ptr(mv_cstr_doc("Only 'Sealed' types are allowed here.", a), &nodes);

    PicoError err = {
        .range = unseal->unseal.sealed->range,
        .message = mv_sep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_incorrect_num_unseal_binds(PiType* type, Syntax* unseal, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    PtrArray nodes = mk_ptr_array(6, a);
    push_ptr(mv_cstr_doc("Attempting to unseal and bind", a), &nodes);
    push_ptr(pretty_u64(unseal->unseal.types.len, a), &nodes);
    push_ptr(mv_cstr_doc("types, however, the seal type was expecting ", a), &nodes);
    push_ptr(pretty_u64(type->sealed.vars.len, a), &nodes);
    push_ptr(mv_cstr_doc("The type being unsealed is:", a), &nodes);
    push_ptr(mv_nest_doc(2, pretty_type(type, a), a), &nodes);

    PicoError err = {
        .range = unseal->range,
        .message = mv_hsep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_invalid_variant_type(PiType *type, Syntax *variant, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    PtrArray nodes = mk_ptr_array(6, a);
    push_ptr(mv_cstr_doc("Contructing a variant from type", a), &nodes);
    push_ptr(mv_nest_doc(2, pretty_type(type, a), a), &nodes);
    push_ptr(mv_cstr_doc("which is not an enum type. Only Enum types can be used in this instance.", a), &nodes);

    PicoError err = {
        .range = variant->range,
        .message = mv_sep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_incorrect_num_variant_args(PiType *type, Syntax *variant, size_t variant_idx, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    PtrArray nodes = mk_ptr_array(6, a);
    PtrArray* variant_args = type->enumeration.variants.data[variant_idx].val;

    push_ptr(mv_cstr_doc("Attempting constructing the variant ", a), &nodes);
    push_ptr(mv_str_doc(view_symbol_string(variant->variant.tagname), a), &nodes);
    push_ptr(mv_cstr_doc("with ", a), &nodes);
    push_ptr(pretty_u64(variant->variant.args.len, a), &nodes);
    push_ptr(mv_cstr_doc("args, however, this variant only accepts", a), &nodes);
    push_ptr(pretty_u64(variant_args->len, a), &nodes);
    push_ptr(mv_cstr_doc("args. The type being construted is:", a), &nodes);
    push_ptr(mv_nest_doc(2, pretty_type(type, a), a), &nodes);

    PicoError err = {
        .range = variant->range,
        .message = mv_hsep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_missing_variant_tag(PiType * type, Syntax * variant, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    PtrArray nodes = mk_ptr_array(6, a);

    push_ptr(mv_cstr_doc("Attempting constructing the variant", a), &nodes);
    push_ptr(mk_paren_doc("'","'", mv_str_doc(view_symbol_string(variant->variant.tagname), a), a), &nodes);
    push_ptr(mv_cstr_doc("which does not exist in the inferred Enum type. The type this should have is:", a), &nodes);
    push_ptr(mv_nest_doc(2, pretty_type(type, a), a), &nodes);

    PicoError err = {
        .range = variant->range,
        .message = mv_hsep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

// Match
_Noreturn void type_error_match_invalid_type(PiType *type, Syntax *match,
                                             TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    PtrArray nodes = mk_ptr_array(2, a);
    push_ptr(mv_cstr_doc("Unexpected type provided to match. Expected an enum type, but got:", a),  &nodes);
    push_ptr(pretty_type(match->match.val->ptype, a), &nodes);

    PicoError err = {
        .range = match->match.val->range,
        .message = mv_hsep_doc(nodes, a),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void type_error_match_duplicate_tag(PiType* type, Syntax* match, size_t variant_idx, TypeCheckContext ctx);
_Noreturn void type_error_match_incorrect_tag(PiType* type, Syntax* match, size_t variant_idx, TypeCheckContext ctx);
_Noreturn void type_error_match_num_binds(PiType* type, Syntax* match, size_t variant_idx, TypeCheckContext ctx);
_Noreturn void type_error_match_missing_variants(PiType* type, Syntax* match, size_t variant_idx, TypeCheckContext ctx);

// Struct
_Noreturn void type_error_struct_invalid_type(PiType *type, Syntax *strct, TypeCheckContext ctx); 
_Noreturn void type_error_struct_missing_field(PiType* type, Syntax* strct, TypeCheckContext ctx);
_Noreturn void type_error_struct_dupliate_field(PiType* type, Syntax* strct, TypeCheckContext ctx);
_Noreturn void type_error_struct_extra_field(PiType* type, Syntax* strct, TypeCheckContext ctx);

// Projection
_Noreturn void type_error_proj_invalid_type(PiType* type, Syntax* proj, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    PtrArray nodes = mk_ptr_array(6, a);

    if (type->sort == TDistinct) {
        push_ptr(mv_cstr_doc("Attempting to access the field", a), &nodes);
        push_ptr(mk_paren_doc("'", "'", mv_str_doc(view_symbol_string(proj->projector.field), a), a), &nodes);
        push_ptr(mv_cstr_doc("however, this field cannot be accessed, as the source has an opaque type.", a), &nodes);
        push_ptr(mv_cstr_doc("This means that the type can only be used within the module it defined. Outside of its' home", a), &nodes);
        push_ptr(mv_cstr_doc("module, a value with opaque type can only be passed into functions. \n\nThe type is provided below for convenience.", a), &nodes);
        push_ptr(pretty_type(proj->projector.val->ptype, a),  &nodes);
    
    } else {
        push_ptr(mv_cstr_doc("Attempting to access the field", a), &nodes);
        push_ptr(mk_paren_doc("'", "'", mv_str_doc(view_symbol_string(proj->projector.field), a), a), &nodes);
        push_ptr(mv_cstr_doc(" however, this field cannot be accessed, as the source has type", a), &nodes);
        push_ptr(mv_nest_doc(2, pretty_type(proj->projector.val->ptype, a), a),  &nodes);
        push_ptr(mv_cstr_doc("which does not allow field access.", a), &nodes);
    }

    PicoError err = {
        .range = proj->range,
        .message = mv_hsep_doc(nodes, a),
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
