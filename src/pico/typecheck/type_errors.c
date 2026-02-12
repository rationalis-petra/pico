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

_Noreturn void type_error_incorrect_num_args(PiType* type, Syntax *app, bool is_function, bool is_value_args, TypeCheckContext ctx) {
    Allocator* a = ctx.a;
    PtrArray err_nodes = mk_ptr_array(4, a);
    if (is_function && is_value_args) {
        push_ptr(mv_cstr_doc("Incorrect number of arguments applied to function - expected", a), &err_nodes);
    } else if (is_function) {
        push_ptr(mv_cstr_doc("Incorrect number of types applied to all - expected", a), &err_nodes);
    } else {
        push_ptr(mv_cstr_doc("Incorrect number of arguments applied to type - expected", a), &err_nodes);
    }
    if (is_function && is_value_args) {
        if (type->sort == TAll) {
            push_ptr(pretty_u64(type->binder.body->proc.args.len, a), &err_nodes);
        } else {
            push_ptr(pretty_u64(type->proc.args.len, a), &err_nodes);
        }
    } else {
        push_ptr(pretty_u64(type->binder.vars.len, a), &err_nodes);
    }
    push_ptr(mv_cstr_doc("but got", a), &err_nodes);

    PtrArray cat_nodes = mk_ptr_array(2, a);
    push_ptr(pretty_u64(app->application.args.len, a), &cat_nodes);
    push_ptr(mv_cstr_doc(".", a), &cat_nodes);
    push_ptr(mv_cat_doc(cat_nodes, a), &err_nodes);

    PtrArray supplement_nodes = mk_ptr_array(4, a);
    if (is_function) {
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
