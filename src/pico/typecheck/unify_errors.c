#include "components/pretty/standard_types.h"

#include "pico/typecheck/unify_errors.h"

UnifyResult unify_app_err_no_args(PiType* app, PiType* val, UnifyContext ctx) {
    Allocator* a = ctx.a;
    PtrArray nodes = mk_ptr_array(6, a);

    push_ptr(mv_cstr_doc("Unification failed: attempting to unify an application", a), &nodes);
    push_ptr(mk_paren_doc("(", ")", pretty_type(app, default_ptp, a), a), &nodes);
    push_ptr(mv_cstr_doc("with", a), &nodes);
    push_ptr(pretty_type(val, default_ptp, a), &nodes);
    push_ptr(mv_cstr_doc("which has no arguments.", a), &nodes);

    return (UnifyResult) {
        .type = USimpleError,
        .message = mv_vsep_doc(nodes, a),
    };
}

UnifyResult unify_app_err_unequal_arglen(PiType* app, PiType* val, UnifyContext ctx) {
    Allocator* a = ctx.a;
    PtrArray nodes = mk_ptr_array(6, a);

    push_ptr(mv_cstr_doc("Unification failed: attempting to unify an application", a), &nodes);
    push_ptr(mk_paren_doc("(", ")", pretty_type(app, default_ptp, a), a), &nodes);
    push_ptr(mv_cstr_doc("with", a), &nodes);
    push_ptr(pretty_type(val, default_ptp, a), &nodes);
    push_ptr(mv_cstr_doc("which has a different number of argument to the application.", a), &nodes);

    return (UnifyResult) {
        .type = USimpleError,
        .message = mv_vsep_doc(nodes, a),
    };
}

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
