#include "platform/signals.h"

#include "rune/syntax/formula.h"


struct FormulaPool { 
    size_t len;
    size_t capacity;
    Formula* formulas;
    Allocator* gpa;
};

FormulaPool* mk_formula_pool(Allocator* a) {
    FormulaPool* pool = mem_alloc(sizeof(FormulaPool), a);
    const size_t initial_capacity = 32;
    *pool = (FormulaPool) {
        .len = 0,
        .capacity = initial_capacity,
        .formulas = mem_alloc(sizeof(Formula) * initial_capacity, a),
        .gpa = a,
    };
    return pool;
}

Formula get_formula(FormulaRef ref, FormulaPool* pool) {
    return pool->formulas[ref.ref];
}

Document* pretty_single_constraint(SingleConstraint constraint, ExprPool* epool, Allocator* a) {
    PtrArray nodes = mk_ptr_array(3, a);

    push_ptr(pretty_rune_expr(get_expr(constraint.lhs, epool), epool, a), &nodes);
    push_ptr(mv_cstr_doc(constraint.sort == SEq ? "≗" : "⋵", a), &nodes);
    push_ptr(pretty_rune_expr(get_expr(constraint.rhs, epool), epool, a), &nodes);

    return mv_group_doc(mv_sep_doc(nodes, a), a);
}

Document* pretty_rune_formula(Formula formula, FormulaPool* fpool, ExprPool* epool, Allocator* a) {
    switch (formula.sort) {
    case FConj: {
        if (formula.conj.len > 0) {
            PtrArray nodes = mk_ptr_array(formula.conj.len * 2 - 1, a);
            for (size_t i = 0; i < formula.conj.len; i++) {
                push_ptr(pretty_single_constraint(formula.conj.data[i], epool, a), &nodes);
                if (i + 1 != formula.conj.len) {
                    push_ptr(mv_cstr_doc("⩑", a), &nodes);
                }
            }
            return mv_group_doc(mv_sep_doc(nodes, a), a);
        } else {
            return mv_cstr_doc("⊤", a);
        }
    }
    case FAnd: {
        PtrArray nodes = mk_ptr_array(3, a);
        push_ptr(pretty_rune_formula(get_formula(formula.and.lhs, fpool), fpool, epool, a), &nodes);
        push_ptr(mv_cstr_doc("⩑", a), &nodes);
        push_ptr(pretty_rune_formula(get_formula(formula.and.rhs, fpool), fpool, epool, a), &nodes);
        return mk_paren_doc("(", ")", mv_group_doc(mv_sep_doc(nodes, a), a), a);
    }
    case FQuant: {
        String qstring = mv_string(formula.quant.quantifier == Exists ? "∃" : "∀");
        PtrArray nodes = mk_ptr_array(4, a);
        push_ptr(mv_str_doc(qstring, a), &nodes);
        push_ptr(pretty_rune_formula(get_formula(formula.quant.body, fpool), fpool, epool, a), &nodes);
        return mv_group_doc(mv_sep_doc(nodes, a), a);
    }
    }
    panic(mv_string("Invalid formula provided to pretty_rune_formula"));
}
