#include "data/meta/slice_header.h"

#include "platform/signals.h"

#include "rune/syntax/expression.h"

typedef struct {
    Expr* data;
    size_t len;
    size_t capacity;
} ExprBacking;

typedef struct {
    NameExprCell* data;
    size_t len;
    size_t capacity;
} NameExprBacking;


struct ExprPool {
    ExprBacking expressions;
    NameExprBacking named_expressions;
    Allocator gpa;
};

ExprPool* mk_expr_pool(Allocator* gpa) {
    ExprPool* out = mem_alloc(sizeof(ExprPool), gpa);   
    const size_t start_len = 128;
    *out = (ExprPool) {
        .expressions = {
            .len = 0,
            .capacity = start_len,
            .data = mem_alloc(start_len * sizeof(Expr), gpa),
        },
        .named_expressions = {
            .len = 0,
            .capacity = start_len,
            .data = mem_alloc(start_len * sizeof(NameExprCell), gpa),
        },
        .gpa = *gpa,
    };
    return out;
}

typedef struct {
    void *data;
    size_t len;
    size_t capacity;
} GenericSlice;

static GenericSlice grow_slice(GenericSlice slice, size_t to_add, size_t val_size, Allocator* a) {
    size_t end_size = slice.len += to_add;
    while (slice.capacity <= end_size) {
        slice.capacity *= 2;
    }
    slice.len = end_size;
    slice.data = mem_realloc(slice.data, val_size * slice.capacity, a);
    return slice;
}

void delete_expr_pool(ExprPool* pool) {
    Allocator a = pool->gpa;
    mem_free(pool->expressions.data, &a);
    mem_free(pool->named_expressions.data, &a);
    mem_free(pool, &a);
}

ExprRef new_expr(ExprPool* pool) {
    ExprRef out = {pool->expressions.len};
    GenericSlice slice = {
        .data = pool->expressions.data,
        .len = pool->expressions.len,
        .capacity = pool->expressions.capacity,
    };
    slice = grow_slice(slice, 1, sizeof(Expr), &pool->gpa);
    pool->expressions = (ExprBacking) {
        .data = slice.data,
        .len = slice.len,
        .capacity = slice.capacity,
    };
    return out;
}

void set_expr(ExprRef ref, Expr expr, ExprPool* pool) {
    // TODO: debug bounds check.
    pool->expressions.data[ref.val] = expr;
}

Expr get_expr(ExprRef ref, ExprPool* pool) {
    // TODO: debug bounds check.
    return pool->expressions.data[ref.val];
}

ExprSlice new_expr_slice(uint32_t len, ExprPool* pool) {
    ExprSlice out = {.start = pool->expressions.len, .len = len};
    GenericSlice slice = {
        .data = pool->expressions.data,
        .len = pool->expressions.len,
        .capacity = pool->expressions.capacity,
    };
    slice = grow_slice(slice, len, sizeof(Expr), &pool->gpa);
    pool->expressions = (ExprBacking) {
        .data = slice.data,
        .len = slice.len,
        .capacity = slice.capacity,
    };
    return out;
}


void set_expr_elt(ExprSlice slice, uint32_t idx, Expr expr, ExprPool* pool) {
    // TODO: bounds check in debug mode
    pool->expressions.data[idx + slice.start] = expr;
}

ExprRef get_expr_elt(ExprSlice slice, uint32_t idx) {
    return (ExprRef) {idx + slice.start};
}

NameExprMap new_expr_map(uint32_t len, ExprPool* pool) {
    NameExprMap out = {.start = pool->expressions.len, .len = len};
    GenericSlice slice = {
        .data = pool->named_expressions.data,
        .len = pool->named_expressions.len,
        .capacity = pool->named_expressions.capacity,
    };
    slice = grow_slice(slice, len, sizeof(NameExprCell), &pool->gpa);
    pool->named_expressions = (NameExprBacking) {
        .data = slice.data,
        .len = slice.len,
        .capacity = slice.capacity,
    };
    return out;
}

void set_expr_map_elt(NameExprMap map, size_t idx, NameExprCell cell, ExprPool* pool) {
    pool->named_expressions.data[idx + map.start] = cell;
}

NameExprCell get_expr_map_elt(NameExprMap map, size_t idx, ExprPool* pool) {
    return pool->named_expressions.data[idx + map.start];
}

Document* pretty_rune_expr(ExprRef ref, ExprPool* pool, Allocator* a) {
    DocStyle former_style = scolour(colour(60, 190, 24), dstyle);
    DocStyle ty_former_style = scolour(colour(209, 118, 219), dstyle);
    DocStyle field_style = scolour(colour(60, 120, 210), dstyle);
    DocStyle const_style = scolour(colour(120, 170, 210), dstyle);
    DocStyle var_style = scolour(colour(212, 130, 42), dstyle);

    Document* out = NULL;

    Expr expr = get_expr(ref, pool);
    switch (expr.type) {
    case EVar:
        out = mv_style_doc(var_style, mv_str_doc(view_name_string(expr.var), a), a);
        break;
    case EFn: {
        PtrArray head = mk_ptr_array(2, a);
        push_ptr(mv_style_doc(former_style, mv_str_doc((mk_string("fn", a)), a), a), &head);

        PtrArray arg_nodes = mk_ptr_array(expr.fn.args.len, a);
        for (size_t i = 0; i < expr.fn.args.len; i++) {
            Document* arg = mv_style_doc(var_style, mk_str_doc(name_to_string(expr.fn.args.data[i], a), a), a);
            push_ptr(arg, &arg_nodes);
        }

        push_ptr(mv_nest_doc(2, mk_paren_doc("[", "]", mv_nest_doc(1, mv_sep_doc(arg_nodes, a), a), a), a), &head);

        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(mv_group_doc(mv_sep_doc(head, a), a), &nodes);
        push_ptr(mv_nest_doc(2, pretty_rune_expr(expr.fn.body, pool, a), a), &nodes);
        out = mk_paren_doc("(", ")", mv_group_doc(mv_sep_doc(nodes, a), a), a);
        break;
    }
    case EApp: {
        PtrArray nodes = mk_ptr_array(expr.app.args.len + 1, a);
        push_ptr(pretty_rune_expr(expr.app.fn, pool, a), &nodes);
        for (size_t i = 0; i < expr.app.args.len; i++) {
            push_ptr(pretty_rune_expr(get_expr_elt(expr.app.args, i), pool, a), &nodes);
        }
        out = mk_paren_doc("(", ")", mv_group_doc(mv_sep_doc(nodes, a), a), a);
        break;
    }

    case EString:
        out = mk_paren_doc("\"", "\"", mv_str_doc(expr.string, a), a);
        out = mv_style_doc(const_style, out, a);
        break;
    case EList:
        panic(mv_string("Not implemented : pretty list"));
        break;
    }
    if (out == NULL) {
        panic(mv_string("Invalid expression provided to pretty_rune_expr"));
    }
    out = mv_group_doc(out, a);
    return out;
}
