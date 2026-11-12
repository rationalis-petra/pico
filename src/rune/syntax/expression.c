#include "data/meta/slice_header.h"

#include "platform/signals.h"

#include "rune/syntax/expression.h"

typedef struct {
    Name* data;
    size_t len;
    size_t capacity;
} NamesBacking;

typedef struct {
    Expr* data;
    size_t len;
    size_t capacity;
} ExprBacking;

typedef struct {
    ExprCell* data;
    size_t len;
    size_t capacity;
} NamedBacking;

typedef struct {
    PatternCell* data;
    size_t len;
    size_t capacity;
} PatternBacking;

typedef struct {
    AlgebraOp* data;
    size_t len;
    size_t capacity;
} AlgebraBacking;

struct ExprPool {
    ExprBacking expressions;
    NamesBacking names;
    NamedBacking named_expressions;
    PatternBacking patterns;
    AlgebraBacking algebras;
    Allocator gpa;
};

ExprPool* mk_expr_pool(Allocator* gpa) {
    ExprPool* out = mem_alloc(sizeof(ExprPool), gpa);   
    const size_t start_len = 128;
    const size_t small_start_len = 16;
    *out = (ExprPool) {
        .expressions = {
            .len = 0,
            .capacity = start_len,
            .data = mem_alloc(start_len * sizeof(Expr), gpa),
        },
        .names = {
            .len = 0,
            .capacity = small_start_len,
            .data = mem_alloc(small_start_len * sizeof(Name), gpa),
        },
        .named_expressions = {
            .len = 0,
            .capacity = start_len,
            .data = mem_alloc(start_len * sizeof(ExprCell), gpa),
        },
        .patterns = {
            .len = 0,
            .capacity = start_len,
            .data = mem_alloc(start_len * sizeof(PatternCell), gpa),
        },
        .algebras = {
            .len = 0,
            .capacity = small_start_len,
            .data = mem_alloc(small_start_len * sizeof(AlgebraOp), gpa),
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
        slice.capacity = slice.capacity < 32 ? 32 : slice.capacity * 2;
    }
    slice.len = end_size;
    slice.data = mem_realloc(slice.data, val_size * slice.capacity, a);
    return slice;
}

void delete_expr_pool(ExprPool* pool) {
    Allocator a = pool->gpa;
    mem_free(pool->names.data, &a);
    mem_free(pool->expressions.data, &a);
    mem_free(pool->named_expressions.data, &a);
    mem_free(pool->patterns.data, &a);
    mem_free(pool->algebras.data, &a);
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

ExprNames new_expr_names(size_t len, ExprPool* pool) {
    ExprNames names = {.start = pool->names.len, .len = len};
    GenericSlice slice = {
        .data = pool->names.data,
        .len = pool->names.len,
        .capacity = pool->names.capacity,
    };
    slice = grow_slice(slice, 1, sizeof(Expr), &pool->gpa);
    pool->names = (NamesBacking) {
        .data = slice.data,
        .len = slice.len,
        .capacity = slice.capacity,
    };
    return names;
}

void set_expr_name(ExprNames names, uint32_t idx, Name name, ExprPool* pool) {
    pool->names.data[names.start + idx] = name;
}

Name get_expr_name(ExprNames names, uint32_t idx, ExprPool* pool) {
    return pool->names.data[names.start + idx];
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

ExprMap new_expr_map(uint32_t len, ExprPool* pool) {
    ExprMap out = {.start = pool->named_expressions.len, .len = len};
    GenericSlice slice = {
        .data = pool->named_expressions.data,
        .len = pool->named_expressions.len,
        .capacity = pool->named_expressions.capacity,
    };
    slice = grow_slice(slice, len, sizeof(ExprCell), &pool->gpa);
    pool->named_expressions = (NamedBacking) {
        .data = slice.data,
        .len = slice.len,
        .capacity = slice.capacity,
    };
    return out;
}

void set_expr_map_elt(ExprMap map, size_t idx, ExprCell cell, ExprPool* pool) {
    pool->named_expressions.data[idx + map.start] = cell;
}

ExprCell get_expr_map_elt(ExprMap map, size_t idx, ExprPool* pool) {
    return pool->named_expressions.data[idx + map.start];
}


PatternMap new_expr_ptn_map(uint32_t len, ExprPool* pool) {
    PatternMap out = {.start = pool->patterns.len, .len = len};
    GenericSlice slice = {
        .data = pool->patterns.data,
        .len = pool->patterns.len,
        .capacity = pool->patterns.capacity,
    };
    slice = grow_slice(slice, len, sizeof(PatternCell), &pool->gpa);
    pool->patterns = (PatternBacking) {
        .data = slice.data,
        .len = slice.len,
        .capacity = slice.capacity,
    };
    return out;
}

void set_expr_ptn_map_elt(PatternMap map, size_t idx, PatternCell cell, ExprPool* pool) {
    pool->patterns.data[idx + map.start] = cell;
}

PatternCell get_expr_ptn_map_elt(PatternMap map, size_t idx, ExprPool* pool) {
    return pool->patterns.data[idx + map.start];
}

AlgebraicTypeSpec new_alg_spec(uint32_t len, ExprPool* pool) {
    AlgebraicTypeSpec out = {.start = pool->algebras.len, .len = len};
    GenericSlice slice = {
        .data = pool->algebras.data,
        .len = pool->algebras.len,
        .capacity = pool->algebras.capacity,
    };
    slice = grow_slice(slice, len, sizeof(AlgebraOp), &pool->gpa);
    pool->algebras = (AlgebraBacking) {
        .data = slice.data,
        .len = slice.len,
        .capacity = slice.capacity,
    };
    return out;
}

void set_alg_spec_op(AlgebraicTypeSpec spec, size_t idx, AlgebraOp op, ExprPool* pool) {
    pool->algebras.data[idx + spec.start] = op;
}

AlgebraOp get_alg_spec_op(AlgebraicTypeSpec spec, size_t idx, ExprPool* pool) {
    return pool->algebras.data[idx + spec.start];
}

void free_vars_internal(Expr expr, ExprPool* pool, NameArray* locals, NameArray* out) {
    switch (expr.type) {
    case EVar:
        if (find_name(expr.var, *locals) == locals->len) {
            if (find_name(expr.var, *out) == out->len) {
                // push name only if
                // 1. Name not in locals 
                // 2. Name is not in output already
                push_name(expr.var, out);
            }
        }
        break;
    case EFn: {
        for (size_t i = 0; i < expr.fn.args.len; i++) {
            push_name(expr.fn.args.data[i], locals);
        }
        free_vars_internal(get_expr(expr.fn.body, pool), pool, locals, out);
        locals->len -= expr.fn.args.len;
        break;
    }
    case EApp: {
        free_vars_internal(get_expr(expr.app.fn, pool), pool, locals, out);
        for (size_t i = 0; i < expr.app.args.len; i++) {
            ExprRef args = get_expr_elt(expr.app.args, i);
            free_vars_internal(get_expr(args, pool), pool, locals, out);
        }
        break;
    }
    case EPr: {
        panic(mv_string("TODO: implement free_vars for pair"));
        break;
    }
    case EElt: {
        panic(mv_string("TODO: implement free_vars for element"));
        break;
    }
    case ECtor:
        // Nothing - constructors are empty!
        break;
    case ERecord: {
        free_vars_internal(get_expr(expr.record.type, pool), pool, locals, out);
        for (size_t i = 0; i < expr.record.fields.len; i++) {
            ExprCell cell = get_expr_map_elt(expr.record.fields, i, pool);
            free_vars_internal(cell.expr, pool, locals, out);
        }
        break;
    }
    case ECase:
        free_vars_internal(get_expr(expr.ecase.val, pool), pool, locals, out);
        for (size_t i = 0; i < expr.ecase.fields.len; i++) {
            PatternCell cell = get_expr_ptn_map_elt(expr.ecase.fields, i, pool);
            for (size_t i = 0; i < cell.pattern.args.len; i++) {
                push_name(cell.pattern.args.data[i], locals);
            }
            free_vars_internal(get_expr(cell.pattern.body, pool), pool, locals, out);
            locals->len -= cell.pattern.args.len;
        }
        break;
    case ECoCase: {
        for (size_t i = 0; i < expr.cocase.fields.len; i++) {
            PatternCell cell = get_expr_ptn_map_elt(expr.cocase.fields, i, pool);
            for (size_t i = 0; i < cell.pattern.args.len; i++) {
                push_name(cell.pattern.args.data[i], locals);
            }
            free_vars_internal(get_expr(cell.pattern.body, pool), pool, locals, out);
            locals->len -= cell.pattern.args.len;
        }
        break;
    }
    case EProject:
        panic(mv_string("TODO: implement free_vars for project"));
    case ERecur:
        panic(mv_string("TODO: implement free_vars for recur"));
    case EDataType: {
        panic(mv_string("TODO: implement free_vars for Data Type"));
        break;
    }
    case ECoDataType: {
        panic(mv_string("TODO: implement free_vars for CoData Type"));
        break;
    }
    case EVal:
        panic(mv_string("TODO: implement free_vars for val"));

    case EInt:
    case EString:
        break;
    case EList:
        for (size_t i = 0; i < expr.list.len; i++) {
            ExprRef elt = get_expr_elt(expr.list, i);
            free_vars_internal(get_expr(elt, pool), pool, locals, out);
        }
        break;
    }
}

NameArray free_vars(Expr expr, ExprPool* pool, Allocator* a) {
    NameArray out = mk_name_array(8, a);
    NameArray locals = mk_name_array(8, a);
    free_vars_internal(expr, pool, &locals, &out);
    sdelete_name_array(locals);
    return out;
}

Document* pretty_rune_expr(Expr expr, ExprPool* pool, Allocator* a) {
    DocStyle former_style = scolour(colour(60, 190, 24), dstyle);
    //DocStyle ty_former_style = scolour(colour(209, 118, 219), dstyle);
    DocStyle field_style = scolour(colour(60, 120, 210), dstyle);
    DocStyle const_style = scolour(colour(120, 170, 210), dstyle);
    DocStyle var_style = scolour(colour(212, 130, 42), dstyle);

    Document* out = NULL;

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
        push_ptr(mv_nest_doc(2, pretty_rune_expr(get_expr(expr.fn.body, pool), pool, a), a), &nodes);
        out = mk_paren_doc("(", ")", mv_group_doc(mv_sep_doc(nodes, a), a), a);
        break;
    }
    case EApp: {
        PtrArray nodes = mk_ptr_array(expr.app.args.len + 1, a);
        push_ptr(pretty_rune_expr(get_expr(expr.app.fn, pool), pool, a), &nodes);
        for (size_t i = 0; i < expr.app.args.len; i++) {
            push_ptr(mv_nest_doc(2, pretty_rune_expr(get_expr(get_expr_elt(expr.app.args, i), pool), pool, a), a), &nodes);
        }
        out = mv_group_doc(mk_paren_doc("(", ")", mv_group_doc(mv_sep_doc(nodes, a), a), a), a);
        break;
    }
    case EPr: {
        panic(mv_string("TODO: implement pretty pair."));
    }
    case EElt: {
        panic(mv_string("TODO: implement pretty element."));
    }
    case ECtor: {
        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(mv_cstr_doc(":", a), &nodes);
        push_ptr(mk_str_doc(view_name_string(expr.ctor.name), a), &nodes);
        out = mv_style_doc(field_style, mv_cat_doc(nodes, a), a);
        break;
    }
    case ERecord: {
        panic(mv_string("TODO: implement pretty record."));
    }
    case ECase: {
        PtrArray nodes = mk_ptr_array(1 + expr.ecase.fields.len, a);
        PtrArray head_nodes = mk_ptr_array(2, a);
        push_ptr(mv_style_doc(former_style, mv_cstr_doc("case", a), a), &head_nodes);
        push_ptr(mv_nest_doc(2, pretty_rune_expr(get_expr(expr.ecase.val, pool), pool, a), a), &head_nodes);
        push_ptr(mv_sep_doc(head_nodes, a), &nodes);
        for (size_t i = 0; i < expr.ecase.fields.len; i++) {
            PatternCell cell = get_expr_ptn_map_elt(expr.ecase.fields, i, pool);
            PtrArray fnodes = mk_ptr_array(2, a);
            PtrArray head =  mk_ptr_array(2, a);
            push_ptr(mv_cstr_doc(".", a), &head);
            push_ptr(mk_str_doc(view_name_string(cell.name), a), &head);
            push_ptr(mv_style_doc(field_style, mv_cat_doc(head, a), a), &fnodes);
            push_ptr(mv_nest_doc(2, pretty_rune_expr(get_expr(cell.pattern.body, pool), pool, a), a), &fnodes);
            push_ptr(mv_nest_doc(2, mv_group_doc(mk_paren_doc("[", "]", mv_sep_doc(fnodes, a), a), a), a), &nodes);
        }
        return mv_group_doc(mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a), a);
    }
    case ECoCase: {
        PtrArray nodes = mk_ptr_array(1 + expr.cocase.fields.len, a);
        push_ptr(mv_style_doc(former_style, mv_cstr_doc("corecord", a), a), &nodes);
        for (size_t i = 0; i < expr.cocase.fields.len; i++) {
            PatternCell cell = get_expr_ptn_map_elt(expr.cocase.fields, i, pool);
            PtrArray fnodes = mk_ptr_array(2, a);
            PtrArray head =  mk_ptr_array(2, a);
            push_ptr(mv_cstr_doc(".", a), &head);
            push_ptr(mk_str_doc(view_name_string(cell.name), a), &head);
            push_ptr(mv_style_doc(field_style, mv_cat_doc(head, a), a), &fnodes);
            push_ptr(mv_nest_doc(2, pretty_rune_expr(get_expr(cell.pattern.body, pool), pool, a), a), &fnodes);
            push_ptr(mv_nest_doc(2, mv_group_doc(mk_paren_doc("[", "]", mv_sep_doc(fnodes, a), a), a), a), &nodes);
        }
        return mv_group_doc(mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a), a);
    }
    case EProject: {
        panic(mv_string("TODO: implement pretty project"));
        break;
    }
    case ERecur: {
        panic(mv_string("TODO: implement pretty recur"));
        break;
    }
    case EDataType: {
        panic(mv_string("TODO: implement pretty data type."));
    }
    case ECoDataType: {
        panic(mv_string("TODO: implement pretty codata (corecord) type."));
    }
    case EVal: {
        panic(mv_string("TODO: implement pretty val"));
        break;
    }
    case EInt: {
        panic(mv_string("TODO: implement pretty int"));
        break;
    }

    case EString:
        out = mk_paren_doc("\"", "\"", mv_str_doc(expr.string, a), a);
        out = mv_style_doc(const_style, out, a);
        break;
    case EList: {
        PtrArray nodes = mk_ptr_array(expr.list.len + 1, a);
        push_ptr(mv_style_doc(former_style, mv_cstr_doc("list", a), a), &nodes);
        for (size_t i = 0; i < expr.list.len; i++) {
            ExprRef elt = get_expr_elt(expr.list, i);
            push_ptr(mv_nest_doc(2, pretty_rune_expr(get_expr(elt, pool), pool, a), a), &nodes);
        }
        out = mv_group_doc(mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a), a);
        break;
    }
    }
    if (out == NULL) {
        panic(mv_string("Invalid expression provided to pretty_rune_expr"));
    }
    out = mv_group_doc(out, a);
    return out;
}
