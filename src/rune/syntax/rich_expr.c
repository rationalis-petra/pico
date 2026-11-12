#include "data/meta/slice_header.h"

#include "platform/signals.h"

#include "rune/syntax/rich_expr.h"

typedef struct {
    RExpr* data;
    size_t len;
    size_t capacity;
} RExprBacking;

typedef struct {
    RExprCell* data;
    size_t len;
    size_t capacity;
} RExprMapBacking;

typedef struct {
    RPatternCell* data;
    size_t len;
    size_t capacity;
} RPatternMapBacking;


struct RExprPool {
    RExprBacking expressions;
    RExprMapBacking named_expressions;
    RPatternMapBacking patterns;
    Allocator gpa;
};

RExprPool* mk_rexpr_pool(Allocator* gpa) {
    RExprPool* out = mem_alloc(sizeof(RExprPool), gpa);   
    const size_t start_len = 128;
    *out = (RExprPool) {
        .expressions = {
            .len = 0,
            .capacity = start_len,
            .data = mem_alloc(start_len * sizeof(RExpr), gpa),
        },
        .named_expressions = {
            .len = 0,
            .capacity = start_len,
            .data = mem_alloc(start_len * sizeof(RExprCell), gpa),
        },
        .patterns = {
            .len = 0,
            .capacity = start_len,
            .data = mem_alloc(start_len * sizeof(RPatternCell), gpa),
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

void reset_rexpr_pool(RExprPool* pool) {
    pool->expressions.len = 0;
    pool->named_expressions.len = 0;
    pool->patterns.len = 0;
}

void delete_rexpr_pool(RExprPool* pool) {
    Allocator a = pool->gpa;
    mem_free(pool->expressions.data, &a);
    mem_free(pool->named_expressions.data, &a);
    mem_free(pool->patterns.data, &a);
    mem_free(pool, &a);
}

RExprRef new_rexpr(RExprPool* pool) {
    RExprRef out = {pool->expressions.len};
    GenericSlice slice = {
        .data = pool->expressions.data,
        .len = pool->expressions.len,
        .capacity = pool->expressions.capacity,
    };
    slice = grow_slice(slice, 1, sizeof(RExpr), &pool->gpa);
    pool->expressions = (RExprBacking) {
        .data = slice.data,
        .len = slice.len,
        .capacity = slice.capacity,
    };
    return out;
}

void set_rexpr(RExprRef ref, RExpr expr, RExprPool* pool) {
    // TODO: debug bounds check.
    pool->expressions.data[ref.val] = expr;
}

RExpr get_rexpr(RExprRef ref, RExprPool* pool) {
    // TODO: debug bounds check.
    return pool->expressions.data[ref.val];
}

RExprSlice new_rexpr_slice(uint32_t len, RExprPool* pool) {
    RExprSlice out = {.start = pool->expressions.len, .len = len};
    GenericSlice slice = {
        .data = pool->expressions.data,
        .len = pool->expressions.len,
        .capacity = pool->expressions.capacity,
    };
    slice = grow_slice(slice, len, sizeof(RExpr), &pool->gpa);
    pool->expressions = (RExprBacking) {
        .data = slice.data,
        .len = slice.len,
        .capacity = slice.capacity,
    };
    return out;
}


void set_rexpr_elt(RExprSlice slice, uint32_t idx, RExpr expr, RExprPool* pool) {
    // TODO: bounds check in debug mode
    pool->expressions.data[idx + slice.start] = expr;
}

RExprRef get_rexpr_elt(RExprSlice slice, uint32_t idx) {
    return (RExprRef) {idx + slice.start};
}

RPatternMap new_rpat_map(uint32_t len, RExprPool* pool) {
    RPatternMap out = {.start = pool->named_expressions.len, .len = len};
    GenericSlice slice = {
        .data = pool->named_expressions.data,
        .len = pool->named_expressions.len,
        .capacity = pool->named_expressions.capacity,
    };
    slice = grow_slice(slice, len, sizeof(RPatternMap), &pool->gpa);
    pool->patterns = (RPatternMapBacking) {
        .data = slice.data,
        .len = slice.len,
        .capacity = slice.capacity,
    };
    return out;
}

void set_rpat_map_elt(RPatternMap map, size_t idx, RPatternCell cell, RExprPool* pool) {
    pool->patterns.data[idx + map.start] = cell;
}

RPatternCell get_rpat_map_elt(RPatternMap map, size_t idx, RExprPool* pool) {
    return pool->patterns.data[idx + map.start];
}

RExprMap new_rexpr_map(uint32_t len, RExprPool* pool) {
    RExprMap out = {.start = pool->named_expressions.len, .len = len};
    GenericSlice slice = {
        .data = pool->named_expressions.data,
        .len = pool->named_expressions.len,
        .capacity = pool->named_expressions.capacity,
    };
    slice = grow_slice(slice, len, sizeof(RExprMap), &pool->gpa);
    pool->named_expressions = (RExprMapBacking) {
        .data = slice.data,
        .len = slice.len,
        .capacity = slice.capacity,
    };
    return out;
}

void set_rexpr_map_elt(RExprMap map, size_t idx, RExprCell cell, RExprPool* pool) {
    pool->named_expressions.data[idx + map.start] = cell;
}

RExprCell get_rexpr_map_elt(RExprMap map, size_t idx, RExprPool* pool) {
    return pool->named_expressions.data[idx + map.start];
}


Document* pretty_rune_rexpr(RExprRef ref, RExprPool* pool, Allocator* a) {
    DocStyle former_style = scolour(colour(60, 190, 24), dstyle);
    //DocStyle ty_former_style = scolour(colour(209, 118, 219), dstyle);
    DocStyle field_style = scolour(colour(60, 120, 210), dstyle);
    DocStyle const_style = scolour(colour(120, 170, 210), dstyle);
    DocStyle var_style = scolour(colour(212, 130, 42), dstyle);

    Document* out = NULL;

    RExpr expr = get_rexpr(ref, pool);
    switch (expr.type) {
    case REVar:
        out = mv_style_doc(var_style, mv_str_doc(view_name_string(expr.var), a), a);
        break;
    case REFn: {
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
        push_ptr(mv_nest_doc(2, pretty_rune_rexpr(expr.fn.body, pool, a), a), &nodes);
        out = mk_paren_doc("(", ")", mv_group_doc(mv_sep_doc(nodes, a), a), a);
        break;
    }
    case REApp: {
        PtrArray nodes = mk_ptr_array(expr.app.args.len + 1, a);
        push_ptr(pretty_rune_rexpr(expr.app.fn, pool, a), &nodes);
        for (size_t i = 0; i < expr.app.args.len; i++) {
            push_ptr(mv_nest_doc(2, pretty_rune_rexpr(get_rexpr_elt(expr.app.args, i), pool, a), a), &nodes);
        }
        out = mv_group_doc(mk_paren_doc("(", ")", mv_group_doc(mv_sep_doc(nodes, a), a), a), a);
        break;
    }
    case RECtor: {
        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(mv_cstr_doc(":", a), &nodes);
        push_ptr(mk_str_doc(view_name_string(expr.ctor.name), a), &nodes);
        out = mv_style_doc(field_style, mv_cat_doc(nodes, a), a);
        break;
    }
    case RECase: {
        PtrArray nodes = mk_ptr_array(1 + expr.ecase.fields.len, a);
        PtrArray head_nodes = mk_ptr_array(2, a);
        push_ptr(mv_style_doc(former_style, mv_cstr_doc("case", a), a), &head_nodes);
        push_ptr(mv_nest_doc(2, pretty_rune_rexpr(expr.ecase.val, pool, a), a), &head_nodes);
        push_ptr(mv_sep_doc(head_nodes, a), &nodes);
        for (size_t i = 0; i < expr.ecase.fields.len; i++) {
            RPatternCell cell = get_rpat_map_elt(expr.ecase.fields, i, pool);
            PtrArray fnodes = mk_ptr_array(2, a);
            PtrArray head =  mk_ptr_array(2, a);
            push_ptr(mv_cstr_doc(".", a), &head);
            push_ptr(mk_str_doc(view_name_string(cell.name), a), &head);
            push_ptr(mv_style_doc(field_style, mv_cat_doc(head, a), a), &fnodes);
            push_ptr(mv_nest_doc(2, pretty_rune_rexpr(cell.pattern.body, pool, a), a), &fnodes);
            push_ptr(mv_nest_doc(2, mv_group_doc(mk_paren_doc("[", "]", mv_sep_doc(fnodes, a), a), a), a), &nodes);
        }
        return mv_group_doc(mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a), a);
        break;
    }
    case RECoRecord: {
        PtrArray nodes = mk_ptr_array(1 + expr.corecord.fields.len, a);
        push_ptr(mv_style_doc(former_style, mv_cstr_doc("corecord", a), a), &nodes);
        for (size_t i = 0; i < expr.corecord.fields.len; i++) {
            RPatternCell cell = get_rpat_map_elt(expr.corecord.fields, i, pool);
            PtrArray fnodes = mk_ptr_array(2, a);
            PtrArray head =  mk_ptr_array(2, a);
            push_ptr(mv_cstr_doc(".", a), &head);
            push_ptr(mk_str_doc(view_name_string(cell.name), a), &head);
            push_ptr(mv_style_doc(field_style, mv_cat_doc(head, a), a), &fnodes);
            push_ptr(mv_nest_doc(2, pretty_rune_rexpr(cell.pattern.body, pool, a), a), &fnodes);
            push_ptr(mv_nest_doc(2, mv_group_doc(mk_paren_doc("[", "]", mv_sep_doc(fnodes, a), a), a), a), &nodes);
        }
        return mv_group_doc(mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a), a);
    }
    case REProject: {
        panic(mv_string("TODO: implement pretty project"));
        break;
    }
    case RERecur: {
        panic(mv_string("TODO: implement pretty recur"));
        break;
    }
    case RERecord: {
        panic(mv_string("TODO: implement pretty record"));
        break;
    }
    case RECoData: {
        panic(mv_string("TODO: implement pretty codata"));
        break;
    }
        // Types
    case REDataType: {
        panic(mv_string("TODO: implement pretty data type"));
        break;
    }
    case RECoDataType: {
        panic(mv_string("TODO: implement pretty codata type"));
        break;
    }
    case RERecordType: {
        panic(mv_string("TODO: implement pretty record type"));
        break;
    }
    case RECoRecordType: {
        panic(mv_string("TODO: implement pretty corecord type"));
        break;
    }
    case REVal: {
        panic(mv_string("TODO: implement pretty val"));
        break;
    }
    case REInt: {
        panic(mv_string("TODO: implement pretty int"));
        break;
    }

    case REString:
        out = mk_paren_doc("\"", "\"", mv_str_doc(expr.string, a), a);
        out = mv_style_doc(const_style, out, a);
        break;
    case REList: {
        PtrArray nodes = mk_ptr_array(expr.list.len + 1, a);
        push_ptr(mv_style_doc(former_style, mv_cstr_doc("list", a), a), &nodes);
        for (size_t i = 0; i < expr.list.len; i++) {
            RExprRef elt = get_rexpr_elt(expr.list, i);
            push_ptr(mv_nest_doc(2, pretty_rune_rexpr(elt, pool, a), a), &nodes);
        }
        out = mv_group_doc(mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a), a);
        break;
    }
    case REIf: {
        panic(mv_string("TODO: implement pretty do if"));
        break;
    }
    case REDoNotation: {
        panic(mv_string("TODO: implement pretty do notation"));
        break;
    }
    }
    if (out == NULL) {
        panic(mv_string("Invalid expression provided to pretty_rune_expr"));
    }
    out = mv_group_doc(out, a);
    return out;
}
