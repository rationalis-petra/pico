#include "data/meta/array_header.h"
#include "data/meta/array_impl.h"

#include "rune/syntax/expression.h"

ARRAY_HEADER(Expr, expr_val, ExprVal);

ARRAY_COMMON_IMPL(Expr, expr_val, ExprVal);
ARRAY_COMMON_IMPL(ExprRef, expr, Expr);

struct ExprPool {
    size_t host_size;
    ExprValArray backing_array;
    void* host_data;
    size_t host_len;
    size_t host_capacity;
};

ExprPool* mk_expr_pool(size_t host_size, Allocator* gpa) {
    ExprPool* out = mem_alloc(sizeof(ExprPool), gpa);   
    *out = (ExprPool) {
        .host_size = host_size,
        .backing_array = mk_expr_val_array(128, gpa),
        .host_data = mem_alloc(64 * host_size, gpa),
        .host_len = 0,
        .host_capacity = 64,
    };
    return out;
}

ExprRef new_expr(ExprPool* pool) {
    ExprRef out = {
        .val = pool->backing_array.len,
    };
    Expr empty = {};
    push_expr_val(empty, &pool->backing_array);
    return out;
}

void set_expr(ExprRef ref, Expr expr, ExprPool* pool) {
    // TODO: debug bounds check.
    pool->backing_array.data[ref.val] = expr;
}

Expr get_expr(ExprRef ref, ExprPool* pool) {
    // TODO: debug bounds check.
    return pool->backing_array.data[ref.val];
}

HostRef new_host(ExprPool* pool) {
    HostRef out = {.ref = pool->host_len};
    if (pool->host_len == pool->host_capacity) {
        pool->host_capacity *= 2;
        mem_realloc(pool->host_data, pool->host_capacity, &pool->backing_array.gpa);
    }
    pool->host_len++;
    return out;
}

void set_host(HostRef ref, void* host, ExprPool* pool) {
    void* dest = pool->host_data + (pool->host_size * ref.ref);
    memcpy(dest, host, pool->host_size);
}

void get_host(HostRef ref, ExprPool* pool, void* host_out) {
    void* src = pool->host_data + (pool->host_size * ref.ref);
    memcpy(host_out, src, pool->host_size);
}
