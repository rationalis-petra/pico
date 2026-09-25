#ifndef __RUNE_SYNTAX_EXPRESSION_H
#define __RUNE_SYNTAX_EXPRESSION_H

#include <stdint.h>
#include "data/option.h"
#include "data/meta/array_header.h"

#include "pico/values/values.h"

typedef enum {
    EHost,
    EFn,
    EApp,
    EVar,
    ETarget,
    EString,
    EList,
} ExprType;

typedef struct {
    uint32_t val;
} ExprRef;

typedef struct {
    uint32_t ref;
} HostRef;

ARRAY_HEADER(ExprRef, expr, Expr);

typedef struct {
    NameArray args;
    ExprRef body;
} ExprFn;

typedef struct {
    ExprRef fn;
    ExprArray args;
} ExprApp;

typedef struct {
    ExprType type;
    union {
        HostRef host;
        ExprFn fn;
        ExprApp app;
        Name var;
        ExprArray list;
    };
} Expr;
OPTION_TYPE(ExprRef, Expr);

typedef struct ExprPool ExprPool;

ExprPool* mk_expr_pool(size_t HostSize, Allocator* gpa);

ExprRef new_expr(ExprPool* pool);
void set_expr(ExprRef ref, Expr expr, ExprPool* pool);
Expr get_expr(ExprRef ref, ExprPool* pool);

HostRef new_host(ExprPool* pool);
void set_host(HostRef ref, void* host, ExprPool* pool);
void get_host(HostRef ref, ExprPool* pool, void* host_out);

typedef struct {
    Name name;
    ExprRef expr;
} Def;

#endif
