#ifndef __RUNE_SYNTAX_EXPRESSION_H
#define __RUNE_SYNTAX_EXPRESSION_H

#include <stdint.h>
#include "data/option.h"

#include "pico/values/values.h"

typedef enum {
    EVar,
    EFn,
    EApp,
    ECtor,
    EMatch,
    ERecord,
    ECoRecord,
    EProject,
    ECoRecur,

    // Embedded Values
    EVal,

    EInt,
    EString,
    EList,
} ExprType;

typedef struct {
    uint32_t val;
} ExprRef;

typedef struct {
    uintptr_t ref;
} ValRef;

typedef struct {
    uint32_t start;
    uint32_t len;
} ExprSlice;
typedef struct {
    uint32_t start;
    uint32_t len;
} NameExprMap;
OPTION_TYPE(ExprRef, Expr);

typedef struct {
    NameArray args;
    ExprRef body;
} ExprFn;

typedef struct {
    ExprRef fn;
    ExprSlice args;
} ExprApp;

typedef struct {
    Name name;
    ExprOption type;
} ExprCtor;

typedef struct {
} ExprMatch; // match/recur

typedef struct {
} ExprCoRecur; // Create

typedef struct {
} ExprLet;

typedef struct {
    ExprOption type;
    NameExprMap fields;
} ExprRecord; // 

typedef struct {
    ExprRef from;
    Name field;
} ExprProject;

typedef struct {
    ExprType type;
    union {
        Name var;

        ExprFn fn;
        ExprApp app;
        ExprCtor ctor;
        ExprRecord record;

        // Embedded value
        ValRef value;

        // Literals 
        int64_t num;
        ExprSlice list;
        String string;
    };
} Expr;

typedef struct ExprPool ExprPool;

ExprPool* mk_expr_pool(Allocator* gpa);
void delete_expr_pool(ExprPool* );

ExprRef new_expr(ExprPool* pool);
void set_expr(ExprRef ref, Expr expr, ExprPool* pool);
Expr get_expr(ExprRef ref, ExprPool* pool);

ExprSlice new_expr_slice(uint32_t len, ExprPool* pool);
void set_expr_elt(ExprSlice slice, uint32_t idx, Expr expr, ExprPool* pool);
ExprRef get_expr_elt(ExprSlice slice, uint32_t idx);

typedef struct {
    Name name;
    ExprRef val;
} NameExprCell;
NameExprMap new_expr_map(uint32_t len, ExprPool* pool);
void set_expr_map_elt(NameExprMap map, size_t idx, NameExprCell cell, ExprPool* pool);
NameExprCell get_expr_map_elt(NameExprMap map, size_t idx, ExprPool* pool);

typedef struct {
    Name name;
    ExprRef expr;
} Def;

NameArray free_vars(ExprRef ref, ExprPool* pool, Allocator* a);

Document* pretty_rune_expr(ExprRef ref, ExprPool* pool, Allocator* a);


#endif
