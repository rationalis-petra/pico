#ifndef __RUNE_SYNTAX_EXPRESSION_H
#define __RUNE_SYNTAX_EXPRESSION_H

#include <stdint.h>
#include "data/option.h"

#include "pico/values/values.h"

#include "rune/syntax/shared.h"

typedef enum {
  EVar,
  // Dependent Product (Function)
  EFn,
  EApp,
  // Dependent Sum (Pair)
  EPr,
  EElt,
  ECtor,
  ERecord,
  ECase,
  ECoCase,
  EProject,
  ERecur,

  // Types
  EDataType,
  ECoDataType,

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
    uint32_t start;
    uint32_t len;
} ExprSlice;

typedef struct {
    uint32_t start;
    uint32_t len;
} AlgebraicTypeSpec;

typedef struct {
    uint32_t start;
    uint32_t len;
} PatternMap;

typedef struct {
    uint32_t start;
    uint32_t len;
} ExprMap;

typedef struct {
    uint32_t start;
    uint32_t len;
} ExprNames;

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
    NameArray args;
    ExprRef body;
} Pattern;

typedef struct {
    ExprRef val;
    PatternMap fields;
} ExprCase;

typedef struct {
    ExprRef type;
    ExprMap fields;
} ExprRecord;

typedef struct {
    ExprOption type;
    PatternMap fields;
} ExprCoCase;

typedef struct {
    ExprRef from;
    Name field;
    ExprSlice args;
} ExprProject;

typedef struct {
    ExprOption type;
    PatternMap fields;
} ExprFix;

// Types
typedef struct {
    Name name;
    ExprOption type;
    bool is_record;
    ExprNames names;
    AlgebraicTypeSpec constructors;
} ExprDataType;

typedef struct {
    ExprOption type;
    PatternMap fields;
} ExprCoRecordType;

typedef struct {
    ExprType type;
    union {
        Name var;

        ExprFn fn;
        ExprApp app;
        ExprCtor ctor;
        ExprCase ecase;
        ExprRecord record;
        ExprCoCase cocase;
        ExprProject project;
        ExprFix fix;

        ExprDataType data_ty;
        ExprCoRecordType corecord_ty;

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

ExprNames new_expr_names(size_t len, ExprPool* pool);
void set_expr_name(ExprNames names, uint32_t idx, Name name, ExprPool* pool);
Name get_expr_name(ExprNames names, uint32_t idx, ExprPool* pool);

ExprSlice new_expr_slice(uint32_t len, ExprPool* pool);
void set_expr_elt(ExprSlice slice, uint32_t idx, Expr expr, ExprPool* pool);
ExprRef get_expr_elt(ExprSlice slice, uint32_t idx);

typedef struct {
    Name name;
    Expr expr;
} ExprCell;
ExprMap new_expr_map(uint32_t len, ExprPool* pool);
void set_expr_map_elt(ExprMap map, size_t idx, ExprCell cell, ExprPool* pool);
ExprCell get_expr_map_elt(ExprMap map, size_t idx, ExprPool* pool);

typedef struct {
    Name name;
    Pattern pattern;
} PatternCell;
PatternMap new_expr_ptn_map(uint32_t len, ExprPool* pool);
void set_expr_ptn_map_elt(PatternMap map, size_t idx, PatternCell cell, ExprPool* pool);
PatternCell get_expr_ptn_map_elt(PatternMap map, size_t idx, ExprPool* pool);

typedef struct {
  Name name;
  ExprMap args;
  ExprRef type;
} AlgebraOp;
AlgebraicTypeSpec new_alg_spec(uint32_t len, ExprPool* pool);
void set_alg_spec_op(AlgebraicTypeSpec spec, size_t idx, AlgebraOp cell, ExprPool* pool);
AlgebraOp get_alg_spec_op(AlgebraicTypeSpec spec, size_t idx, ExprPool* pool);

typedef struct {
    Name name;
    ExprRef expr;
} Def;

NameArray free_vars(Expr expr, ExprPool* pool, Allocator* a);

Document* pretty_rune_expr(Expr expr, ExprPool* pool, Allocator* a);

#endif
