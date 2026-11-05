#ifndef __RUNE_SYNTAX_RICH_EXPRESSION_H
#define __RUNE_SYNTAX_RICH_EXPRESSION_H

#include <stdint.h>
#include "data/option.h"

#include "rune/syntax/shared.h"
#include "pico/values/values.h"

typedef enum {
  REVar,
  REFn,
  REApp,
  RECtor,
  RECase,
  RECoRecord,
  REProject,
  RERecur,
  RERecord,
  RECoData,

  // Types
  REDataType,
  RECoDataType,
  RERecordType,
  RECoRecordType,

  // Embedded Values
  REVal,

  REInt,
  REString,
  REList,
} RExprType;

typedef struct {
    uint32_t val;
} RExprRef;

typedef struct {
    uint32_t start;
    uint32_t len;
} RExprSlice;

typedef struct {
    uint32_t start;
    uint32_t len;
} RExprMap;

typedef struct {
    uint32_t start;
    uint32_t len;
} RPatternMap;
OPTION_TYPE(RExprRef, RExpr);

typedef struct {
    NameArray args;
    RExprRef body;
} RExprFn;

typedef struct {
    RExprRef fn;
    RExprSlice args;
} RExprApp;

typedef struct {
    Name name;
    RExprOption type;
} RExprCtor;

typedef struct {
    NameArray args;
    RExprRef body;
} RPattern;

typedef struct {
    RExprRef val;
    RPatternMap fields;
} RExprCase;

typedef struct {
    RExprOption type;
    RExprMap fields;
} RExprRecord; // 

typedef struct {
    RExprOption type;
    RPatternMap fields;
} RExprCoRecord; // 

typedef struct {
    RExprRef from;
    Name field;
    RExprSlice args;
} RExprProject;

typedef struct {
    RExprOption type;
    RPatternMap fields;
} RExprFix; // 

typedef struct {
    RExprType type;
    union {
        Name var;

        RExprFn fn;
        RExprApp app;
        RExprCtor ctor;
        RExprCase ecase;
        RExprCoRecord corecord;
        RExprRecord record;
        RExprFix fix;

        RExprRecord record_ty;

        // Embedded value
        ValRef value;

        // Literals 
        int64_t num;
        RExprSlice list;
        String string;
    };
} RExpr;

typedef struct RExprPool RExprPool;

RExprPool* mk_rexpr_pool(Allocator* gpa);
void reset_rexpr_pool(RExprPool* pool);
void delete_rexpr_pool(RExprPool* pool);

RExprRef new_rexpr(RExprPool* pool);
void set_rexpr(RExprRef ref, RExpr expr, RExprPool* pool);
RExpr get_rexpr(RExprRef ref, RExprPool* pool);

RExprSlice new_rexpr_slice(uint32_t len, RExprPool* pool);
void set_rexpr_elt(RExprSlice slice, uint32_t idx, RExpr expr, RExprPool* pool);
RExprRef get_rexpr_elt(RExprSlice slice, uint32_t idx);

typedef struct {
    Name name;
    RExpr expr;
} RExprCell;
RExprMap new_rexpr_map(uint32_t len, RExprPool* pool);
void set_rexpr_map_elt(RExprMap map, size_t idx, RExprCell cell, RExprPool* pool);
RExprCell get_rexpr_map_elt(RExprMap map, size_t idx, RExprPool* pool);

typedef struct {
    Name name;
    RPattern pattern;
} RPatternCell;
RPatternMap new_rpat_map(uint32_t len, RExprPool* pool);
void set_rpat_map_elt(RPatternMap map, size_t idx, RPatternCell cell, RExprPool* pool);
RPatternCell get_rpat_map_elt(RPatternMap map, size_t idx, RExprPool* pool);

typedef struct {
    Name name;
    RExprRef expr;
} RDef;

//NameArray free_vars(RExprRef ref, RExprPool* pool, Allocator* a);

Document* pretty_rune_rexpr(RExprRef ref, RExprPool* pool, Allocator* a);

#endif
