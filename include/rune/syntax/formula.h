#ifndef __RUNE_SYNTAX_FORMULA_H
#define __RUNE_SYNTAX_FORMULA_H

/**
 * This file contains type definitions for unification formulas (in Rune).
 * 
 */

#include <stdint.h>
#include "data/option.h"

#include "pico/values/values.h"

#include "rune/syntax/expression.h"
#include "rune/eval/values.h"

typedef enum {
    // Equality and type inhabitation constraints
  SEq, SHab,
} SingleConstraintSort;

typedef struct {
    SingleConstraintSort sort;
    union {
        ExprRef lhs;
        ExprRef val;
    };
    union {
        ExprRef rhs;
        ExprRef type;
    };
} SingleConstraint;

typedef enum {
  Exists, Forall,
} Quant;

typedef enum {
    FConj, FAnd, FQuant,
} FormulaSort;

typedef struct {uint64_t ref;} FormulaRef;

SLICE_TYPE(SingleConstraint, SingleConstraint);

typedef struct {
    FormulaRef lhs;
    FormulaRef rhs;
} AndFormula;

typedef struct {
    Quant quantifier;
    Name name;
    FormulaRef body;
} QuantFormula;

typedef struct {
    FormulaSort sort;
    union {
        SingleConstraintSlice conj;
        AndFormula and;
        QuantFormula quant;
    };
} Formula;

typedef struct FormulaPool FormulaPool;

FormulaPool* mk_formula_pool(Allocator* a);
void delete_formula_pool(FormulaPool* pool);

FormulaRef new_formula(FormulaPool* pool);
Formula get_formula(FormulaRef ref, FormulaPool* pool);

Document* pretty_rune_formula(Formula formula, FormulaPool* fpool, ExprPool* epool, Allocator* a);

Document* pretty_single_constraint(SingleConstraint constraint, ExprPool* epool, Allocator* a);

#endif
