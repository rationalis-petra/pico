#ifndef __ATLAS_SYNTAX_STANZA_H
#define __ATLAS_SYNTAX_STANZA_H

#include "components/pretty/document.h"

#include "rune/syntax/expression.h"

typedef enum {
    Executable, Library, General
} TargetType;

typedef struct {
    ExprRef filename;
    ExprRef entry_point;
} TEExecutable;

typedef struct {
    ExprOption filename;
    ExprArray submodules;
} TELibrary;

typedef struct {
    ExprArray provides;
    ExprRef run;
} TEGeneral;

typedef struct {
    TargetType type;
    ExprArray dependencies;
    union {
        TEExecutable executable;
        TELibrary library;
        TEGeneral general; 
    };
} TargetExpr;

Document* pretty_target_expr(TargetExpr expr, Allocator* a);

#endif
