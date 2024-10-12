#ifndef __PICO_SYNTAX_SYNTAX_H
#define __PICO_SYNTAX_SYNTAX_H

#include "memory/allocator.h"

#include "pico/data/sym_ptr_assoc.h"
#include "pico/data/sym_ptr_amap.h"
#include "pico/values/values.h"
#include "pico/values/types.h"

/* A syntax tree has the following grammar
 * E = call <field> <function> <argument>
 *   | value <value>
 *   | var <location>
 *   | proj <structure> <field list>
 *   | structure <field assignments>
 *   | constructor <values>
 *   | recursor <bind>? <values> <match expressions>
 *
 * A toplevel term has the form:
 * TL = Expr 
 *    | Def <symbol> <value>
 *    | Ann <symbol> <value>
 */

typedef enum {
    // Atoms
    SLitI64,
    SLitBool,
    SVariable,

    // Terms & term formers
    SProcedure,
    SAll,
    SApplication,
    SConstructor,
    SVariant,
    SMatch,
    SStructure,
    SProjector,

    // Helpers/convenience
    SLet,
    SIf,

    // Types & Type formers
    SProcType,
    SStructType,
    SEnumType,
    SForallType,
    SExistsType,
    STypeFamily,

    SCheckedType,

    SAnnotation,
} Syntax_t;


typedef struct Syntax Syntax;
typedef PtrArray SynArray;
typedef PtrArray ClauseArray;
typedef SymPtrAMap SymSynAMap;

typedef struct {
    SymPtrAssoc args;
    Syntax* body;
} SynProcedure;

typedef struct {
    SymbolArray args;
    Syntax* body;
} SynAll;

typedef struct {
    Syntax* function;
    SynArray args;
} SynApp;

typedef struct {
    Syntax* enum_type;
    Symbol tagname;
    size_t tag;
} SynConstructor;

typedef struct {
    Syntax* enum_type;
    Symbol tagname;
    size_t tag;
    SynArray args;
} SynVariant;

typedef struct {
    Symbol tagname;
    size_t tag;
    SymbolArray vars;
    Syntax* body;
} SynClause;

typedef struct {
    Symbol recfn;
    Syntax* val;
    ClauseArray clauses;
} SynMatch;

typedef struct {
    Syntax* ptype;
    SymSynAMap fields;
} SynStructure;

typedef struct {
    Symbol field;
    Syntax* val;
} SynProjector;

// Sugaring Syntax
typedef struct {
    SymSynAMap bindings;
    Syntax* body;
} SynLet;

typedef struct {
    Syntax* condition;
    Syntax* true_branch;
    Syntax* false_branch;
} SynIf;

// Types
typedef struct {
    SymSynAMap bindings;
    Syntax* body;
} SynBind;

typedef struct {
    PtrArray args;
    Syntax* return_type;
} SynProcType;

typedef struct {
    SymSynAMap fields;
} SynStructType;

typedef struct {
    SymPtrAMap variants;
} SynEnumType;

typedef struct {
    Syntax* val;
    Syntax* type;
} SynAnnotation;


struct Syntax {
    Syntax_t type;
    union {
        int64_t lit_i64;
        Symbol variable;

        SynProcedure procedure;
        SynAll all;
        SynApp application;
        SynConstructor constructor;
        SynVariant variant;
        SynMatch match;
        SynStructure structure;
        SynProjector projector;

        SynLet let_expr;
        SynIf if_expr;

        SynProcType proc_type;
        SynStructType struct_type;
        SynEnumType enum_type;
        SynBind bind_type;
        PiType* type_val;

        SynAnnotation annotation;
    };
    PiType* ptype;
};


/* The Syntax Destructor */
void delete_syntax(Syntax syntax, Allocator* a);
void delete_syntax_pointer(Syntax* syntax, Allocator* a);


/* Other instances */
Document* pretty_syntax(Syntax* syntax, Allocator* a);

// -----------------------------------------------------------------------------
//   Toplevel
// -----------------------------------------------------------------------------

typedef enum {
    TLDef,
    TLExpr,
} TopLevel_t;

typedef struct {
    Symbol bind;
    Syntax* value;
} Definition;

typedef struct {
    TopLevel_t type;
    union {
        Definition def;
        Syntax* expr;
    };
} TopLevel;

void delete_def(Definition def, Allocator* a);
void delete_toplevel(TopLevel top, Allocator* a);

Document* pretty_def(Definition* def, Allocator* a);
Document* pretty_toplevel(TopLevel* TopLevel, Allocator* a);

PiType* toplevel_type(TopLevel top);

#endif
