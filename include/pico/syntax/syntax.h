#ifndef __PICO_SYNTAX_SYNTAX_H
#define __PICO_SYNTAX_SYNTAX_H

#include "memory/allocator.h"

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

// Use visitor pattern for tree-walkers

typedef enum syntax_t {
    SLitI64,
    SLitBool,

    SType,

    SVariable,
    SProcedure,
    SApplication,
    SConstructor,
    SRecursor,
    SStructure,
    SProjector,

    SLet,
    SIf,
} syntax_t;


typedef struct syntax syntax;
typedef ptr_array syn_array;
typedef ptr_array ptn_array;
typedef sym_ptr_amap sym_syn_amap;

typedef struct syn_procedure {
    symbol_array args;
    syntax* body;
} syn_procedure;

typedef struct syn_app {
    syntax* function;
    syn_array args;
} syn_app;

typedef struct syn_constructor {
    pi_symbol name;
    syn_array args;
} syn_constructor;

typedef struct syn_recursor {
    pi_symbol recfn;
    syn_array vals;
    ptn_array clauses;
} syn_recursor;

typedef struct syn_structure {
    syntax* ptype;
    sym_syn_amap fields;
} syn_structure;

typedef struct syn_projector {
    pi_symbol field;
    syntax* val;
} syn_projector;

// Sugaring Syntax
typedef struct syn_let {
    sym_syn_amap bindings;
    syntax* body;
} syn_let;

typedef struct syn_if {
    syntax* condition;
    syntax* true_branch;
    syntax* false_branch;
} syn_if;


typedef struct syntax {
    syntax_t type;
    union {
        int64_t lit_i64;
        pi_symbol variable;
        pi_type* type_val;

        syn_procedure procedure;
        syn_app application;
        syn_constructor constructor;
        syn_recursor recursor;
        syn_structure structure;
        syn_projector projector;

        syn_let let_expr;
        syn_if if_expr;
    };
    pi_type* ptype;
} syntax;


/* The Syntax Destructor */
void delete_syntax(syntax syntax, allocator a);
void delete_syntax_pointer(syntax* syntax, allocator a);


/* Other instances */
document* pretty_syntax(syntax* syntax, allocator a);

// -----------------------------------------------------------------------------
//   Toplevel
// -----------------------------------------------------------------------------

typedef enum toplevel_t {
    TLDef,
    TLExpr,
} toplevel_t;

typedef struct definition {
    pi_symbol bind;
    syntax* value;
} definition;

typedef struct toplevel {
    toplevel_t type;
    union {
        definition def;
        syntax expr;
    };
} toplevel;

void delete_def(definition def, allocator a);
void delete_toplevel(toplevel top, allocator a);

document* pretty_def(definition* def, allocator a);
document* pretty_toplevel(toplevel* toplevel, allocator a);

pi_type* toplevel_type(toplevel top);

#endif
