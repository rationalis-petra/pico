#ifndef __PICO_SYNTAX_SYNTAX_H
#define __PICO_SYNTAX_SYNTAX_H

#include "memory/allocator.h"
#include "pico/values/values.h"
#include "pico/values/types.h"

/* A syntax tree has the following grammar
 * E = call <field> <function> <argument>
 *   | value <value>
 *   | var <location>
 *   | proj <structure> <field list>
 *   | structure <field assignments>
 *   | constructor <values>
 *   | recursor <bind?> <values> <match expressions>
 *   | destructor <values>
 *   | corecursor <bind> <values> <destructor assignments>
 *
 * Note on contexts, environments & vars:
 *  When converting from a raw tree to a syntax tree, symbols will be replaced with locations.
 *  A location will indicate that a variable is either in a local environment, or in a module,
 *  and will contain a payload that allows it to be looked up in either. 
 * 
 *  With this in mind, it is worth noting that an Environment is a mapping of locations to values,
 *  while a context is a mapping of symbols to locations.
 * 
 */

// Use visitor pattern for tree-walkers

typedef enum SyntaxType {
    SLiteral,
    SVariable,
    SFunction,
    SApplication,
    SConstructor,
    SRecursor,
    SDestructor,
    SCorecursor,
    SStructure,
    SProjector,

    SLet,
    SIf,
} SyntaxType;


typedef struct syntax syntax;
typedef ptr_array syn_array;
typedef ptr_array ptn_array;
typedef ptr_array coptn_array;
typedef sym_ptr_amap sym_syn_amap;

typedef struct syn_function {
    symbol_array args;
    syntax* body;
} syn_function;

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

typedef struct syn_destructor {
    pi_symbol name;
    syntax* value;
} syn_destructor;

typedef struct syn_copattern {
    pi_symbol name;
    symbol_array vars;
    syntax* body;
} copattern;

typedef struct syn_corecursor {
    pi_symbol recfn;
    syn_array vals;
    coptn_array functions;
} syn_corecursor;

typedef struct syn_structure {
    sym_syn_amap fields;
} syn_structure;

typedef struct syn_projector {
    symbol_array fields;
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


struct syntax {
    SyntaxType type;
    union {
        int64_t lit_i64;
        pi_symbol variable;

        syn_function function;
        syn_app application;
        syn_constructor constructor;
        syn_recursor recursor;
        syn_destructor destructor;
        syn_corecursor corecursor;
        syn_structure structure;
        syn_projector projector;

        syn_let let_expr;
        syn_if if_expr;
    } data;
    pi_type* ptype;
};


/* The Syntax Destructor */
void delete_syntax(syntax syntax, allocator a);
void delete_syntax_pointer(syntax* syntax, allocator a);


/* Other instances */
document* pretty_syntax(syntax* syntax, allocator a);

#endif
