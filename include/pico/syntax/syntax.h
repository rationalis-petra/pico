#ifndef __PICO_SYNTAX_SYNTAX_H
#define __PICO_SYNTAX_SYNTAX_H

#include "platform/memory/allocator.h"

#include "pico/data/range.h"
#include "pico/data/sym_ptr_assoc.h"
#include "pico/data/sym_ptr_amap.h"
#include "pico/data/symbol_array.h"
#include "pico/syntax/concrete.h"
#include "pico/syntax/header.h"
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
    SLitUntypedIntegral,
    SLitTypedIntegral,
    SLitUntypedFloating,
    SLitTypedFloating,
    SLitString,
    SLitBool,
    SLitUnit,
    SLitArray,
    SVariable,
    SAbsVariable,

    // Terms & term formers
    SProcedure,
    SAll,
    SMacro,
    SApplication,
    SAllApplication,
    SConstructor,
    SVariant,
    SMatch,
    SStructure,
    SProjector,
    SInstance,
    SDynamic,
    SDynamicUse,
    SDynamicSet,
    SDynamicLet,

    // Control Flow & Binding
    SLet,
    SIf,
    SLabels,
    SGoTo,
    SSequence,
    SWithReset,
    SResetTo,

    // Special
    SIs,
    SInTo,
    SOutOf,
    SName,
    SUnName,
    SWiden,
    SNarrow,
    SDynAlloc,
    SSizeOf,
    SAlignOf,
    SOffsetOf,
    SModule,

    // Types & Type formers
    SProcType,
    SStructType,
    SEnumType,
    SResetType,
    SDynamicType,
    SNamedType,
    SDistinctType,
    SOpaqueType,
    STraitType,
    SAllType,
    SExistsType,
    STypeFamily,
    SLiftCType,

    SCheckedType,

    SAnnotation,

    // Should be moved to macros!(?)
    SReinterpret,
    SConvert,

    // Meta/reflection
    STypeOf,
    SDescribe,
    SQuote,
    SCapture,
} Syntax_t;


typedef struct Syntax Syntax;
typedef PtrArray SynArray;
typedef PtrArray ClauseArray;
typedef SymPtrAMap SymSynAMap;

typedef struct {
    int64_t value;
    PrimType type;
} SynIntegralLiteral;

typedef struct {
    double value;
    PrimType type;
} SynFloatingLiteral;

typedef struct {
    U64Array shape;
    PtrArray subterms;
} SynArrayLiteral;

typedef struct {
    Symbol symbol;
    size_t index;
    void* value;
} AbsVariable;

typedef struct {
    SymPtrAssoc args;
    SymPtrAssoc implicits;
    Syntax* body;
} SynProcedure;

typedef struct {
    SymbolArray args;
    Syntax* body;
} SynAll;

typedef struct {
    Syntax* function;
    SynArray implicits;
    SynArray args;
} SynApp;

typedef struct {
    Syntax* function;
    SynArray types;
    SynArray implicits;
    SynArray args;
} SynAllApp;

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
    Syntax* base;
    SymSynAMap fields;
} SynStructure;

typedef struct {
    Symbol field;
    Syntax* val;
} SynProjector;

typedef struct {
    Syntax* dynamic;
    Syntax* new_val;
} SynDynSet;

typedef struct {
    SymbolArray params;
    SymPtrAssoc implicits;
    Syntax* constraint;
    SymSynAMap fields;
} SynInstance;

typedef struct {
    SymPtrAssoc args;
    Syntax* body;
} SynLabelBranch;

typedef struct {
    Syntax* entry;
    SymPtrAssoc terms;
} SynLabels;

typedef struct {
    Symbol label;
    PtrArray args;
} SynGoTo;

typedef struct {
    bool is_binding;
    Symbol symbol;
    Syntax* expr;
} SeqElt;

typedef struct {
    PtrArray elements;
} SynSequence;

typedef struct {
    Symbol point_sym;
    Syntax* expr;

    PiType* in_arg_ty;
    Symbol in_sym;

    PiType* cont_arg_ty;
    Symbol cont_sym;
    Syntax* handler;
} SynWithReset;

typedef struct {
    Syntax* point;
    Syntax* arg;
} SynResetTo;

// Sugaring Syntax
typedef struct {
    SymSynAMap bindings;
    Syntax* body;
} SynLet;

typedef struct {
    Syntax* var;
    Syntax* expr;
} DynBinding;

typedef struct {
    PtrArray bindings;
    Syntax* body;
} SynDynLet;

typedef struct {
    Syntax* condition;
    Syntax* true_branch;
    Syntax* false_branch;
} SynIf;

typedef struct {
    SymbolArray vars;
    SymPtrAMap fields;
} SynTrait;

//------------------------------------------------------------------------------
// Special
//------------------------------------------------------------------------------

typedef struct {
    Syntax* val;
    Syntax* type;
} SynIs;

typedef struct {
    Syntax* type;
} SynSize;

typedef struct {
    // imports
    // exports
    // body - terms
} SynModule;

// Types
typedef struct {
    SymbolArray bindings;
    Syntax* body;
} SynBind;

typedef struct {
    Symbol name;
    Syntax* body;
} SynName;

typedef struct {
    Symbol field;
    Syntax* body;
} SynOffsetOf;

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
    Syntax* in;
    Syntax* out;
} SynResetType;

typedef struct {
    bool from_native;
    Syntax* type;
    Syntax* body;
} SynReinterpret;

typedef struct {
    bool from_native;
    Syntax* type;
    Syntax* body;
} SynConvert;

typedef struct {
    PiType* type;
    void* value;
} SynCapture;

struct Syntax {
    Syntax_t type;
    union {
        SynIntegralLiteral integral;
        SynFloatingLiteral floating;
        SynArrayLiteral array_lit;
        bool boolean;
        String string;

        AbsVariable abvar;
        Symbol variable;

        SynProcedure procedure;
        SynAll all;
        Syntax* transformer;
        SynApp application;
        SynAllApp all_application;
        SynConstructor constructor;
        SynVariant variant;
        SynMatch match;
        SynStructure structure;
        SynProjector projector;
        SynInstance instance;

        Syntax* dynamic;
        SynDynSet dynamic_set;
        Syntax* use;
        SynDynLet dyn_let_expr;

        SynLet let_expr;
        SynIf if_expr;
        SynLabels labels;
        SynGoTo go_to;
        SynWithReset with_reset;
        SynResetTo reset_to;
        SynSequence sequence;

        SynIs is;
        SynIs into;
        SynIs out_of;
        SynIs name;
        Syntax* unname;
        SynIs widen;
        SynIs narrow;
        Syntax* size;
        SynOffsetOf offset_of;

        SynProcType proc_type;
        SynStructType struct_type;
        SynEnumType enum_type;
        SynResetType reset_type;
        Syntax* dynamic_type;
        SynBind bind_type;
        SynName named_type;
        Syntax* distinct_type;
        Syntax* opaque_type;
        SynTrait trait;
        PiType* type_val;
        Syntax* c_type;

        SynReinterpret reinterpret;
        SynConvert convert;

        // Metaprogramming
        Syntax* type_of;
        SymbolArray to_describe;
        RawTree quoted;
        SynCapture capture;
    };
    PiType* ptype;
    Range range;
};

/* Other instances */
Document* pretty_syntax(Syntax* syntax, Allocator* a);

// -----------------------------------------------------------------------------
//   Toplevel
// -----------------------------------------------------------------------------

typedef enum {
    TLDef,
    TLDecl,
    TLImport,
    TLExpr,
} TopLevel_t;

typedef struct {
    Symbol bind;
    Syntax* value;
} Definition;

typedef struct {
    Symbol bind;
    union {
        SymPtrAMap properties;
        PtrArray decls;
    };
} Declaration;

typedef struct {
    Range range;
    ImportClauseArray clauses;
} TLImportClause;

typedef struct {
    TopLevel_t type;
    union {
        Definition def;
        Declaration decl;
        TLImportClause import;
        Syntax* expr;
    };
} TopLevel;

Document* pretty_def(Definition* def, Allocator* a);
Document* pretty_toplevel(TopLevel* TopLevel, Allocator* a);

PiType* toplevel_type(TopLevel top);

#endif
