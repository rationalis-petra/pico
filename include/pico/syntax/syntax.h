#ifndef __PICO_SYNTAX_SYNTAX_H
#define __PICO_SYNTAX_SYNTAX_H

#include "data/option.h"
#include "data/meta/amap_header.h"

#include "platform/memory/allocator.h"

#include "pico/data/range.h"
#include "pico/data/sym_ptr_assoc.h"
#include "pico/data/sym_ptr_amap.h"
#include "pico/data/symbol_array.h"
#include "pico/syntax/concrete.h"
#include "pico/syntax/header.h"
#include "pico/syntax/synrange.h"
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
    SVariable,
    SAbsVariable,

    // Terms & term formers
    SProcedure,
    SAll,
    SMacro,
    SApplication,
    SAllApplication,
    SSeal,
    SUnseal,
    // TODO (Refactor) can probably remove SConstructor?
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
    SCond,
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
    SSizeOf,
    SAlignOf,
    SOffsetOf,
    SDynAlloc,
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
    SSealedType,
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

    SDevAnnotation,
} Syntax_t;

typedef struct {
    uint64_t idx;
} SynRef;

ARRAY_HEADER(SynRef, syn, Syn);
AMAP_HEADER(Symbol, SynRef, sym_syn, SymSyn);

typedef struct Syntax Syntax;
typedef PtrArray ClauseArray;

typedef struct {
    int64_t value;
    PrimType type;
} SynIntegralLiteral;

typedef struct {
    double value;
    PrimType type;
} SynFloatingLiteral;

typedef struct {
    Symbol symbol;
    size_t index;
    void* value;
    PiType* type;
} AbsVariable;

typedef struct {
    SymPtrAMap args;
    SymPtrAMap implicits;
    SynRef body;
    bool preserve_dyn_memory;
} SynProcedure;

typedef struct {
    SymbolArray args;
    SynRef body;
} SynAll;

typedef struct {
    SynRef function;
    SynArray implicits;
    SynArray args;
} SynApp;

typedef struct {
    SynRef function;
    SynArray types;
    SynArray implicits;
    SynArray args;
} SynAllApp;

typedef struct {
    SynRef type;
    SynArray types;
    SynArray implicits;
    SynRef body;
} SynSeal;

typedef struct {
    SynRef sealed;
    Symbol binder;
    SymbolArray types;
    SymbolArray implicits;
    SynRef body;
} SynUnseal;

typedef struct {
    Option_t has_enum_type;
    SynRef enum_type;
    Symbol tagname;
    size_t tag;
} SynConstructor;

typedef struct {
    Option_t has_enum_type;
    SynRef enum_type;
    Symbol tagname;
    size_t tag;
    SynArray args;
} SynVariant;

typedef struct {
    Symbol tagname;
    size_t tag;
    SymbolArray vars;
    SynRef body;
    bool is_wildcard;
} SynClause;

typedef struct {
    Symbol recfn;
    SynRef val;
    ClauseArray clauses;
} SynMatch;

typedef struct {
    Option_t has_base;
    SynRef base;
    SymSynAMap fields;
} SynStructure;

typedef struct {
    Symbol field;
    SynRef val;
} SynProjector;

typedef struct {
    SynRef dynamic;
    SynRef new_val;
} SynDynSet;

typedef struct {
    SymbolArray params;
    SymPtrAMap implicits;
    SynRef constraint;
    SymSynAMap fields;
} SynInstance;

typedef struct {
    SymPtrAMap args;
    SynRef body;
} SynLabelBranch;

typedef struct {
    SynRef entry;
    SymPtrAssoc terms;
} SynLabels;

typedef struct {
    Symbol label;
    SynArray args;
} SynGoTo;

typedef struct {
    bool is_binding;
    Symbol symbol;
    SynRef expr;
} SeqElt;

typedef struct {
    PtrArray elements;
} SynSequence;

typedef struct {
    Symbol point_sym;
    SynRef expr;

    PiType* in_arg_ty;
    Symbol in_sym;

    PiType* cont_arg_ty;
    Symbol cont_sym;
    SynRef handler;
} SynWithReset;

typedef struct {
    SynRef point;
    SynRef arg;
} SynResetTo;

// Sugaring Syntax

typedef struct {
    SymSynAMap bindings;
    SynRef body;
} SynLet;

typedef struct {
    SynRef var;
    SynRef expr;
} DynBinding;

typedef struct {
    PtrArray bindings;
    SynRef body;
} SynDynLet;

typedef struct {
    SynRef condition;
    SynRef true_branch;
    SynRef false_branch;
} SynIf;

typedef struct {
    SynRef condition;
    SynRef branch;
} CondClause;

typedef struct {
    PtrArray clauses;
    SynRef otherwise;
} SynCond;

typedef struct {
    Symbol name;
    SymbolArray vars;
    SymSynAMap fields;
} SynTrait;

//------------------------------------------------------------------------------
// Special
//------------------------------------------------------------------------------

typedef struct {
    SynRef val;
    SynRef type;
} SynIs;

typedef struct {
    Symbol name;
    SynArray args;
    SynRef body;
} SynName;

typedef struct {
    SynRef type;
} SynSize;


typedef struct {
    SymbolArray bindings;
    SynRef body;
} SynBind;

// ----------------------------------------------------------------------
// Types
// ----------------------------------------------------------------------

typedef struct {
    SymbolArray vars;
    SynArray implicits;
    SynRef body;
} SynSealedType;

typedef struct {
    Symbol name;
    SynRef body;
} SynNamed;

typedef struct {
    Symbol field;
    SynRef body;
} SynOffsetOf;

typedef struct {
    SynArray args;
    SynRef return_type;
} SynProcType;

typedef struct {
    SymSynAMap fields;
    bool packed;
} SynStructType;

typedef struct {
    uint8_t tag_size;
    SymPtrAMap variants;
} SynEnumType;

typedef struct {
    SynRef in;
    SynRef out;
} SynResetType;

typedef struct {
    bool from_native;
    SynRef type;
    SynRef body;
} SynReinterpret;

typedef struct {
    bool from_native;
    SynRef type;
    SynRef body;
} SynConvert;

typedef struct {
    PiType* type;
    void* value;
} SynCapture;

typedef enum {
    DevNone     = 0,
    // breaks
    DBAbstract  = 0x1,
    DBTypecheck = 0x2,
    DBGenerate  = 0x4,
    // print
    DPAbstract  = 0x8,
    DPTypecheck = 0x10,
    DPGenerate  = 0x20,
} DevFlag;

typedef struct {
    DevFlag flags;
    SynRef inner;
} SynDev;

struct Syntax {
    Syntax_t type;
    union {
        SynIntegralLiteral integral;
        SynFloatingLiteral floating;
        bool boolean;
        String string;

        AbsVariable abvar;
        Symbol variable;

        SynProcedure procedure;
        SynAll all;
        SynRef transformer;
        SynApp application;
        SynAllApp all_application;
        SynSeal seal;
        SynUnseal unseal;
        SynConstructor constructor;
        SynVariant variant;
        SynMatch match;
        SynStructure structure;
        SynProjector projector;
        SynInstance instance;

        SynRef dynamic;
        SynDynSet dynamic_set;
        SynRef use;
        SynDynLet dyn_let_expr;

        SynLet let_expr;
        SynIf if_expr;
        SynCond cond;
        SynLabels labels;
        SynGoTo go_to;
        SynWithReset with_reset;
        SynResetTo reset_to;
        SynSequence sequence;

        SynIs is;
        SynIs into;
        SynIs out_of;
        SynName name;
        SynRef unname;
        SynIs widen;
        SynIs narrow;
        SynRef size;
        SynOffsetOf offset_of;

        SynProcType proc_type;
        SynStructType struct_type;
        SynEnumType enum_type;
        SynResetType reset_type;
        SynRef dynamic_type;
        SynBind bind_type;
        SynSealedType sealed_type;
        SynNamed named_type;
        SynNamed distinct_type;
        SynNamed opaque_type;
        SynTrait trait;
        PiType* type_val;
        SynRef c_type;

        SynReinterpret reinterpret;
        SynConvert convert;

        // Metaprogramming
        SynRef type_of;
        SymbolArray to_describe;
        RawTree quoted;
        SynCapture capture;
        SynDev dev;
    };
};

ARRAY_HEADER(Syntax, syntax, Syntax);
typedef struct {
    SyntaxArray* syns;
    SynRangeArray* ranges;
    PtrArray* types;
} SynTape;

SynTape mk_syn_tape(Allocator* a, size_t size);
void allocate_types(SynTape tape);

SynRef new_syntax(SynTape tape);
Syntax get_syntax(SynRef ref, SynTape tape);
void set_syntax(SynRef ref, Syntax syn, SynTape tape);

SynRange get_range(SynRef ref, SynTape tape);
void set_range(SynRef ref, SynRange range, SynTape tape);

PiType* get_type(SynRef ref, SynTape tape);
void set_type(SynRef ref, PiType* type, SynTape tape);

/* Other instances */
String syntax_type_to_string(Syntax_t type);
Document* pretty_syntax(SynRef syntax, SynTape tape, Allocator* a);

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
    SynRef value;
} Definition;

typedef struct {
    Symbol bind;
    union {
        SymSynAMap properties;
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
        SynRef expr;
    };
} TopLevel;

Document* pretty_def(Definition* def, SynTape tape, Allocator* a);
Document* pretty_toplevel(TopLevel* TopLevel, SynTape tape, Allocator* a);

PiType* toplevel_type(TopLevel top, SynTape tape);

#endif
