#ifndef __PICO_VALUES_TYPES_H
#define __PICO_VALUES_TYPES_H

#include "data/array.h"
#include "data/result.h"
#include "pretty/document.h"

#include "pico/data/sym_ptr_amap.h"
#include "pico/data/sym_ptr_assoc.h"
#include "pico/data/symbol_array.h"
#include "pico/values/ctypes.h"

/* Basic types in pico typesystem */
// Forward declarations
typedef struct PiType PiType;
typedef struct UVarGenerator UVarGenerator;

typedef enum {
    Int_8  = 0b000,
    Int_16 = 0b001,
    Int_32 = 0b010,
    Int_64 = 0b011,

    UInt_8  = 0b100,
    UInt_16 = 0b101,
    UInt_32 = 0b110,
    UInt_64 = 0b111,

    Float_32,
    Float_64,

    Unit,
    Bool,
    Address,
    TFormer,
    TMacro,
} PrimType;

typedef enum {
  TPrim,
  TProc,
  TStruct,
  TEnum,
  TReset,
  TResumeMark,
  TDynamic,

  // 'Special'
  TNamed,
  TDistinct,
  TTrait,
  TTraitInstance, // note: not a "real" type in the theory

  TCType, // native (C) type

  // Quantified Types
  TVar,
  TAll,
  TExists,

  // Used by Sytem-Fω (type constructors)
  TCApp,
  TFam,

  // Kinds (higher kinds not supported)
  TKind,
  TConstraint,

  // Used only during unification
  TUVar,
  TUVarIntegral,
  TUVarFloating,
} PiType_t;

typedef struct {
    PtrArray args;
    PtrArray implicits;
    PiType* ret;
} ProcType;

typedef struct {
    SymPtrAMap fields;
} StructType;

typedef struct {
    SymPtrAMap variants;
} EnumType;

typedef struct {
    PiType* in;
    PiType* out;
} ResetType;

typedef struct {
    uint64_t id;
    SymbolArray vars;
    SymPtrAMap fields; 
} TraitType; 

typedef struct {
    uint64_t instance_of;
    PtrArray args; 
    SymPtrAMap fields; 
} TraitInstance; 

typedef struct {
    PtrArray args;
    PiType* fam;
} TAppType;

typedef struct {
    uint64_t id;
    PiType* subst;
} UVarType;

typedef struct {
    SymbolArray vars;
    PiType* body;
} TypeBinder;

typedef struct {
    Symbol name;
    PiType* type;
    PtrArray* args;
} NamedType;

typedef struct {
    Symbol name;
    PiType* type;
    uint64_t id;
    void* source_module;
    PtrArray* args;
} DistinctType;

typedef struct {
    size_t nargs;
} PiKind;

typedef struct {
    size_t nargs;
} PiConstraint;

struct PiType {
    PiType_t sort; 
    union {
        PrimType prim;
        ProcType proc;
        StructType structure;
        EnumType enumeration;
        ResetType reset;
        PiType* dynamic;

        TraitType trait;
        TraitInstance instance;

        CType c_type;

        NamedType named;
        DistinctType distinct;

        // From System Fω: variables, application, abstraction (exists, forall, lambda)
        Symbol var;
        TAppType app;
        TypeBinder binder;

        PiKind kind;
        PiConstraint constraint;

        // For typechecking/inference
        UVarType* uvar;
    };
};

// Transform and Analyse
Document* pretty_pi_value(void* val, PiType* types, Allocator* a);
Document* pretty_type(PiType* type, Allocator* a);

PiType* pi_type_subst(PiType* type, SymPtrAssoc binds, Allocator* a);
bool pi_type_eql(PiType* lhs, PiType* rhs);

size_t pi_size_of(PiType type);
size_t pi_align_of(PiType type);
Result_t pi_maybe_size_of(PiType type, size_t* out);

size_t pi_size_align(size_t size, size_t align);
size_t pi_stack_align(size_t in);
size_t pi_stack_size_of(PiType type);

// Resource Management
void delete_pi_type(PiType t, Allocator* a);
void delete_pi_type_p(PiType* t, Allocator* a);

PiType copy_pi_type(PiType t, Allocator* a);
PiType* copy_pi_type_p(PiType* t, Allocator* a);

PiType* mk_uvar(UVarGenerator* gen, Allocator* a);
PiType* mk_uvar_integral(UVarGenerator* gen, Allocator* a);
PiType* mk_uvar_floating(UVarGenerator* gen, Allocator* a);
UVarGenerator* mk_gen(Allocator* a);
void delete_gen(UVarGenerator* gen, Allocator* a);

// Misc. and utility
// Utilities for generating or manipulating types
// Generate distinct id
uint64_t distinct_id();

// Recursively extracts the inner type from distinct types (but not opaque)
// Upon encountering a named type, it will substitute the name for the 
// (wrapped) named type within the type, then contine descending.
PiType* unwrap_type(PiType *ty, Allocator* a);

// Recursively extracts the inner type from named, distinct and opaque types.
PiType* strip_type(PiType* ty);
PiType* type_app (PiType family, PtrArray args, Allocator* a);

// Generators 
PiType* mk_prim_type(Allocator* a, PrimType t);
PiType* mk_dynamic_type(Allocator* a, PiType* t);

// Sample usage: mk_proc_type(a, 2, arg_1_ty, arg_2_ty, ret_ty)
PiType* mk_proc_type(Allocator* a, size_t nargs, ...);

// Sample usage: mk_proc_type(a, 2, "field-1", field_1_ty, "field-2", arg_2_ty)
PiType* mk_struct_type(Allocator* a, size_t nfields, ...);

// Sample usage: mk_enum_type(a, 3,
//   "Pair", 2, mk_prim_type(Int_64), mk_prim_type(Int_64),
//   "Singleton", 1, mk_prim_type(Int_64),
//   "None", 0)
PiType* mk_enum_type(Allocator* a, size_t nfields, ...);

// Sample usage: mk_distinct_type(a, "List", ...)
PiType* mk_named_type(Allocator* a, const char* name, PiType* inner);

// Sample usage: mk_distinct_type(a, mk_prim_type(Address))
PiType* mk_distinct_type(Allocator* a, PiType* inner);

// Sample usage: mk_opaque_type(a, mod, mk_prim_type(Address))
PiType* mk_opaque_type(Allocator* a, void* module, PiType* inner);

PiType* mk_var_type(Allocator* a, const char* name);

// Sample usage: mk_distinct_type(a, vars, mk_prim_type(Address))
PiType* mk_type_family(Allocator* a, SymbolArray vars, PiType* body);

// Sample usage: mk_app_type(a, array_type, int_type);
PiType* mk_app_type(Allocator* a, PiType* fam, ...);

// Types from the standard library
// Struct [.len U64] [.capacity U64] [.bytes Address]
PiType* mk_string_type(Allocator* a);


#endif
