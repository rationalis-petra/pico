#ifndef __PICO_VALUES_TYPES_H
#define __PICO_VALUES_TYPES_H

#include "data/array.h"
#include "data/result.h"
#include "components/pretty/document.h"

#include "pico/data/client/list.h"
#include "pico/data/client/symbol_list.h"
#include "pico/data/client/sym_addr_piamap.h"
#include "pico/data/sym_ptr_assoc.h"
#include "pico/data/symbol_array.h"
#include "pico/values/ctypes.h"

/* Basic types in pico typesystem */
// Forward declarations
typedef struct PiType PiType;

// Forward declaration: these types are defined and used
// in unify.c
typedef struct UVarType UVarType;

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
  TArray,
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
  TSealed,

  // Used by Sytem-Fω (type constructors)
  TCApp,
  TFam,

  // Kinds (higher kinds not supported)
  TKind,
  TConstraint,

  // Used only during unification
  TUVar,
} PiType_t;

typedef enum { Any, FixedDimension, Fixed } ArraySort;

typedef struct {
    bool is_any;
    uint64_t value;
} ArrayDimType;

typedef struct {
    ArraySort sort; 
    AddrPiList dimensions;
    PiType* element_type; 
} ArrayType;

typedef struct {
    AddrPiList args;
    AddrPiList implicits;
    PiType* ret;
} ProcType;

typedef struct {
    SymAddrPiAMap fields;
    bool packed;
} StructType;

typedef struct {
    SymAddrPiAMap variants;
    uint8_t tag_size;
} EnumType;

typedef struct {
    PiType* in;
    PiType* out;
} ResetType;

typedef struct {
    uint64_t id;
    SymbolPiList vars;
    SymAddrPiAMap fields; 
} TraitType; 

typedef struct {
    uint64_t instance_of;
    AddrPiList args; 
    SymAddrPiAMap fields; 
} TraitInstance; 

typedef struct {
    AddrPiList args;
    PiType* fam;
} TAppType;

typedef struct {
    SymbolPiList vars;
    PiType* body;
} TypeBinder;

typedef struct {
    SymbolPiList vars;
    AddrPiList implicits;
    PiType* body;
} SealedType;

typedef struct {
    Symbol name;
    PiType* type;
    AddrPiList* args;
} NamedType;

typedef struct {
    Symbol name;
    PiType* type;
    uint64_t id;
    void* source_module;
    AddrPiList* args;
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
        ArrayType array;
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

        // From System Fω: variables, application, abstraction (sealed, forall, lambda)
        Symbol var;
        TAppType app;
        SealedType sealed;
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

PiType* pi_type_subst(PiType* type, SymPtrAssoc binds, PiAllocator* pia, Allocator* a);
bool pi_type_eql(PiType* lhs, PiType* rhs, Allocator* a);
bool pi_value_eql(PiType* type, void* lhs, void* rhs, Allocator* a);

bool is_variable_for(PiType *ty, SymbolArray vars);

size_t pi_size_of(PiType type);
size_t pi_align_of(PiType type);

Result_t pi_maybe_align_of(PiType type, size_t* out);
Result_t pi_maybe_size_of(PiType type, size_t* out);

size_t pi_size_align(size_t size, size_t align);
size_t pi_stack_align(size_t in);

size_t pi_stack_size_of(PiType type);
Result_t pi_maybe_stack_size_of(PiType type, size_t* out);

// Resource Management
void delete_pi_type(PiType t, PiAllocator* pia);
void delete_pi_type_p(PiType* t, PiAllocator* pia);

PiType copy_pi_type(PiType t, PiAllocator* pia);
PiType* copy_pi_type_p(PiType* t, PiAllocator* pia);

// Misc. and utility
// Utilities for generating or manipulating types
// Generate distinct id
uint64_t distinct_id();

bool is_wider(PiType* narrow, PiType* wide);
bool is_narrower(PiType* wide, PiType* narrow);

// Recursively extracts the inner type from distinct types (but not opaque)
// Upon encountering a named type, it will substitute the name for the 
// (wrapped) named type within the type, then contine descending.
PiType* unwrap_type(PiType *ty, PiAllocator* pia, Allocator* a);

// Recursively extracts the inner type from named, distinct and opaque types.
PiType* strip_type(PiType* ty);

// type_app: apply the arguments (args) to a type family (fam)
//  Memory guarantes: both the arguments (args) and famiy are untouched, and can
//  be safely deleted etc. without affecting the returned type.
PiType* type_app (PiType family, PtrArray args, PiAllocator* pia, Allocator* a);

// Generators 
PiType* mk_prim_type(PiAllocator* pia, PrimType t);
PiType* mk_dynamic_type(PiAllocator* pia, PiType* t);

// Sample usage: mk_proc_type(a, 2, arg_1_ty, arg_2_ty, ret_ty)
PiType* mk_proc_type(PiAllocator* pia, size_t nargs, ...);

// Sample usage: mk_proc_type(a, 2, "field-1", field_1_ty, "field-2", arg_2_ty)
PiType* mk_struct_packed_type(PiAllocator* pia, bool packed, size_t nfields, ...);
PiType* mk_struct_type(PiAllocator* pia, size_t nfields, ...);

// Sample usage: mk_trait_type(a, 1, "A", 2
//   "val", mk_var_type(a, "A"),
//   "mon", mk_proc_type(a, 2, mk_var_type(a, "A"), mk_var_type(a, "A"), mk_var_type(a, "A")))
PiType* mk_trait_type(PiAllocator* pia, size_t nfields, ...);

// Sample usage: mk_enum_type(a, 3,
//   "Pair", 2, mk_prim_type(Int_64), mk_prim_type(Int_64),
//   "Singleton", 1, mk_prim_type(Int_64),
//   "None", 0)
PiType* mk_enum_type(PiAllocator* pia, size_t nfields, ...);
PiType* mk_sz_enum_type(PiAllocator* pia, uint8_t tagsize, size_t nfields, ...);

// Sample usage: mk_distinct_type(a, "List", ...)
PiType* mk_named_type(PiAllocator* pia, const char* name, PiType* inner);

// Sample usage: mk_distinct_type(a, mk_prim_type(Address))
PiType* mk_distinct_type(PiAllocator* pia, PiType* inner);

// Sample usage: mk_opaque_type(a, mod, mk_prim_type(Address))
PiType* mk_opaque_type(PiAllocator* pia, void* module, PiType* inner);

PiType* mk_var_type(PiAllocator* pia, const char* name);

// Sample usage: mk_all_type(a, 2, "A", "B", mk_prim_type(Address));
PiType* mk_all_type(PiAllocator* pia, size_t nsymbols, ...);

// Sample usage: mk_sealed_type(a, 2, "A", "B", 1, addable, mk_prim_type(Address));
PiType* mk_sealed_type(PiAllocator* pia, size_t nsymbols, ...);

// Sample usage: mk_distinct_type(a, vars, mk_prim_type(Address))
PiType* mk_type_family(PiAllocator* pia, SymbolPiList vars, PiType* body);

// Sample usage: mk_app_type(a, array_type, int_type);
PiType* mk_app_type(PiAllocator* pia, PiType* fam, ...);

// Types from the standard library
// Struct [.len U64] [.capacity U64] [.bytes Address]
PiType* mk_string_type(PiAllocator* pia);


#endif
