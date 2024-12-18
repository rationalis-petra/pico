#ifndef __PICO_VALUES_TYPES_H
#define __PICO_VALUES_TYPES_H

#include "data/array.h"
#include "pretty/document.h"
#include "pico/data/sym_ptr_amap.h"
#include "pico/data/sym_ptr_assoc.h"

/* Basic types in pico typesystem */
// Forward declarations
typedef struct PiType PiType;
typedef struct UVarGenerator UVarGenerator;

typedef enum {
    Unit = 0b0000,
    Bool = 0b0001,
    Address = 0b0010,
    TFormer = 0b0011,

    Int_8  = 0b0100,
    Int_16 = 0b0101,
    Int_32 = 0b0110,
    Int_64 = 0b0111,

    UInt_8  = 0b1100,
    UInt_16 = 0b1101,
    UInt_32 = 0b1110,
    UInt_64 = 0b1111,
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
    TDistinct,
    TTrait,
    TTraitInstance, // note: not a "real" type in the theory

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
    TUVarDefaulted,
} PiType_t;

typedef struct {
    PtrArray args;
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
    PiType* type;
    uint64_t id;
    void* source_module;
    PtrArray* args;
} DistinctType;

typedef struct {
    PiType* body;
    uint64_t id;
    PtrArray* args;
    void* source_module;
} DistinctTypeApp;

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
        ResetType reset;
        EnumType enumeration;
        PiType* dynamic;

        TraitType trait;
        TraitInstance instance;

        DistinctType distinct;
        DistinctTypeApp distinct_app;

        // From System Fω: variables, application, abstraction (exists, forall, lambda)
        uint64_t var;
        TAppType app;
        TypeBinder binder;

        PiKind kind;
        PiConstraint constraint;

        // For typechecking/inference
        UVarType* uvar;
    };
};

Document* pretty_pi_value(void* val, PiType* types, Allocator* a);

Document* pretty_type(PiType* type, Allocator* a);

PiType* pi_type_subst(PiType* type, SymPtrAssoc binds, Allocator* a);

size_t pi_size_of(PiType t);
size_t pi_mono_size_of(PiType t);
size_t runtime_size_of(PiType* t, void* data);

// Resource Management
void delete_pi_type(PiType t, Allocator* a);
void delete_pi_type_p(PiType* t, Allocator* a);

PiType copy_pi_type(PiType t, Allocator* a);
PiType* copy_pi_type_p(PiType* t, Allocator* a);

PiType* mk_uvar(UVarGenerator* gen, Allocator* a);
PiType* mk_uvar_with_default(UVarGenerator* gen, Allocator* a);
UVarGenerator* mk_gen(Allocator* a);
void delete_gen(UVarGenerator* gen, Allocator* a);

// Misc. and utility
// Utilities for generating or manipulating types
uint64_t distinct_id();

PiType* type_app (PiType family, PtrArray args, Allocator* a);

PiType mk_prim_type(PrimType t);
PiType mk_dynamic_type(Allocator* a, PiType t);
PiType mk_proc_type(Allocator* a, size_t nargs, ...);
PiType mk_struct_type(Allocator* a, size_t nfields, ...);

// Types from the standard library
// Struct [.len U64] [.capacity U64] [.bytes Address]
PiType mk_string_type(Allocator* a);


#endif
