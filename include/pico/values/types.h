#ifndef __PICO_VALUES_TYPES_H
#define __PICO_VALUES_TYPES_H

#include "data/option.h"
#include "data/array.h"
#include "pretty/document.h"
#include "pico/data/sym_ptr_amap.h"

/* Basic types in pico typesystem */

// Forward declarations
typedef struct PiType PiType;
typedef struct UVarGenerator UVarGenerator;

typedef enum {
    Unit,
    Bool,
    Address,
    Int_64,
    TFormer,
} PrimType;

typedef enum {
    TPrim,
    TProc,
    TStruct,
    TEnum,

    // Quantified Types
    TAll,
    TExists,
    TVar,

    // Used by Sytem-Fω (type constructors)
    TCApp,
    TCLam,

    // Kinds (higher kinds not supported)
    TKind,

    // Used only during unification
    TUVar
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
    size_t nargs;
} PiKind;


struct PiType {
    PiType_t sort; 
    union {
        PrimType prim;
        ProcType proc;
        StructType structure;
        EnumType enumeration;

        // From System Fω: variables, application, abstraction (exists, forall, lambda)
        uint64_t var;
        TAppType app;
        TypeBinder binder;

        PiKind kind;

        // For typechecking/inference
        UVarType* uvar;
    };
};

typedef struct {
    Option_t type;
    size_t size;
} PiSize;

Document* pretty_pi_value(void* val, PiType* types, Allocator* a);

Document* pretty_type(PiType* type, Allocator* a);
void delete_pi_type(PiType t, Allocator* a);
void delete_pi_type_p(PiType* t, Allocator* a);

PiType copy_pi_type(PiType t, Allocator* a);
PiType* copy_pi_type_p(PiType* t, Allocator* a);

size_t pi_size_of(PiType t);
size_t runtime_size_of(PiType* t, void* data);
size_t comptime_size_of(PiType t);

// Utilities for generating types
PiType mk_prim_type(PrimType t);

PiType* mk_uvar(UVarGenerator* gen, Allocator* a);
UVarGenerator* mk_gen(Allocator* a);
void delete_gen(UVarGenerator* gen, Allocator* a);

#endif
