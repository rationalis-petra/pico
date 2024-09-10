#ifndef __PICO_VALUES_TYPES_H
#define __PICO_VALUES_TYPES_H

#include "data/array.h"
#include "pretty/document.h"
#include "pico/data/sym_ptr_amap.h"

/* Basic types in pico typesystem */

// Forward declarations
typedef struct PiType PiType;
typedef struct UVarGenerator UVarGenerator;

typedef enum PrimType {
    Int_64,
    Bool,
    TFormer,
    TType,
} PrimType;

typedef enum PiType_t {
    TPrim,
    TProc,
    TStruct,
    TEnum,

    // Special sort: unification variable
    TUVar
} PiType_t;

typedef struct ProcType {
    PtrArray args;
    struct PiType* ret;
} ProcType;

typedef struct StructType {
    SymPtrAMap fields;
} StructType;

typedef struct EnumType {
    SymPtrAMap variants;
} EnumType;

typedef struct UVarType {
    uint64_t id;
    struct PiType* subst;
} UVarType;

struct PiType {
    PiType_t sort; 
    union {
        PrimType prim;
        ProcType proc;
        StructType structure;
        EnumType enumeration;
        UVarType* uvar;
    };
};

Document* pretty_pi_value(void* val, PiType* types, Allocator* a);

Document* pretty_type(PiType* type, Allocator* a);
void delete_pi_type(PiType t, Allocator* a);
void delete_pi_type_p(PiType* t, Allocator* a);

PiType copy_pi_type(PiType t, Allocator* a);
PiType* copy_pi_type_p(PiType* t, Allocator* a);
size_t pi_size_of(PiType t);

// Utilities for generating types
PiType mk_prim_type(PrimType t);

PiType* mk_uvar(UVarGenerator* gen, Allocator* a);
UVarGenerator* mk_gen(Allocator* a);
void delete_gen(UVarGenerator* gen, Allocator* a);

#endif
