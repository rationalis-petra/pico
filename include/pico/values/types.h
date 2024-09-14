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

typedef enum PrimType {
    Unit,
    Bool,
    Address,
    Int_64,
    TFormer,
    TType,
} PrimType;

typedef enum PiType_t {
    TPrim,
    TProc,
    TStruct,
    TEnum,
    TQVar,
    TApp,

    // Special sort: unification variable
    TUVar
} PiType_t;

typedef struct ProcType {
    SymbolArray quants;
    PtrArray args;
    PiType* ret;
} ProcType;

typedef struct StructType {
    SymbolArray quants;
    SymPtrAMap fields;
} StructType;

typedef struct EnumType {
    SymbolArray quants;
    SymPtrAMap variants;
} EnumType;

typedef struct QVarType {
    uint64_t id;
} QVarType;

typedef struct TAppType {
    PtrArray args;
    PiType* fam;
} TAppType;

typedef struct UVarType {
    uint64_t id;
    PiType* subst;
} UVarType;

struct PiType {
    PiType_t sort; 
    union {
        PrimType prim;
        ProcType proc;
        StructType structure;
        EnumType enumeration;
        TAppType app;
        QVarType qvar;
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
