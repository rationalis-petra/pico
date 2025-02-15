#ifndef __PICO_VALUES_CTYPES_H
#define __PICO_VALUES_CTYPES_H

#include "pico/data/sym_ptr_assoc.h"
#include "pico/data/sym_i64_assoc.h"
#include "pico/data/sym_ptr_amap.h"

typedef struct CType CType;

typedef enum {
    CSPrim,
    CSCEnum,
    CSProc,
    CSStruct,
    CSUnion,
    CSPtr,
    CSIncomplete,
} CSort;

typedef enum {
    CChar,
    CShort,
    CInt,
    CLong,

    CUChar,
    CUShort,
    CUInt,
    CULong,

    CVoid,
} CPrim;

typedef struct {
    bool named;
    Symbol name;
    SymPtrAssoc args;
    CType* ret;
} CProc;

typedef struct {
    bool named;
    Symbol name; 
    SymPtrAMap fields;
} CStruct;

typedef struct {
    CPrim base;
    SymI64Assoc vals;
} CEnum;

typedef struct {
    SymPtrAMap fields;
} CUnion;

typedef struct {
    CType* inner;
} CPtr;

struct CType {
    CSort sort;
    union {
        CPrim prim;
        CProc proc;
        CStruct structure;
        CEnum enumeration;
        CUnion cunion;
        CPtr ptr;
        Symbol incomplete;
    };
};

Document* pretty_cprim(CPrim prim, Allocator* a);
Document* pretty_ctype(CType* type, Allocator* a);
size_t c_size_of(CType type);
size_t c_align_of(CType type);

// Resource Management
void delete_c_type(CType t, Allocator* a);
void delete_c_type_p(CType* t, Allocator* a);

CType copy_c_type(CType t, Allocator* a);
CType* copy_c_type_p(CType* t, Allocator* a);

// Misc. and utility
// Utilities for generating or manipulating types
CType mk_prim_ctype(CPrim t);

// Sample usage: mk_proc_type(a, 2, arg_1_ty, arg_2_ty, ret_ty)
CType mk_fn_ctype(Allocator* a, size_t nargs, ...);

// Sample usage: mk_proc_type(a, 2, "field-1", field_1_ty, "field-2", arg_2_ty)
CType mk_struct_ctype(Allocator* a, size_t nfields, ...);

// Sample usage: mk_enum_type(a, CInt, 2, "true", 0, "false", 1)
CType mk_enum_ctype(Allocator* a, CPrim store, size_t nfields, ...);

// Sample usage: mk_union_type(a, 3, )
CType mk_union_ctype(Allocator* a, size_t nfields, ...);

#endif
