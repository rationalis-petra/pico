#ifndef __PICO_VALUES_CTYPES_H
#define __PICO_VALUES_CTYPES_H

#include "data/meta/assoc_header.h"
#include "pico/data/name_i64_assoc.h"
#include "pico/data/name_ptr_amap.h"

typedef struct CType CType;

ASSOC_HEADER_NOCELL(Name, CType, name_ctype, NameCType)

typedef enum {
    CSVoid,
    CSPrimInt,
    CSFloat,
    CSDouble,
    CSPtr,
    CSProc,
    CSStruct,
    CSIncomplete,
    CSUnion,
    CSCEnum,
} CSort;

typedef enum {
    CChar,
    CShort,
    CInt,
    CLong,
    CLongLong,
} CIntType;

typedef enum {
    Signed,
    Unsigned,
    Unspecified,
} CSigned;

typedef struct {
    // Padding is to ensure that the implementation matches the Relic types (where enums are 64 bit)
    uint64_t prim;
    uint64_t is_signed;
} CPrimInt;

typedef struct {
    uint64_t named_tag;
    Name name;
    NameCTypeAssoc args;
    CType* ret;
} CProc;

typedef struct {
    uint64_t named_tag;
    Name name; 
    NameCTypeAssoc fields;
} CStruct;

typedef struct {
    CPrimInt base;
    NameI64Assoc vals;
} CEnum;

typedef struct {
    NamePtrAMap fields;
} CUnion;

typedef struct {
    CType* inner;
} CPtr;

struct CType {
    union {
        CSort sort;
        uint64_t pad; // To ensure that layout is same as Relic
    };
    union {
        CPrimInt prim;
        CProc proc;
        CStruct structure;
        CEnum enumeration;
        CUnion cunion;
        CPtr ptr;
        Name incomplete;
    };
};

ASSOC_HEADER_CELL(Name, CType, name_cty, NameCType)

Document* pretty_cprimint(CPrimInt prim, Allocator* a);
Document* pretty_ctype(CType* type, Allocator* a);
Document* pretty_cval(CType* type, void* value, Allocator* a);
size_t c_size_align(size_t size, size_t align);
size_t c_size_of(CType type);
size_t c_align_of(CType type);

// Resource Management
void delete_c_type(CType t, Allocator* a);
void delete_c_type_p(CType* t, Allocator* a);

CType copy_c_type(CType t, Allocator* a);
CType* copy_c_type_p(CType* t, Allocator* a);

// Misc. and utility
// Utilities for generating or manipulating types
CType mk_voidptr_ctype(Allocator* a);

CType mk_primint_ctype(CPrimInt t);

// Sample usage: mk_proc_type(a, 2, arg_1_ty, arg_2_ty, ret_ty)
CType mk_fn_ctype(Allocator* a, size_t nargs, ...);

// Sample usage: mk_proc_type(a, 2, "field-1", field_1_ty, "field-2", arg_2_ty)
CType mk_struct_ctype(Allocator* a, size_t nfields, ...);
 
// Sample usage: mk_enum_type(a, CInt, 2, "true", 0, "false", 1)
CType mk_enum_ctype(Allocator* a, CPrimInt store, size_t nfields, ...);

// Sample usage: mk_union_type(a, 3, )
CType mk_union_ctype(Allocator* a, size_t nfields, ...);

void init_ctypes();

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
extern CType c_size_type;
extern CType c_void;
#pragma GCC diagnostic pop

#endif
