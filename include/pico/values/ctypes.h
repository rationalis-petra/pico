#ifndef __PICO_VALUES_CTYPES_H
#define __PICO_VALUES_CTYPES_H

#include "pico/data/client/allocator.h"
#include "pico/data/client/meta/amap_header.h"
#include "pico/data/client/name_i64_piamap.h"
#include "pico/data/client/name_addr_piamap.h"

typedef struct CType CType;

PICO_AMAP_HEADER_NOCELL(Name, CType, name_ctype, NameCType)

typedef enum : uint64_t {
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

typedef enum : uint64_t{
    CChar,
    CShort,
    CInt,
    CLong,
    CLongLong,
} CIntType;

typedef enum : uint64_t{
    Signed,
    Unsigned,
    Unspecified,
} CSigned;

typedef struct {
    CIntType prim;
    CSigned is_signed;
} CPrimInt;

typedef struct {
    uint64_t named_tag;
    Name name;
    NameCTypePiAMap args;
    CType* ret;
} CProc;

typedef struct {
    uint64_t named_tag;
    Name name; 
    NameCTypePiAMap fields;
} CStruct;

typedef struct {
    CPrimInt base;
    NameI64PiAMap vals;
} CEnum;

typedef struct {
    NameAddrPiAMap fields;
} CUnion;

typedef struct {
    CType* inner;
} CPtr;

struct CType {
    CSort sort;
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

PICO_AMAP_HEADER_CELL(Name, CType, name_cty, NameCType)

Document* pretty_cprimint(CPrimInt prim, Allocator* a);
Document* pretty_ctype(CType* type, Allocator* a);
Document* pretty_cval(CType* type, void* value, Allocator* a);
size_t c_size_align(size_t size, size_t align);
size_t c_size_of(CType type);
size_t c_align_of(CType type);

// Resource Management
void delete_c_type(CType t, PiAllocator* a);
void delete_c_type_p(CType* t, PiAllocator* a);

CType copy_c_type(CType t, PiAllocator* a);
CType* copy_c_type_p(CType* t, PiAllocator* a);

// Misc. and utility
// Utilities for generating or manipulating types
CType mk_voidptr_ctype(PiAllocator* a);

CType mk_primint_ctype(CPrimInt t);

// Sample usage: mk_proc_type(a, 2, arg_1_ty, arg_2_ty, ret_ty)
CType mk_fn_ctype(PiAllocator* a, size_t nargs, ...);

// Sample usage: mk_proc_type(a, 2, "field-1", field_1_ty, "field-2", arg_2_ty)
CType mk_struct_ctype(PiAllocator* a, size_t nfields, ...);
 
// Sample usage: mk_enum_type(a, CInt, 2, "true", 0, "false", 1)
CType mk_enum_ctype(PiAllocator* a, CPrimInt store, size_t nfields, ...);

// Sample usage: mk_union_type(a, 3, )
CType mk_union_ctype(PiAllocator* a, size_t nfields, ...);

// Ctypes used within our data/* libraries 
CType mk_string_ctype(PiAllocator* a);
CType mk_allocator_ctype(PiAllocator* a);
CType mk_list_ctype(PiAllocator* a);

void init_ctypes();

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
extern CType c_size_type;
extern CType c_void;
#pragma GCC diagnostic pop

#endif
