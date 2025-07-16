#include <stdarg.h>
#include "platform/machine_info.h"
#include "platform/signals.h"
#include "pretty/document.h"
#include "pretty/standard_types.h"
#include "data/meta/assoc_impl.h"

#include "pico/values/ctypes.h"

ASSOC_IMPL(Name, CType, name_ctype, NameCType);

Document* pretty_cprim(CPrimInt prim, Allocator* a) {
    PtrArray nodes = mk_ptr_array(2, a);
    if (prim.is_signed == Signed) {
        push_ptr(mk_str_doc(mv_string("signed"), a), &nodes);
    } else if (prim.is_signed == Unsigned) {
        push_ptr(mk_str_doc(mv_string("unsigned"), a), &nodes);
    }

    switch (prim.prim) {
    case CChar:
        push_ptr(mk_str_doc(mv_string("char"), a), &nodes);
        break;
    case CShort:
        push_ptr(mk_str_doc(mv_string("short"), a), &nodes);
        break;
    case CInt:
        push_ptr(mk_str_doc(mv_string("int"), a), &nodes);
        break;
    case CLong:
        push_ptr(mk_str_doc(mv_string("long"), a), &nodes);
        break;
    case CLongLong:
        push_ptr(mk_str_doc(mv_string("long long"), a), &nodes);
        break;
    }

    return mv_sep_doc(nodes, a);
}

Document* pretty_ctype(CType* type, Allocator* a) {
    switch (type->sort) {
    case CSVoid:
        return mk_str_doc(mv_string("void"), a);
    case CSPrimInt:
        return pretty_cprim(type->prim, a);
    case CSFloat:
        return mk_str_doc(mv_string("float"), a);
    case CSDouble:
        return mk_str_doc(mv_string("double"), a);
    case CSCEnum: {
        // enum name { l1 = n1, l2 = n2 }
        PtrArray main_nodes = mk_ptr_array(3, a);
        push_ptr(mk_str_doc(mv_string("enum"), a), &main_nodes);
        push_ptr(pretty_cprim(type->enumeration.base, a), &main_nodes);

        PtrArray arg_nodes = mk_ptr_array(type->enumeration.vals.len * 4, a);
        for (size_t i = 0; i < type->proc.args.len; i++) {
            push_ptr(mk_str_doc(*name_to_string(type->enumeration.vals.data[i].key), a), &arg_nodes);
            push_ptr(mk_str_doc(mv_string(" = "), a), &arg_nodes);
            push_ptr(pretty_i64(type->enumeration.vals.data[i].val, a), &arg_nodes);
            if (i - 1 != type->proc.args.len) {
                push_ptr(mk_str_doc(mv_string(", "), a), &arg_nodes);
            }
        }
        push_ptr(mk_paren_doc("{", "}", mv_sep_doc(arg_nodes, a), a), &main_nodes);
        return mv_sep_doc(main_nodes, a);
    }
    case CSProc: {
        // ret_ty name (n1 a1, n2 a2, ...)
        PtrArray main_nodes = mk_ptr_array(3, a);
        push_ptr(pretty_ctype(type->proc.ret, a), &main_nodes);
        if (!type->proc.named_tag) {
            push_ptr(mk_str_doc(*name_to_string(type->proc.name), a), &main_nodes);
        }

        PtrArray arg_nodes = mk_ptr_array(type->proc.args.len * 4, a);
        for (size_t i = 0; i < type->proc.args.len; i++) {
            push_ptr(mk_str_doc(*name_to_string(type->proc.args.data[i].key), a), &arg_nodes);
            push_ptr(mk_str_doc(mv_string(": "), a), &arg_nodes);
            push_ptr(pretty_ctype(&type->proc.args.data[i].val, a), &arg_nodes);
            if (i + 1 != type->proc.args.len) {
                push_ptr(mk_str_doc(mv_string(", "), a), &arg_nodes);
            }
        }
        push_ptr(mk_paren_doc("(", ")", mv_cat_doc(arg_nodes, a), a), &main_nodes);
        return mv_sep_doc(main_nodes, a);
    }
    case CSStruct: {
        // struct name { name : var, n2 : var2 }
        PtrArray main_nodes = mk_ptr_array(3, a);
        push_ptr(mk_str_doc(mv_string("struct {"), a), &main_nodes);
        if (!type->proc.named_tag) {
            push_ptr(mk_str_doc(*name_to_string(type->proc.name), a), &main_nodes);
        }

        PtrArray field_nodes = mk_ptr_array(type->structure.fields.len, a);
        for (size_t i = 0; i < type->structure.fields.len; i++) {
            PtrArray arg_nodes = mk_ptr_array(4, a);
            push_ptr(mk_str_doc(*name_to_string(type->structure.fields.data[i].key), a), &arg_nodes);
            Document* tydoc = pretty_ctype(&type->structure.fields.data[i].val, a);
            if (i - 1 != type->proc.args.len) {
                PtrArray cat_nodes = mk_ptr_array(2, a);
                push_ptr(tydoc, &cat_nodes);
                push_ptr(mk_str_doc(mv_string(";"), a), &cat_nodes);
                tydoc = mv_cat_doc(cat_nodes, a);
            } 
            push_ptr(mv_nest_doc(2, tydoc, a), &arg_nodes);
            push_ptr(mv_group_doc(mv_sep_doc(arg_nodes, a), a), &field_nodes);
        }
        push_ptr(mv_nest_doc(2, mv_sep_doc(field_nodes, a), a), &main_nodes);
        push_ptr(mk_cstr_doc("}", a), &main_nodes);
        return mv_sep_doc(main_nodes, a);
    }
    
    case CSUnion: {
        // union { name : var, n2 : var2 }
        PtrArray main_nodes = mk_ptr_array(2, a);
        push_ptr(mk_str_doc(mv_string("union {"), a), &main_nodes);

        PtrArray field_nodes = mk_ptr_array(type->cunion.fields.len, a);
        for (size_t i = 0; i < type->cunion.fields.len; i++) {
            PtrArray arg_nodes = mk_ptr_array(4, a);
            push_ptr(mk_str_doc(*name_to_string(type->cunion.fields.data[i].key), a), &arg_nodes);
            Document* tydoc = pretty_ctype(type->cunion.fields.data[i].val, a);
            if (i - 1 != type->proc.args.len) {
                PtrArray cat_nodes = mk_ptr_array(2, a);
                push_ptr(tydoc, &cat_nodes);
                push_ptr(mk_str_doc(mv_string(";"), a), &cat_nodes);
                tydoc = mv_cat_doc(cat_nodes, a);
            }
            push_ptr(tydoc, &arg_nodes);
            push_ptr(mv_group_doc(mv_sep_doc(arg_nodes, a), a), &field_nodes);
        }
        push_ptr(mv_nest_doc(2, mv_vsep_doc(field_nodes, a), a), &main_nodes);
        push_ptr(mk_cstr_doc("}", a), &main_nodes);
        return mv_sep_doc(main_nodes, a);
    }
    case CSPtr: {
        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(mk_str_doc(mv_string("*"), a) ,&nodes);
        push_ptr(pretty_ctype(type->ptr.inner, a) ,&nodes);
        return mv_cat_doc(nodes, a);
    }
    case CSIncomplete:
        return mk_str_doc(*name_to_string(type->incomplete), a);
    }
    // TODO (LOGIC BUG): this should be a return result or thrown error,
    // as it may indicate an error in user code, not internal code!
    panic(mv_string("Invalid CType"));
}

Document* pretty_cprimval(CPrimInt prim, void* data, Allocator* a) {
    if (prim.is_signed == Unsigned) {
        switch (prim.prim) {
        case CChar:
            return pretty_uchar(*(unsigned char*)data, a);
        case CShort:
            return pretty_ushort(*(unsigned short*)data, a);
        case CInt:
            return pretty_uint(*(unsigned int*)data, a);
        case CLong:
            return pretty_ulong(*(unsigned long*)data, a);
        case CLongLong:
            return pretty_ulong_long(*(unsigned long long*)data, a);
        }
    } else {
        // Otherwise, assume signed
        switch (prim.prim) {
        case CChar:
            return pretty_char(*(char*)data, a);
        case CShort:
            return pretty_short(*(short*)data, a);
        case CInt:
            return pretty_int(*(int*)data, a);
        case CLong:
            return pretty_long(*(long*)data, a);
        case CLongLong:
            return pretty_long_long(*(long long*)data, a);
        }
    }

    // TODO (LOGIC BUG): this should be a return result or thrown error,
    // as it may indicate an error in user code, not internal code!
    panic(mv_string("Invalid CPrim provided to pretty_cprimval"));
}

Document* pretty_cval(CType* type, void* data, Allocator* a) {
    switch (type->sort) {
    case CSVoid:
        return mk_str_doc(mv_string("<void>"), a);
    case CSPrimInt:
        return pretty_cprimval(type->prim, data, a);
    case CSFloat:
        return pretty_float(*(float*)data, a);
    case CSDouble:
        return pretty_double(*(double*)data, a);
    case CSCEnum: {
        return mk_str_doc(mv_string("pretty_cval not implemented for enum"), a);
    }
    case CSProc: {
        void** addr = (void**) data;
        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(mk_str_doc(mv_string("c-fun"), a), &nodes);
        push_ptr(pretty_ptr(*addr, a), &nodes);
        return mk_paren_doc("#<", ">", mv_sep_doc(nodes, a), a);
    }
    case CSStruct: {
        return mk_str_doc(mv_string("pretty_cval not implemented for struct"), a);
    }
    case CSUnion: {
        return mk_str_doc(mv_string("pretty_cval not implemented for union"), a);
    }
    case CSPtr: {
        return mk_str_doc(mv_string("pretty_cval not implemented for ptr"), a);
    }
    case CSIncomplete:
        return mk_str_doc(mv_string("pretty_cval not implemented for incomplete"), a);
    }

    // TODO (LOGIC BUG): this should be a return result or thrown error,
    // as it may indicate an error in user code, not internal code!
    panic(mv_string("Invalid CType provided to pretty_cval"));
}

size_t c_prim_size_of(CPrimInt type)
{
#if ABI == SYSTEM_V_64
    switch (type.prim)
    {
    // System V ABI
    case CChar:
        return 1;
    case CShort:
        return 2;
    case CInt:
        return 4;
    case CLong:
        // TODO (FEATURE):
        // May be 4? (ILP32)
        return 8;
    case CLongLong:
        // TODO (FEATURE):
        // May be 4? (ILP32)
        return 8;
    }
#elif ABI == WIN_64
    switch (type.prim)
    {
    // Win 64 V ABI
    case CChar:
        return 1;
    case CShort:
        return 2;
    case CInt:
        return 4;
    case CLong:
        return 4;
    case CLongLong:
        return 8;
    }
#else 
#error "Unknown host ABI"
#endif
    // TODO (LOGIC BUG): this should be a return result or thrown error,
    // as it may indicate an error in user code, not internal code!
    panic(mv_string("Invalid CType"));
}

size_t c_size_align(size_t size, size_t align) {
    size_t rem = size % align;
    size_t pad = rem == 0 ? 0 : align - rem;
    return size + pad;
}

size_t c_size_of(CType type) {
#if (ABI == SYSTEM_V_64 || ABI == WIN_64)
    // System V ABI
    switch (type.sort) {
    case CSVoid:
        return 0;
    case CSPrimInt:
        return c_prim_size_of(type.prim);
    case CSFloat:
        return 4;
    case CSDouble:
        return 8;
    case CSCEnum:
        return c_prim_size_of(type.enumeration.base);
    case CSProc:
        // NOTE: seems like you can get 32-bit pointers somehow
        // (ILP32)
        return 8;
    case CSStruct: {
        // From the standard:
        // Structures and unions assume the alignment of their most strictly aligned component. Each
        // member is assigned to the lowest available offset with the appropriate alignment. The size
        // of any object is always a multiple of the object‘s alignment.
        // 
        // Structure and union objects can require padding to meet size and alignment constraints.
        // The contents of any padding is undefined.
        size_t size = 0;
        size_t align = 0;
        size_t max_align = 0;
        for (size_t i = 0; i < type.structure.fields.len; i++) {
            align = c_align_of(type.structure.fields.data[i].val);
            max_align = max_align > align ? max_align : align;
            size = c_size_align(size, align);
            size += c_size_of(type.structure.fields.data[i].val);
        }
        return c_size_align(size, max_align);
    }
    case CSUnion: {
        // See struct for details
        size_t size = 0;
        for (size_t i = 0; i < type.cunion.fields.len; i++) {
            size_t tmp = c_size_of(*(CType*)type.cunion.fields.data[i].val);
            size = size > tmp ? size : tmp;
        }
        return size;
    }
    case CSPtr:
        // NOTE: seems like you can get 32-bit pointers somehow
        // (ILP32)
        return 8;
    case CSIncomplete:
        // TODO (LOGIC BUG): this should be a return result or thrown error,
        // as it may indicate an error in user code, not internal code!
        panic(mv_string("Cannot take size of incomplete c type"));
    }

#else 
#error "Unknown architecture when compiling c_size_of not implemented"
#endif
    // TODO (LOGIC BUG): this should be a return result or thrown error,
    // as it may indicate an error in user code, not internal code!
    panic(mv_string("invalid c size"));
}

size_t c_align_of(CType type) {
#if (ABI == SYSTEM_V_64 || ABI == WIN_64)

    // System V ABI
    switch (type.sort) {
    case CSVoid:
        return 0;
    case CSPrimInt:
        // In System V, size = align for primitive types.
        return c_prim_size_of(type.prim);
    case CSFloat:
        return c_prim_size_of(type.prim);
    case CSDouble:
        return c_prim_size_of(type.prim);
    case CSCEnum:
        // In System V, size = align for primitive types.
        return c_prim_size_of(type.enumeration.base);
    case CSProc:
        // NOTE: seems like you can get 32-bit pointers somehow
        // (ILP32)
        return 8;
    case CSStruct: {
        // From the standard:
        // Structures and unions assume the alignment of their most strictly aligned component. Each
        // member is assigned to the lowest available offset with the appropriate alignment. The size
        // of any object is always a multiple of the object‘s alignment.
        // 
        // Structure and union objects can require padding to meet size and alignment constraints.
        // The contents of any padding is undefined.
        size_t align = 0;
        for (size_t i = 0; i < type.structure.fields.len; i++) {
            size_t tmp = c_align_of(type.structure.fields.data[i].val);
            align = align > tmp ? align : tmp;
        }
        return align;
    }
    case CSUnion: {
        // See struct for details
        size_t align = 0;
        for (size_t i = 0; i < type.cunion.fields.len; i++) {
            size_t tmp = c_align_of(*(CType*)type.cunion.fields.data[i].val);
            align = align > tmp ? align : tmp;
        }
        return align;
    }
    case CSPtr:
        // NOTE: seems like you can get 32-bit pointers somehow
        // (ILP32)
        return 8;
    case CSIncomplete:
        // TODO (LOGIC BUG): this should be a return result or thrown error,
        // as it may indicate an error in user code, not internal code!
        panic(mv_string("Cannot take align of incomplete c type"));
    }

#else 
#error "Win 64 c alignof not implemented"
#endif
    // TODO (LOGIC BUG): this should be a return result or thrown error,
    // as it may indicate an error in user code, not internal code!
    panic(mv_string("invalid c size"));
}

// Resource Management
void delete_c_type(CType t, Allocator* a) {
    switch(t.sort) {
    case CSVoid:
    case CSPrimInt:
    case CSFloat:
    case CSDouble:
        break;
    case CSCEnum:
        sdelete_name_i64_assoc(t.enumeration.vals);
        break;
    case CSProc:
        for (size_t i = 0; i < t.proc.args.len; i++) {
            delete_c_type(t.proc.args.data[i].val, a);
        }
        sdelete_name_ctype_assoc(t.proc.args);
        delete_c_type_p(t.proc.ret, a);
        break;
    case CSStruct:
        for (size_t i = 0; i < t.structure.fields.len; i++) {
            delete_c_type(t.structure.fields.data[i].val, a);
        }
        sdelete_name_ctype_assoc(t.structure.fields);
        break;
    case CSUnion:
        for (size_t i = 0; i < t.cunion.fields.len; i++) {
            CType* ty = t.cunion.fields.data[i].val;
            delete_c_type_p(ty, a);
        }
        sdelete_name_ptr_amap(t.cunion.fields);
        break;
    case CSPtr:
        delete_c_type_p(t.ptr.inner, a);
        break;
    case CSIncomplete:
        break;
    }
}

void delete_c_type_p(CType* t, Allocator* a) {
    delete_c_type(*t, a);
    mem_free(t, a);
}

CType copy_c_type(CType t, Allocator* a) {
    CType out = t;
    switch(t.sort) {
    case CSPrimInt:
    case CSFloat:
    case CSDouble:
    case CSVoid:
        break;
    case CSCEnum:
        out.enumeration.vals = scopy_name_i64_assoc(t.enumeration.vals, a);
        break;
    case CSProc:
        out.proc.args = scopy_name_ctype_assoc(t.proc.args, a);
        for (size_t i = 0; i < t.proc.args.len; i++) {
            out.proc.args.data[i].val = copy_c_type(t.proc.args.data[i].val, a);
        }
        out.proc.ret = copy_c_type_p(t.proc.ret, a);
        break;
    case CSStruct:
        out.structure.fields = scopy_name_ctype_assoc(t.structure.fields, a);
        for (size_t i = 0; i < t.structure.fields.len; i++) {
            out.structure.fields.data[i].val = copy_c_type(t.structure.fields.data[i].val, a);
        }
        break;
    case CSUnion:
        out.cunion.fields = scopy_name_ptr_amap(t.cunion.fields, a);
        for (size_t i = 0; i < t.cunion.fields.len; i++) {
            out.cunion.fields.data[i].val = copy_c_type_p(t.cunion.fields.data[i].val, a);
        }
        break;
    case CSPtr:
        out.ptr.inner = copy_c_type_p(t.ptr.inner, a);
        break;
    case CSIncomplete:
        break;
    }
    return out;
}

CType* copy_c_type_p(CType* t, Allocator* a) {
    CType* ty = mem_alloc(sizeof(CType), a);
    *ty = copy_c_type(*t, a);
    return ty;
}

// Constructors and Utilities
// --------------------------
CType mk_voidptr_ctype(Allocator *a) {
    CType* void_ty = mem_alloc(sizeof(CType), a);
    *void_ty = (CType) {.sort = CSVoid};
    return (CType) {
        .sort = CSPtr,
        .ptr.inner = void_ty,
    };
}

CType mk_primint_ctype(CPrimInt t) {
    return (CType) {
        .sort = CSPrimInt,
        .prim = t,
    };
}

// Sample usage: mk_proc_type(a, 2, n1, arg_1_ty, n2, arg_2_ty, n3, ret_ty)
CType mk_fn_ctype(Allocator* a, size_t nargs, ...) {
    va_list args;
    va_start(args, nargs);

    NameCTypeAssoc pargs = mk_name_ctype_assoc(nargs, a);
    for (size_t i = 0; i < nargs; i++) {
        Name name = string_to_name(mv_string(va_arg(args, const char*)));
        CType arg = va_arg(args, CType);
        name_ctype_bind(name, arg, &pargs);
    }

    CType* ret = mem_alloc(sizeof(CType), a);
    *ret = va_arg(args, CType);
    va_end(args);

    return (CType) {
        .sort = CSProc,
        .proc.named_tag = 1,
        .proc.args = pargs,
        .proc.ret= ret,
    };
}

// Sample usage: mk_proc_type(a, 2, "field-1", field_1_ty, "field-2", arg_2_ty)
CType mk_struct_ctype(Allocator* a, size_t nfields, ...) {
    va_list args;
    va_start(args, nfields);

    NameCTypeAssoc fields = mk_name_ctype_assoc(nfields, a);
    for (size_t i = 0; i < nfields; i++) {
        Name name = string_to_name(mv_string(va_arg(args, const char*)));
        name_ctype_bind(name, va_arg(args, CType), &fields);
    }

    return (CType) {
        .sort = CSStruct,
        .structure.named_tag = 1, // None
        .structure.fields = fields,
    };
}

// Sample usage: mk_enum_type(a, CInt, 2, "true", 0, "false", 1)
CType mk_enum_ctype(Allocator* a, CPrimInt store, size_t nfields, ...) {
    va_list args;
    va_start(args, nfields);

    NameI64Assoc vals = mk_name_i64_assoc(nfields, a);
    for (size_t i = 0; i < nfields; i++) {
        Name name = string_to_name(mv_string(va_arg(args, const char*)));
        int64_t arg = va_arg(args, int64_t);
        name_i64_bind(name, arg, &vals);
    }

    CType* ret = mem_alloc(sizeof(CType), a);
    *ret = va_arg(args, CType);
    va_end(args);

    return (CType) {
        .sort = CSStruct,
        .enumeration.base = store,
        .enumeration.vals = vals,
    };
}

// Sample usage: mk_union_type(a, 3, )
CType mk_union_ctype(Allocator* a, size_t nfields, ...) {
    va_list args;
    va_start(args, nfields);

    NamePtrAMap fields = mk_name_ptr_amap(nfields, a);
    for (size_t i = 0; i < nfields; i++) {
        Name name = string_to_name(mv_string(va_arg(args, const char*)));
        CType* arg = mem_alloc(sizeof(CType), a);
        *arg = va_arg(args, CType);
        name_ptr_insert(name, arg, &fields);
    }

    return (CType) {
        .sort = CSUnion,
        .cunion.fields = fields,
    };
}

CType mk_string_ctype(Allocator *a) {
    return mk_struct_ctype(a, 2,
                    "memsize", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                    "bytes", mk_voidptr_ctype(a));
}

CType mk_allocator_ctype(Allocator *a) {
    return mk_struct_ctype(a, 4,
                           "malloc", mk_voidptr_ctype(a),
                           "realloc", mk_voidptr_ctype(a),
                           "free", mk_voidptr_ctype(a),
                           "context", mk_voidptr_ctype(a));
}

CType mk_list_ctype(Allocator *a) {
    return mk_struct_ctype(a, 4,
                           "data", mk_voidptr_ctype(a),
                           "len", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                           "capacity", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                           "allocator", mk_allocator_ctype(a));
}

CType c_size_type;
CType c_void;
void init_ctypes() {
#if (ABI == SYSTEM_V_64 || ABI == WIN_64)
    c_size_type = (CType) {.sort = CSPrimInt, .prim = (CPrimInt){.prim = CLongLong, .is_signed = Unsigned}};
    c_void = (CType){.sort = CSVoid};
#else 
#error "Unknown host ABI"
#endif
}
