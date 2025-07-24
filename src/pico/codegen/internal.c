#include "platform/machine_info.h"
#include "data/meta/array_impl.h"

#include "pico/codegen/codegen.h"
#include "pico/codegen/internal.h"
#include "pico/stdlib/extra.h"

int compare_to_generate(ToGenerate lhs, ToGenerate rhs) {
    int diff_1 = lhs.offset - rhs.offset;
    if (diff_1) return diff_1;
    return lhs.expr - rhs.expr;
}

ARRAY_CMP_IMPL(ToGenerate, compare_to_generate, to_gen, ToGen);

void backlink_global(Symbol sym, size_t offset, InternalLinkData* links, Allocator* a) {
    // Step 1: Try lookup or else create & insert 
    SizeArray* sarr = sym_sarr_lookup(sym, links->links.external_links);

    if (!sarr) {
        // Create & Insert
        sym_sarr_insert(sym, mk_size_array(4, a), &links->links.external_links);
        sarr = sym_sarr_lookup(sym, links->links.external_links);
    }

    // Step 2: insert offset into array
    push_size(offset, sarr);
}

void backlink_code(Target target, size_t offset, InternalLinkData* links) {
    U8Array assembly = get_instructions(target.code_aux);
    
    LinkMetaData link = (LinkMetaData) {
        .source_offset = offset,
        .dest_offset = assembly.len,
    };

    if (target.target == target.code_aux) {
        push_link_meta(link, &links->links.cc_links);
    } else {
        push_link_meta(link, &links->links.ec_links);
    }
}

void backlink_data(Target target, size_t offset, InternalLinkData* links) {
    LinkMetaData link = (LinkMetaData) {
        .source_offset = offset,
        .dest_offset = target.data_aux->len,
    };

    if (target.target == target.code_aux) {
        push_link_meta(link, &links->links.cd_links);
    } else {
        push_link_meta(link, &links->links.ed_links);
    }
}

void backlink_data_data(Target target, size_t location, size_t offset, InternalLinkData* links) {
    LinkMetaData link = (LinkMetaData) {
        .source_offset = offset,
        .dest_offset = location,
    };

    push_link_meta(link, &links->links.dd_links);
}

void backlink_goto(Symbol sym, size_t offset, InternalLinkData* links, Allocator* a) {
    // Step 1: Try lookup or else create & insert 
    SizeArray* sarr = sym_sarr_lookup(sym, links->gotolinks);

    if (!sarr) {
        // Create & Insert
        sym_sarr_insert(sym, mk_size_array(4, a), &links->gotolinks);
        sarr = sym_sarr_lookup(sym, links->gotolinks);
    }

    // Step 2: insert offset into array
    push_size(offset, sarr);
}

void generate_stack_move(size_t dest_stack_offset, size_t src_stack_offset, size_t size, Assembler* ass, Allocator* a, ErrorPoint* point) {
    if (size== 0 || dest_stack_offset == src_stack_offset) return; // nothing to do

    if ((dest_stack_offset + size) > 127 || (src_stack_offset + size) > 127)  {
        // TODO: check to ensure offsets don't exceed 32 bit max

#if ABI == SYSTEM_V_64
        // stack_move (dest = rdi, src = rsi, size = rdx)
        // copy size into RDX
        build_binary_op(ass, Mov, reg(RDI, sz_64), imm32(dest_stack_offset), a, point);
        build_binary_op(ass, Add, reg(RDI, sz_64), reg(RSP, sz_64), a, point);
        build_binary_op(ass, Mov, reg(RSI, sz_64), imm32(src_stack_offset), a, point);
        build_binary_op(ass, Add, reg(RSI, sz_64), reg(RSP, sz_64), a, point);
        build_binary_op(ass, Mov, reg(RDX, sz_64), imm32(size), a, point);

#elif ABI == WIN_64
        // stack_move (dest = rcx, src = rdx, size = r8)
        build_binary_op(ass, Mov, reg(RCX, sz_64), imm32(dest_stack_offset), a, point);
        build_binary_op(ass, Add, reg(RCX, sz_64), reg(RSP, sz_64), a, point);
        build_binary_op(ass, Mov, reg(RDX, sz_64), imm32(src_stack_offset), a, point);
        build_binary_op(ass, Add, reg(RDX, sz_64), reg(RSP, sz_64), a, point);
        build_binary_op(ass, Mov, reg(R8, sz_64), imm32(size), a, point);
#else
#error "Unknown calling convention"
#endif

        generate_c_call(memmove, ass, a, point);
    } else  {
        // Note: we are justified here in using Using 8-bit immediate (rref8)
        size_t leftover = size % 8;
        if (leftover >= 4) {
            build_binary_op(ass, Mov, reg(RAX, sz_32), rref8(RSP, src_stack_offset + (size & ~7), sz_32), a, point);
            build_binary_op(ass, Mov, rref8(RSP, dest_stack_offset + (size & ~7), sz_32), reg(RAX, sz_32), a, point);
            leftover -= 4;
        }
        if (leftover >= 2) {
            build_binary_op(ass, Mov, reg(RAX, sz_16), rref8(RSP, src_stack_offset + (size & ~3), sz_16), a, point);
            build_binary_op(ass, Mov, rref8(RSP, dest_stack_offset + (size & ~3), sz_16), reg(RAX, sz_16), a, point);
            leftover -= 2;
        }
        if (leftover >= 1) {
            build_binary_op(ass, Mov, reg(RAX, sz_8), rref8(RSP, src_stack_offset + (size & ~1), sz_8), a, point);
            build_binary_op(ass, Mov, rref8(RSP, dest_stack_offset + (size & ~1), sz_8), reg(RAX, sz_8), a, point);
            leftover -= 1;
        }

        for (size_t i = 0; i < size / 8; i++) {
            build_binary_op(ass, Mov, reg(RAX, sz_64), rref8(RSP, src_stack_offset + (i * 8) , sz_64), a, point);
            build_binary_op(ass, Mov, rref8(RSP, dest_stack_offset + (i * 8), sz_64), reg(RAX, sz_64), a, point);
        }
    }
}

void generate_stack_copy(Regname dest, size_t size, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // Like a monomorphic copy, except that we start at the 'end' and go backwards
    const Regname src = RSP;

    size_t leftover = size % 8;
    if (leftover >= 4) {
        build_binary_op(ass, Mov, reg(RAX, sz_32), rref8(src, (size & ~7), sz_32), a, point);
        build_binary_op(ass, Mov, rref8(dest, (size & ~7), sz_32), reg(RAX, sz_32), a, point);
        leftover -= 4;
    }
    if (leftover >= 2) {
        build_binary_op(ass, Mov, reg(RAX, sz_16), rref8(src, (size & ~3), sz_16), a, point);
        build_binary_op(ass, Mov, rref8(dest, (size & ~3), sz_16), reg(RAX, sz_16), a, point);
        leftover -= 2;
    }
    if (leftover >= 1) {
        build_binary_op(ass, Mov, reg(RAX, sz_8), rref8(src, (size & ~1), sz_8), a, point);
        build_binary_op(ass, Mov, rref8(dest, (size & ~1), sz_8), reg(RAX, sz_8), a, point);
        leftover -= 1;
    }

    // first, assert that size_t is divisible by 8 (as we generally expect 8-byte alignment to be maintained on the stack at all times.)
    if (size > 127)  {
        throw_error(point, mv_string("Error in generate_stack_copy expected copy size to be divisible by 8"));
    };

    // Then, check that using an 8-bit immediate offset is ok
    if (size > 127)  {
        throw_error(point, mv_string("Error in generate_stack_copy: copy size must be smaller than 127!"));
    };

    if (src == RAX || dest == RAX)  {
        throw_error(point, mv_string("Error in generate_stack_copy: cannoy copy from/to RAX"));
    };

    for (size_t i = 0; i < size / 8; i++) {
        size_t j = (size / 8) - (i + 1);
        build_binary_op(ass, Mov, reg(RAX, sz_64), rref8(src, j * 8, sz_64), a, point);
        build_binary_op(ass, Mov, rref8(dest, j * 8, sz_64), reg(RAX, sz_64), a, point);
    }
}

void generate_monomorphic_copy(Regname dest, Regname src, size_t size, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // First, assert that size_t is divisible by 8 ( we use rax for copies )
    if (size > 127)  {
        throw_error(point, mv_string("Error in generate_monomorphic_copy: copy size must be smaller than 127!"));
    };

    if (src == RAX || dest == RAX)  {
        throw_error(point, mv_string("Error in generate_monomorphic_copy: cannoy copy from/to RAX"));
    };

    for (size_t i = 0; i < size / 8; i++) {
        build_binary_op(ass, Mov, reg(RAX, sz_64), rref8(src, i * 8, sz_64), a, point);
        build_binary_op(ass, Mov, rref8(dest, i * 8, sz_64), reg(RAX, sz_64), a, point);
    }

    size_t leftover = size % 8;
    if (leftover >= 4) {
        build_binary_op(ass, Mov, reg(RAX, sz_32), rref8(src, (size & ~7), sz_32), a, point);
        build_binary_op(ass, Mov, rref8(dest, size / 8, sz_32), reg(RAX, sz_32), a, point);
        leftover -= 4;
    }
    if (leftover >= 2) {
        build_binary_op(ass, Mov, reg(RAX, sz_16), rref8(src, (size & ~3), sz_16), a, point);
        build_binary_op(ass, Mov, rref8(dest, size / 8, sz_16), reg(RAX, sz_16), a, point);
        leftover -= 2;
    }
    if (leftover >= 1) {
        build_binary_op(ass, Mov, reg(RAX, sz_8), rref8(src, (size & ~1), sz_8), a, point);
        build_binary_op(ass, Mov, rref8(dest, size / 8, sz_8), reg(RAX, sz_8), a, point);
        leftover -= 1;
    }
}

void generate_monomorphic_swap(Regname loc1, Regname loc2, size_t size, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // First, assert that size_t is divisible by 8 ( we use rax for copies )
    if (size % 8 != 0)  {
        throw_error(point, mv_string("Error in generate_monomorphic_swap expected copy size to be divisible by 8"));
    };

    if (size > 127)  {
        throw_error(point, mv_string("Error in generate_monomorphic_swap copy size must be smaller than 127!"));
    };

    if (loc1 == RDI || loc2 == RDI || loc1 == RSI || loc2 == RSI)  {
        throw_error(point, mv_string("Error in generate_monomorphic_swap cannot swap with RDI or RSI"));
    };

    for (size_t i = 0; i < size / 8; i++) {
        build_binary_op(ass, Mov, reg(RDI, sz_64), rref8(loc1, i * 8, sz_64), a, point);
        build_binary_op(ass, Mov, reg(RSI, sz_64), rref8(loc2, i * 8, sz_64), a, point);
        build_binary_op(ass, Mov, rref8(loc1, i * 8, sz_64), reg(RSI, sz_64), a, point);
        build_binary_op(ass, Mov, rref8(loc2, i * 8, sz_64), reg(RDI, sz_64), a, point);
    }
}

void generate_c_call(void* cfn, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // Align RSP to closest 16 bytes
    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(8), a, point);
    build_binary_op(ass, Mov, reg(RAX, sz_64), reg(RSP, sz_64), a, point);
    build_binary_op(ass, And, reg(RAX, sz_64), imm8(0xf), a, point);
    build_binary_op(ass, Mov, reg(R9, sz_64), imm32(0x10), a, point);
    build_binary_op(ass, Sub, reg(R9, sz_64), reg(RAX, sz_64), a, point);
    build_binary_op(ass, And, reg(R9, sz_64), imm8(0xf), a, point);
    build_binary_op(ass, Sub, reg(RSP, sz_64), reg(R9, sz_64), a, point);
    build_binary_op(ass, Mov, rref8(RSP, 0, sz_64), reg(R9, sz_64), a, point);
    
#if ABI == WIN_64
    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#endif 

    build_binary_op(ass, Mov, reg(RAX, sz_64), imm64((uint64_t)cfn), a, point);
    build_unary_op(ass, Call, reg(RAX, sz_64), a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(32), a, point);
#endif 
    
    build_unary_op(ass, Pop, reg(RCX, sz_64),  a, point);
    build_binary_op(ass, Add, reg(RSP, sz_64), reg(RCX, sz_64), a, point);
}

void* tmp_malloc(uint64_t memsize) {
    Allocator a = get_std_temp_allocator();
    return mem_alloc(memsize, &a);
}

void generate_tmp_malloc(Location dest, Location mem_size, Assembler* ass, Allocator* a, ErrorPoint* point) {
#if ABI == SYSTEM_V_64
    build_binary_op(ass, Mov, reg(RDI, sz_64), mem_size, a, point);
#elif ABI == WIN_64
    build_binary_op(ass, Mov, reg(RCX, sz_64), mem_size, a, point);
#else 
    #error "Unknown calling convention"
#endif

    generate_c_call(tmp_malloc, ass, a, point);

    if (dest.type != Dest_Register && dest.reg != RAX) {
        build_binary_op(ass, Mov, dest, reg(RAX, sz_64), a, point);
    }
}

void* perm_malloc(uint64_t memsize) {
    Allocator a = get_std_perm_allocator();
    return mem_alloc(memsize, &a);
}

void generate_perm_malloc(Location dest, Location mem_size, Assembler* ass, Allocator* a, ErrorPoint* point) {
#if ABI == SYSTEM_V_64
    build_binary_op(ass, Mov, reg(RDI, sz_64), mem_size, a, point);
#elif ABI == WIN_64
    build_binary_op(ass, Mov, reg(RCX, sz_64), mem_size, a, point);
#else 
    #error "Unknown calling convention"
#endif

    generate_c_call(perm_malloc, ass, a, point);

    if (dest.type != Dest_Register && dest.reg != RAX) {
        build_binary_op(ass, Mov, dest, reg(RAX, sz_64), a, point);
    }
}

PiType* internal_type_app(PiType* val, PiType** args_rev, size_t num_args) {
    Allocator a = get_std_temp_allocator();
    // make args correct way round!
    void** args = mem_alloc(sizeof(void*) * num_args, &a);
    for (size_t i = 0; i < num_args; i++){
        args[i] = args_rev[(num_args - 1) - i];
    }
    PiType fam = *val;
    PtrArray arr = (PtrArray){.data = (void**)args, .len = num_args, .size = num_args};
    return type_app (fam, arr, &a);
}

void gen_mk_family_app(size_t nfields, Assembler* ass, Allocator* a, ErrorPoint* point) {
#if ABI == SYSTEM_V_64
    build_unary_op(ass, Pop, reg(RDI, sz_64), a, point);
    build_binary_op(ass, Mov, reg(RSI, sz_64), reg(RSP, sz_64), a, point);
    build_binary_op(ass, Mov, reg(RDX, sz_64), imm32(nfields), a, point);
#elif ABI == WIN_64
    build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);
    build_binary_op(ass, Mov, reg(RDX, sz_64), reg(RSP, sz_64), a, point);
    build_binary_op(ass, Mov, reg(R8, sz_64), imm32(nfields), a, point);
#else 
    #error "Unknown calling convention"
#endif

    generate_c_call(internal_type_app, ass, a, point);

    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(nfields * ADDRESS_SIZE), a, point);
    build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
}

void* mk_struct_ty(size_t len, void* data) {
    Allocator a = get_std_temp_allocator();

    PiType* ty = mem_alloc(sizeof(PiType), &a);
    *ty = (PiType) {
        .sort = TStruct,
        .structure.fields.data = data,
        .structure.fields.len = len,
        .structure.fields.capacity = len,
        .structure.fields.gpa = a,
    };
    return ty;
}

void gen_mk_struct_ty(Location dest, Location nfields, Location data, Assembler* ass, Allocator* a, ErrorPoint* point) {
#if ABI == SYSTEM_V_64
    build_binary_op(ass, Mov, reg(RDI, sz_64), nfields, a, point);
    build_binary_op(ass, Mov, reg(RSI, sz_64), data, a, point);
#elif ABI == WIN_64
    build_binary_op(ass, Mov, reg(RCX, sz_64), nfields, a, point);
    build_binary_op(ass, Mov, reg(RDX, sz_64), data, a, point);
    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#else 
    #error "Unknown calling convention"
#endif

    generate_c_call(mk_struct_ty, ass, a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(32), a, point);
#endif 

    if (dest.type != Dest_Register && dest.reg != RAX) {
        build_binary_op(ass, Mov, dest, reg(RAX, sz_64), a, point);
    }
}

void* mk_proc_ty(size_t len, void** data, void* ret) {
    Allocator a = get_std_temp_allocator();

    PiType* ty = mem_alloc(sizeof(PiType), &a);
    *ty = (PiType) {
        .sort = TProc,
        .proc.args.data = data,
        .proc.args.len = len,
        .proc.args.size = len,
        .proc.args.gpa = a,
        .proc.ret = ret,
    };
    return ty;
}

void gen_mk_proc_ty(Location dest, Location nfields, Location data, Location ret, Assembler* ass, Allocator* a, ErrorPoint* point) {
#if ABI == SYSTEM_V_64
    build_binary_op(ass, Mov, reg(RDI, sz_64), nfields, a, point);
    build_binary_op(ass, Mov, reg(RSI, sz_64), data, a, point);
    build_binary_op(ass, Mov, reg(RDX, sz_64), ret, a, point);
#elif ABI == WIN_64
    build_binary_op(ass, Mov, reg(RCX, sz_64), nfields, a, point);
    build_binary_op(ass, Mov, reg(RDX, sz_64), data, a, point);
    build_binary_op(ass, Mov, reg(R8, sz_64), ret, a, point);
#else 
    #error "Unknown calling convention"
#endif

    generate_c_call(mk_proc_ty, ass, a, point);

    if (dest.type != Dest_Register && dest.reg != RAX) {
        build_binary_op(ass, Mov, dest, reg(RAX, sz_64), a, point);
    }
}

void* mk_enum_ty(size_t len, uint64_t* shape, SymPtrCell* data) {
    Allocator a = get_std_temp_allocator();

    SymPtrAMap variants = mk_sym_ptr_amap(len, &a);
    for (size_t i = 0; i < len; i++) {
        PtrArray* arr = mem_alloc(sizeof(PtrArray), &a);
        uint64_t num_vals = shape[i];

        *arr = (PtrArray) {
            .data = data[i].val,
            .len = num_vals,
            .size = num_vals,
            .gpa = a,
        };
        sym_ptr_insert(data[i].key, arr, &variants);
    }

    PiType* ty = mem_alloc(sizeof(PiType), &a);
    *ty = (PiType) {
        .sort = TEnum,
        .enumeration.variants = variants,
    };
    return ty;
}

void gen_mk_enum_ty(Location dest, SynEnumType shape, Location data, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // Generate a dynamic allocation
    // Note: this allocation is fine for definitions as types get copied,
    // probably not fine if we have a proc which returns an enum!
    // in that case we maybe want this in a data-segment?
    uint64_t* sml_shape = mem_alloc(sizeof(uint64_t) * shape.variants.len, a);
    for (size_t i = 0; i < shape.variants.len; i++) {
        sml_shape[i] = ((PtrArray*)shape.variants.data[i].val)->len;
    }

#if ABI == SYSTEM_V_64
    build_binary_op(ass, Mov, reg(RDI, sz_64), imm64(shape.variants.len), a, point);
    build_binary_op(ass, Mov, reg(RSI, sz_64), imm64((uint64_t)sml_shape), a, point);
    build_binary_op(ass, Mov, reg(RDX, sz_64), data, a, point);
#elif ABI == WIN_64
    build_binary_op(ass, Mov, reg(RCX, sz_64), imm64(shape.variants.len), a, point);
    build_binary_op(ass, Mov, reg(RDX, sz_64), imm64((uint64_t)sml_shape), a, point);
    build_binary_op(ass, Mov, reg(R8, sz_64), data, a, point);

    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#else 
    #error "Unknown calling convention"
#endif

    generate_c_call(mk_enum_ty, ass, a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(32), a, point);
#endif 

    if (dest.type != Dest_Register && dest.reg != RAX) {
        build_binary_op(ass, Mov, dest, reg(RAX, sz_64), a, point);
    }
}

void* mk_reset_ty(PiType* in, PiType* out) {
    Allocator a = get_std_temp_allocator();

    PiType* ty = mem_alloc(sizeof(PiType), &a);
    *ty = (PiType) {
        .sort = TReset,
        .reset.in = in,
        .reset.out = out,
    };
    return ty;
}

void gen_mk_reset_ty(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // Pop in reverse order
#if ABI == SYSTEM_V_64
    build_unary_op(ass, Pop, reg(RSI, sz_64), a, point);
    build_unary_op(ass, Pop, reg(RDI, sz_64), a, point);
#elif ABI == WIN_64
    build_unary_op(ass, Pop, reg(RDX, sz_64), a, point);
    build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);

    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#else 
    #error "Unknown calling convention"
#endif

    generate_c_call(mk_reset_ty, ass, a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(32), a, point);
#endif 
    build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
}


void* mk_dynamic_ty(PiType* dynamic) {
    Allocator a = get_std_temp_allocator();

    PiType* ty = mem_alloc(sizeof(PiType), &a);
    *ty = (PiType) {
        .sort = TDynamic,
        .dynamic = dynamic,
    };
    return ty;
}

void gen_mk_dynamic_ty(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // Pop in reverse order
#if ABI == SYSTEM_V_64
    build_unary_op(ass, Pop, reg(RDI, sz_64), a, point);
#elif ABI == WIN_64
    build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);

    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#else 
    #error "Unknown calling convention"
#endif

    generate_c_call(mk_dynamic_ty, ass, a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(32), a, point);
#endif 
    build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
}

void* mk_type_var(Symbol var) {
    Allocator a = get_std_temp_allocator();

    PiType* ty = mem_alloc(sizeof(PiType), &a);
    *ty = (PiType) {
        .sort = TVar,
        .var = var,
    };
    return ty;
}

void gen_mk_type_var(Symbol var, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // TODO (BUG) 
#if ABI == SYSTEM_V_64
    build_binary_op(ass, Mov, reg(RDI, sz_64), imm64(var.name), a, point);
    build_binary_op(ass, Mov, reg(RSI, sz_64), imm64(var.did), a, point);
#elif ABI == WIN_64
    build_unary_op(ass, Push, imm32(var.did), a, point);
    build_unary_op(ass, Push, imm32(var.name), a, point);
    build_binary_op(ass, Mov, reg(RCX, sz_64), reg(RSP, sz_64), a, point);

    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(0x20), a, point);
#else 
    #error "Unknown calling convention"
#endif

    generate_c_call(mk_type_var, ass, a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(0x30), a, point);
#endif 
    build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
}

void* mk_forall_ty(size_t len, Symbol* syms, PiType* body) {
    Allocator a = get_std_temp_allocator();

    PiType* ty = mem_alloc(sizeof(PiType), &a);
    *ty = (PiType) {
        .sort = TAll,
        .binder.vars.data = syms,
        .binder.vars.len = len,
        .binder.vars.size = len,
        .binder.vars.gpa = a,
        .binder.body = body,
    };
    return ty;
}


void gen_mk_forall_ty(SymbolArray syms, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // Note: this allocation is fine for definitions as types get copied,
    // probably not fine if we have a proc which returns a forall!
    // in that case we maybe want this in a data-segment?

    void* data = mem_alloc(syms.len * sizeof(Symbol), a);
    memcpy(data, syms.data, syms.len * sizeof(Symbol));

#if ABI == SYSTEM_V_64
    build_binary_op(ass, Mov, reg(RDI, sz_64), imm64(syms.len), a, point);
    build_binary_op(ass, Mov, reg(RSI, sz_64), imm64((uint64_t)data),a, point);
    build_unary_op(ass, Pop, reg(RDX, sz_64), a, point);
#elif ABI == WIN_64
    build_binary_op(ass, Mov, reg(RCX, sz_64), imm64(syms.len), a, point);
    build_binary_op(ass, Mov, reg(RDX, sz_64), imm64((uint64_t)data),a, point);
    build_unary_op(ass, Pop, reg(R8, sz_64), a, point);

    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#else 
    #error "Unknown calling convention"
#endif

    generate_c_call(mk_forall_ty, ass, a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(32), a, point);
#endif 
    build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
}

void* mk_fam_ty(size_t len, Symbol* syms, PiType* body) {
    Allocator a = get_std_temp_allocator();

    PiType* ty = mem_alloc(sizeof(PiType), &a);
    *ty = (PiType) {
        .sort = TFam,
        .binder.vars.data = syms,
        .binder.vars.len = len,
        .binder.vars.size = len,
        .binder.vars.gpa = a,
        .binder.body = body,
    };
    return ty;
}

void gen_mk_fam_ty(SymbolArray syms, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // Note: this allocation is fine for definitions as types get copied,
    // probably not fine if we have a proc which returns a family!
    // in that case we maybe want this in a data-segment?
    void* data = mem_alloc(syms.len * sizeof(Symbol), a);
    memcpy(data, syms.data, syms.len * sizeof(Symbol));

#if ABI == SYSTEM_V_64
    build_binary_op(ass, Mov, reg(RDI, sz_64), imm64(syms.len), a, point);
    build_binary_op(ass, Mov, reg(RSI, sz_64), imm64((uint64_t)data),a, point);
    build_unary_op(ass, Pop, reg(RDX, sz_64), a, point);
#elif ABI == WIN_64
    build_binary_op(ass, Mov, reg(RCX, sz_64), imm64(syms.len), a, point);
    build_binary_op(ass, Mov, reg(RDX, sz_64), imm64((uint64_t)data),a, point);
    build_unary_op(ass, Pop, reg(R8, sz_64), a, point);

    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#else 
    #error "Unknown calling convention"
#endif

    generate_c_call(mk_fam_ty, ass, a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(32), a, point);
#endif 
    build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
}

void* mk_c_ty(CType* body) {
    Allocator a = get_std_temp_allocator();

    PiType* ty = mem_alloc(sizeof(PiType), &a);
    *ty = (PiType) {
        .sort = TCType,
        .c_type = *body,
    };
    return ty;
}

void gen_mk_c_ty(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // The current RSP is the first argument we want to pass in

#if ABI == SYSTEM_V_64
    build_binary_op(ass, Mov, reg(RDI, sz_64), reg(RSP, sz_64), a, point);
#elif ABI == WIN_64
    build_binary_op(ass, Mov, reg(RCX, sz_64), reg(RSP, sz_64), a, point);

    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#else 
    #error "Unknown calling convention"
#endif

    generate_c_call(mk_c_ty, ass, a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(32), a, point);
#endif 
    build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
}

void* mk_named_ty(Symbol name, PiType* body) {
    Allocator a = get_std_temp_allocator();

    PiType* ty = mem_alloc(sizeof(PiType), &a);
    *ty = (PiType) {
        .sort = TNamed,
        .named.name = name,
        .named.type = body,
        .named.args = NULL,
    };
    return ty;
}

void gen_mk_named_ty(Assembler* ass, Allocator* a, ErrorPoint* point) {
#if ABI == SYSTEM_V_64
    build_unary_op(ass, Pop, reg(RSI, sz_64), a, point);
    build_unary_op(ass, Pop, reg(RDI, sz_64), a, point);
#elif ABI == WIN_64
    build_unary_op(ass, Pop, reg(RDX, sz_64), a, point);
    build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);
    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#else 
    #error "Unknown calling convention"
#endif

    generate_c_call(mk_named_ty, ass, a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(32), a, point);
#endif 
    build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
}

void* mk_distinct_ty(PiType* body) {
    Allocator a = get_std_temp_allocator();

    PiType* ty = mem_alloc(sizeof(PiType), &a);
    *ty = (PiType) {
        .sort = TDistinct,
        .distinct.type = body,
        .distinct.id = distinct_id(),
        .distinct.source_module = NULL,
        .distinct.args = NULL,
    };
    return ty;
}

void gen_mk_distinct_ty(Assembler* ass, Allocator* a, ErrorPoint* point) {
#if ABI == SYSTEM_V_64
    build_unary_op(ass, Pop, reg(RDI, sz_64), a, point);
#elif ABI == WIN_64
    build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);
    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#else 
    #error "Unknown calling convention"
#endif

    generate_c_call(mk_distinct_ty, ass, a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(32), a, point);
#endif 
    build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
}

void* mk_opaque_ty(PiType* body) {
    Allocator a = get_std_temp_allocator();
    Module* current = get_std_current_module();

    PiType* ty = mem_alloc(sizeof(PiType), &a);
    *ty = (PiType) {
        .sort = TDistinct,
        .distinct.type = body,
        .distinct.id = distinct_id(),
        .distinct.source_module = current,
        .distinct.args = NULL,
    };
    return ty;
}

void gen_mk_opaque_ty(Assembler* ass, Allocator* a, ErrorPoint* point) {
#if ABI == SYSTEM_V_64
    build_unary_op(ass, Pop, reg(RDI, sz_64), a, point);
#elif ABI == WIN_64
    build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);
    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#else 
    #error "Unknown calling convention"
#endif

    generate_c_call(mk_opaque_ty, ass, a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(32), a, point);
#endif 
    build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
}


void* mk_trait_ty(size_t sym_len, Symbol* syms, size_t field_len, void* data) {
    Allocator a = get_std_temp_allocator();

    PiType* ty = mem_alloc(sizeof(PiType), &a);
    *ty = (PiType) {
        .sort = TTrait,
        .trait.id = distinct_id(),

        .trait.vars.data = syms,
        .trait.vars.len = sym_len,
        .trait.vars.size = sym_len,
        .trait.vars.gpa = a,

        .trait.fields.data = data,
        .trait.fields.len = field_len,
        .trait.fields.capacity = field_len,
        .trait.fields.gpa = a,
    };
    return ty;
}

void gen_mk_trait_ty(SymbolArray syms, Location dest, Location nfields, Location data, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // Note: this allocation is fine for definitions as types get copied,
    // probably not fine if we have a proc which returns a family!
    // in that case we maybe want this in a data-segment?
    void* sym_data = mem_alloc(syms.len * sizeof(Symbol), a);
    memcpy(sym_data, syms.data, syms.len * sizeof(Symbol));

#if ABI == SYSTEM_V_64
    build_binary_op(ass, Mov, reg(RDI, sz_64), imm64(syms.len), a, point);
    build_binary_op(ass, Mov, reg(RSI, sz_64), imm64((uint64_t)sym_data), a, point);
    build_binary_op(ass, Mov, reg(RDX, sz_64), nfields, a, point);
    build_binary_op(ass, Mov, reg(RCX, sz_64), data, a, point);
#elif ABI == WIN_64
    build_binary_op(ass, Mov, reg(RCX, sz_64), imm64(syms.len), a, point);
    build_binary_op(ass, Mov, reg(RDX, sz_64), imm64((uint64_t)sym_data), a, point);
    build_binary_op(ass, Mov, reg(R8, sz_64), nfields, a, point);
    build_binary_op(ass, Mov, reg(R9, sz_64), data, a, point);

    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#else 
    #error "Unknown calling convention"
#endif

    generate_c_call(mk_trait_ty, ass, a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(32), a, point);
#endif 

    if (dest.type != Dest_Register && dest.reg != RAX) {
        build_binary_op(ass, Mov, dest, reg(RAX, sz_64), a, point);
    }
}

