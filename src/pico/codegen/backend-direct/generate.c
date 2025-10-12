#include <string.h>

#include "data/num.h"
#include "data/stringify.h"
#include "platform/signals.h"
#include "platform/machine_info.h"
#include "platform/memory/executable.h"

#include "components/pretty/string_printer.h"

#include "pico/data/error.h"
#include "pico/codegen/backend-direct/generate.h"
#include "pico/codegen/backend-direct/internal.h"
#include "pico/codegen/backend-direct/polymorphic.h"
#include "pico/codegen/backend-direct/foreign_adapters.h"
#include "pico/binding/address_env.h"
#include "pico/eval/call.h"

#define STACK_ALIGN 8

/* Code Generation Assumptions:
 * • All expressions evaluate to integers or functions 
 * • All functions are just an address to call to (or jmp to for tail calls) 
 * • All terms are well-typed
 * 
 * On the calling convention used:
 * Calling a function:
 * • Push all arguments onto the stack in left-right order (i.e. rightmost arg
 *   is top of stack)
 */

// Internal functions
static void generate_entry(size_t out_sz, Target target, Allocator* a, ErrorPoint* point);
static void generate_exit(size_t out_sz, Target target, Allocator* a, ErrorPoint* point);
void* const_fold(Syntax *syn, AddressEnv *env, Target target, InternalLinkData* links, Allocator *a, ErrorPoint *point);

LinkData bd_generate_toplevel(TopLevel top, Environment* env, Target target, Allocator* a, ErrorPoint* point) {
    InternalLinkData links = (InternalLinkData) {
        .links = (LinkData) {
            .external_links = mk_sym_sarr_amap(8, a),
            .ec_links = mk_link_meta_array(32, a),
            .ed_links = mk_link_meta_array(8, a),
            .cc_links = mk_link_meta_array(32, a),
            .cd_links = mk_link_meta_array(8, a),
            .dd_links = mk_link_meta_array(8, a),
        },
        .gotolinks = mk_sym_sarr_amap(8, a),
    };

    switch(top.type) {
    case TLDef: {
        // Note: types can only be recursive via 'Name', so we do not recursively bind if
        // generating a type.
        Symbol* recsym = top.def.value->ptype->sort != TKind ? 
            &top.def.bind : NULL;
        AddressEnv* a_env = mk_address_env(env, recsym, a);
        size_t out_sz = pi_size_of(*top.def.value->ptype);

        generate_entry(out_sz, target, a, point);
        generate_i(*top.def.value, a_env, target, &links, a, point);
        generate_exit(out_sz, target, a, point);

        delete_address_env(a_env, a);
        break;
    }
    case TLDecl: {
        // Do nothing; open only affects the environment
        break;
    }
    case TLImport: {
        // Do nothing; open only affects the environment
        break;
    }
    case TLExpr: {
        AddressEnv* a_env = mk_address_env(env, NULL, a);

        size_t out_sz = pi_size_of(*top.expr->ptype);
        generate_entry(out_sz, target, a, point);
        generate_i(*top.expr, a_env, target, &links, a, point);
        generate_exit(out_sz, target, a, point);

        delete_address_env(a_env, a);
        break;
    }
    }

    // The data chunk may be moved around during code-generation via 'realloc'
    // if it needs to grow. Thus, we backlink data here, to be safe.
    // TODO (INVESTIGATE BUG): check if also backlinking code makes sense?
    for (size_t i = 0; i < links.links.ed_links.len; i++) {
        LinkMetaData link = links.links.ed_links.data[i];
        void** address_ptr = (void**) ((void*)get_instructions(target.target).data + link.source_offset);
        set_unaligned_ptr(address_ptr, target.data_aux->data + link.dest_offset);
    }
    for (size_t i = 0; i < links.links.cd_links.len; i++) {
        LinkMetaData link = links.links.cd_links.data[i];
        void** address_ptr = (void**) ((void*)get_instructions(target.code_aux).data + link.source_offset);
        set_unaligned_ptr(address_ptr, target.data_aux->data + link.dest_offset);
    }
    for (size_t i = 0; i < links.links.dd_links.len; i++) {
        LinkMetaData link = links.links.dd_links.data[i];
        void** address_ptr = (void**) ((void*)target.data_aux->data + link.source_offset);
        set_unaligned_ptr(address_ptr, target.data_aux->data + link.dest_offset);
    }

    return links.links;
}

LinkData bd_generate_expr(Syntax* syn, Environment* env, Target target, Allocator* a, ErrorPoint* point) {
    
    AddressEnv* a_env = mk_address_env(env, NULL, a);
    InternalLinkData links = (InternalLinkData) {
        .links = (LinkData) {
            .external_links = mk_sym_sarr_amap(8, a),
            .ec_links = mk_link_meta_array(8, a),
            .ed_links = mk_link_meta_array(8, a),
            .cc_links = mk_link_meta_array(8, a),
            .cd_links = mk_link_meta_array(8, a),
            .dd_links = mk_link_meta_array(8, a),
        },
        .gotolinks = mk_sym_sarr_amap(8, a),
    };

    size_t out_sz = pi_size_of(*syn->ptype);
    generate_entry(out_sz, target, a, point);
    generate_i(*syn, a_env, target, &links, a, point);
    generate_exit(out_sz, target, a, point);

    delete_address_env(a_env, a);

    // The data chunk may be moved around during code-generation via 'realloc'
    // if it needs to grow. Thus, we backlink data here, to be safe.
    // TODO (INVESTIGATE BUG): check if also backlinking code makes sense?
    for (size_t i = 0; i < links.links.ed_links.len; i++) {
        LinkMetaData link = links.links.ed_links.data[i];
        void** address_ptr = (void**) ((void*)get_instructions(target.target).data + link.source_offset);
        *address_ptr= target.data_aux->data + link.dest_offset;
    }
    for (size_t i = 0; i < links.links.cd_links.len; i++) {
        LinkMetaData link = links.links.cd_links.data[i];
        void** address_ptr = (void**) ((void*)get_instructions(target.code_aux).data + link.source_offset);
        *address_ptr= target.data_aux->data + link.dest_offset;
    }
    for (size_t i = 0; i < links.links.dd_links.len; i++) {
        LinkMetaData link = links.links.dd_links.data[i];
        void** address_ptr = (void**) ((void*)target.data_aux->data + link.source_offset);
        *address_ptr= target.data_aux->data + link.dest_offset;
    }

    return links.links;
}

void bd_generate_type_expr(Syntax* syn, TypeEnv* env, Target target, Allocator* a, ErrorPoint* point) {
    AddressEnv* a_env = mk_type_address_env(env, NULL, a);
    InternalLinkData links = (InternalLinkData) {
        .links = (LinkData) {
            .external_links = mk_sym_sarr_amap(8, a),
            .ec_links = mk_link_meta_array(8, a),
            .ed_links = mk_link_meta_array(8, a),
            .cc_links = mk_link_meta_array(8, a),
            .cd_links = mk_link_meta_array(8, a),
            .dd_links = mk_link_meta_array(8, a),
        },
        .gotolinks = mk_sym_sarr_amap(8, a),
    };

    size_t out_sz = pi_size_of(*syn->ptype);
    generate_entry(out_sz, target, a, point);
    generate_i(*syn, a_env, target, &links, a, point);
    generate_exit(out_sz, target, a, point);

    delete_address_env(a_env, a);
}

static void generate_entry(size_t out_sz, Target target, Allocator *a, ErrorPoint *point) {
    // Generate as if this is a native function called as 
    // void function(void* out_mem, void* dyamic_memor, void* temp_memory, void* shadow_stack_memory)

    Assembler* ass = target.target;
    build_unary_op(Push, reg(RBP, sz_64), ass, a, point);
    build_unary_op(Push, reg(RBX, sz_64), ass, a, point);
    build_unary_op(Push, reg(RDI, sz_64), ass, a, point);
    build_unary_op(Push, reg(RSI, sz_64), ass, a, point);
    build_unary_op(Push, reg(R15, sz_64), ass, a, point);
    build_unary_op(Push, reg(R14, sz_64), ass, a, point);
    build_unary_op(Push, reg(R13, sz_64), ass, a, point);
    build_unary_op(Push, reg(R12, sz_64), ass, a, point);

    // Push the argument onto the stack
#if ABI == SYSTEM_V_64
    if (out_sz != 0) {
        build_unary_op(Push, reg(RDI, sz_64), ass, a, point);
    }

    // Both R14 and R15 have same value, as the variable stack has not moved
    build_binary_op(Mov, reg(R15, sz_64), reg(RSI, sz_64), ass, a, point);
    build_binary_op(Mov, reg(R14, sz_64), reg(RSI, sz_64), ass, a, point);
    build_binary_op(Mov, reg(R13, sz_64), reg(RCX, sz_64), ass, a, point);
#elif ABI == WIN_64
    if (out_sz != 0) {
        build_unary_op(Push, reg(RCX, sz_64), ass, a, point);
    }

    build_binary_op(Mov, reg(R15, sz_64), reg(RDX, sz_64), ass, a, point);
    build_binary_op(Mov, reg(R14, sz_64), reg(RDX, sz_64), ass, a, point);
    build_binary_op(Mov, reg(R13, sz_64), reg(R8, sz_64), ass, a, point);
#endif

    // Code generated here may assume $RBP is the base of the stack, they are
    // not aware of all the values we just pushed;
    build_binary_op(Mov, reg(RBP, sz_64), reg(RSP, sz_64), ass, a, point);
}

static void generate_exit(size_t out_sz, Target target, Allocator *a, ErrorPoint *point) {
    Assembler* ass = target.target;

#if ABI == SYSTEM_V_64
    // memcpy (dest = rdi, src = rsi, size = rdx)
    // retval = rax
    if (out_sz != 0) {
        build_binary_op(Mov, reg(RDI, sz_64), rref8(RSP, pi_stack_align(out_sz), sz_64), ass, a, point);
        build_binary_op(Mov, reg(RSI, sz_64), reg(RSP, sz_64), ass, a, point);
        build_binary_op(Mov, reg(RDX, sz_64), imm64((int64_t)out_sz), ass, a, point);

        build_binary_op(Mov, reg(RCX, sz_64), imm64((int64_t)&memcpy), ass, a, point);
        build_unary_op(Call, reg(RCX, sz_64), ass, a, point);
        // pop value from stack
        build_binary_op(Add, reg(RSP, sz_64), imm32(pi_stack_align(out_sz + 8)), ass, a, point);
    }

#elif ABI == WIN_64
    // memcpy (dest = rcx, src = rdx, size = r8)
    // retval = rax
    if (out_sz != 0) {
        build_binary_op(Mov, reg(RCX, sz_64), rref8(RSP, pi_stack_align(out_sz), sz_64), ass,a, point);
        build_binary_op(Mov, reg(RDX, sz_64), reg(RSP, sz_64), ass, a, point);
        build_binary_op(Mov, reg(R8, sz_64), imm64((int64_t)out_sz), ass, a, point);
        build_binary_op(Sub, reg(RSP, sz_64), imm32(32), ass, a, point);

        build_binary_op(Mov, reg(RAX, sz_64), imm64((int64_t)&memcpy), ass, a, point);
        build_unary_op(Call, reg(RAX, sz_64), ass, a, point);
        // pop value from stack
        build_binary_op(Add, reg(RSP, sz_64), imm32(pi_stack_align(out_sz) + 32 + 8), ass, a, point);
    }
#else
#error "Unknown calling convention"
#endif

    build_unary_op(Pop, reg(R12, sz_64), ass, a, point);
    build_unary_op(Pop, reg(R13, sz_64), ass, a, point);
    build_unary_op(Pop, reg(R14, sz_64), ass, a, point);
    build_unary_op(Pop, reg(R15, sz_64), ass, a, point);
    build_unary_op(Pop, reg(RSI, sz_64), ass, a, point);
    build_unary_op(Pop, reg(RDI, sz_64), ass, a, point);
    build_unary_op(Pop, reg(RBX, sz_64), ass, a, point);
    build_unary_op(Pop, reg(RBP, sz_64), ass, a, point);

    // Return
    build_nullary_op(Ret, ass, a, point);
}

void generate_i(Syntax syn, AddressEnv* env, Target target, InternalLinkData* links, Allocator* a, ErrorPoint* point) {
#ifdef DEBUG_ASSERT
    int64_t old_head = get_stack_head(env);
#endif

    Assembler* ass = target.target;
    switch (syn.type) {
    case SLitUntypedIntegral:
        panic(mv_string("Cannot generate monomorphic code for untyped integral!"));
    case SLitTypedIntegral: {
        // Does it fit into 32 bits?
        if (syn.integral.value >= 0x80000000) 
            throw_error(point, mv_string("Codegen: Literals must fit into less than 32 bits"));

        int32_t immediate = (int32_t)syn.integral.value;
        build_unary_op(Push, imm32(immediate), ass, a, point);
        data_stack_grow(env, pi_stack_size_of(*syn.ptype));
        break;
    }
    case SLitUntypedFloating: 
        panic(mv_string("Cannot generate monomorphic code for untyped floating!"));
    case SLitTypedFloating: {
        if (syn.ptype->prim == Float_32) {
            float f = syn.floating.value;
            void* raw = &f;
            int32_t immediate = *(int32_t*)raw;
            build_unary_op(Push, imm32(immediate), ass, a, point);
            data_stack_grow(env, pi_stack_size_of(*syn.ptype));
        }
        else if (syn.ptype->prim == Float_64) {
            void* raw = &syn.floating.value;
            int64_t immediate = *(int64_t*)raw;
            build_binary_op(Mov, reg(RAX,sz_64), imm64(immediate), ass, a, point);
            build_unary_op(Push, reg(RAX,sz_64), ass, a, point);
            data_stack_grow(env, pi_stack_size_of(*syn.ptype));
        } else {
            panic(mv_string("Floating literal has non-float type!"));
        }
        break;
    }
    case SLitBool: {
        int8_t immediate = syn.boolean;
        build_unary_op(Push, imm8(immediate), ass, a, point);
        data_stack_grow(env, pi_stack_size_of(*syn.ptype));
        break;
    }
    case SLitUnit: 
        break;
    case SLitString: {
        String immediate = syn.string; 
        if (immediate.memsize > UINT32_MAX) 
            throw_error(point, mv_string("Codegen: String literal length must fit into less than 32 bits"));

        // Push the u8
        AsmResult out = build_binary_op(Mov, reg(RAX, sz_64), imm64(0), ass, a, point);

        // Backlink the data & copy the bytes into the data-segment.
        backlink_data(target, out.backlink, links);
        add_u8_chunk(immediate.bytes, immediate.memsize, target.data_aux);

        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        build_unary_op(Push, imm32(immediate.memsize), ass, a, point);

        data_stack_grow(env, pi_stack_size_of(*syn.ptype));
        break;
    }
    case SLitArray: {
        size_t element_size = pi_size_of(*syn.ptype->array.element_type);
        size_t element_stack_size = pi_stack_align(element_size);
        size_t index_mul = pi_size_align(element_size, pi_align_of(*syn.ptype->array.element_type));
        data_stack_grow(env, ADDRESS_SIZE);

        // array - memory/data
        generate_perm_malloc(reg(RAX, sz_64), imm32(index_mul * syn.array_lit.subterms.len), ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);

        for (size_t i = 0; i < syn.array_lit.subterms.len; i++) {
            generate_i(*(Syntax*)syn.array_lit.subterms.data[i], env, target, links, a, point);

            // Copy the element into the array-block
            size_t offset = i * index_mul;
            build_binary_op(Mov, reg(RDI, sz_64), rref8(RSP, element_stack_size, sz_64), ass, a, point);
            build_binary_op(Add, reg(RDI, sz_64), imm32(offset), ass, a, point);
            generate_monomorphic_copy(RDI,RSP, element_size, ass, a, point);
            build_binary_op(Add, reg(RSP, sz_64), imm32(element_stack_size), ass, a, point);
            
            data_stack_shrink(env, element_stack_size);
        }

        // Shape - len + data
        generate_perm_malloc(reg(RAX, sz_64), imm32(ADDRESS_SIZE * syn.array_lit.shape.len), ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        for (size_t i = 0; i < syn.array_lit.shape.len; i++) {
            size_t dimension_len = syn.array_lit.shape.data[i];
            build_binary_op(Mov, rref32(RAX, i * ADDRESS_SIZE, sz_64), imm32(dimension_len), ass, a, point);
        }

        build_unary_op(Push, imm32(syn.array_lit.shape.len), ass, a, point);
        data_stack_grow(env, pi_stack_size_of(*syn.ptype) - ADDRESS_SIZE);
        break;
    }
    case SVariable:
    case SAbsVariable: {
        // Lookup the variable in the assembly envionrment
        AddressEntry e = (syn.type == SVariable)
            ? address_env_lookup(syn.variable, env)
            : address_abs_lookup(syn.abvar, env);
        switch (e.type) {
        case ALocalDirect: {
            // Copy to the current level 
            size_t size = pi_stack_size_of(*syn.ptype);
            data_stack_grow(env, size);
            build_binary_op(Sub, reg(RSP, sz_64), imm32(size), ass, a, point);

            // The size | 7 "rounds up" size to the nearet multiple of eight. 
            // All local variables on the stack are stored with size rounded up
            // to nearest eight, so this allows objects with size, e.g. 5, 12,
            // etc. to have all their data copied  
            size = size | 7;
            if (e.stack_offset + size > INT8_MAX || (e.stack_offset - (int64_t)size) < INT8_MIN) {
                for (size_t i = 0; i < size / 8; i++) {
                    build_binary_op(Mov, reg(RAX, sz_64), rref32(RBP, e.stack_offset + (i * 8) , sz_64), ass, a, point);
                    build_binary_op(Mov, rref32(RSP, (i * 8), sz_64), reg(RAX, sz_64), ass, a, point);
                }
            } else {
                for (size_t i = 0; i < size / 8; i++) {
                    build_binary_op(Mov, reg(RAX, sz_64), rref8(RBP, e.stack_offset + (i * 8) , sz_64), ass, a, point);
                    build_binary_op(Mov, rref8(RSP, (i * 8), sz_64), reg(RAX, sz_64), ass, a, point);
                }
            }
            break;
        }
        case ALocalIndexed:
            // First, we need the size of the variable & allocate space for it on the stack
            // ------------------------------------------------------------------------------------------
            // Store stack size in R9
            generate_stack_size_of(R9, syn.ptype, env, ass, a, point);
            build_binary_op(Sub, reg(VSTACK_HEAD, sz_64), reg(R9, sz_64), ass, a, point);
            build_unary_op(Push, reg(VSTACK_HEAD, sz_64), ass, a, point);
            data_stack_grow(env, ADDRESS_SIZE);

            // Next, find the source location on the variable stack
            // ------------------------------
            build_binary_op(Mov, reg(R8, sz_64), rref8(RBP, e.stack_offset, sz_64), ass, a, point);

            // Finally, move the value from the source to the stack head
            generate_poly_move(reg(VSTACK_HEAD, sz_64), reg(R8, sz_64), reg(R9, sz_64), ass, a, point);
            break;
        case ATypeVar:
            gen_mk_type_var(syn.variable, ass, a, point);
            data_stack_grow(env, ADDRESS_SIZE);
            break;
        case AGlobal: {
            PiType indistinct_type = *strip_type(syn.ptype);

            // Procedures (inc. polymorphic procedures), Types are passed by reference (i.e. they are addresses). 
            // Dynamic Vars and instances pby value, but are guaranteed to take up 64 bits.
            if (indistinct_type.sort == TProc || indistinct_type.sort == TAll || indistinct_type.sort == TKind
                || indistinct_type.sort == TDynamic || indistinct_type.sort == TTraitInstance) {
                AsmResult out = build_binary_op(Mov, reg(R9, sz_64), imm64(*(uint64_t*)e.value), ass, a, point);
                backlink_global(syn.variable, out.backlink, links, a);
                build_unary_op(Push, reg(R9, sz_64), ass, a, point);

            // Primitives are (currently) all <= 64 bits, but may treating them
            // as 64-bits may overflow a allocated portion of memory, so we must
            // be more careful here.
            } else if (indistinct_type.sort == TPrim) {
                size_t prim_size = pi_size_of(indistinct_type);
                AsmResult out;
                /* if (prim_size == 0) { */
                /*     // Do nothing, unit value */
                /* } else */
                if (prim_size == 1) {
                    out = build_binary_op(Mov, reg(R9, sz_64), imm64(*(uint8_t*)e.value), ass, a, point);
                } else if (prim_size == 2) {
                    out = build_binary_op(Mov, reg(R9, sz_64), imm64(*(uint16_t*)e.value), ass, a, point);
                } else if (prim_size == 4) {
                    out = build_binary_op(Mov, reg(R9, sz_64), imm64(*(uint32_t*)e.value), ass, a, point);
                } else if (prim_size == 8) {
                    out = build_binary_op(Mov, reg(R9, sz_64), imm64(*(uint64_t*)e.value), ass, a, point);
                } else {
                    panic(mv_string("Codegen expects globals bound to primitives to have size 1, 2, 4 or 8."));
                }
                backlink_global(syn.variable, out.backlink, links, a);
                build_unary_op(Push, reg(R9, sz_64), ass, a, point);

            // Structs and Enums are passed by value, and have variable size.
            } else if (indistinct_type.sort == TStruct || indistinct_type.sort == TEnum) {
                size_t value_size = pi_size_of(indistinct_type);
                size_t stack_size = pi_stack_align(value_size);
                AsmResult out = build_binary_op(Mov, reg(RCX, sz_64), imm64((uint64_t)e.value), ass, a, point);
                backlink_global(syn.variable, out.backlink, links, a);

                // Allocate space on the stack for composite type (struct/enum)
                build_binary_op(Sub, reg(RSP, sz_64), imm32(stack_size), ass, a, point);

                generate_monomorphic_copy(RSP, RCX, value_size, ass, a, point);
            } else {
                throw_error(point,
                            string_ncat(a, 3,
                                        mv_string("Codegen: Global var '"),
                                        symbol_to_string(syn.variable, a),
                                        mv_string("' has unsupported sort")));
            }
            data_stack_grow(env, pi_stack_size_of(indistinct_type));
            break;
        }
        case ANotFound: {
            String sym = symbol_to_string(syn.variable, a);
            String msg = mv_string("Couldn't find variable during codegen: ");
            throw_error(point, string_cat(msg, sym, a));
            break;
        }
        case ATooManyLocals:
            throw_error(point, mk_string("Too Many Local variables!", a));
            break;
        }
        break;
    }
    case SProcedure: {
        // Get the curret address 
        void* proc_address = get_instructions(target.code_aux).data;
        proc_address += get_instructions(target.code_aux).len;

        // Generate procedure value (push the address onto the stack)
        AsmResult out = build_binary_op(Mov, reg(RAX, sz_64), imm64((uint64_t)proc_address), ass, a, point);
        backlink_code(target, out.backlink, links);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);

        // Now, change the target and the assembler, such that code is now
        // generated in the 'code segment'. Then, generate the function body
        ass = target.code_aux;
        target.target = target.code_aux;

        // Codegen function setup
        build_unary_op(Push, reg(R14, sz_64), ass, a, point);
        build_unary_op(Push, reg(RBP, sz_64), ass, a, point);
        build_binary_op(Mov, reg(RBP, sz_64), reg(RSP, sz_64), ass, a, point);

        // Codegen Procedure Body 
        size_t args_size = 0;
        SymSizeAssoc impl_sizes = mk_sym_size_assoc(syn.procedure.implicits.len, a);
        for (size_t i = 0; i < syn.procedure.implicits.len; i++) {
            size_t arg_size = pi_stack_size_of(*(PiType*)syn.ptype->proc.implicits.data[i]);
            args_size += pi_stack_align(arg_size);
            sym_size_bind(syn.procedure.implicits.data[i].key , arg_size , &impl_sizes);
        }

        SymSizeAssoc arg_sizes = mk_sym_size_assoc(syn.procedure.args.len, a);
        for (size_t i = 0; i < syn.procedure.args.len; i++) {
            size_t arg_size = pi_stack_size_of(*(PiType*)syn.ptype->proc.args.data[i]);
            args_size += arg_size;
            sym_size_bind(syn.procedure.args.data[i].key , arg_size , &arg_sizes);
        }

        address_start_proc(impl_sizes, arg_sizes, env, a);
        generate_i(*syn.procedure.body, env, target, links, a, point);
        address_end_proc(env, a);

        // Codegen function teardown:
        // + restore old RBP & R14 in registers
        // + stash return address
        // + copy result down stack, accounting for
        //   + Return address, old RBP & old RSP
        //   + all arguments
        // + return to stashed address

        // Storage of function output 
        size_t ret_size = pi_stack_size_of(*syn.procedure.body->ptype);

        // Note: R9, R14, RBP were saved previously (in the prolog)
        //       R12 is for the return address
        build_binary_op(Mov, reg(R12, sz_64), rref8(RBP, 16, sz_64), ass, a, point);
        build_binary_op(Mov, reg(R14, sz_64), rref8(RBP, 8, sz_64), ass, a, point);
        build_binary_op(Mov, reg(RBP, sz_64), rref8(RBP, 0, sz_64), ass, a, point);

        generate_stack_move(args_size + 3 * ADDRESS_SIZE, 0, ret_size, ass, a, point);

        // Pop args
        build_binary_op(Add, reg(RSP, sz_64), imm32(args_size + 3 * ADDRESS_SIZE), ass, a, point);

        // push return address 
        build_unary_op(Push, reg(R12, sz_64), ass, a, point);
        build_nullary_op(Ret, ass, a, point);
        break;
    }
    case SAll: {
        void* all_address = get_instructions(target.code_aux).data;
        all_address += get_instructions(target.code_aux).len;

        // Generate procedure value (push the address onto the stack)
        AsmResult out = build_binary_op(Mov, reg(RAX, sz_64), imm64((uint64_t)all_address), ass, a, point);
        backlink_code(target, out.backlink, links);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);

        // Now, change the target and the assembler, such that code is now
        // generated in the 'code segment'. Then, generate the function body
        ass = target.code_aux;
        target.target = target.code_aux;

        generate_polymorphic(syn.all.args, *syn.all.body, env, target, links, a, point);
        break;
    }
    case SMacro:
        generate_i(*syn.transformer, env, target, links, a, point);
        break;
    case SApplication: {
        if (syn.application.function->ptype->sort == TProc) {
            // Generate the arguments
            bool variable_args = false;
            int64_t arg_base = get_stack_head(env);
            for (size_t i = 0; i < syn.application.implicits.len; i++) {
                Syntax* arg = (Syntax*) syn.application.implicits.data[i];
                variable_args |= is_variable_in(arg->ptype, env);
                generate_i(*arg, env, target, links, a, point);
            }
            for (size_t i = 0; i < syn.application.args.len; i++) {
                Syntax* arg = (Syntax*) syn.application.args.data[i];
                variable_args |= is_variable_in(arg->ptype, env);
                generate_i(*arg, env, target, links, a, point);
            }

            if (!variable_args) {
                // Calculate size to pop
                size_t pop_sz = ADDRESS_SIZE;
                for (size_t i = 0; i < syn.application.implicits.len; i++) {
                    PiType* aty = ((Syntax*)syn.application.implicits.data[i])->ptype;
                    pop_sz += pi_stack_size_of(*aty);
                }
                for (size_t i = 0; i < syn.application.args.len; i++) {
                    PiType* aty = ((Syntax*)syn.application.args.data[i])->ptype;
                    pop_sz += pi_stack_size_of(*aty);
                }
                // This will push a function pointer onto the stack
                generate_i(*syn.application.function, env, target, links, a, point);
        
                // Regular Function Call
                // Pop the function into RCX; call the function
                build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);
                build_unary_op(Call, reg(RCX, sz_64), ass, a, point);
                data_stack_shrink(env, pop_sz);
                if (is_variable_in(syn.ptype, env)) {
                    // Move value from data to 'variable' stack.
                    panic(mv_string("not implemented: variable return from static function in poly code."));
                } else {
                    data_stack_grow(env, pi_stack_size_of(*syn.ptype));
                }
            } else {
                generate_i(*syn.application.function, env, target, links, a, point);

                size_t args_size = 0;
                // Move variable args from the data stack to the variable stack
                for (size_t i = 0; i < syn.application.implicits.len; i++) {
                    PiType* aty = ((Syntax*)syn.application.implicits.data[i])->ptype;
                    if (is_variable_in(aty, env)) {

                        // Unfold argument onto data-stack
                        args_size += ADDRESS_SIZE;
                        generate_stack_size_of(RAX, aty, env, ass, a, point);
                        build_binary_op(Sub, reg(RSP, sz_64), reg(RAX, sz_64), ass, a, point);
                        generate_poly_copy_from_base(0, arg_base - args_size, reg(RAX, sz_64), ass, a, point);
                    } else {

                        // Copy down From Above
                        size_t argsz = pi_stack_size_of(*aty);
                        args_size += argsz;
                        build_binary_op(Sub, reg(RSP, sz_64), imm8(argsz), ass, a, point);
                        generate_stack_copy_from_base(0, arg_base - args_size, argsz, ass, a, point);
                    }
                }
                for (size_t i = 0; i < syn.application.args.len; i++) {
                    PiType* aty = ((Syntax*)syn.application.args.data[i])->ptype;
                    if (is_variable_in(aty, env)) {

                        // Unfold argument onto data-stack
                        args_size += ADDRESS_SIZE;;
                        generate_stack_size_of(RAX, aty, env, ass, a, point);
                        build_binary_op(Sub, reg(RSP, sz_64), reg(RAX, sz_64), ass, a, point);
                        build_binary_op(Mov, reg(RDX, sz_64), rref8(RBP, arg_base - args_size, sz_64), ass, a, point);
                        generate_poly_move(reg(RSP, sz_64), reg(RDX, sz_64), reg(RAX, sz_64), ass, a, point);
                    } else {

                        // Copy down From Above
                        size_t argsz = pi_stack_size_of(*aty);
                        args_size += argsz;
                        build_binary_op(Sub, reg(RSP, sz_64), imm8(argsz), ass, a, point);
                        generate_stack_copy_from_base(0, arg_base - args_size, argsz, ass, a, point);
                    }
                }
                // Account for the function
                args_size += ADDRESS_SIZE;
                
                // Copy down the function itself & call
                build_binary_op(Mov, reg(RCX, sz_64), rref8(RBP, arg_base - args_size, sz_64), ass, a, point);
                build_unary_op(Call, reg(RCX, sz_64), ass, a, point);
                data_stack_shrink(env, args_size);

                if (is_variable_in(syn.ptype, env)) {
                    // Move value from data to 'variable' stack.
                    generate_stack_size_of(RAX, syn.ptype, env, ass, a, point);
                    build_binary_op(Sub, reg(VSTACK_HEAD, sz_64), reg(RAX, sz_64), ass, a, point);
                    build_binary_op(Mov, reg(R9, sz_64), reg(RSP, sz_64), ass, a, point);
                    build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
                    generate_poly_move(reg(VSTACK_HEAD, sz_64), reg(R9, sz_64), reg(RAX, sz_64), ass, a, point);

                    build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);
                    build_binary_op(Add, reg(RSP, sz_64), imm8(args_size), ass, a, point);
                    build_binary_op(Add, reg(RSP, sz_64), reg(RAX, sz_64), ass, a, point);
                    build_unary_op(Push, reg(VSTACK_HEAD, sz_64), ass, a, point);
                    data_stack_grow(env, ADDRESS_SIZE);
                } else {
                    // Regular move up the data-stack
                    size_t out_sz = pi_stack_size_of(*syn.ptype);
                    generate_stack_move(args_size, 0, out_sz, ass, a, point);
                    build_binary_op(Add, reg(RSP, sz_64), imm8(args_size), ass, a, point);
                    data_stack_grow(env, pi_stack_size_of(*syn.ptype));
                }
            }
        } else {
            size_t args_size = 0;
            for (size_t i = 0; i < syn.application.args.len; i++) {
                Syntax* arg = (Syntax*) syn.application.args.data[i];
                args_size += pi_stack_size_of(*arg->ptype);
                generate_i(*arg, env, target, links, a, point);
            }

            // push the type onto the stack:
            generate_i(*syn.application.function, env, target, links, a, point);

            gen_mk_family_app(syn.application.args.len, ass, a, point);
            // Update for popping all values off the stack (also the function itself)
            data_stack_shrink(env, args_size + ADDRESS_SIZE);

            // Update as pushed the final value onto the stac
            data_stack_grow(env, pi_stack_size_of(*syn.ptype));
        }
        break;
    }
    case SAllApplication: {
        // Polymorphic Funcall
        build_binary_op(Mov, reg(R15, sz_64), reg(VSTACK_HEAD, sz_64), ass, a, point);
        size_t static_arg_size = 0;

        SymbolArray type_vars = syn.all_application.function->ptype->binder.vars;
        for (size_t i = 0; i < syn.all_application.types.len; i++) {
            PiType* type = ((Syntax*)syn.all_application.types.data[i])->type_val;
            generate_pi_type(type, env, ass, a, point);
            static_arg_size += ADDRESS_SIZE;
        }

        bool mismatch_variable_args = false;
        PiType* fn_ty = strip_type(syn.all_application.function->ptype);
        if (fn_ty->sort == TAll) { fn_ty = fn_ty->binder.body; }

        for (size_t i = 0; i < syn.all_application.implicits.len; i++) {
            PiType* argty = fn_ty->proc.implicits.data[i];
            Syntax* arg = (Syntax*) syn.all_application.implicits.data[i];
            generate_i(*arg, env, target, links, a, point);

            // The argument is variable for for the callee
            if (is_variable_for(argty, type_vars)) {
                // Is it also variable in our context?
                // if not, we need to move to variable stack, otherwise can keep here.
                static_arg_size += ADDRESS_SIZE;
                if (!is_variable_in(arg->ptype, env)) {
                    size_t arg_size = pi_stack_size_of(*arg->ptype);
                    build_binary_op(Sub, reg(R14, sz_64), imm8(arg_size), ass, a, point);
                    generate_monomorphic_copy(VSTACK_HEAD, RSP, arg_size, ass, a, point);
                    build_binary_op(Add, reg(RSP, sz_64), imm8(arg_size), ass, a, point);
                    build_unary_op(Push, reg(VSTACK_HEAD, sz_64), ass, a, point);

                    data_stack_shrink(env, arg_size);
                    data_stack_grow(env, ADDRESS_SIZE);

                }
            } else {
                mismatch_variable_args |= is_variable_in(arg->ptype, env);
                static_arg_size += is_variable_in(arg->ptype, env) ? ADDRESS_SIZE : pi_stack_size_of(*arg->ptype);
            }
        }

        for (size_t i = 0; i < syn.all_application.args.len; i++) {
            PiType* argty = fn_ty->proc.args.data[i];
            Syntax* arg = (Syntax*) syn.all_application.args.data[i];
            generate_i(*arg, env, target, links, a, point);

            // The argument is variable for for the callee
            if (is_variable_for(argty, type_vars)) {
                // Is it also variable in our context?
                // if not, we need to move to variable stack, otherwise can keep here.
                static_arg_size += ADDRESS_SIZE;
                if (!is_variable_in(arg->ptype, env)) {
                    size_t arg_size = pi_stack_size_of(*arg->ptype);
                    build_binary_op(Sub, reg(R14, sz_64), imm8(arg_size), ass, a, point);
                    generate_monomorphic_copy(VSTACK_HEAD, RSP, arg_size, ass, a, point);
                    build_binary_op(Add, reg(RSP, sz_64), imm8(arg_size), ass, a, point);
                    build_unary_op(Push, reg(VSTACK_HEAD, sz_64), ass, a, point);

                    data_stack_shrink(env, arg_size);
                    data_stack_grow(env, ADDRESS_SIZE);
                }
            } else {
                mismatch_variable_args |= is_variable_in(arg->ptype, env);
                static_arg_size += is_variable_in(arg->ptype, env) ? ADDRESS_SIZE : pi_stack_size_of(*arg->ptype);
            }
        }
        generate_i(*syn.all_application.function, env, target, links, a, point);
        static_arg_size += ADDRESS_SIZE;

        if (mismatch_variable_args) {
            panic(mv_string("Mismatch in variable args: codegen for this scenario not implemented."));
        }

        build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);

        build_unary_op(Call, reg(RCX, sz_64), ass, a, point);
        data_stack_shrink(env, static_arg_size);

        PiType* ty = fn_ty->sort == TProc ? fn_ty->proc.ret : fn_ty;
        bool callee_varstack = is_variable_for(ty, type_vars);
        bool caller_varstack = is_variable_in(syn.ptype, env);

        data_stack_grow(env, caller_varstack ? ADDRESS_SIZE : pi_stack_size_of(*syn.ptype));
        if (callee_varstack != caller_varstack) {
            if (callee_varstack) {
                size_t out_size = pi_stack_size_of(*syn.ptype);
                // Copy from varstack to our stack 
                build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);
                build_binary_op(Sub, reg(RSP, sz_64), imm32(out_size), ass, a, point);
                generate_monomorphic_copy(RSP, RCX, out_size, ass, a, point);

                // Pop value from variable stack
                build_binary_op(Add, reg(VSTACK_HEAD, sz_64), imm32(out_size), ass, a, point);
            } else {
                panic(mv_string("not implemented: Calling with value on caller varstack and callee static stack"));
            }
        }

        break;
    }
    case SExists: {
        not_implemented(mv_string("Poly Direct Codegen for Exists"));
        break; 
    }
    case SUnpack: {
        not_implemented(mv_string("Poly Direct Codegen for Unpack"));
        break; 
    }
    case SStructure: {
        PiType* struct_type = strip_type(syn.ptype);
        if (is_variable_in(syn.ptype, env)) {
            // Reserve space on the stack for the structure to go
            if (syn.structure.base && syn.structure.base->type != SCheckedType) {
                generate_i(*syn.structure.base, env, target, links, a, point);
            } else {
                generate_stack_size_of(RAX, syn.ptype, env, ass, a, point);
                build_binary_op(Sub, reg(VSTACK_HEAD, sz_64), reg(RAX, sz_64), ass, a, point);
                build_unary_op(Push, reg(VSTACK_HEAD, sz_64), ass, a, point);
                data_stack_grow(env, ADDRESS_SIZE);
            }
            int64_t head = get_stack_head(env);

            // Generate code for each of the fields (in order)
            for (size_t i = 0; i < struct_type->structure.fields.len; i++)
                generate_i(*(Syntax *)syn.structure.fields.data[i].val, env, target, links, a, point);

            // Copy the field contents into the final struct.
            // ----------------------------------------------
            // Precompute dest_pos - the position in the stack where the
            // variable pointer to the output struct is = difference between
            // here & head + offset stored on stack + field size storedon stack.
            int64_t dest_pos = 2 * ADDRESS_SIZE + (head - get_stack_head(env));
            build_unary_op(Push, imm8(0), ass, a, point);
            for (size_t i = 0; i < struct_type->structure.fields.len; i++) {
                Symbol field = struct_type->structure.fields.data[i].key;
                PiType* ty = struct_type->structure.fields.data[i].val;

                size_t source_offset = 2 * ADDRESS_SIZE;
                bool field_present = false;
                for (size_t j = syn.structure.fields.len; j > 0; j--) {
                    size_t idx = j - 1;
                    if (symbol_eq(field, syn.structure.fields.data[idx].key)) { field_present = true; break; }
                    PiType* field_ty = ((Syntax*)syn.structure.fields.data[idx].val)->ptype;
                    source_offset += is_variable_in(field_ty, env) ? ADDRESS_SIZE : pi_stack_size_of(*field_ty);
                }

                // Update destination with alignment
                generate_align_of(R10, ty, env, ass, a, point);
                build_binary_op(Mov, reg(R9, sz_64), rref8(RSP, 0, sz_64), ass, a, point);
                generate_align_to(R9, R10, ass, a, point);
                build_binary_op(Mov, rref8(RSP, 0, sz_64), reg(R9, sz_64), ass, a, point);

                generate_size_of(RAX, ty, env, ass, a, point);
                if (field_present) {
                    build_unary_op(Push, reg(RAX, sz_64), ass, a, point);

                    // Copy to dest
                    build_binary_op(Mov, reg(RSI, sz_64), rref8(RSP, dest_pos, sz_64), ass, a, point);
                    build_binary_op(Add, reg(RSI, sz_64), rref8(RSP, 8, sz_64), ass, a, point);
                    generate_poly_move(reg(RSI, sz_64), rref8(RSP, source_offset, sz_64), reg(RAX, sz_64), ass, a, point);

                    // Add to size
                    build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);
                }

                // Add field size to offset
                build_binary_op(Add, rref8(RSP, 0, sz_64), reg(RAX, sz_64), ass, a, point);
            }
            // Now, copy up the stack, restore stack head
            int64_t tmps_size = head - get_stack_head(env);
            build_binary_op(Add, reg(RSP, sz_64), imm8(tmps_size + ADDRESS_SIZE), ass, a, point);
            build_binary_op(Mov, reg(VSTACK_HEAD, sz_64), rref8(RSP, 0, sz_64), ass, a, point);
            data_stack_shrink(env, tmps_size);

        } else {
            // The following code is copied exactly from the 'regular' codegen 
            // ------------
            // For structures, we have to be careful - this is because the order in
            // which arguments are evaluated is not necessarily the order in which
            // arguments are inserted into the structure.

            // Step 1: Make room on the stack for our struct (OR, just generate the
            // base struct)
            if (syn.structure.base && syn.structure.base->type != SCheckedType) {
                generate_i(*syn.structure.base, env, target, links, a, point);
            } else {
                size_t struct_size = pi_stack_size_of(*struct_type);
                build_binary_op(Sub, reg(RSP, sz_64), imm32(struct_size), ass, a, point);
                data_stack_grow(env, struct_size);
            }

            // Step 2: evaluate each element/variable binding
            for (size_t i = 0; i < syn.structure.fields.len; i++) {
                generate_i(*(Syntax*)syn.structure.fields.data[i].val, env, target, links, a, point);
            }
        
            // Step 3: copy each element of the array into it's place on the stack.
            // Note: for, e.g. Struct [.x I64] [.y I64] we expect the stack to look 
            // something like the below.
            // ...  ..
            // 144 y  } Destination - elements are ordered bottom-top
            // 136 x  } 
            // --- 
            // 128 x   } Source - elements can be in any arbitrary order 
            // 120 y   } <`top' of stack - grows down>
            // ------------

            // Copy from the bottom (of the destination) to the top (also of the destination) 
            size_t source_region_size = 0;
            for (size_t i = 0; i < syn.structure.fields.len; i++) {
                source_region_size += pi_stack_size_of(*((Syntax*)syn.structure.fields.data[i].val)->ptype); 
            }
            size_t src_offset = 0;
            for (size_t i = 0; i < syn.structure.fields.len; i++) {
                // Find the field in the source & compute offset
                size_t dest_offset = 0;
                for (size_t j = 0; j < struct_type->structure.fields.len; j++) {
                    PiType* t = struct_type->structure.fields.data[j].val;
                    dest_offset = pi_size_align(dest_offset, pi_align_of(*t)); 

                    if (symbol_eq(syn.structure.fields.data[i].key, struct_type->structure.fields.data[j].key)) {
                        break; // offset is correct, end the loop
                    }
                    dest_offset += pi_size_of(*t); 
                }

                // We now both the source_offset and dest_offset. These are both
                // relative to the 'bottom' of their respective structures.
                // Therefore, we now need to find their offsets relative to the `top'
                // of the stack.
                size_t field_size = pi_size_of(*((Syntax*)syn.structure.fields.data[i].val)->ptype);
                src_offset += pi_stack_align(field_size);
                size_t src_stack_offset = source_region_size - src_offset;
                size_t dest_stack_offset = source_region_size + dest_offset;

                // Now, move the data.
                generate_stack_move(dest_stack_offset, src_stack_offset, field_size, ass, a, point);
            }

            // Remove the space occupied by the temporary values 
            build_binary_op(Add, reg(RSP, sz_64), imm32(source_region_size), ass, a, point);
            data_stack_shrink(env, source_region_size);
            break;
        }
        break;
    }
    case SProjector: {
        PiType* source_type = strip_type(syn.projector.val->ptype);
        if (source_type->sort == TStruct) {
            // -----------------------------------------------------------------
            //
            //                               STRUCTURE
            //
            // -----------------------------------------------------------------

            if (is_variable_in(syn.projector.val->ptype, env)) {
                panic(mv_string("not (yet) projecting out of variable structs."));
            } else {
                size_t out_sz = pi_stack_size_of(*syn.ptype);
                build_binary_op(Sub, reg(RSP, sz_64), imm32(out_sz), ass, a, point);
                data_stack_grow(env, out_sz);

                // Second, generate the structure/instance object
                generate_i(*syn.projector.val, env, target, links, a, point);
                size_t src_sz = pi_stack_size_of(*source_type);
                // Now, copy the structure to the destination
                // for this, we need the struct size + offset of field in the struct
                size_t offset = 0;
                for (size_t i = 0; i < source_type->structure.fields.len; i++) {
                    size_t align = pi_align_of(*(PiType*)source_type->structure.fields.data[i].val);
                    offset = pi_size_align(offset, align);
                    if (symbol_eq(source_type->structure.fields.data[i].key, syn.projector.field))
                        break;
                    offset += pi_size_of(*(PiType*)source_type->structure.fields.data[i].val);
                }

                generate_stack_move(src_sz, offset, out_sz, ass, a, point);
                // Now, remove the original struct from the stack
                build_binary_op(Add, reg(RSP, sz_64), imm32(src_sz), ass, a, point);

                data_stack_shrink(env, src_sz);
            } 
        } else {
            // -----------------------------------------------------------------
            //
            //                               INSTANCE
            //
            // -----------------------------------------------------------------

            // Second, generate the structure/instance object
            generate_i(*syn.projector.val, env, target, links, a, point);
            // Both instances (passed by reference) and structs (on variable
            // stack) will occupy an address size on the stack.
            size_t src_sz = ADDRESS_SIZE;

            // From this point, behaviour depends on whether we are projecting from
            // a structure or from an instance
            bool field_is_var = is_variable_in(syn.ptype, env);
            bool offset_is_var = false;
            for (size_t i = 0; i < source_type->instance.fields.len; i++) {
                offset_is_var |= is_variable_in(source_type->instance.fields.data[i].val, env);

                // Note: we only break *after* checking the field, as the offset
                //       of a field is dependent on its' alignment
                if (symbol_eq(source_type->instance.fields.data[i].key, syn.projector.field))
                    break;
            }

            if (!field_is_var && !offset_is_var) {
                // Pop the pointer to the instance from the stack - store in RSI
                data_stack_shrink(env, src_sz);
                build_unary_op(Pop, reg(RSI, sz_64), ass, a, point);

                // Now, calculate offset for field 
                size_t offset = 0;
                for (size_t i = 0; i < source_type->instance.fields.len; i++) {
                    offset = pi_size_align(offset, pi_align_of(*(PiType*)source_type->instance.fields.data[i].val));
                    if (symbol_eq(source_type->instance.fields.data[i].key, syn.projector.field))
                        break;
                    offset += pi_size_of(*(PiType*)source_type->instance.fields.data[i].val);
                }
                build_binary_op(Add, reg(RSI, sz_64), imm32(offset), ass, a, point);

                size_t val_sz = pi_size_of(*syn.ptype);
                size_t val_stack_sz = pi_stack_align(val_sz);
                build_binary_op(Sub, reg(RSP, sz_64), imm32(val_stack_sz), ass, a, point);
                data_stack_grow(env, val_stack_sz);

                generate_monomorphic_copy(RSP, RSI, val_sz, ass, a, point);
            } else if (!field_is_var && offset_is_var) {
                // Now, calculate offset for field 
                generate_offset_of(RDI, syn.projector.field, source_type->instance.fields, env, ass, a, point);

                // Pop the pointer to the instance from the stack - store in RSI
                data_stack_shrink(env, src_sz);
                build_unary_op(Pop, reg(RSI, sz_64), ass, a, point);
                build_binary_op(Add, reg(RSI, sz_64), reg(RDI, sz_64), ass, a, point);

                size_t val_sz = pi_size_of(*syn.ptype);
                size_t val_stack_sz = pi_stack_align(val_sz);
                build_binary_op(Sub, reg(RSP, sz_64), imm32(val_stack_sz), ass, a, point);
                data_stack_grow(env, val_stack_sz);

                generate_monomorphic_copy(RSP, RSI, val_sz, ass, a, point);
            } else {
                // Note: stack remains unchanged; as we popped of instance, but
                //       dynamic stack val ptr

                // Generate field offset and size. 
                generate_offset_of(RDI, syn.projector.field, source_type->instance.fields, env, ass, a, point);
                build_unary_op(Push, reg(RDI, sz_64), ass, a, point);
                generate_size_of(RAX, syn.ptype, env, ass, a, point);
                build_unary_op(Push, reg(RAX, sz_64), ass, a, point);

                build_binary_op(Mov, reg(R9, sz_64), rref8(RSP, 0, sz_64), ass, a, point);
                build_binary_op(Mov, reg(R8, sz_64), imm32(STACK_ALIGN), ass, a, point);
                generate_align_to(R9, R8, ass, a, point);

                build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);
                build_unary_op(Pop, reg(RDI, sz_64), ass, a, point);

                // source
                build_unary_op(Pop, reg(RSI, sz_64), ass, a, point);
                build_binary_op(Add, reg(RSI, sz_64), reg(RDI, sz_64), ass, a, point);

                build_binary_op(Sub, reg(VSTACK_HEAD, sz_64), reg(RAX, sz_64), ass, a, point);

                build_unary_op(Push, reg(VSTACK_HEAD, sz_64), ass, a, point);
                generate_poly_move(reg(VSTACK_HEAD, sz_64), reg(RSI, sz_64), reg(RAX, sz_64), ass, a, point);
            }
        }
        break;
    }
    case SInstance: {
        // TODO: check that the instance handles stack alignment correctly
        /* Instances work as follows:
         * • Instances as values are expected to be passed as pointers and allocated temporarily. 
         * • Non-parametric instances are simply pointers : codegen generates a
         *   malloc and then assigns all values.
         * • Parametric instances are functions (which may be instantiated by
         *   the runtime)
         */

        size_t immediate_sz = 0;
        for (size_t i = 0; i < syn.ptype->instance.fields.len; i++) {
            immediate_sz = pi_size_align(immediate_sz, pi_align_of(*(PiType*)syn.ptype->instance.fields.data[i].val));
            immediate_sz += pi_size_of(*(PiType*)syn.ptype->instance.fields.data[i].val);
        }
        build_binary_op(Mov, reg(RSI, sz_64), imm32(immediate_sz), ass, a, point);
        generate_tmp_malloc(reg(RAX, sz_64), reg(RSI, sz_64), ass, a, point);
        build_binary_op(Mov, reg(RCX, sz_64), reg(RAX, sz_64), ass, a, point);

        // Grow by address size to account for the fact that the for loop
        // keeps a stack of the address, which is updated each iteration.
        data_stack_grow(env, ADDRESS_SIZE);
        build_unary_op(Push, reg(RCX, sz_64), ass, a, point);

        // TODO (BUG): generate code that doesn't assume fields are in same order   

        // Alignment
        size_t index_offset = 0;
        for (size_t i = 0; i < syn.ptype->instance.fields.len; i++) {
            // Generate field
            Syntax* val = syn.instance.fields.data[i].val;
            generate_i(*val, env, target, links, a, point);

            // The offset tells us how far up the stack we look to find the instance ptr
            size_t offset = pi_stack_size_of(*val->ptype);

            // Retrieve index (ptr) 
            // TODO (BUG): Check offset is < int8_t max.
            build_binary_op(Mov, reg(RCX, sz_64), rref8(RSP, pi_stack_align(offset), sz_64), ass, a, point);

            // Align RCX
            size_t aligned_offset = pi_size_align(index_offset, pi_align_of(*val->ptype));
            if (index_offset != aligned_offset) {
                size_t align = aligned_offset - index_offset;
                build_binary_op(Add, reg(RCX, sz_64), imm32(align), ass, a, point);
            }

            // TODO (check if replace with stack copy)
            generate_monomorphic_copy(RCX, RSP, offset, ass, a, point);

            // We need to increment the current field index to be able ot access
            // the next
            build_binary_op(Add, reg(RCX, sz_64), imm32(offset), ass, a, point);
            index_offset += offset;

            // Pop value from stack
            build_binary_op(Add, reg(RSP, sz_64), imm32(pi_stack_align(offset)), ass, a, point);
            data_stack_shrink(env, offset);

            // Override index with new value
            build_binary_op(Mov, rref8(RSP, 0, sz_64), reg(RCX, sz_64), ass, a, point);
        }

        build_binary_op(Mov, reg(RCX, sz_64), rref8(RSP, 0, sz_64), ass, a, point);
        build_binary_op(Sub, reg(RCX, sz_64), imm32(immediate_sz), ass, a, point);
        build_binary_op(Mov, rref8(RSP, 0, sz_64), reg(RCX, sz_64), ass, a, point);

        // Note: we don't shrink as the final address (on stack) is accounted
        // for by the 'grow' prior to the above for-loop
        break;
    }
    case SConstructor: {
        if (is_variable_in(syn.ptype, env)) {
            not_implemented(mv_string("Polymorphic constructor"));
        } else {
            PiType* enum_type = strip_type(syn.ptype);
            size_t enum_size = pi_stack_size_of(*enum_type);
            size_t variant_size = calc_variant_size(enum_type->enumeration.variants.data[syn.variant.tag].val);

            build_binary_op(Sub, reg(RSP, sz_64), imm32(enum_size - variant_size), ass, a, point);
            build_unary_op(Push, imm32(syn.constructor.tag), ass, a, point);

            data_stack_grow(env, enum_size);
        }
        break;
    }
    case SVariant: {
        if (is_variable_in(syn.ptype, env)) {
            generate_stack_size_of(RAX, syn.ptype, env, ass, a, point);
            build_binary_op(Sub, reg(VSTACK_HEAD, sz_64), reg(RAX, sz_64), ass, a, point);
            build_unary_op(Push, reg(VSTACK_HEAD, sz_64), ass, a, point);
            data_stack_grow(env, ADDRESS_SIZE);
            int64_t head = get_stack_head(env);

            // Set the tag
            build_binary_op(Mov, rref8(VSTACK_HEAD, 0, sz_64), imm32(syn.constructor.tag), ass, a, point);

            // Generate each argument
            for (size_t i = 0; i < syn.variant.args.len; i++) {
                generate_i(*(Syntax*)syn.variant.args.data[i], env, target, links, a, point);
            }

            // Now, copy arguments up into the variant.
            // ----------------------------------------

            // push the dest offset (starts ad address size) 
            build_unary_op(Push, imm8(ADDRESS_SIZE), ass, a, point);
            int64_t dest_pos = (head - get_stack_head(env));

            size_t source_offset = 0;
            for (size_t i = 0; i < syn.variant.args.len; i++) {
                PiType* field_ty = ((Syntax*)syn.variant.args.data[i])->ptype;
                // We now have both the source_offset and dest_offset. These are both
                // relative to the 'bottom' of their respective structures.
                // Therefore, we now need to find their offsets relative to the `top'
                // of the stack.
                if (is_variable_in(field_ty, env)) {
                    // Update destination with alignment
                    generate_align_of(R10, field_ty, env, ass, a, point);
                    build_binary_op(Mov, reg(R9, sz_64), rref8(RSP, 0, sz_64), ass, a, point);
                    generate_align_to(R9, R10, ass, a, point);
                    build_binary_op(Mov, rref8(RSP, 0, sz_64), reg(R9, sz_64), ass, a, point);

                    generate_size_of(RAX, field_ty, env, ass, a, point);
                    build_unary_op(Push, reg(RAX, sz_64), ass, a, point);

                    // Copy to dest
                    build_binary_op(Mov, reg(RDI, sz_64), rref8(RSP, dest_pos + (2*ADDRESS_SIZE), sz_64), ass, a, point);
                    build_binary_op(Add, reg(RDI, sz_64), rref8(RSP, 8, sz_64), ass, a, point);
                    generate_poly_move(reg(RDI, sz_64), rref8(RSP, source_offset + 2*ADDRESS_SIZE, sz_64), reg(RAX, sz_64), ass, a, point);

                    // Add field size to dest offset
                    build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);
                    build_binary_op(Add, rref8(RSP, 0, sz_64), reg(RAX, sz_64), ass, a, point);

                    source_offset += ADDRESS_SIZE;
                } else {
                    size_t field_size = pi_size_of(*field_ty);
                    build_binary_op(Mov, reg(R9, sz_64), imm32(pi_align_of(*field_ty)), ass, a, point);
                    build_binary_op(Mov, reg(R9, sz_64), rref8(RSP, 0, sz_64), ass, a, point);
                    generate_align_to(R9, R10, ass, a, point);
                    build_binary_op(Mov, rref8(RSP, 0, sz_64), reg(R9, sz_64), ass, a, point);

                    build_binary_op(Mov, reg(RDI, sz_64), rref8(RSP, dest_pos + ADDRESS_SIZE, sz_64), ass,a, point);
                    build_binary_op(Add, reg(RDI, sz_64), rref8(RSP, 0, sz_64), ass,a, point);

                    build_binary_op(Mov, reg(RSI, sz_64), reg(RSP, sz_64), ass,a, point);
                    generate_monomorphic_copy(RDI, RSI, field_size, ass, a, point);

                    build_binary_op(Add, rref8(RSP, 0, sz_64), imm32(field_size), ass, a, point);
                    source_offset += pi_stack_align(field_size);
                }
            }
            data_stack_shrink(env, dest_pos);
            build_binary_op(Add, reg(RSP, sz_64), imm32(dest_pos + ADDRESS_SIZE), ass, a, point);
            build_binary_op(Mov, reg(VSTACK_HEAD, sz_64), rref8(RSP, 0, sz_64), ass, a, point);

        } else {
            const size_t tag_size = sizeof(uint64_t);
            PiType* enum_type = strip_type(syn.ptype);
            size_t enum_size = pi_size_of(*enum_type);
            size_t variant_size = calc_variant_size(enum_type->enumeration.variants.data[syn.variant.tag].val);
            size_t variant_stack_size = calc_variant_stack_size(enum_type->enumeration.variants.data[syn.variant.tag].val);

            // Make space to fit the (final) variant
            build_binary_op(Sub, reg(RSP, sz_64), imm32(enum_size), ass, a, point);
            data_stack_grow(env, enum_size);

            // Set the tag
            build_binary_op(Mov, rref8(RSP, 0, sz_64), imm32(syn.constructor.tag), ass, a, point);

            // Generate each argument
            for (size_t i = 0; i < syn.variant.args.len; i++) {
                generate_i(*(Syntax*)syn.variant.args.data[i], env, target, links, a, point);
            }

            // Now, move them into the space allocated in reverse order
            PtrArray args = *(PtrArray*)enum_type->enumeration.variants.data[syn.variant.tag].val;

            // Note, as we are reversing the order, we start at the top of the stack (last enum element),
            // which gets copied to the end of the enum
            size_t src_stack_offset = 0;
            size_t dest_stack_offset = variant_size + variant_stack_size - tag_size;
            for (size_t i = 0; i < syn.variant.args.len; i++) {
                // We now have both the source_offset and dest_offset. These are both
                // relative to the 'bottom' of their respective structures.
                // Therefore, we now need to find their offsets relative to the `top'
                // of the stack.
            
                size_t field_size = pi_size_of(*(PiType*)args.data[syn.variant.args.len - (i + 1)]);

                dest_stack_offset -= field_size;
                generate_stack_move(dest_stack_offset, src_stack_offset, field_size, ass, a, point);
                src_stack_offset += pi_stack_align(field_size);
            }

            // Remove the space occupied by the temporary values 
            build_binary_op(Add, reg(RSP, sz_64), imm32(src_stack_offset), ass, a, point);

            // Grow the stack to account for the difference in enum & variant sizes
            data_stack_shrink(env, variant_stack_size - tag_size);
        }
        break;
    }
    case SMatch: {
        // Generate code for the value
        Syntax* match_value = syn.match.val;
        PiType* enum_type = strip_type(syn.match.val->ptype);

        if (is_variable_in(enum_type, env)) {
            build_unary_op(Push, reg(VSTACK_HEAD, sz_64), ass, a, point);
            data_stack_grow(env, ADDRESS_SIZE);
            int64_t var_head_offset = get_stack_head(env);
            generate_i(*match_value, env, target, links, a, point);

            SizeArray back_positions = mk_size_array(syn.match.clauses.len, a);
            SizeArray back_refs = mk_size_array(syn.match.clauses.len, a);

            // For each pattern match, generate two things 
            // 1. A comparison/check that the pattern has been matched
            // 2. A jump to the relevant location
            for (size_t i = 0; i < syn.match.clauses.len; i++) {
                SynClause clause = *(SynClause*)syn.match.clauses.data[i];

                // Note: the match value is on the variable stack, so we use VSTACK_HEAD.
                build_binary_op(Cmp, rref8(VSTACK_HEAD, 0, sz_64), imm32(clause.tag), ass, a, point);
                AsmResult out = build_unary_op(JE, imm32(0), ass, a, point);
                push_size(get_pos(ass), &back_positions);
                push_size(out.backlink, &back_refs);
            }

            // The 'body positions' and 'body_refs' store the inidices we need to
            // use to calculate jumps (and the bytes we need to update with those jumps)
            SizeArray body_positions = mk_size_array(syn.match.clauses.len, a);
            U64Array body_refs = mk_u64_array(syn.match.clauses.len, a);

            for (size_t i = 0; i < syn.match.clauses.len; i++) {
                // 1. Backpatch the jump to this clause that it jumps to here
                size_t branch_pos = back_positions.data[i];
                size_t branch_ref = back_refs.data[i];

                // calc backlink offset
                size_t body_pos = get_pos(ass);
                if (body_pos - branch_pos > INT32_MAX) {
                    throw_error(point, mk_string("Jump in match too large", a));
                } 

                set_i32_backlink(ass, branch_ref, (int32_t)(body_pos - branch_pos));

                SynClause clause = *(SynClause*)syn.match.clauses.data[i];
                PtrArray variant_types = *(PtrArray*)enum_type->enumeration.variants.data[clause.tag].val; 

                // Bind Clause Vars 
                // ------------------
                // At this point, it is known that the value within the enum is
                // on the variable stack, so we need to unpack the member(s)
                // individually before binding them
                // 
                // We employ a strategy of using the pointer to this value on
                // the variable stack, and destrictively writing the value to be
                // copied out. The first step here is adding 8 to account for
                // the tag, which is not bound.
                int64_t current_val_offset = get_stack_head(env);
                build_binary_op(Add, rref8(RSP, 0, sz_64), imm8(ADDRESS_SIZE), ass, a, point);

                BindingArray vars = mk_binding_array(variant_types.len, a);
                for (size_t i = 0; i < variant_types.len; i++) {
                    PiType* ty = variant_types.data[i];

                    if (is_variable_in(ty, env)) {
                        Binding bind = (Binding) {
                            .sym = clause.vars.data[i],
                            .size = ADDRESS_SIZE,
                            .is_variable = true,
                        };
                        push_binding(bind, &vars);

                        // TODO (BUG): account for alignment
                        build_binary_op(Sub, reg(RSP, sz_64), imm8(ADDRESS_SIZE), ass, a, point);
                        build_binary_op(Mov, reg(RCX, sz_64), rref8(RBP, current_val_offset, sz_64), ass, a, point);
                        build_binary_op(Mov, rref8(RSP, 0, sz_64), reg(RCX, sz_64), ass, a, point);
                        data_stack_grow(env, ADDRESS_SIZE);

                        if (i + 1 != variant_types.len) {
                            generate_size_of(RAX, ty, env, ass, a, point);
                            build_binary_op(Add, rref8(RBP, current_val_offset, sz_64), reg(RAX, sz_64), ass, a, point);
                        }

                    } else {
                        size_t arg_sz = pi_stack_size_of(*ty);
                        Binding bind = (Binding) {
                            .sym = clause.vars.data[i],
                            .size = arg_sz,
                            .is_variable = false,
                        };
                        push_binding(bind, &vars);

                        build_binary_op(Sub, reg(RSP, sz_64), imm8(arg_sz), ass, a, point);
                        data_stack_grow(env, arg_sz);

                        // Push the value onto the stack - perform a monomorphic stack
                    }
                }
                address_bind_enum_vars(vars, true, env);

                generate_i(*clause.body, env, target, links, a, point);
                if (is_variable_in(syn.ptype, env)) {
                    data_stack_shrink(env, ADDRESS_SIZE);
                } else {
                    data_stack_shrink(env, pi_stack_size_of(*clause.body->ptype));
                }

                // Generate jump to end of match expression to be backlinked later
                AsmResult out = build_unary_op(JMP, imm32(0), ass, a, point);
                push_size(get_pos(ass), &body_positions);
                push_u64(out.backlink, &body_refs);

                address_unbind_enum_vars(env);
            }

            // Finally, backlink all jumps from the bodies to the end.
            size_t curr_pos = get_pos(ass);
            for (size_t i = 0; i < body_positions.len; i++) {
                size_t body_pos = body_positions.data[i];
                size_t body_ref = body_refs.data[i];

                if (curr_pos - body_pos > INT32_MAX) {
                    throw_error(point, mk_string("Jump in match too large", a));
                } 

                set_i32_backlink(ass, body_ref, (int32_t)(curr_pos - body_pos));
            }

            // Cleanup code: All branches of the enum will jump here. 
            //  This lets us copy the resultant value up the stack.
            int64_t current_head_offset = get_stack_head(env);
            if (is_variable_in(syn.ptype, env)) {
                size_t mov_offset = var_head_offset - current_head_offset;
                generate_size_of(RAX, syn.ptype, env, ass, a, point);

                // Allocate space on the variable stack, then copy the variable value up the variable stack
                build_binary_op(Mov, reg(R10, sz_64), reg(VSTACK_HEAD, sz_64), ass, a, point);

                build_binary_op(Mov, reg(VSTACK_HEAD, sz_64), rref8(RBP, var_head_offset, sz_64), ass, a, point);
                build_binary_op(Sub, reg(VSTACK_HEAD, sz_64), reg(RAX, sz_64), ass, a, point);
                generate_poly_move(reg(R10, sz_64), reg(VSTACK_HEAD, sz_64), reg(RAX, sz_64), ass, a, point);

                // Restore the data stack.
                build_binary_op(Add, reg(RSP, sz_64), imm8(mov_offset), ass, a, point);
                build_binary_op(Mov, rref8(RSP, 0, sz_64), reg(VSTACK_HEAD, sz_64), ass, a, point);
                data_stack_shrink(env, mov_offset);
            } else {
                // Restore old variable stack head (pop variable stack values)
                build_binary_op(Mov, reg(VSTACK_HEAD, sz_64), rref8(RBP, var_head_offset, sz_64), ass, a, point);

                // Move the value up the stack 
                size_t val_sz = pi_size_of(*syn.ptype);
                size_t mov_offset = (var_head_offset + ADDRESS_SIZE) - current_head_offset - val_sz;
                generate_stack_move(val_sz, 0, val_sz, ass, a, point);

                // Pop intermediate data stack contents
                build_binary_op(Add, reg(RSP, sz_64), imm32(mov_offset), ass, a, point);
                data_stack_shrink(env, mov_offset);
            }

        } else {
            // Generate code for the value
            size_t enum_stack_size = pi_stack_size_of(*match_value->ptype);
            size_t out_size = pi_stack_size_of(*syn.ptype);

            generate_i(*match_value, env, target, links, a, point);

            SizeArray back_positions = mk_size_array(syn.match.clauses.len, a);
            SizeArray back_refs = mk_size_array(syn.match.clauses.len, a);

            // For each pattern match, generate two things 
            // 1. A comparison/check that the pattern has been matched
            // 2. A jump to the relevant location
            for (size_t i = 0; i < syn.match.clauses.len; i++) {
                SynClause clause = *(SynClause*)syn.match.clauses.data[i];
                build_binary_op(Cmp, rref8(RSP, 0, sz_64), imm32(clause.tag), ass, a, point);
                AsmResult out = build_unary_op(JE, imm32(0), ass, a, point);
                push_size(get_pos(ass), &back_positions);
                push_size(out.backlink, &back_refs);
            }

            // The 'body positions' and 'body_refs' store the inidices we need to
            // use to calculate jumps (and the bytes we need to update with those jumps)
            SizeArray body_positions = mk_size_array(syn.match.clauses.len, a);
            U64Array body_refs = mk_u64_array(syn.match.clauses.len, a);

            for (size_t i = 0; i < syn.match.clauses.len; i++) {
                // 1. Backpatch the jump so that it jumps to here
                size_t branch_pos = back_positions.data[i];
                size_t branch_ref = back_refs.data[i];

                // calc backlink offset
                size_t body_pos = get_pos(ass);
                if (body_pos - branch_pos > INT32_MAX) {
                    throw_error(point, mk_string("Jump in match too large", a));
                } 

                set_i32_backlink(ass, branch_ref, (int32_t)(body_pos - branch_pos));

                SynClause clause = *(SynClause*)syn.match.clauses.data[i];
                PtrArray variant_types = *(PtrArray*)enum_type->enumeration.variants.data[clause.tag].val; 

                // Bind Clause Vars 
                BindingArray arg_sizes = mk_binding_array(variant_types.len, a);
                for (size_t i = 0; i < variant_types.len; i++) {
                    Binding binder = (Binding) {
                        .sym = clause.vars.data[i],
                        .is_variable = false,
                        .size = pi_size_of(*(PiType*)variant_types.data[i]),
                    };
                    push_binding(binder, &arg_sizes);
                }
                address_bind_enum_vars(arg_sizes, false, env);

                generate_i(*clause.body, env, target, links, a, point);

                // Generate jump to end of match expression to be backlinked later
                AsmResult out = build_unary_op(JMP, imm32(0), ass, a, point);
                push_size(get_pos(ass), &body_positions);
                push_u64(out.backlink, &body_refs);

                address_unbind_enum_vars(env);
                data_stack_shrink(env, out_size);
            }

            // Finally, backlink all jumps from the bodies to the end.
            size_t curr_pos = get_pos(ass);
            for (size_t i = 0; i < body_positions.len; i++) {
                size_t body_pos = body_positions.data[i];
                size_t body_ref = body_refs.data[i];

                if (curr_pos - body_pos > INT32_MAX) {
                    throw_error(point, mk_string("Jump in match too large", a));
                } 

                set_i32_backlink(ass, body_ref, (int32_t)(curr_pos - body_pos));
            }

            generate_stack_move(enum_stack_size, 0, out_size, ass, a, point);

            build_binary_op(Add, reg(RSP, sz_64), imm32(enum_stack_size), ass, a, point);

            data_stack_shrink(env, enum_stack_size);
            data_stack_grow(env, out_size);
        }
        break;
    }
    case SDynamic: {
        // Create a new dynamic variable, i.e. call the C function 
        // mk_dynamic_var(size_t size, void* default_val)
        generate_i(*syn.dynamic, env, target, links, a, point);

        // currently RSP = default_val
        size_t val_size = pi_size_of(*syn.dynamic->ptype);

#if ABI == SYSTEM_V_64 
        // arg1 = rdi, arg2 = rsi
        build_binary_op(Mov, reg(RDI, sz_64), imm32(val_size), ass, a, point);
        build_binary_op(Mov, reg(RSI, sz_64), reg(RSP, sz_64), ass, a, point);
#elif ABI == WIN_64 
        // arg1 = rcx, arg2 = rdx
        build_binary_op(Mov, reg(RCX, sz_64), imm32(val_size), ass, a, point);
        build_binary_op(Mov, reg(RDX, sz_64), reg(RSP, sz_64), ass, a, point);
#else
#error "unknown ABI"
#endif

        // call function
        generate_c_call(mk_dynamic_var, ass, a, point);

        // Pop value off stack
        build_binary_op(Add, reg(RSP, sz_64), imm32(val_size), ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        
        data_stack_grow(env, ADDRESS_SIZE);
        data_stack_shrink(env, val_size);
        break;
    }
    case SDynamicUse: {
        // TODO: check that the dynamic use handles stack alignment correctly
        generate_i(*syn.use, env, target, links, a, point);

        // We now have a dynamic variable: get its' value as ptr

#if ABI == SYSTEM_V_64 
        // arg1 = rdi
        build_unary_op(Pop, reg(RDI, sz_64), ass, a, point);
#elif ABI == WIN_64 
        // arg1 = rcx
        build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);
#else
#error "unknown ABI"
#endif

        // call function
        generate_c_call(get_dynamic_val, ass, a, point);

        if (is_variable_in(syn.ptype, env)) {
            panic(mv_string("Not yet generating code for polymorphic dynamic use"));
        } else{
            // Now, allocate space on stack
            size_t val_size = pi_size_of(*syn.ptype);
            build_binary_op(Sub, reg(RSP, sz_64), imm32(pi_stack_align(val_size)), ass, a, point);
            build_binary_op(Mov, reg(RCX, sz_64), reg(RAX, sz_64), ass, a, point);

            generate_monomorphic_copy(RSP, RCX, val_size, ass, a, point);
            data_stack_shrink(env, ADDRESS_SIZE);
            data_stack_grow(env, val_size);
        }
        break;
    }
    case SDynamicSet: {
        size_t val_size = pi_size_of(*syn.dynamic_set.new_val->ptype);
        generate_i(*syn.dynamic_set.dynamic, env, target, links, a, point);
        generate_i(*syn.dynamic_set.new_val, env, target, links, a, point);

#if ABI == SYSTEM_V_64 
        // arg1 = rdi, arg2 = rsi
        build_binary_op(Mov, reg(RDI, sz_64), rref8(RSP, val_size, sz_64), ass, a, point);
        build_binary_op(Mov, reg(RSI, sz_64), reg(RSP, sz_64), ass, a, point);
        build_binary_op(Mov, reg(RDX, sz_64), imm32(val_size), ass, a, point);
#elif ABI == WIN_64 
        // arg1 = rcx, arg2 = rdx
        build_binary_op(Mov, reg(RCX, sz_64), rref8(RSP, val_size, sz_64), ass, a, point);
        build_binary_op(Mov, reg(RDX, sz_64), reg(RSP, sz_64), ass, a, point);
        build_binary_op(Mov, reg(R8, sz_64), imm32(val_size), ass, a, point);
#else
#error "unknown ABI"
#endif
        // call function
        generate_c_call(set_dynamic_val, ass, a, point);

        if (is_variable_in(syn.ptype, env)) {
            panic(mv_string("Not yet generating code for polymorphic dynamic set"));
        } else {
            build_binary_op(Add, reg(RSP, sz_64), imm32(val_size + ADDRESS_SIZE), ass, a, point);
            data_stack_shrink(env, ADDRESS_SIZE + val_size);
        }
        break;
    }
    case SDynamicLet: {
        // TODO: check that the dynamic let handles stack alignment correctly

        // Step 1: evaluate all dynamic bindings & bind them
        for (size_t i = 0; i < syn.dyn_let_expr.bindings.len; i++) {
            DynBinding* dbind = syn.dyn_let_expr.bindings.data[i];
            size_t bind_size = pi_size_of(*dbind->expr->ptype);
            generate_i(*dbind->var, env, target, links, a, point);
            generate_i(*dbind->expr, env, target, links, a, point);

            // Copy the value into the dynamic var
            // Currently, the stack looks like:
            // + dynamic-var-index
            //   new-value
            // R15 is the dynamic memory register, move it into RCX
            // Move the index into RAX
            build_binary_op(Mov, reg(RCX, sz_64), reg(DVARS_REGISTER, sz_64), ass, a, point);
            build_binary_op(Mov, reg(RAX, sz_64), rref8(RSP, bind_size, sz_64), ass, a, point);
            // RCX holds the array - we need to index it with index located in RAX
            build_binary_op(Mov, reg(RCX, sz_64), sib(RCX, RAX, 8, sz_64), ass, a, point);
            // Now we have a pointer to the value stored in RCX, swap it with
            // the value on the stack

            // TODO (check if replace with stack copy)
            generate_monomorphic_swap(RCX, RSP, bind_size, ass, a, point);
        }

        // Step 2: generate the body
        generate_i(*syn.let_expr.body, env, target, links, a, point);
        size_t val_size = pi_size_of(*syn.dyn_let_expr.body->ptype);

        // Step 3: unwind the bindings
        //  • Store the (address of the) current dynamic value index to restore in RDX
        build_binary_op(Mov, reg(RDX, sz_64), reg(RSP, sz_64), ass, a, point);
        build_binary_op(Add, reg(RDX, sz_64), imm32(val_size), ass, a, point);

        size_t offset_size = 0;
        for (size_t i = 0; i < syn.let_expr.bindings.len; i++) {
            DynBinding* dbind = syn.dyn_let_expr.bindings.data[i];
            size_t bind_size = pi_size_of(*dbind->expr->ptype);

            // Store ptr to dynamic memory (array) in RCX, and the index in RAX
            build_binary_op(Mov, reg(RCX, sz_64), reg(DVARS_REGISTER, sz_64), ass, a, point);
            build_binary_op(Mov, reg(RAX, sz_64), rref8(RDX, 0, sz_64), ass, a, point);
            // RCX holds the array - we need to index it with index located in RAX
            build_binary_op(Mov, reg(RCX, sz_64), sib(RCX, RAX, 8, sz_64), ass, a, point);

            // Store ptr to local value to restore in RDX 
            // TODO (check if replace with stack copy)
            generate_monomorphic_swap(RCX, RDX, bind_size, ass, a, point);
            build_binary_op(Add, reg(RDX, sz_64), imm32(bind_size + ADDRESS_SIZE), ass, a, point);

            offset_size += bind_size + ADDRESS_SIZE;
        }

        // Step 4: cleanup: pop bindings
        generate_stack_move(offset_size, 0, val_size, ass, a, point);
        build_binary_op(Add, reg(RSP, sz_64), imm32(offset_size), ass, a, point);
        data_stack_shrink(env, offset_size);
        break;
    }
    case SLet: {
        size_t bind_sz = ADDRESS_SIZE; 

        build_unary_op(Push, reg(VSTACK_HEAD, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);

        for (size_t i = 0; i < syn.let_expr.bindings.len; i++) {
            SymPtrCell elt = syn.let_expr.bindings.data[i];
            generate_i(*(Syntax*)elt.val, env, target, links, a, point);

            if (is_variable_in(((Syntax*)elt.val)->ptype, env)) {
                address_bind_relative_index(elt.key, 0, env);
                bind_sz += ADDRESS_SIZE;
            } else {
                size_t stack_sz = pi_stack_size_of(*((Syntax*)elt.val)->ptype);
                address_bind_relative(elt.key, 0, env);
                bind_sz += stack_sz;
            }
        }
        generate_i(*syn.let_expr.body, env, target, links, a, point);

        if (is_variable_in(syn.ptype, env)) {
            generate_stack_size_of(RAX, syn.ptype, env, ass, a, point);

            // Grab the value of VSTACK_HEAD we pushed at the beginning - offset bind_sz + stack head 
            build_binary_op(Mov, reg(VSTACK_HEAD, sz_64), rref8(RSP, bind_sz, sz_64), ass, a, point);
            build_binary_op(Sub, reg(VSTACK_HEAD, sz_64), reg(RAX, sz_64), ass, a, point);

            build_binary_op(Mov, reg(R9, sz_64), rref8(RSP, 0, sz_64), ass, a, point);

            generate_poly_move(reg(VSTACK_HEAD, sz_64), reg(R9, sz_64), reg(RAX, sz_64), ass, a, point);

            // Store current index in stack return position
            build_binary_op(Mov, rref8(RSP, bind_sz, sz_64), reg(VSTACK_HEAD, sz_64), ass, a, point);
            build_binary_op(Add, reg(RSP, sz_64), imm8(bind_sz), ass, a, point);
            data_stack_shrink(env, bind_sz);
        } else {
            size_t stack_sz = pi_stack_size_of(*syn.ptype);
            build_binary_op(Mov, reg(VSTACK_HEAD, sz_64), rref8(RSP, bind_sz + stack_sz - ADDRESS_SIZE, sz_64), ass, a, point);
            generate_stack_move(bind_sz, 0, stack_sz, ass, a, point);
            build_binary_op(Add, reg(RSP, sz_64), imm8(bind_sz), ass, a, point);
            data_stack_shrink(env, bind_sz);
        }
        break;
    }
    case SIf: {
        // Generate the condition
        generate_i(*syn.if_expr.condition, env, target, links, a, point);

        // Pop the bool into R9; compare with 0
        build_unary_op(Pop, reg(R9, sz_64), ass, a, point);
        build_binary_op(Cmp, reg(R9, sz_64), imm32(0), ass, a, point);
        data_stack_shrink(env, ADDRESS_SIZE);

        // ---------- CONDITIONAL JUMP ----------
        // compare the value to 0
        // jump to false branch if equal to 0 -- the 32-bit immediate is a placeholder
        AsmResult out = build_unary_op(JE, imm32(0), ass, a, point);
        size_t start_pos = get_pos(ass);

        size_t jmp_loc = out.backlink;

        // ---------- TRUE BRANCH ----------
        // now, generate the code to run (if true)
        generate_i(*syn.if_expr.true_branch, env, target, links, a, point);

        // Generate jump to end of false branch to be backlinked later
        out = build_unary_op(JMP, imm32(0), ass, a, point);

        // calc backlink offset
        size_t end_pos = get_pos(ass);
        if (end_pos - start_pos > INT32_MAX) {
            throw_error(point, mk_string("Jump in conditional too large", a));
        } 

        // backlink
        set_i32_backlink(ass, jmp_loc, end_pos - start_pos);
        jmp_loc = out.backlink;
        start_pos = get_pos(ass);

        data_stack_shrink(env, is_variable_in(syn.ptype, env) ? ADDRESS_SIZE : pi_stack_size_of(*syn.ptype));

        // ---------- FALSE BRANCH ----------
        // Generate code for the false branch
        generate_i(*syn.if_expr.false_branch, env, target, links, a, point);

        // calc backlink offset
        end_pos = get_pos(ass);
        if (end_pos - start_pos > INT32_MAX) {
            throw_error(point, mk_string("Jump in conditional too large", a));
        } 
        set_i32_backlink(ass, jmp_loc, end_pos - start_pos);
        break;
    }
    case SLabels: {
        // Labels: The code-generation for labels is as follows: 
        // 1. Push the current variable stack head ($r14)  
        // 2. Add labels to environment
        // 3. Generate expression
        // 4. For each expression:
        //  5.1 Mark Beginning of label expression
        //  6.2 Generate label expressions
        // 7. Backlink all labels.
        
        /* build_unary_op(Push, reg(VSTACK_HEAD, sz_64), ass, a, point); */
        /* data_stack_grow(env, ADDRESS_SIZE); */

        SymbolArray labels = mk_symbol_array(syn.labels.terms.len, a);
        for (size_t i = 0; i < syn.labels.terms.len; i++) 
            push_symbol(syn.labels.terms.data[i].key, &labels);

        address_start_labels(labels, env);

        generate_i(*syn.labels.entry, env, target, links, a, point);

        SymSizeAssoc label_points = mk_sym_size_assoc(syn.labels.terms.len, a);
        SymSizeAssoc label_jumps = mk_sym_size_assoc(syn.labels.terms.len, a);

        size_t out_size = pi_size_of(*syn.ptype);
        for (size_t i = 0; i < syn.labels.terms.len; i++) {
            // Clear the stack offset, either from expression or previous label
            data_stack_shrink(env,out_size);
            SymPtrACell cell = syn.labels.terms.data[i];

            // Mark Label
            size_t pos = get_pos(ass);
            sym_size_bind(cell.key, pos, &label_points);
            
            SynLabelBranch* branch = cell.val;

            // Bind Label Vars 
            size_t arg_total = 0;
            SymSizeAssoc arg_sizes = mk_sym_size_assoc(branch->args.len, a);
            for (size_t i = 0; i < branch->args.len; i++) {
                size_t arg_size = pi_size_of(*(PiType*)branch->args.data[i].val);
                sym_size_bind(branch->args.data[i].key, arg_size, &arg_sizes);
                arg_total += arg_size;
            }

            data_stack_grow(env,arg_total);
            address_bind_label_vars(arg_sizes, env);
            generate_i(*branch->body, env, target, links, a, point);
            address_unbind_label_vars(env);

            LabelEntry lble = label_env_lookup(cell.key, env);
            if (lble.type == Err)
                throw_error(point, mv_string("Label not found during codegen!!"));
            data_stack_shrink(env, arg_total);

            // Copy the result down the stack
            generate_stack_move(arg_total, 0, out_size, ass, a, point);
            build_binary_op(Add, reg(RSP, sz_64), imm8(arg_total), ass, a, point);

            AsmResult out = build_unary_op(JMP, imm32(0), ass, a, point);
            sym_size_bind(cell.key, out.backlink, &label_jumps);
        }

        size_t label_end = get_pos(ass);

        for (size_t i = 0; i < label_points.len; i++)  {
            Symbol sym = label_points.data[i].key;
            size_t dest = label_points.data[i].val;

            // Step 1: make the end of the label jump to the end of the expression.
            {
                size_t backlink = label_jumps.data[i].val;
                size_t origin = backlink + 4; // the + 4 accounts for the 4-byte immediate
                size_t dest = label_end;

                int64_t amt = dest - origin;
                if (amt < INT32_MIN || amt > INT32_MAX) panic(mv_string("Label jump too large!"));
                
                set_i32_backlink(ass, backlink, amt);
            }


            // Step 2: fill out each jump to this label.
            SizeArray* arr = sym_sarr_lookup(sym, links->gotolinks);
            if (!arr) panic(mv_string("Can't find size array when backlinking label!"));

            for (size_t i = 0; i < arr->len; i++) {
                size_t backlink = arr->data[i];
                size_t origin = backlink + 4; // the + 4 accounts for the 4-byte immediate

                int64_t amt = dest - origin;
                if (amt < INT32_MIN || amt > INT32_MAX) panic(mv_string("Label jump too large!"));
                
                set_i32_backlink(ass, backlink, amt);
            }
        }

        address_end_labels(env);
        break;
    }
    case SGoTo: {
        // Generating code for a goto:
        // 1. Generate args
        size_t arg_total = 0;
        for (size_t i = 0; i < syn.go_to.args.len; i++) {
            Syntax* expr = syn.go_to.args.data[i];
            arg_total += pi_stack_size_of(*expr->ptype);
            generate_i(*expr, env, target, links, a, point);
        }

        // 2. Backlink the label
        LabelEntry lble = label_env_lookup(syn.go_to.label, env);
        if (lble.type == Ok) {
            size_t delta = lble.stack_offset - arg_total;
            generate_stack_move(delta, 0, arg_total, ass, a, point);
            if (delta != 0) {
                // Adjust the stack so it has the same value that one would have
                // going into a labels expression.
                // TODO handle dynamic variable unbinding (if needed!)
                build_binary_op(Add, reg(RSP, sz_64), imm32(delta), ass, a, point);
            }

            // Stack sould "pretend" it pushed a value of type syn.ptype and
            // consumed all args. This is so that, e.g. if this go-to is inside
            // a seq or if, the other branches of the if or the rest of the seq
            // generates assuming the correct stack offset.
            data_stack_shrink(env, arg_total);
            data_stack_grow(env, pi_size_of(*syn.ptype));

            AsmResult out = build_unary_op(JMP, imm32(0), ass, a, point);

            backlink_goto(syn.go_to.label, out.backlink, links, a);
        } else {
            throw_error(point, mv_string("Label not found during codegen!!"));
        }
        break;
    }
    case SWithReset: {
        // TODO: check that the with-reset handles stack alignment correctly
        // TODO: make sure that with-reset and reset-to handle R13 and R14 correctly
        //                (dynamic vars + index stack)
        // Overview of reset codegen
        // 1. Push as reset point onto the stack
        // 2. Push the ptr to reset point onto the stack - bind it

        // 3. Evaluate Expr
        // 4. Cleanup
        // 5. Insert jump - on expr completion & cleanup, goto end of expr

        // 6. Entry to handler - rework stack binding/understanding of stackstructure 
        // 7. Generate handler code
        // 8. Cleanup after handler and backlink jump from end of expr

        // Step 1.
        //-------
        AsmResult out = build_binary_op(LEA, reg(RAX, sz_64), rref32(RIP, 0, sz_64), ass, a, point); // TODO: backlink N
        size_t reset_backlink = out.backlink;
        size_t reset_instr_end = get_pos(ass);

        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        build_unary_op(Push, reg(RBP, sz_64), ass, a, point);

        // Step 2.
        //-------
        build_unary_op(Push, reg(RSP, sz_64), ass, a, point);
        data_stack_grow(env, 3*ADDRESS_SIZE);
        address_bind_relative(syn.with_reset.point_sym, 0, env);

        // Step 3.
        //--------
        generate_i(*syn.with_reset.expr,env, target, links, a, point);

        // Step 4.
        //--------

        // Cleanup code for both 
        // At this point, the stack should have the appearance:
        // -- RIP for resetting to
        // -- Old RBP
        // -- Self-ptr + RSP
        // -- Out Value
        // So, the goal is do to a stack move from current RSP to current RSP - 3*ADDRESS

        size_t val_size = pi_size_of(*syn.ptype);
        size_t in_val_size = pi_size_of(*syn.with_reset.in_arg_ty);
        generate_stack_move(3 * ADDRESS_SIZE, 0, val_size, ass, a, point);
        build_binary_op(Add, reg(RSP, sz_64), imm32(3 * ADDRESS_SIZE), ass, a, point);
        generate_stack_move(3 * ADDRESS_SIZE, 0, val_size, ass, a, point);

        data_stack_shrink(env, 3*ADDRESS_SIZE + val_size);
        address_pop(env);

        // Step 5.
        //--------
        out = build_unary_op(JMP, imm32(0), ass, a, point);
        size_t end_expr_link = out.backlink;
        size_t end_expr_pos = get_pos(ass);

        // Note: 
        const size_t reset_intro_pos = get_pos(ass);
        size_t dist = reset_intro_pos - reset_instr_end; 
        if (dist > INT32_MAX) {
            panic(mv_string("Reset expression jump distance too large."));
        }
        // Note: cast to uint8_t to get correct ptr arithmetic!
        *(int32_t*)((uint8_t*)get_instructions(ass).data + reset_backlink) = (int32_t)dist;

        // TODO: backlink to jump here
        // If we end up here, then we know the stacklooks like
        //     > Value (argument)  / ADRESS_SIZE
        // RSP > Continuation Mark / ADDRESS SIZE

        //----------------------------------------------------------------------
        // Handler Code begins here
        //----------------------------------------------------------------------
        // Step 6.
        //--------
        // Note: from reset-to, the stack now currently looks like
        // value 
        // Bind value + continuation mark
        data_stack_grow(env, ADDRESS_SIZE + in_val_size);
        address_bind_relative(syn.with_reset.cont_sym, 0, env);
        address_bind_relative(syn.with_reset.in_sym, 8, env);
        
        /* Symbol cont_sym; */
        /* Symbol in_sym; */
        generate_i(*syn.with_reset.handler, env, target, links, a, point);
        data_stack_shrink(env, ADDRESS_SIZE + in_val_size);
        address_pop_n(2, env);

        // Step 7.
        //--------
        // At this point the stack looks like:
        //     > in-value
        //     > return-addr
        // RSP > out value
        // So do some cleanup
        // - generate_stack_move(ADDRESS_SIZE + val_size, 0, val_size, ass, ass, a, point);
        generate_stack_move(ADDRESS_SIZE + in_val_size, 0, val_size, ass, a, point);
        build_binary_op(Add, reg(RSP, sz_64), imm32(ADDRESS_SIZE + in_val_size), ass, a, point);


        // Step 8. 
        //--------
        size_t cleanup_start_pos = get_pos(ass);
        dist = cleanup_start_pos - end_expr_pos;
        if (dist > INT32_MAX) {
            throw_error(point, mv_string("Internal error in codegen: jump distance exceeded INT32_MAX"));
        }
        *(get_instructions(ass).data + end_expr_link) = (int32_t) dist;

        break;
    }
    case SResetTo: {
        generate_i(*syn.reset_to.arg, env, target, links, a, point);
        generate_i(*syn.reset_to.point, env, target, links, a, point);
        // there should how be a stack with the following:
        // > arg
        // > Ptr to reset-point

        // Step 1: take the ptr to reset-point
        build_unary_op(Pop, reg(R10, sz_64), ass, a, point);

        // Step 2: stash the current stack ptr in R9 (this will be used to copy the argument )
        build_binary_op(Mov, reg(R9, sz_64), reg(RSP, sz_64), ass, a, point);

        // Step 3: Deref return point ptr to get old RBP, RSP & RIP to jump up 
        build_binary_op(Mov, reg(RSP, sz_64), rref8(R10, -8, sz_64), ass, a, point);
        build_unary_op(Pop, reg(RBP, sz_64), ass, a, point);
        build_unary_op(Pop, reg(RDI, sz_64), ass, a, point); //RDI = jump dest!

        // Step 4: Copy the argument onto the (new) stack.
        PiType* arg_type = syn.reset_to.arg->ptype;
        size_t asize = pi_size_of(*arg_type);
        build_binary_op(Sub, reg(RSP, sz_64), imm32(asize), ass, a, point);
        // TODO (check if replace with stack copy)
        generate_monomorphic_copy(RSP, R9, asize, ass, a, point);

        // Step 5: Long Jump (call) register
        build_unary_op(Call, reg(RDI, sz_64), ass, a, point);

        // Address environment bookkeeping: shrink the stack appropriately
        data_stack_shrink(env, pi_size_of(*syn.reset_to.arg->ptype) + pi_size_of(*syn.reset_to.point->ptype));
        data_stack_grow(env, pi_size_of(*syn.ptype));

        break;
    }
    case SSequence: {
        size_t bind_sz = ADDRESS_SIZE; 

        build_unary_op(Push, reg(VSTACK_HEAD, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);

        for (size_t i = 0; i < syn.sequence.elements.len; i++) {
            SeqElt* elt = syn.sequence.elements.data[i];
            generate_i(*elt->expr, env, target, links, a, point);

            if (elt->is_binding && i + 1 != syn.sequence.elements.len) {
                if (is_variable_in(elt->expr->ptype, env)) {
                    address_bind_relative_index(elt->symbol, 0, env);
                    bind_sz += ADDRESS_SIZE;
                } else {
                    size_t stack_sz = pi_stack_size_of(*elt->expr->ptype);
                    address_bind_relative(elt->symbol, 0, env);
                    bind_sz += stack_sz;
                }
            }

            // Remember: We do not pop off the stack if this is
            // a) a binding (hence this is an else) 
            // b) the last value in the sequence (as this value is preserved)
            else if (i + 1 != syn.sequence.elements.len) {
                if (is_variable_in(elt->expr->ptype, env)) {
                    generate_stack_size_of(RAX, elt->expr->ptype, env, ass, a, point);
                    build_binary_op(Add, reg(VSTACK_HEAD, sz_64), reg(RAX, sz_64), ass, a, point);
                    build_binary_op(Add, reg(RSP, sz_64), imm8(ADDRESS_SIZE), ass, a, point);
                    data_stack_shrink(env, ADDRESS_SIZE);
                } else {
                    size_t stack_sz = pi_stack_size_of(*elt->expr->ptype);
                    build_binary_op(Add, reg(RSP, sz_64), imm8(stack_sz), ass, a, point);
                    data_stack_shrink(env, stack_sz);
                }
            }
            
        }
        if (is_variable_in(syn.ptype, env)) {
            generate_stack_size_of(RAX, syn.ptype, env, ass, a, point);

            // Grab the value of VSTACK_HEAD we pushed at the beginning - offset bind_sz + stack head 
            build_binary_op(Mov, reg(VSTACK_HEAD, sz_64), rref8(RSP, bind_sz + ADDRESS_SIZE, sz_64), ass, a, point);
            build_binary_op(Sub, reg(VSTACK_HEAD, sz_64), reg(RAX, sz_64), ass, a, point);

            build_binary_op(Mov, reg(R9, sz_64), rref8(RSP, 0, sz_64), ass, a, point);

            generate_poly_move(reg(VSTACK_HEAD, sz_64), reg(R9, sz_64), reg(RAX, sz_64), ass, a, point);

            // Store current index in stack return position
            generate_stack_move(bind_sz, 0, ADDRESS_SIZE, ass, a, point);
            build_binary_op(Add, reg(RSP, sz_64), imm8(bind_sz), ass, a, point);
            data_stack_shrink(env, bind_sz);
        } else {
            size_t stack_sz = pi_stack_size_of(*syn.ptype);
            build_binary_op(Mov, reg(VSTACK_HEAD, sz_64), rref32(RSP, bind_sz + stack_sz, sz_64), ass, a, point);
            generate_stack_move(bind_sz, 0, stack_sz, ass, a, point);
            build_binary_op(Add, reg(RSP, sz_64), imm32(bind_sz), ass, a, point);
            data_stack_shrink(env, bind_sz);
        }
        break;
    }
    case SIs:
        generate_i(*syn.is.val, env, target, links, a, point);
        break;
    case SInTo:
        generate_i(*syn.into.val, env, target, links, a, point);
        break;
    case SOutOf:
        generate_i(*syn.out_of.val, env, target, links, a, point);
        break;
    case SName:
        generate_i(*syn.name.val, env, target, links, a, point);
        break;
    case SUnName:
        generate_i(*syn.unname, env, target, links, a, point);
        break;
    case SWiden:
        if (is_variable_in(syn.ptype, env)) {
            // TODO: throw error with range/report in typecheck phase?
            panic(mv_string("Can't generate code for polymorphic-widen"));
        }

        // TODO (BUG): appropriately widen (sign-extend/double broaden)
        generate_i(*syn.widen.val, env, target, links, a, point);
        if (syn.widen.val->ptype->prim < UInt_8) {
            // is signed, 
            panic(mv_string("can't widen signed ints yet!"));
        } else if (syn.widen.val->ptype->prim < Float_32) {
            // is unsigned, so the movzx instruction will do:
            LocationSize sz;
            switch (syn.widen.val->ptype->prim) {
            case UInt_8:
                sz = sz_8;
                break;
            case UInt_16:
                sz = sz_16;
                break;
            case UInt_32:
                sz = sz_32;
                break;
            case UInt_64:
                sz = sz_64;
                break;
            default:
                panic(mv_string("impossible code path"));
            }

            build_binary_op(Mov, reg(RAX, sz_64), rref8(RSP, 0, sz_64), ass, a, point);
            build_binary_op(Mov, reg(RCX, sz), reg(RAX, sz), ass, a, point);
            build_binary_op(Mov, rref8(RSP, 0, sz_64), reg(RCX, sz_64), ass, a, point);
        } else {
            panic(mv_string("can't widen this yet!"));
        }

        break;
    case SNarrow:
        if (is_variable_in(syn.ptype, env)) {
            // TODO: throw error with range/report in typecheck phase?
            panic(mv_string("Can't generate code for polymorphic-narrow"));
        }
        // TODO: if signed -ve => 0??
        // TODO (BUG): appropriately narrow (sign-extend/double broaden)
        generate_i(*syn.narrow.val, env, target, links, a, point);
        break;
    case SSizeOf: {
        generate_size_of(RAX, syn.size->type_val, env, ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);
        break;
    }
    case SAlignOf: {
        generate_align_of(RAX, syn.size->type_val, env, ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);
        break;
    }
    case SOffsetOf: {
        PiType* struct_type = strip_type(syn.offset_of.body->type_val);
        generate_offset_of(RAX, syn.offset_of.field, struct_type->structure.fields, env, ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);
        break;
    }
    case SCheckedType: {
        build_binary_op(Mov, reg(R9, sz_64), imm64((uint64_t)syn.type_val), ass, a, point);
        build_unary_op(Push, reg(R9, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);
        break;
    }
    case SProcType: {
        // Generate proc type: start by malloc'ing size for args
        generate_tmp_malloc(reg(RAX, sz_64), imm32(syn.proc_type.args.len * ADDRESS_SIZE), ass, a, point);
        build_binary_op(Mov, reg(RCX, sz_64), imm32(0), ass, a, point);

        for (size_t i = 0; i < syn.proc_type.args.len; i++) {
            Syntax* arg = syn.proc_type.args.data[i];

            // Second, generate & move the type (note: stash & pop RCX)
            build_unary_op(Push, reg(RCX, sz_64), ass, a, point);
            build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
            data_stack_grow(env, 2*ADDRESS_SIZE);
            generate_i(*arg, env, target, links, a, point);

            data_stack_shrink(env, 3*ADDRESS_SIZE);
            build_unary_op(Pop, reg(R9, sz_64), ass, a, point);
            build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);
            build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);

            build_binary_op(Mov, sib(RAX, RCX, 8, sz_64), reg(R9, sz_64), ass, a, point);

            // Now, incremenet index by 1
            build_binary_op(Add, reg(RCX, sz_64), imm32(1), ass, a, point);
        }
        // Stash RAX
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);
        generate_i(*syn.proc_type.return_type, env, target, links, a, point);
        data_stack_shrink(env, 2*ADDRESS_SIZE);
        build_unary_op(Pop, reg(R9, sz_64), ass, a, point);
        build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);

        // Finally, generate function call to make type
        gen_mk_proc_ty(reg(RAX, sz_64), imm32(syn.proc_type.args.len), reg(RAX, sz_64), reg(R9, sz_64), ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);

        break;
    }
    case SStructType: {
        // Generate struct type: for each element of the struct type
        // First, malloc enough data for the array:
        generate_tmp_malloc(reg(RAX, sz_64), imm32(syn.struct_type.fields.len * 3 * ADDRESS_SIZE), ass, a, point);
        build_binary_op(Mov, reg(RCX, sz_64), imm32(0), ass, a, point);

        for (size_t i = 0; i < syn.struct_type.fields.len; i++) {
            SymPtrCell field = syn.struct_type.fields.data[i];
            // First, move the field name
            build_binary_op(Mov, sib8(RAX, RCX, 8, 0, sz_64), imm32(field.key.name), ass, a, point);
            build_binary_op(Mov, sib8(RAX, RCX, 8, 8, sz_64), imm32(field.key.did), ass, a, point);

            // Second, generate & move the type (note: stash & pop RCX)
            build_unary_op(Push, reg(RCX, sz_64), ass, a, point);
            build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
            data_stack_grow(env, 2*ADDRESS_SIZE);
            generate_i(*(Syntax*)field.val, env, target, links, a, point);

            data_stack_shrink(env, 3*ADDRESS_SIZE);
            build_unary_op(Pop, reg(R9, sz_64), ass, a, point);
            build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);
            build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);

            build_binary_op(Mov, sib8(RAX, RCX, 8, 16, sz_64), reg(R9, sz_64), ass, a, point);

            // Now, incremenet index by 3 (to account for struct size!)
            build_binary_op(Add, reg(RCX, sz_64), imm32(3), ass, a, point);
        }

        // Finally, generate function call to make type
        gen_mk_struct_ty(reg(RAX, sz_64), imm32(syn.struct_type.fields.len), reg(RAX, sz_64), ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);
        break;
    }
    case SEnumType: {
        // Generate enum type: malloc array for enum
        // First, malloc enough data for the array:
        generate_tmp_malloc(reg(RAX, sz_64), imm32(syn.enum_type.variants.len * 3 * ADDRESS_SIZE), ass, a, point);
        build_binary_op(Mov, reg(RCX, sz_64), imm32(0), ass, a, point);

        for (size_t i = 0; i < syn.enum_type.variants.len; i++) {
            SymPtrCell field = syn.enum_type.variants.data[i];
            // First, move the field name
            build_binary_op(Mov, sib8(RAX, RCX, 8, 0, sz_64), imm32(field.key.name), ass, a, point);
            build_binary_op(Mov, sib8(RAX, RCX, 8, 8, sz_64), imm32(field.key.did), ass, a, point);

            // Second, generate & variant (note: stash & pop RCX)
            build_unary_op(Push, reg(RCX, sz_64), ass, a, point);
            build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
            data_stack_grow(env, 2*ADDRESS_SIZE);

            PtrArray variant = *(PtrArray*)field.val;
            generate_tmp_malloc(reg(RAX, sz_64), imm32(variant.len * ADDRESS_SIZE), ass, a, point);
            build_binary_op(Mov, reg(RCX, sz_64), imm32(0), ass, a, point);

            for (size_t i = 0; i < variant.len; i++) {

                build_unary_op(Push, reg(RCX, sz_64), ass, a, point);
                build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
                data_stack_grow(env, 2*ADDRESS_SIZE);

                generate_i(*(Syntax*)variant.data[i], env, target, links, a, point);

                data_stack_shrink(env, 3*ADDRESS_SIZE);
                build_unary_op(Pop, reg(R9, sz_64), ass, a, point);
                build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);
                build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);

                build_binary_op(Mov, sib(RAX, RCX, 8, sz_64), reg(R9, sz_64), ass, a, point);

                // Now, incremenet index
                build_binary_op(Add, reg(RCX, sz_64), imm32(1), ass, a, point);
            }

            // The variant was just stored in RAX, move it to R9
            build_binary_op(Mov, reg(R9, sz_64), reg(RAX, sz_64), ass, a, point);

            build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);
            build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);
            data_stack_shrink(env, 2*ADDRESS_SIZE);

            build_binary_op(Mov, sib8(RAX, RCX, 8, 16, sz_64), reg(R9, sz_64), ass, a, point);

            // Now, incremenet index by 3 (to account for ptr + symbol)
            build_binary_op(Add, reg(RCX, sz_64), imm32(3), ass, a, point);
        }

        // Finally, generate function call to make type
        gen_mk_enum_ty(reg(RAX, sz_64), syn.enum_type, reg(RAX, sz_64), ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);
        break;
    }
    case SResetType:
        generate_i(*(Syntax*)syn.reset_type.in, env, target, links, a, point);
        generate_i(*(Syntax*)syn.reset_type.out, env, target, links, a, point);
        gen_mk_reset_ty(ass, a, point);
        data_stack_shrink(env, ADDRESS_SIZE);
        break;
    case SDynamicType:
        generate_i(*(Syntax*)syn.dynamic_type, env, target, links, a, point);
        gen_mk_dynamic_ty(ass, a, point);
        break;
    case SAllType:
        // Forall type structure: (array symbol) body
        for (size_t i = 0; i < syn.bind_type.bindings.len; i++) {
            address_bind_type(syn.bind_type.bindings.data[i], env);
        }
        generate_i(*(Syntax*)syn.bind_type.body, env, target, links, a, point);
        gen_mk_forall_ty(syn.bind_type.bindings, ass, a, point);
        address_pop_n(syn.bind_type.bindings.len, env);
        break;
    case SExistsType: {
        for (size_t i = 0; i < syn.exists_type.vars.len; i++) {
            address_bind_type(syn.bind_type.bindings.data[i], env);
        }

        generate_tmp_malloc(reg(RAX, sz_64), imm32(syn.exists_type.implicits.len * ADDRESS_SIZE), ass, a, point);
        build_binary_op(Mov, reg(RCX, sz_64), imm32(0), ass, a, point);

        for (size_t i = 0; i < syn.exists_type.implicits.len; i++) {
            Syntax* arg = syn.exists_type.implicits.data[i];

            // Second, generate & move the type (note: stash & pop RCX)
            build_unary_op(Push, reg(RCX, sz_64), ass, a, point);
            build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
            data_stack_grow(env, 2*ADDRESS_SIZE);
            generate_i(*arg, env, target, links, a, point);

            data_stack_shrink(env, 3*ADDRESS_SIZE);
            build_unary_op(Pop, reg(R9, sz_64), ass, a, point);
            build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);
            build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);

            build_binary_op(Mov, sib(RAX, RCX, 8, sz_64), reg(R9, sz_64), ass, a, point);

            // Now, incremenet index by 1
            build_binary_op(Add, reg(RCX, sz_64), imm32(1), ass, a, point);
        }

        // Stash RAX
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);
        generate_i(*syn.exists_type.body, env, target, links, a, point);
        data_stack_shrink(env, 2*ADDRESS_SIZE);
        // build_unary_op(Pop, reg(R9, sz_64), ass, a, point);
        // build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);

        gen_mk_exists_ty(syn.exists_type.vars, imm32(syn.exists_type.implicits.len), ass, a, point);
        address_pop_n(syn.exists_type.vars.len, env);
        data_stack_grow(env, ADDRESS_SIZE);
        break;
    }
    case STypeFamily:
        // Family type structure: (array symbol) body
        for (size_t i = 0; i < syn.bind_type.bindings.len; i++) {
            address_bind_type(syn.bind_type.bindings.data[i], env);
        }
        generate_i(*(Syntax*)syn.bind_type.body, env, target, links, a, point);
        gen_mk_fam_ty(syn.bind_type.bindings, ass, a, point);
        address_pop_n(syn.bind_type.bindings.len, env);
        break;
    case SLiftCType: {
        generate_i(*syn.c_type, env, target, links, a, point);
        gen_mk_c_ty(ass, a, point);
        // Now, the type lies atop the stack, and we must pop the ctype out from
        // under it
        size_t cts = pi_stack_size_of(*syn.c_type->ptype);

        // TODO (IMPROVEMENT) this assumes address-size == 64 bits 
        build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);
        build_binary_op(Add, reg(RSP, sz_64), imm32(cts), ass, a, point);
        build_unary_op(Push, reg(RCX, sz_64), ass, a, point);

        data_stack_shrink(env, cts - ADDRESS_SIZE);
        break;
    }
    case SNamedType: {
        address_bind_type(syn.named_type.name, env);
        build_binary_op(Mov, reg(RAX, sz_64), imm64(syn.named_type.name.name), ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        build_binary_op(Mov, reg(RAX, sz_64), imm64(syn.named_type.name.did), ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        data_stack_grow(env, sizeof(Symbol));

        address_bind_type(syn.named_type.name, env);
        generate_i(*(Syntax*)syn.named_type.body, env, target, links, a, point);
        address_pop(env);

        gen_mk_named_ty(ass, a, point);
        data_stack_shrink(env, sizeof(Symbol));
        address_pop(env);
        break;
    }
    case SDistinctType:
        generate_i(*(Syntax*)syn.distinct_type, env, target, links, a, point);
        gen_mk_distinct_ty(ass, a, point);
        break;
    case SOpaqueType:
        generate_i(*(Syntax*)syn.opaque_type, env, target, links, a, point);
        gen_mk_opaque_ty(ass, a, point);
        break;
    case STraitType: {
        // Generate trait type: first bind relevant variables
        for (size_t i = 0; i < syn.bind_type.bindings.len; i++) {
            address_bind_type(syn.bind_type.bindings.data[i], env);
        }

        // First, malloc enough data for the array:
        generate_tmp_malloc(reg(RAX, sz_64), imm32(syn.trait.fields.len * (sizeof(Symbol) + ADDRESS_SIZE)), ass, a, point);
        build_binary_op(Mov, reg(RCX, sz_64), imm32(0), ass, a, point);


        for (size_t i = 0; i < syn.trait.fields.len; i++) {
            SymPtrCell field = syn.trait.fields.data[i];
            // First, move the field name
            build_binary_op(Mov, sib8(RAX, RCX, 8, 0, sz_64), imm32(field.key.name), ass, a, point);
            build_binary_op(Mov, sib8(RAX, RCX, 8, 8, sz_64), imm32(field.key.did), ass, a, point);

            // Second, generate & move the type (note: stash & pop RCX)
            build_unary_op(Push, reg(RCX, sz_64), ass, a, point);
            build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
            data_stack_grow(env, 2*ADDRESS_SIZE);
            generate_i(*(Syntax*)field.val, env, target, links, a, point);

            data_stack_shrink(env, 3*ADDRESS_SIZE);
            build_unary_op(Pop, reg(R9, sz_64), ass, a, point);
            build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);
            build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);

            build_binary_op(Mov, sib8(RAX, RCX, 8, 16, sz_64), reg(R9, sz_64), ass, a, point);

            // Now, incremenet index by 2 (to account for trait size!)
            build_binary_op(Add, reg(RCX, sz_64), imm32(3), ass, a, point);
        }

        // Finally, generate function call to make type
        gen_mk_trait_ty(syn.trait.vars, reg(RAX, sz_64), imm32(syn.trait.fields.len), reg(RAX, sz_64), ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);

        address_pop_n(syn.trait.vars.len, env);
        break;
    }

    case SReinterpret:
        // reinterpret has no associated codegen
        generate_i(*syn.reinterpret.body, env, target, links, a, point);
        break;
    case SConvert: {
        if (syn.convert.from_native) {
            void* cfn = *(void**)const_fold(syn.convert.body, env, target, links, a, point);
            void* proc_address = get_instructions(target.code_aux).data;
            proc_address += get_instructions(target.code_aux).len;

            // Generate procedure value (push the address onto the stack)
            AsmResult out = build_binary_op(Mov, reg(RAX, sz_64), imm64((uint64_t)proc_address), ass, a, point);
            backlink_code(target, out.backlink, links);
            build_unary_op(Push, reg(RAX, sz_64), ass, a, point);

            // Now, change the target and the assembler, such that code is now
            // generated in the 'code segment'. Then, generate the function body
            ass = target.code_aux;
            target.target = target.code_aux;

            bd_convert_c_fn(cfn, &syn.convert.body->ptype->c_type, syn.ptype, ass, a, point);
        } else {
            panic(mv_string("Cannot yet convert pico value to c value."));
        }
        break;
    }
    case STypeOf: {
        build_binary_op(Mov, reg(R9, sz_64), imm64((uint64_t)syn.type_of->ptype), ass, a, point);
        build_unary_op(Push, reg(R9, sz_64), ass, a, point);
        data_stack_grow(env, pi_size_of(*syn.ptype));
        break;
    }
    case SDescribe: {
        Environment* base = get_addr_base(env);
        if (syn.to_describe.len == 0) panic(mv_string("unexepcted empty path provided to describe"));

        EnvEntry entry = env_lookup(syn.to_describe.data[0], base);
        for (size_t i = 1; i < syn.to_describe.len; i++) {
            if (entry.is_module) {
                ModuleEntry* mentry = get_def(syn.to_describe.data[i], entry.value);
                if (mentry) {
                    entry.is_module = mentry->is_module;
                    entry.value = mentry->value;
                    entry.type = &mentry->type;
                } else {
                    throw_error(point, mv_string("Unknown symbol in path to describe."));
                }
            } else {
                throw_error(point, mv_string("Unknown symbol in path to describe."));
            }
        }
        String immediate;

        if (entry.success == Ok) {
          if (entry.is_module) {
              SymbolArray syms = get_defined_symbols(entry.value, a);
              PtrArray lines = mk_ptr_array(syms.len + 8, a);
              {
                  PtrArray moduledesc = mk_ptr_array(2, a);
                  String module_name = get_name(entry.value, a);
                  push_ptr(mk_str_doc(mv_string("Module: "), a), &moduledesc);
                  push_ptr(mk_str_doc(module_name, a), &moduledesc);
                  push_ptr(mv_sep_doc(moduledesc, a), &lines);
              }
              push_ptr(mk_str_doc(mv_string("────────────────────────────────────────────"), a), &lines);

              for (size_t i = 0; i < syms.len; i++) {
                  Symbol symbol = syms.data[i];
                  ModuleEntry* mentry = get_def(symbol, entry.value);
                  if (mentry) {
                      PtrArray desc = mk_ptr_array(3, a);
                      if (mentry->is_module) {
                          push_ptr(mk_str_doc(mv_string("Module"), a), &desc);
                          push_ptr(mk_str_doc(symbol_to_string(symbol, a), a), &desc);
                      } else {
                          push_ptr(mk_str_doc(symbol_to_string(symbol, a), a), &desc);
                          push_ptr(mk_str_doc(mv_string(":"), a), &desc);
                          push_ptr(mv_nest_doc(2, pretty_type(&mentry->type, a), a), &desc);
                      }
                      push_ptr(mv_hsep_doc(desc, a), &lines);
                  } else {
                      // TODO: report error - exported symbol not in module?
                  }
              }

              push_ptr(mk_str_doc(mv_string("────────────────────────────────────────────\n"), a), &lines);

              Document* doc = mv_grouped_vsep_doc(lines, a);
              immediate = doc_to_str(doc, 80, a);

          } else {
              PtrArray lines = mk_ptr_array(8, a);
              {
                  PtrArray header = mk_ptr_array(syn.to_describe.len + 1, a);
                  push_ptr(mk_str_doc(mv_string("Path: "), a), &header);
                  for (size_t i = 0; i < syn.to_describe.len; i++) {
                      push_ptr(mk_str_doc(symbol_to_string(syn.to_describe.data[i], a), a), &header);
                  }
                  push_ptr(mv_sep_doc(header, a), &lines);
              }
              push_ptr(mk_str_doc(mv_string("────────────────────────────────────────────"), a), &lines);
              {
                  PtrArray moduledesc = mk_ptr_array(2, a);
                  String module_name = get_name(entry.source, a);
                  push_ptr(mk_str_doc(mv_string("Source Module: "), a), &moduledesc);
                  push_ptr(mk_str_doc(module_name, a), &moduledesc);
                  push_ptr(mv_sep_doc(moduledesc, a), &lines);
              }
              {
                  PtrArray typedesc = mk_ptr_array(2, a);
                  push_ptr(mk_str_doc(mv_string("Type: "), a), &typedesc);
                  push_ptr(pretty_type(entry.type, a), &typedesc);
                  push_ptr(mv_sep_doc(typedesc, a), &lines);
              }
              {
                  PtrArray valdesc = mk_ptr_array(2, a);
                  push_ptr(mk_str_doc(mv_string("Value: "), a), &valdesc);
                  push_ptr(pretty_pi_value(entry.value, entry.type, a), &valdesc);
                  push_ptr(mv_sep_doc(valdesc, a), &lines);
              }

              // end line
              push_ptr(mk_str_doc(mv_string("────────────────────────────────────────────\n"), a), &lines);

              Document* doc = mv_grouped_vsep_doc(lines, a);
              immediate = doc_to_str(doc, 120, a);
          }
        } else {
            immediate = mv_string("Local variable.");
        }


        if (immediate.memsize > UINT32_MAX) 
            throw_error(point, mv_string("Codegen: String literal length must fit into less than 32 bits"));

        // Push the string onto the stack
        // '0' is used as a placeholder, as the correct address will be
        // substituted when backlinking!
        AsmResult out = build_binary_op(Mov, reg(RAX, sz_64), imm64(0), ass, a, point);

        // Backlink the data & copy the bytes into the data-segment.
        backlink_data(target, out.backlink, links);
        add_u8_chunk(immediate.bytes, immediate.memsize, target.data_aux);

        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        build_unary_op(Push, imm32(immediate.memsize), ass, a, point);

        data_stack_grow(env, pi_stack_size_of(*syn.ptype));
        break;
    }
    case SQuote: {
        // Setup: push the memory address (in data) of the Syntax value into stack.
        AsmResult out = build_binary_op(Mov, reg(RAX, sz_64), imm64(0), ass, a, point);
        backlink_data(target, out.backlink, links);

        // Now copy the entire concrete syntax tree into the data-segment,
        // setting allocators to null
        add_rawtree(syn.quoted, target, links);

        build_binary_op(Sub, reg(RSP, sz_64), imm8(sizeof(RawTree)), ass, a, point);
        for (size_t i = 0; i < sizeof(RawTree); i += sizeof(uint64_t)) {
            build_binary_op(Mov, reg(RCX, sz_64), rref8(RAX, i, sz_64), ass, a, point);
            build_binary_op(Mov, rref8(RSP, i, sz_64), reg(RCX, sz_64), ass, a, point);
        }
        data_stack_grow(env, pi_stack_size_of(*syn.ptype));
        break;
    }
    case SCapture: {
        // Setup: push the memory address (in data) of the Syntax value into stack.
        AsmResult out = build_binary_op(Mov, reg(RAX, sz_64), imm64(0), ass, a, point);
        backlink_data(target, out.backlink, links);

        RawTree raw = (RawTree) {
          .type = RawAtom,
          .atom = (Atom) {
              .type = ACapture,
              .capture = (Capture) {
                  .type = syn.capture.type,
                  .value = syn.capture.value,
              }
          },
        };
        // Now copy the entire concrete syntax tree into the data-segment,
        // setting allocators to null
        add_rawtree(raw, target, links);

        build_binary_op(Sub, reg(RSP, sz_64), imm8(sizeof(RawTree)), ass, a, point);
        for (size_t i = 0; i < sizeof(RawTree); i += sizeof(uint64_t)) {
            build_binary_op(Mov, reg(RCX, sz_64), rref8(RAX, i, sz_64), ass, a, point);
            build_binary_op(Mov, rref8(RSP, i, sz_64), reg(RCX, sz_64), ass, a, point);
        }
        data_stack_grow(env, pi_stack_size_of(*syn.ptype));
        break;
    }

    default: {
        panic(mv_string("Invalid abstract term in polymorphic codegen."));
    }
    }

#ifdef DEBUG_ASSERT
    int64_t new_head = get_stack_head(env);
    int64_t diff = old_head - new_head;
    if (diff < 0) panic(mv_string("diff < 0!"));

    size_t stack_sz = is_variable_in(syn.ptype, env) ?
        ADDRESS_SIZE : pi_stack_size_of(*syn.ptype);

    // Justification for cast: stack size of is extremely unlikely to be 2^63 bytes
    //  in size!
    if (diff != (int64_t)stack_sz) {
        String expected = string_u64(stack_sz, a);
        String actual = string_i64(diff, a);
        String message = string_ncat(a, 4,
                                     mv_string("Address environment constraint violated: expected size of the stack is wrong!\nExpected "),
                                     expected, mv_string(" but got "), actual);
        panic(message);
    }
#endif
}

size_t calc_variant_size(PtrArray* types) {
    size_t total = sizeof(uint64_t);
    for (size_t i = 0; i < types->len; i++) {
        size_t field_align;
        Result_t res = pi_maybe_align_of(*(PiType*)types->data[i], &field_align);
        if (res != Ok) return res;
        total = pi_size_align(total, field_align);
        size_t field_size;
        res = pi_maybe_size_of(*(PiType*)types->data[i], &field_size);
        if (res != Ok) return res;
        total += field_size;
    }
    return total;
}

size_t calc_variant_stack_size(PtrArray* types) {
    size_t total = sizeof(uint64_t);
    for (size_t i = 0; i < types->len; i++) {
        total += pi_stack_size_of(*(PiType*)types->data[i]);
    }
    return total;
}

// Const_fold: evaluate and place 
void *const_fold(Syntax *syn, AddressEnv *env, Target target, InternalLinkData* links, Allocator *a, ErrorPoint *point) {
    Allocator exalloc = mk_executable_allocator(a);

    // Catch error here; so can cleanup after self before further unwinding.
    ErrorPoint cleanup_point;
    if (catch_error(cleanup_point)) goto on_error;

    // As we will The 
    Target gen_target = {
        .target = mk_assembler(current_cpu_feature_flags(), &exalloc),
        .code_aux = target.code_aux,
        .data_aux = target.data_aux,
    };

    generate_i(*syn, env, gen_target, links, a, point);
    
    // The data chunk may be moved around during code-generation via 'realloc'
    // if it needs to grow. Thus, we backlink data here, to be safe.
    // TODO (INVESTIGATE BUG): check if also backlinking code makes sense?
    for (size_t i = 0; i < links->links.ed_links.len; i++) {
        LinkMetaData link = links->links.ed_links.data[i];
        void** address_ptr = (void**) ((void*)get_instructions(target.target).data + link.source_offset);
        *address_ptr= target.data_aux->data + link.dest_offset;
    }
    for (size_t i = 0; i < links->links.cd_links.len; i++) {
        LinkMetaData link = links->links.cd_links.data[i];
        void** address_ptr = (void**) ((void*)get_instructions(target.code_aux).data + link.source_offset);
        *address_ptr= target.data_aux->data + link.dest_offset;
    }
    for (size_t i = 0; i < links->links.dd_links.len; i++) {
        LinkMetaData link = links->links.dd_links.data[i];
        void** address_ptr = (void**) ((void*)target.data_aux->data + link.source_offset);
        *address_ptr= target.data_aux->data + link.dest_offset;
    }

    void* result = pico_run_expr(gen_target, pi_size_of(*syn->ptype), a, &cleanup_point);

    delete_assembler(gen_target.target);
    release_executable_allocator(exalloc);
    return result;

 on_error:
    delete_assembler(gen_target.target);
    release_executable_allocator(exalloc);
    throw_error(point, cleanup_point.error_message);
}

