#include "data/num.h"
#include "data/meta/array_impl.h"
#include "platform/signals.h"
#include "platform/machine_info.h"
#include "platform/memory/executable.h"

#include "pretty/string_printer.h"

#include "pico/codegen/codegen.h"
#include "pico/codegen/internal.h"
#include "pico/codegen/polymorphic.h"
#include "pico/codegen/foreign_adapters.h"
#include "pico/binding/address_env.h"
#include "pico/eval/call.h"

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

int compare_link_meta(LinkMetaData lhs, LinkMetaData rhs) {
    long int diff = lhs.source_offset - rhs.source_offset;
    if (diff) return diff;
    return lhs.dest_offset - rhs.dest_offset;
}

ARRAY_CMP_IMPL(LinkMetaData, compare_link_meta, link_meta, LinkMeta)

// Implementation details
void generate(Syntax syn, AddressEnv* env, Target target, InternalLinkData* links, Allocator* a, ErrorPoint* point);

void get_variant_fun(size_t idx, size_t vsize, size_t esize, uint64_t* out, ErrorPoint* point);
size_t calc_variant_stack_size(PtrArray* types);
size_t calc_variant_size(PtrArray* types);
void *const_fold(Syntax *typed, AddressEnv *env, Target target, InternalLinkData* links, Allocator *a, ErrorPoint *point);
void add_rawtree(RawTree tree, Target target, InternalLinkData* links);

LinkData generate_toplevel(TopLevel top, Environment* env, Target target, Allocator* a, ErrorPoint* point) {
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
        AddressEnv* a_env = mk_address_env(env, &top.def.bind, a);
        generate(*top.def.value, a_env, target, &links, a, point);
        delete_address_env(a_env, a);
        break;
    }
    case TLOpen: {
        // Do nothing; open only affects the environment
        break;
    }
    case TLExpr: {
        AddressEnv* a_env = mk_address_env(env, NULL, a);
        generate(*top.expr, a_env, target, &links, a, point);
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

LinkData generate_expr(Syntax* syn, Environment* env, Target target, Allocator* a, ErrorPoint* point) {
    
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
    generate(*syn, a_env, target, &links, a, point);
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

void generate_type_expr(Syntax* syn, TypeEnv* env, Target target, Allocator* a, ErrorPoint* point) {
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
    generate(*syn, a_env, target, &links, a, point);
    delete_address_env(a_env, a);
}

void generate(Syntax syn, AddressEnv* env, Target target, InternalLinkData* links, Allocator* a, ErrorPoint* point) {
#ifdef DEBUG_ASSERT
    int64_t old_head = debug_get_stack_head(env);
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
        build_unary_op(ass, Push, imm32(immediate), a, point);
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
            build_unary_op(ass, Push, imm32(immediate), a, point);
            data_stack_grow(env, pi_stack_size_of(*syn.ptype));
        }
        else if (syn.ptype->prim == Float_64) {
            void* raw = &syn.floating.value;
            int64_t immediate = *(int64_t*)raw;
            build_binary_op(ass, Mov, reg(RAX,sz_64), imm64(immediate), a, point);
            build_unary_op(ass, Push, reg(RAX,sz_64), a, point);
            data_stack_grow(env, pi_stack_size_of(*syn.ptype));
        } else {
            panic(mv_string("Floating literal has non-float type!"));
        }
        break;
    }
    case SLitBool: {
        int8_t immediate = syn.boolean;
        build_unary_op(ass, Push, imm8(immediate), a, point);
        data_stack_grow(env, pi_stack_size_of(*syn.ptype));
        break;
    }
    case SLitUnit: {
        break;
    }
    case SLitString: {
        String immediate = syn.string; 
        if (immediate.memsize > UINT32_MAX) 
            throw_error(point, mv_string("Codegen: String literal length must fit into less than 32 bits"));

        // Push the u8
        AsmResult out = build_binary_op(ass, Mov, reg(RAX, sz_64), imm64(0), a, point);

        // Backlink the data & copy the bytes into the data-segment.
        backlink_data(target, out.backlink, links);
        add_u8_chunk(immediate.bytes, immediate.memsize, target.data_aux);

        build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
        build_unary_op(ass, Push, imm32(immediate.memsize), a, point);

        data_stack_grow(env, pi_stack_size_of(*syn.ptype));
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
            build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(size), a, point);

            if (e.stack_offset + size > INT8_MAX || (e.stack_offset - (int64_t)size) < INT8_MIN) {
                for (size_t i = 0; i < size / 8; i++) {
                    build_binary_op(ass, Mov, reg(RAX, sz_64), rref32(RBP, e.stack_offset + (i * 8) , sz_64), a, point);
                    build_binary_op(ass, Mov, rref32(RSP, (i * 8), sz_64), reg(RAX, sz_64), a, point);
                }
            } else {
                for (size_t i = 0; i < size / 8; i++) {
                    build_binary_op(ass, Mov, reg(RAX, sz_64), rref8(RBP, e.stack_offset + (i * 8) , sz_64), a, point);
                    build_binary_op(ass, Mov, rref8(RSP, (i * 8), sz_64), reg(RAX, sz_64), a, point);
                }
            }
            break;
        }
        case ALocalIndirect:
            panic(mv_string("Monomorphic code does not expect address entries of type 'local indirect'"));
            break;
        case ALocalIndexed:
            panic(mv_string("Monomorphic code does not expect address entries of type 'local indexed'"));
            break;
        case AGlobal: {
            PiType indistinct_type = *strip_type(syn.ptype);

            // Procedures (inc. polymorphic procedures), Types and types are passed by reference (i.e. they are addresses). 
            // Dynamic Vars and instances passed by value, but are guaranteed to take up 64 bits.
            if (indistinct_type.sort == TProc || indistinct_type.sort == TAll || indistinct_type.sort == TKind
                || indistinct_type.sort == TDynamic || indistinct_type.sort == TTraitInstance) {
                AsmResult out = build_binary_op(ass, Mov, reg(R9, sz_64), imm64(*(uint64_t*)e.value), a, point);
                backlink_global(syn.variable, out.backlink, links, a);
                build_unary_op(ass, Push, reg(R9, sz_64), a, point);

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
                    out = build_binary_op(ass, Mov, reg(R9, sz_64), imm64(*(uint8_t*)e.value), a, point);
                } else if (prim_size == 2) {
                    out = build_binary_op(ass, Mov, reg(R9, sz_64), imm64(*(uint16_t*)e.value), a, point);
                } else if (prim_size == 4) {
                    out = build_binary_op(ass, Mov, reg(R9, sz_64), imm64(*(uint32_t*)e.value), a, point);
                } else if (prim_size == 8) {
                    out = build_binary_op(ass, Mov, reg(R9, sz_64), imm64(*(uint64_t*)e.value), a, point);
                } else {
                    panic(mv_string("Codegen expects globals bound to primitives to have size 1, 2, 4 or 8."));
                }
                backlink_global(syn.variable, out.backlink, links, a);
                build_unary_op(ass, Push, reg(R9, sz_64), a, point);

            // Structs and Enums are passed by value, and have variable size.
            } else if (indistinct_type.sort == TStruct || indistinct_type.sort == TEnum) {
                size_t value_size = pi_size_of(indistinct_type);
                size_t stack_size = pi_stack_align(value_size);
                AsmResult out = build_binary_op(ass, Mov, reg(RCX, sz_64), imm64((uint64_t)e.value), a, point);
                backlink_global(syn.variable, out.backlink, links, a);

                // Allocate space on the stack for composite type (struct/enum)
                build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(stack_size), a, point);

                generate_monomorphic_copy(RSP, RCX, value_size, ass, a, point);
            } else {
                throw_error(point,
                            string_ncat(a, 3,
                                        mv_string("Codegen: Global var '"),
                                        *symbol_to_string(syn.variable),
                                        mv_string("' has unsupported sort")));
            }
            break;
        }
        case ATypeVar:
            gen_mk_type_var(syn.variable, ass, a, point);
            break;
        case ANotFound: {
            String* sym = symbol_to_string(syn.variable);
            String msg = mv_string("Couldn't find variable during codegen: ");
            throw_error(point, string_cat(msg, *sym, a));
            break;
        }
        case ATooManyLocals: {
            throw_error(point, mk_string("Too Many Local variables!", a));
            break;
        }
        }
        data_stack_grow(env, pi_stack_size_of(*syn.ptype));
        break;
    }
    case SProcedure: {
        // Get the curret address 
        void* proc_address = get_instructions(target.code_aux).data;
        proc_address += get_instructions(target.code_aux).len;

        // Generate procedure value (push the address onto the stack)
        AsmResult out = build_binary_op(ass, Mov, reg(RAX, sz_64), imm64((uint64_t)proc_address), a, point);
        backlink_code(target, out.backlink, links);
        build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
        data_stack_grow(env, ADDRESS_SIZE);

        // Now, change the target and the assembler, such that code is now
        // generated in the 'code segment'. Then, generate the function body
        ass = target.code_aux;
        target.target = target.code_aux;

        // Codegen function setup
        build_unary_op(ass, Push, reg(R14, sz_64), a, point);
        build_unary_op(ass, Push, reg(RBP, sz_64), a, point);
        build_binary_op(ass, Mov, reg(RBP, sz_64), reg(RSP, sz_64), a, point);

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
        generate(*syn.procedure.body, env, target, links, a, point);
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
        build_binary_op(ass, Mov, reg(R12, sz_64),  rref8(RBP, 16, sz_64), a, point);
        build_binary_op(ass, Mov, reg(R14, sz_64), rref8(RBP, 8, sz_64), a, point);
        build_binary_op(ass, Mov, reg(RBP, sz_64), rref8(RBP, 0, sz_64), a, point);

        generate_stack_move(args_size + 3 * ADDRESS_SIZE, 0, ret_size, ass, a, point);

        // Pop args
        build_binary_op(ass, Add, reg(RSP, sz_64), imm32(args_size + 3 * ADDRESS_SIZE), a, point);

        // push return address 
        build_unary_op(ass, Push, reg(R12, sz_64), a, point);
        build_nullary_op(ass, Ret, a, point);
        break;
    }
    case SAll: {
        void* all_address = get_instructions(target.code_aux).data;
        all_address += get_instructions(target.code_aux).len;

        // Generate procedure value (push the address onto the stack)
        AsmResult out = build_binary_op(ass, Mov, reg(RAX, sz_64), imm64((uint64_t)all_address), a, point);
        backlink_code(target, out.backlink, links);
        build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
        data_stack_grow(env, ADDRESS_SIZE);

        // Now, change the target and the assembler, such that code is now
        // generated in the 'code segment'. Then, generate the function body
        ass = target.code_aux;
        target.target = target.code_aux;

        generate_polymorphic(syn.all.args, *syn.all.body, env, target, links, a, point);
        break;
    }
    case SMacro: {
        generate(*syn.transformer, env, target, links, a, point);
        break;
    }
    case SApplication: {
        // Monomorphic Codegen
        if (syn.application.function->ptype->sort == TProc) {
            size_t args_size = 0;
            for (size_t i = 0; i < syn.application.implicits.len; i++) {
                Syntax* arg = (Syntax*) syn.application.implicits.data[i];
                args_size += pi_stack_size_of(*arg->ptype);
                generate(*arg, env, target, links, a, point);
            }
            for (size_t i = 0; i < syn.application.args.len; i++) {
                Syntax* arg = (Syntax*) syn.application.args.data[i];
                args_size += pi_stack_size_of(*arg->ptype);
                generate(*arg, env, target, links, a, point);
            }

            // This will push a function pointer onto the stack
            generate(*syn.application.function, env, target, links, a, point);
        
            // Regular Function Call
            // Pop the function into RCX; call the function
            build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);
            build_unary_op(ass, Call, reg(RCX, sz_64), a, point);
            // Update for popping all values off the stack (also the function itself)
            data_stack_shrink(env, args_size + ADDRESS_SIZE);

            // Update as pushed the final value onto the stac
            data_stack_grow(env, pi_stack_size_of(*syn.ptype));

        // Is a type family 
        } else {
            size_t args_size = 0;
            for (size_t i = 0; i < syn.application.args.len; i++) {
                Syntax* arg = (Syntax*) syn.application.args.data[i];
                args_size += pi_stack_size_of(*arg->ptype);
                generate(*arg, env, target, links, a, point);
            }

            // push the type onto the stack:
            generate(*syn.application.function, env, target, links, a, point);

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
        // The polymorphic codegen is different, so we therefore must also
        // call polymorphic functions differently!
        // Step1: reserve space for types. 
        // Recall the setup
        // > Types
        // > Argument Offsets
        // > Old RBP
        // > 64-bit space
        // > arguments (in order)

        for (size_t i = 0; i < syn.all_application.types.len; i++) {
            // The 'type' looks as follows:
            // | 16 bits | 16 bits     | 32 bits |
            // | align   | stack align | size    |
            size_t align = pi_align_of(*((Syntax*)syn.all_application.types.data[i])->type_val);
            size_t size = pi_size_of(*((Syntax*)syn.all_application.types.data[i])->type_val);
            size_t stack_sz = pi_stack_align(size);
            // TODO BUG LOGIC Check that stack_sz < max_uint_28
            uint64_t result = (align << 56) | (size << 28) | stack_sz;


            build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(ADDRESS_SIZE), a, point);
            build_binary_op(ass, Mov, reg(RAX, sz_64), imm64(result), a, point);
            build_binary_op(ass, Mov, rref8(RSP, 0, sz_64), reg(RAX, sz_64), a, point);
        }

        // Calculation of offsets:
        // • Remaining offset starts @ sum of ADDRESS_SIZE * 2
        int32_t offset = 0;
        
        for (size_t i = 0; i < syn.all_application.implicits.len; i++) {
            offset += pi_stack_size_of(*((Syntax*)syn.all_application.implicits.data[i])->ptype);
            build_unary_op(ass, Push, imm32(-offset), a, point);
        }
        for (size_t i = 0; i < syn.all_application.args.len; i++) {
            offset += pi_stack_size_of(*((Syntax*)syn.all_application.args.data[i])->ptype);
            build_unary_op(ass, Push, imm32(-offset), a, point);
        }

        // Reserve for return address
        build_binary_op(ass, Sub, reg(RSP, sz_64), imm8(ADDRESS_SIZE), a, point);
        // Store "Old" RBP (RBP to restore)
        build_unary_op(ass, Push, reg(RBP, sz_64), a, point);

        data_stack_grow(env, ADDRESS_SIZE * (syn.all_application.types.len
                                                + syn.all_application.implicits.len
                                                + syn.all_application.args.len + 2));

        size_t args_size = 0;
        for (size_t i = 0; i < syn.all_application.implicits.len; i++) {
            Syntax* arg = (Syntax*) syn.all_application.implicits.data[i];
            args_size += pi_stack_size_of(*arg->ptype);
            generate(*arg, env, target, links, a, point);
        }
        for (size_t i = 0; i < syn.all_application.args.len; i++) {
            Syntax* arg = (Syntax*) syn.all_application.args.data[i];
            args_size += pi_stack_size_of(*arg->ptype);
            generate(*arg, env, target, links, a, point);
        }

        // This will push a function pointer onto the stack
        generate(*syn.application.function, env, target, links, a, point);

        // Now, calculate what RBP should be 
        // RBP = RSP - (args_size + ADDRESS_SIZE) ;; (ADDRESS_SIZE accounts for function ptr)
        //     = RSP - offset
        build_binary_op(ass, Mov, reg(RBP, sz_64), reg(RSP, sz_64), a, point);
        build_binary_op(ass, Add, reg(RBP, sz_64), imm32(offset + ADDRESS_SIZE), a, point);
        
        // Regular Function Call
        // Pop the function into RCX; call the function
        build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);
        build_unary_op(ass, Call, reg(RCX, sz_64), a, point);

        // Update as popped args from stack
        data_stack_shrink(env, args_size + ADDRESS_SIZE);
        data_stack_shrink(env, ADDRESS_SIZE * (syn.all_application.types.len
                                                + syn.all_application.implicits.len
                                                + syn.all_application.args.len + 2));
        // Update as pushed the final value onto the stack
        data_stack_grow(env, pi_stack_size_of(*syn.ptype));
        break;
            
    }
    case SConstructor: {
        PiType* enum_type = strip_type(syn.ptype);
        size_t enum_size = pi_stack_size_of(*enum_type);
        size_t variant_size = calc_variant_size(enum_type->enumeration.variants.data[syn.variant.tag].val);

        build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(enum_size - variant_size), a, point);
        build_unary_op(ass, Push, imm32(syn.constructor.tag), a, point);

        data_stack_grow(env, enum_size);
        break;
    }
    case SVariant: {
        // TODO (FEAT BUG): ensure this will correctly handle non-stack aligned
        // enum tags, members and overall enums gracefully.
        const size_t tag_size = sizeof(uint64_t);
        PiType* enum_type = strip_type(syn.ptype);
        size_t enum_size = pi_size_of(*enum_type);
        size_t variant_size = calc_variant_size(enum_type->enumeration.variants.data[syn.variant.tag].val);
        size_t variant_stack_size = calc_variant_stack_size(enum_type->enumeration.variants.data[syn.variant.tag].val);

        // Make space to fit the (final) variant
        build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(enum_size), a, point);
        data_stack_grow(env, enum_size);

        // Set the tag
        build_binary_op(ass, Mov, rref8(RSP, 0, sz_64), imm32(syn.constructor.tag), a, point);

        // Generate each argument
        for (size_t i = 0; i < syn.variant.args.len; i++) {
            generate(*(Syntax*)syn.variant.args.data[i], env, target, links, a, point);
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
        build_binary_op(ass, Add, reg(RSP, sz_64), imm32(src_stack_offset), a, point);

        // Grow the stack to account for the difference in enum & variant sizes
        data_stack_shrink(env, variant_stack_size - tag_size);
        break;
    }
    case SMatch: {
        // TODO: check that the match handles stack alignment correctly
        // Generate code for the value
        Syntax* match_value = syn.match.val;
        PiType* enum_type = strip_type(syn.match.val->ptype);
        size_t enum_stack_size = pi_stack_size_of(*match_value->ptype);
        size_t out_size = pi_stack_size_of(*syn.ptype);

        generate(*match_value, env, target, links, a, point);

        SizeArray back_positions = mk_size_array(syn.match.clauses.len, a);
        PtrArray back_refs = mk_ptr_array(syn.match.clauses.len, a);

        // For each pattern match, generate two things 
        // 1. A comparison/check that the pattern has been matched
        // 2. A jump to the relevant location
        for (size_t i = 0; i < syn.match.clauses.len; i++) {
            SynClause clause = *(SynClause*)syn.match.clauses.data[i];
            build_binary_op(ass, Cmp, rref8(RSP, 0, sz_64), imm32(clause.tag), a, point);
            AsmResult out = build_unary_op(ass, JE, imm32(0), a, point);
            push_size(get_pos(ass), &back_positions);
            push_ptr(get_instructions(ass).data + out.backlink, &back_refs);
        }

        // The 'body positions' and 'body_refs' store the inidices we need to
        // use to calculate jumps (and the bytes we need to update with those jumps)
        SizeArray body_positions = mk_size_array(syn.match.clauses.len, a);
        PtrArray body_refs = mk_ptr_array(syn.match.clauses.len, a);

        for (size_t i = 0; i < syn.match.clauses.len; i++) {
            // 1. Backpatch the jump so that it jumps to here
            size_t branch_pos = back_positions.data[i];
            uint8_t* branch_ref = back_refs.data[i];

            // calc backlink offset
            size_t body_pos = get_pos(ass);
            if (body_pos - branch_pos > INT32_MAX) {
                throw_error(point, mk_string("Jump in match too large", a));
            } 

            *branch_ref = (int32_t)(body_pos - branch_pos);

            SynClause clause = *(SynClause*)syn.match.clauses.data[i];
            PtrArray variant_types = *(PtrArray*)enum_type->enumeration.variants.data[clause.tag].val; 

            // Bind Clause Vars 
            SymSizeAssoc arg_sizes = mk_sym_size_assoc(variant_types.len, a);
            for (size_t i = 0; i < variant_types.len; i++) {
                sym_size_bind(clause.vars.data[i]
                              , pi_size_of(*(PiType*)variant_types.data[i])
                              , &arg_sizes);
            }
            address_bind_enum_vars(arg_sizes, env);

            generate(*clause.body, env, target, links, a, point);

            // Generate jump to end of match expression to be backlinked later
            AsmResult out = build_unary_op(ass, JMP, imm32(0), a, point);
            push_size(get_pos(ass), &body_positions);
            push_ptr(get_instructions(ass).data + out.backlink, &body_refs);

            address_unbind_enum_vars(env);
            data_stack_shrink(env, out_size);
        }

        // Finally, backlink all jumps from the bodies to the end.
        size_t curr_pos = get_pos(ass);
        for (size_t i = 0; i < body_positions.len; i++) {
            size_t body_pos = body_positions.data[i];
            int32_t* body_ref = body_refs.data[i];

            if (curr_pos - body_pos > INT32_MAX) {
                throw_error(point, mk_string("Jump in match too large", a));
            } 

            set_unaligned_i32(body_ref, (int32_t)(curr_pos - body_pos));
        }

        generate_stack_move(enum_stack_size, 0, out_size, ass, a, point);

        build_binary_op(ass, Add, reg(RSP, sz_64), imm32(enum_stack_size), a, point);

        data_stack_shrink(env, enum_stack_size);
        data_stack_grow(env, out_size);
        break;
    }
    case SStructure: {
        // For structures, we have to be careful - this is because the order in
        // which arguments are evaluated is not necessarily the order in which
        // arguments are inserted into the structure.
        PiType* struct_type = strip_type(syn.ptype);

        // Step 1: Make room on the stack for our struct
        size_t struct_size = pi_stack_size_of(*struct_type);
        build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(struct_size), a, point);
        data_stack_grow(env, struct_size);

        // Step 2: evaluate each element/variable binding
        for (size_t i = 0; i < syn.structure.fields.len; i++) {
            generate(*(Syntax*)syn.structure.fields.data[i].val, env, target, links, a, point);
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
        for (size_t i = 0; i < struct_type->structure.fields.len; i++) {
            source_region_size += pi_stack_size_of(*((Syntax*)syn.structure.fields.data[i].val)->ptype); 
        }
        size_t dest_offset = 0;
        for (size_t i = 0; i < struct_type->structure.fields.len; i++) {
            dest_offset = pi_size_align(dest_offset, pi_align_of(*(PiType*)struct_type->structure.fields.data[i].val));

            // Find the field in the source & compute offset
            size_t src_offset = 0;
            for (size_t j = 0; j < syn.structure.fields.len; j++) {
                PiType* t = ((Syntax*)syn.structure.fields.data[j].val)->ptype;
                src_offset += pi_stack_size_of(*t); 

                if (symbol_eq(syn.structure.fields.data[j].key, struct_type->structure.fields.data[i].key)) {
                    break; // offset is correct, end the loop
                }
            }

            // We now both the source_offset and dest_offset. These are both
            // relative to the 'bottom' of their respective structures.
            // Therefore, we now need to find their offsets relative to the `top'
            // of the stack.
            size_t src_stack_offset = source_region_size - src_offset;
            size_t dest_stack_offset = source_region_size + dest_offset;
            size_t field_size = pi_size_of(*(PiType*)struct_type->structure.fields.data[i].val);

            // Now, move the data.
            generate_stack_move(dest_stack_offset, src_stack_offset, field_size, ass, a, point);

            // Compute dest_offset for next loop
            dest_offset += pi_size_of(*(PiType*)struct_type->structure.fields.data[i].val);
        }

        // Remove the space occupied by the temporary values 
        build_binary_op(ass, Add, reg(RSP, sz_64), imm32(source_region_size), a, point);
        data_stack_shrink(env, source_region_size);
        break;
    }
    case SProjector: {
        // TODO: check that the projector handles stack alignment correctly
        // First, allocate space on the stack for the value
        PiType* source_type = strip_type(syn.projector.val->ptype);
        size_t out_sz = pi_stack_size_of(*syn.ptype);
        build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(out_sz), a, point);
        data_stack_grow(env, out_sz);

        // Second, generate the structure/instance object
        generate(*syn.projector.val, env, target, links, a, point);
        size_t src_sz = pi_stack_size_of(*source_type);

        // From this point, behaviour depends on whether we are projecting from
        // a structure or from an instance
        if (source_type->sort == TStruct) {
            // Now, copy the structure to the destination
            // for this, we need the struct size + offset of field in the struct
            size_t offset = 0;
            for (size_t i = 0; i < source_type->structure.fields.len; i++) {
                if (symbol_eq(source_type->structure.fields.data[i].key, syn.projector.field))
                    break;
                offset += pi_size_of(*(PiType*)source_type->structure.fields.data[i].val);
            }

            generate_stack_move(src_sz + out_sz - 0x8, offset, out_sz, ass, a, point);
            // Now, remove the original struct from the stack
            build_binary_op(ass, Add, reg(RSP, sz_64), imm32(src_sz), a, point);

            data_stack_shrink(env, src_sz);
        } else {
            // Pop the pointer to the instance from the stack - store in RSI
            data_stack_shrink(env, src_sz);
            build_unary_op(ass, Pop, reg(RSI, sz_64), a, point);

            // Now, calculate offset for field 
            size_t offset = 0;
            for (size_t i = 0; i < source_type->instance.fields.len; i++) {
                offset = pi_size_align(offset, pi_align_of(*(PiType*)source_type->instance.fields.data[i].val));
                if (symbol_eq(source_type->instance.fields.data[i].key, syn.projector.field))
                    break;
                offset += pi_size_of(*(PiType*)source_type->instance.fields.data[i].val);
            }
            build_binary_op(ass, Add, reg(RSI, sz_64), imm32(offset), a, point);

            // TODO (check if replace with stack copy)
            generate_monomorphic_copy(RSP, RSI, out_sz, ass, a, point);
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
        build_binary_op(ass, Mov, reg(RSI, sz_64), imm32(immediate_sz), a, point);
        generate_tmp_malloc(reg(RAX, sz_64), reg(RSI, sz_64), ass, a, point);
        build_binary_op(ass, Mov, reg(RCX, sz_64), reg(RAX, sz_64), a, point);

        // Grow by address size to account for the fact that the for loop
        // keeps a stack of the address, which is updated each iteration.
        data_stack_grow(env, ADDRESS_SIZE);
        build_unary_op(ass, Push, reg(RCX, sz_64), a, point);

        // TODO (BUG): generate code that doesn't assume fields are in same order   

        // Alignment
        size_t index_offset = 0;
        for (size_t i = 0; i < syn.ptype->instance.fields.len; i++) {
            // Generate field
            Syntax* val = syn.instance.fields.data[i].val;
            generate(*val, env, target, links, a, point);

            // The offset tells us how far up the stack we look to find the instance ptr
            size_t offset = pi_stack_size_of(*val->ptype);

            // Retrieve index (ptr) 
            // TODO (BUG): Check offset is < int8_t max.
            build_binary_op(ass, Mov, reg(RCX, sz_64), rref8(RSP, pi_stack_align(offset), sz_64), a, point);

            // Align RCX
            size_t aligned_offset = pi_size_align(index_offset, pi_align_of(*val->ptype));
            if (index_offset != aligned_offset) {
                size_t align = aligned_offset - index_offset;
                build_binary_op(ass, Add, reg(RCX, sz_64), imm32(align), a, point);
            }

            // TODO (check if replace with stack copy)
            generate_monomorphic_copy(RCX, RSP, offset, ass, a, point);

            // We need to increment the current field index to be able ot access
            // the next
            build_binary_op(ass, Add, reg(RCX, sz_64), imm32(offset), a, point);
            index_offset += offset;

            // Pop value from stack
            build_binary_op(ass, Add, reg(RSP, sz_64), imm32(pi_stack_align(offset)), a, point);
            data_stack_shrink(env, offset);

            // Override index with new value
            build_binary_op(ass, Mov, rref8(RSP, 0, sz_64), reg(RCX, sz_64), a, point);
        }

        build_binary_op(ass, Mov, reg(RCX, sz_64), rref8(RSP, 0, sz_64), a, point);
        build_binary_op(ass, Sub, reg(RCX, sz_64), imm32(immediate_sz), a, point);
        build_binary_op(ass, Mov, rref8(RSP, 0, sz_64), reg(RCX, sz_64), a, point);

        // Note: we don't shrink as the final address (on stack) is accounted
        // for by the 'grow' prior to the above for-loop
        break;
    }
    case SDynamic: {
        // TODO: check that the dynamic handles stack alignment correctly
        // Create a new dynamic variable, i.e. call the C function 
        // mk_dynamic_var(size_t size, void* default_val)
        generate(*syn.dynamic, env, target, links, a, point);

        // currently RSP = default_val
        size_t val_size = pi_size_of(*syn.ptype);

#if ABI == SYSTEM_V_64 
        // arg1 = rdi, arg2 = rsi
        build_binary_op(ass, Mov, reg(RDI, sz_64), imm32(val_size), a, point);
        build_binary_op(ass, Mov, reg(RSI, sz_64), reg(RSP, sz_64), a, point);
#elif ABI == WIN_64 
        // arg1 = rcx, arg2 = rdx
        build_binary_op(ass, Mov, reg(RCX, sz_64), imm32(val_size), a, point);
        build_binary_op(ass, Mov, reg(RDX, sz_64), reg(RSP, sz_64), a, point);
#else
#error "unknown ABI"
#endif

        // call function
        generate_c_call(mk_dynamic_var, ass, a, point);

        // Pop value off stack
        build_binary_op(ass, Add, reg(RSP, sz_64), imm32(val_size), a, point);
        build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
        
        data_stack_shrink(env, ADDRESS_SIZE);
        data_stack_grow(env, val_size);
        break;
    }
    case SDynamicUse: {
        // TODO: check that the dynamic use handles stack alignment correctly
        generate(*syn.use, env, target, links, a, point);

        // We now have a dynamic variable: get its' value as ptr

#if ABI == SYSTEM_V_64 
        // arg1 = rdi
        build_unary_op(ass, Pop, reg(RDI, sz_64), a, point);
#elif ABI == WIN_64 
        // arg1 = rcx
        build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);
#else
#error "unknown ABI"
#endif

        // call function
        generate_c_call(get_dynamic_val, ass, a, point);

        // Now, allocate space on stack
        size_t val_size = pi_size_of(*syn.ptype);
        build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(val_size), a, point);
        build_binary_op(ass, Mov, reg(RCX, sz_64), reg(RAX, sz_64), a, point);

        // TODO (check if replace with stack copy)
        generate_monomorphic_copy(RSP, RCX, val_size, ass, a, point);
        
        data_stack_shrink(env, ADDRESS_SIZE);
        data_stack_grow(env, val_size);
        break;
    }
    case SDynamicLet: {
        // TODO: check that the dynamic let handles stack alignment correctly

        // Step 1: evaluate all dynamic bindings & bind them
        for (size_t i = 0; i < syn.dyn_let_expr.bindings.len; i++) {
            DynBinding* dbind = syn.dyn_let_expr.bindings.data[i];
            size_t bind_size = pi_size_of(*dbind->expr->ptype);
            generate(*dbind->var, env, target, links, a, point);
            generate(*dbind->expr, env, target, links, a, point);

            // Copy the value into the dynamic var
            // Currently, the stack looks like:
            // + dynamic-var-index
            //   new-value
            // R15 is the dynamic memory register, move it into RCX
            // Move the index into RAX
            build_binary_op(ass, Mov, reg(RCX, sz_64), reg(R15, sz_64), a, point);
            build_binary_op(ass, Mov, reg(RAX, sz_64), rref8(RSP, bind_size, sz_64), a, point);
            // RCX holds the array - we need to index it with index located in RAX
            build_binary_op(ass, Mov, reg(RCX, sz_64), sib(RCX, RAX, 8, sz_64), a, point);
            // Now we have a pointer to the value stored in RCX, swap it with
            // the value on the stack

            // TODO (check if replace with stack copy)
            generate_monomorphic_swap(RCX, RSP, bind_size, ass, a, point);
        }

        // Step 2: generate the body
        generate(*syn.let_expr.body, env, target, links, a, point);
        size_t val_size = pi_size_of(*syn.dyn_let_expr.body->ptype);

        // Step 3: unwind the bindings
        //  • Store the (address of the) current dynamic value index to restore in RDX
        build_binary_op(ass, Mov, reg(RDX, sz_64), reg(RSP, sz_64), a, point);
        build_binary_op(ass, Add, reg(RDX, sz_64), imm32(val_size), a, point);

        size_t offset_size = 0;
        for (size_t i = 0; i < syn.let_expr.bindings.len; i++) {
            DynBinding* dbind = syn.dyn_let_expr.bindings.data[i];
            size_t bind_size = pi_size_of(*dbind->expr->ptype);

            // Store ptr to dynamic memory (array) in RCX, and the index in RAX
            build_binary_op(ass, Mov, reg(RCX, sz_64), reg(R15, sz_64), a, point);
            build_binary_op(ass, Mov, reg(RAX, sz_64), rref8(RDX, 0, sz_64), a, point);
            // RCX holds the array - we need to index it with index located in RAX
            build_binary_op(ass, Mov, reg(RCX, sz_64), sib(RCX, RAX, 8, sz_64), a, point);

            // Store ptr to local value to restore in RDX 
            // TODO (check if replace with stack copy)
            generate_monomorphic_swap(RCX, RDX, bind_size, ass, a, point);
            build_binary_op(ass, Add, reg(RDX, sz_64), imm32(bind_size + ADDRESS_SIZE), a, point);

            offset_size += bind_size + ADDRESS_SIZE;
        }

        // Step 4: cleanup: pop bindings
        generate_stack_move(offset_size, 0, val_size, ass, a, point);
        build_binary_op(ass, Add, reg(RSP, sz_64), imm32(offset_size), a, point);
        data_stack_shrink(env, offset_size);
        break;
    }
    case SLet: {
        // TODO: check that the let handles stack alignment correctly
        size_t bsize = 0;
        for (size_t i = 0; i < syn.let_expr.bindings.len; i++) {
            Syntax* sy = syn.let_expr.bindings.data[i].val;
            generate(*sy, env, target, links, a, point);
            address_bind_relative(syn.let_expr.bindings.data[i].key, 0, env);
            bsize += pi_size_of(*sy->ptype);
        }
        generate(*syn.let_expr.body, env, target, links, a, point);
        address_pop_n(syn.let_expr.bindings.len, env);

        generate_stack_move(bsize, 0, pi_size_of(*syn.let_expr.body->ptype), ass, a, point);
        build_binary_op(ass, Add, reg(RSP, sz_64), imm32(bsize), a, point);
        data_stack_shrink(env, bsize);
        address_pop_n(syn.let_expr.bindings.len, env);
        break;
    }
    case SIf: {
        // generate the condition
        generate(*syn.if_expr.condition, env, target, links, a, point);

        // Pop the bool into R9; compare with 0
        build_unary_op(ass, Pop, reg(R9, sz_64), a, point);
        build_binary_op(ass, Cmp, reg(R9, sz_64), imm32(0), a, point);
        data_stack_shrink(env, pi_stack_size_of((PiType) {.sort = TPrim, .prim = Bool}));

        // ---------- CONDITIONAL JUMP ----------
        // compare the value to 0
        // jump to false branch if equal to 0 -- the immediate 8 is a placeholder
        AsmResult out = build_unary_op(ass, JE, imm32(0), a, point);
        size_t start_pos = get_pos(ass);

        int32_t* jmp_loc = (int32_t*)(get_instructions(ass).data + out.backlink);

        // ---------- TRUE BRANCH ----------
        // now, generate the code to run (if true)
        generate(*syn.if_expr.true_branch, env, target, links, a, point);
        // Shrink the stack by the result size
        data_stack_shrink(env, pi_stack_size_of(*syn.ptype));

        // Generate jump to end of false branch to be backlinked later
        out = build_unary_op(ass, JMP, imm32(0), a, point);

        // calc backlink offset
        size_t end_pos = get_pos(ass);
        if (end_pos - start_pos > INT32_MAX) {
            throw_error(point, mk_string("Jump in conditional too large", a));
        } 

        // backlink
        set_unaligned_i32(jmp_loc, end_pos - start_pos);
        jmp_loc = (int32_t*)(get_instructions(ass).data + out.backlink);
        start_pos = get_pos(ass);


        // ---------- FALSE BRANCH ----------
        // Generate code for the false branch
        generate(*syn.if_expr.false_branch, env, target, links, a, point);

        // calc backlink offset
        end_pos = get_pos(ass);
        if (end_pos - start_pos > INT32_MAX) {
            throw_error(point, mk_string("Jump in conditional too large", a));
        } 
        set_unaligned_i32(jmp_loc, end_pos - start_pos);
        break;
    }
    case SLabels: {
        // Labels: The code-generation for labels is as follows: 
        // 1. Add labels to environment
        // 2. Generate expression
        // 3. For each expression:
        //  3.1 Mark Beginning of label expression
        //  3.2 Generate label expressions
        // 4. Backlink all labels.
        SymbolArray labels = mk_symbol_array(syn.labels.terms.len, a);
        for (size_t i = 0; i < syn.labels.terms.len; i++) 
            push_symbol(syn.labels.terms.data[i].key, &labels);

        address_start_labels(labels, env);

        generate(*syn.labels.entry, env, target, links, a, point);

        SymSizeAssoc label_points = mk_sym_size_assoc(syn.labels.terms.len, a);
        SymSizeAssoc label_jumps = mk_sym_size_assoc(syn.labels.terms.len, a);

        size_t out_size = pi_size_of(*syn.ptype);
        for (size_t i = 0; i < syn.labels.terms.len; i++) {
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
            generate(*branch->body, env, target, links, a, point);
            address_unbind_label_vars(env);

            LabelEntry lble = label_env_lookup(cell.key, env);
            if (lble.type == Err)
                throw_error(point, mv_string("Label not found during codegen!!"));
            data_stack_shrink(env, lble.stack_offset);

            // Copy the result down the stack
            generate_stack_move(arg_total, 0, out_size, ass, a, point);

            // ensure the stack is at the sam epoint for the next iteration! 
            data_stack_shrink(env,out_size);

            AsmResult out = build_unary_op(ass, JMP, imm32(0), a, point);
            sym_size_bind(cell.key, out.backlink, &label_jumps);
        }

        // The final iteration will have shrunk the stack "too much", so add the
        // result back in.
        data_stack_grow(env,out_size);

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
                
                int8_t* loc_byte = (int8_t*) get_instructions(ass).data + backlink;
                int32_t* loc = (int32_t*)loc_byte;
                set_unaligned_i32(loc, (int32_t)amt);
            }


            // Step 2: fill out each jump to this label.
            SizeArray* arr = sym_sarr_lookup(sym, links->gotolinks);
            if (!arr) panic(mv_string("Can't find size array when backlinking label!"));

            for (size_t i = 0; i < arr->len; i++) {
                size_t backlink = arr->data[i];
                size_t origin = backlink + 4; // the + 4 accounts for the 4-byte immediate

                int64_t amt = dest - origin;
                if (amt < INT32_MIN || amt > INT32_MAX) panic(mv_string("Label jump too large!"));
                
                int8_t* loc_byte = (int8_t*) get_instructions(ass).data + backlink;
                int32_t* loc = (int32_t*)loc_byte;
                set_unaligned_i32(loc, (int32_t)amt);
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
            generate(*expr, env, target, links, a, point);
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
                build_binary_op(ass, Add, reg(RSP, sz_64), imm32(delta), a, point);
            }

            // Stack sould "pretend" it pushed a value of type syn.ptype and
            // consumed all args. This is so that, e.g. if this go-to is inside
            // a seq or if, the other branches of the if or the rest of the seq
            // generates assuming the correct stack offset.
            data_stack_shrink(env, arg_total);
            data_stack_grow(env, pi_size_of(*syn.ptype));

            AsmResult out = build_unary_op(ass, JMP, imm32(0), a, point);

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
        AsmResult out = build_binary_op(ass, LEA, reg(RAX, sz_64), rref32(RIP, 0, sz_64), a, point); // TODO: backlink N
        size_t reset_backlink = out.backlink;
        size_t reset_instr_end = get_pos(ass);

        build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
        build_unary_op(ass, Push, reg(RBP, sz_64), a, point);

        // Step 2.
        //-------
        build_unary_op(ass, Push, reg(RSP, sz_64), a, point);
        data_stack_grow(env, 3*ADDRESS_SIZE);
        address_bind_relative(syn.with_reset.point_sym, 0, env);

        // Step 3.
        //--------
        generate(*syn.with_reset.expr,env, target, links, a, point);

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
        build_binary_op(ass, Add, reg(RSP, sz_64), imm32(3 * ADDRESS_SIZE), a, point);
        generate_stack_move(3 * ADDRESS_SIZE, 0, val_size, ass, a, point);

        data_stack_shrink(env, 3*ADDRESS_SIZE + val_size);
        address_pop(env);

        // Step 5.
        //--------
        out = build_unary_op(ass, JMP, imm32(0), a, point);
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
        generate(*syn.with_reset.handler, env, target, links, a, point);
        data_stack_shrink(env, ADDRESS_SIZE + in_val_size);
        address_pop_n(2, env);

        // Step 7.
        //--------
        // At this point the stack looks like:
        //     > in-value
        //     > return-addr
        // RSP > out value
        // So do some cleanup
        // - generate_stack_move(ADDRESS_SIZE + val_size, 0, val_size, ass, a, point);
        generate_stack_move(ADDRESS_SIZE + in_val_size, 0, val_size, ass, a, point);
        build_binary_op(ass, Add, reg(RSP, sz_64), imm32(ADDRESS_SIZE + in_val_size), a, point);


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
        generate(*syn.reset_to.arg, env, target, links, a, point);
        generate(*syn.reset_to.point, env, target, links, a, point);
        // there should how be a stack with the following:
        // > arg
        // > Ptr to reset-point

        // Step 1: take the ptr to reset-point
        build_unary_op(ass, Pop, reg(R10, sz_64), a, point);

        // Step 2: stash the current stack ptr in R9 (this will be used to copy the argument )
        build_binary_op(ass, Mov, reg(R9, sz_64), reg(RSP, sz_64), a, point);

        // Step 3: Deref return point ptr to get old RBP, RSP & RIP to jump up 
        build_binary_op(ass, Mov, reg(RSP, sz_64), rref8(R10, -8, sz_64), a, point);
        build_unary_op(ass, Pop, reg(RBP, sz_64), a, point);
        build_unary_op(ass, Pop, reg(RDI, sz_64), a, point); //RDI = jump dest!

        // Step 4: Copy the argument onto the (new) stack.
        PiType* arg_type = syn.reset_to.arg->ptype;
        size_t asize = pi_size_of(*arg_type);
        build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(asize), a, point);
        // TODO (check if replace with stack copy)
        generate_monomorphic_copy(RSP, R9, asize, ass, a, point);

        // Step 5: Long Jump (call) register
        build_unary_op(ass, Call, reg(RDI, sz_64), a, point);

        // Address environment bookkeeping: shrink the stack appropriately
        data_stack_shrink(env, pi_size_of(*syn.reset_to.arg->ptype) + pi_size_of(*syn.reset_to.point->ptype));
        data_stack_grow(env, pi_size_of(*syn.ptype));

        break;
    }
    case SSequence: {
        size_t binding_size = 0;
        size_t num_bindings = 0;
        size_t last_size = 0;
        for (size_t i = 0; i < syn.sequence.elements.len; i++) {
            SeqElt* elt = syn.sequence.elements.data[i];
            generate(*elt->expr, env, target, links, a, point);

            size_t sz = pi_stack_size_of(*elt->expr->ptype);
            if (elt->is_binding) {
                num_bindings++;
                binding_size += i + 1 == syn.sequence.elements.len ? 0 : sz;
                address_bind_relative(elt->symbol, 0, env);
            }
            else if (i + 1 != syn.sequence.elements.len) {
                if (sz != 0) {
                    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(sz), a, point);
                }
                data_stack_shrink(env, sz);
            }

            if (i + 1 == syn.sequence.elements.len) {
                last_size = sz;
            }
            
        }
        if (binding_size != 0) {
            generate_stack_move(binding_size, 0, last_size, ass, a, point);
            build_binary_op(ass, Add, reg(RSP, sz_64), imm32(binding_size), a, point);
            data_stack_shrink(env, binding_size);
            address_pop_n(num_bindings, env);
        }
        break;
    }
    case SIs:
        generate(*syn.is.val, env, target, links, a, point);
        break;
    case SInTo:
        generate(*syn.into.val, env, target, links, a, point);
        break;
    case SOutOf:
        generate(*syn.out_of.val, env, target, links, a, point);
        break;
    case SName:
        generate(*syn.name.val, env, target, links, a, point);
        break;
    case SUnName:
        generate(*syn.unname, env, target, links, a, point);
        break;
    case SWiden:
        // TODO (BUG): appropriately widen (sign-extend/double broaden)
        generate(*syn.widen.val, env, target, links, a, point);
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

            build_binary_op(ass, Mov, reg(RAX, sz_64), rref8(RSP, 0, sz_64), a, point);
            build_binary_op(ass, Mov, reg(RCX, sz), reg(RAX, sz), a, point);
            build_binary_op(ass, Mov, rref8(RSP, 0, sz_64), reg(RCX, sz_64), a, point);
        } else {
            panic(mv_string("can't widen this yet!"));
        }

        break;
    case SNarrow:
        // TODO: if signed -ve => 0??
        // TODO (BUG): appropriately narrow (sign-extend/double broaden)
        generate(*syn.narrow.val, env, target, links, a, point);
        break;
    case SDynAlloc:
        generate(*syn.size, env, target, links, a, point);

        // The size to allocate will sit atop the stack; swap it with 
        // the pointer to free stack memory
        build_unary_op(ass, Pop, reg(RAX, sz_64), a, point);
        build_unary_op(ass, Push, reg(R14, sz_64), a, point);

        // Now, increment pointer to reserve the stack memory
        build_binary_op(ass, Add, reg(R14, sz_64), reg(RAX, sz_64), a, point);

        break;
    case SSizeOf: {
        size_t sz = pi_size_of(*syn.size->type_val);
        build_unary_op(ass, Push, imm32(sz), a, point);
        data_stack_grow(env, pi_stack_align(sizeof(uint32_t)));
        break;
    }
    case SAlignOf: {
        size_t sz = pi_align_of(*syn.size->type_val);
        build_unary_op(ass, Push, imm32(sz), a, point);
        data_stack_grow(env, pi_stack_align(sizeof(uint32_t)));
        break;
    }
    case SOffsetOf: {
        size_t sz = 0;
        PiType* struct_type = strip_type(syn.offset_of.body->type_val);
        for (size_t i = 0; i < struct_type->structure.fields.len; i++) {
            sz = pi_size_align(sz, pi_align_of(*(PiType*)struct_type->structure.fields.data[i].val));
            if (symbol_eq(struct_type->structure.fields.data[i].key, syn.offset_of.field))
                break;
            sz += pi_size_of(*(PiType*)struct_type->structure.fields.data[i].val);
        }
        build_unary_op(ass, Push, imm32(sz), a, point);
        data_stack_grow(env, pi_stack_align(sizeof(uint32_t)));
        break;
    }
    case SCheckedType: {
        build_binary_op(ass, Mov, reg(R9, sz_64), imm64((uint64_t)syn.type_val), a, point);
        build_unary_op(ass, Push, reg(R9, sz_64), a, point);
        data_stack_grow(env, pi_stack_size_of(*syn.ptype));
        break;
    }
    case SProcType:
        // Generate proc type: start by malloc'ing size for args
        generate_tmp_malloc(reg(RAX, sz_64), imm32(syn.proc_type.args.len * ADDRESS_SIZE), ass, a, point);
        build_binary_op(ass, Mov, reg(RCX, sz_64), imm32(0), a, point);

        for (size_t i = 0; i < syn.proc_type.args.len; i++) {
            Syntax* arg = syn.proc_type.args.data[i];

            // Second, generate & move the type (note: stash & pop RCX)
            build_unary_op(ass, Push, reg(RCX, sz_64), a, point);
            build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
            data_stack_grow(env, 2*ADDRESS_SIZE);
            generate(*arg, env, target, links, a, point);

            data_stack_shrink(env, 3*ADDRESS_SIZE);
            build_unary_op(ass, Pop, reg(R9, sz_64), a, point);
            build_unary_op(ass, Pop, reg(RAX, sz_64), a, point);
            build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);

            build_binary_op(ass, Mov, sib(RAX, RCX, 8, sz_64), reg(R9, sz_64), a, point);

            // Now, incremenet index by 1
            build_binary_op(ass, Add, reg(RCX, sz_64), imm32(1), a, point);
        }
        // Stash RAX
        build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
        data_stack_grow(env, ADDRESS_SIZE);
        generate(*syn.proc_type.return_type, env, target, links, a, point);
        data_stack_shrink(env, 2*ADDRESS_SIZE);
        build_unary_op(ass, Pop, reg(R9, sz_64), a, point);
        build_unary_op(ass, Pop, reg(RAX, sz_64), a, point);

        // Finally, generate function call to make type
        gen_mk_proc_ty(reg(RAX, sz_64), imm32(syn.proc_type.args.len), reg(RAX, sz_64), reg(R9, sz_64), ass, a, point);
        build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
        data_stack_grow(env, ADDRESS_SIZE);

        break;
    case SStructType:
        // Generate struct type: for each element of the struct type
        // First, malloc enough data for the array:
        generate_tmp_malloc(reg(RAX, sz_64), imm32(syn.struct_type.fields.len * 3 * ADDRESS_SIZE), ass, a, point);
        build_binary_op(ass, Mov, reg(RCX, sz_64), imm32(0), a, point);

        for (size_t i = 0; i < syn.struct_type.fields.len; i++) {
            SymPtrCell field = syn.struct_type.fields.data[i];
            // First, move the field name
            build_binary_op(ass, Mov, sib8(RAX, RCX, 8, 0, sz_64), imm32(field.key.name), a, point);
            build_binary_op(ass, Mov, sib8(RAX, RCX, 8, 8, sz_64), imm32(field.key.did), a, point);

            // Second, generate & move the type (note: stash & pop RCX)
            build_unary_op(ass, Push, reg(RCX, sz_64), a, point);
            build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
            data_stack_grow(env, 2*ADDRESS_SIZE);
            generate(*(Syntax*)field.val, env, target, links, a, point);

            data_stack_shrink(env, 3*ADDRESS_SIZE);
            build_unary_op(ass, Pop, reg(R9, sz_64), a, point);
            build_unary_op(ass, Pop, reg(RAX, sz_64), a, point);
            build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);

            build_binary_op(ass, Mov, sib8(RAX, RCX, 8, 16, sz_64), reg(R9, sz_64), a, point);

            // Now, incremenet index by 3 (to account for struct size!)
            build_binary_op(ass, Add, reg(RCX, sz_64), imm32(3), a, point);
        }

        // Finally, generate function call to make type
        gen_mk_struct_ty(reg(RAX, sz_64), imm32(syn.struct_type.fields.len), reg(RAX, sz_64), ass, a, point);
        build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
        data_stack_grow(env, ADDRESS_SIZE);
        break;
    case SEnumType:
        // Generate enum type: malloc array for enum
        // First, malloc enough data for the array:
        generate_tmp_malloc(reg(RAX, sz_64), imm32(syn.enum_type.variants.len * 3 * ADDRESS_SIZE), ass, a, point);
        build_binary_op(ass, Mov, reg(RCX, sz_64), imm32(0), a, point);

        for (size_t i = 0; i < syn.enum_type.variants.len; i++) {
            SymPtrCell field = syn.enum_type.variants.data[i];
            // First, move the field name
            build_binary_op(ass, Mov, sib8(RAX, RCX, 8, 0, sz_64), imm32(field.key.name), a, point);
            build_binary_op(ass, Mov, sib8(RAX, RCX, 8, 8, sz_64), imm32(field.key.did), a, point);

            // Second, generate & variant (note: stash & pop RCX)
            build_unary_op(ass, Push, reg(RCX, sz_64), a, point);
            build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
            data_stack_grow(env, 2*ADDRESS_SIZE);

            PtrArray variant = *(PtrArray*)field.val;
            generate_tmp_malloc(reg(RAX, sz_64), imm32(variant.len * ADDRESS_SIZE), ass, a, point);
            build_binary_op(ass, Mov, reg(RCX, sz_64), imm32(0), a, point);

            for (size_t i = 0; i < variant.len; i++) {

                build_unary_op(ass, Push, reg(RCX, sz_64), a, point);
                build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
                data_stack_grow(env, 2*ADDRESS_SIZE);

                generate(*(Syntax*)variant.data[i], env, target, links, a, point);

                data_stack_shrink(env, 3*ADDRESS_SIZE);
                build_unary_op(ass, Pop, reg(R9, sz_64), a, point);
                build_unary_op(ass, Pop, reg(RAX, sz_64), a, point);
                build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);

                build_binary_op(ass, Mov, sib(RAX, RCX, 8, sz_64), reg(R9, sz_64), a, point);

                // Now, incremenet index
                build_binary_op(ass, Add, reg(RCX, sz_64), imm32(1), a, point);
            }

            // The variant was just stored in RAX, move it to R9
            build_binary_op(ass, Mov, reg(R9, sz_64), reg(RAX, sz_64), a, point);

            build_unary_op(ass, Pop, reg(RAX, sz_64), a, point);
            build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);
            data_stack_shrink(env, 2*ADDRESS_SIZE);

            build_binary_op(ass, Mov, sib8(RAX, RCX, 8, 16, sz_64), reg(R9, sz_64), a, point);

            // Now, incremenet index by 3 (to account for ptr + symbol)
            build_binary_op(ass, Add, reg(RCX, sz_64), imm32(3), a, point);
        }

        // Finally, generate function call to make type
        gen_mk_enum_ty(reg(RAX, sz_64), syn.enum_type, reg(RAX, sz_64), ass, a, point);
        build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
        data_stack_grow(env, ADDRESS_SIZE);
        break;
    case SResetType:
        generate(*(Syntax*)syn.reset_type.in, env, target, links, a, point);
        generate(*(Syntax*)syn.reset_type.out, env, target, links, a, point);
        gen_mk_reset_ty(ass, a, point);
        data_stack_shrink(env, ADDRESS_SIZE);
        break;
    case SDynamicType:
        generate(*(Syntax*)syn.dynamic_type, env, target, links, a, point);
        gen_mk_dynamic_ty(ass, a, point);
        break;
    case SAllType:
        // Forall type structure: (array symbol) body
        for (size_t i = 0; i < syn.bind_type.bindings.len; i++) {
            address_bind_type(syn.bind_type.bindings.data[i], env);
        }
        generate(*(Syntax*)syn.bind_type.body, env, target, links, a, point);
        gen_mk_forall_ty(syn.bind_type.bindings, ass, a, point);
        address_pop_n(syn.bind_type.bindings.len, env);
        break;
    case SExistsType:
        panic(mv_string("Monomorphic codegen does not support exists type!"));
        break;
    case STypeFamily:
        // Family type structure: (array symbol) body
        for (size_t i = 0; i < syn.bind_type.bindings.len; i++) {
            address_bind_type(syn.bind_type.bindings.data[i], env);
        }
        generate(*(Syntax*)syn.bind_type.body, env, target, links, a, point);
        gen_mk_fam_ty(syn.bind_type.bindings, ass, a, point);
        address_pop_n(syn.bind_type.bindings.len, env);
        break;
    case SLiftCType:
        generate(*syn.c_type, env, target, links, a, point);
        gen_mk_c_ty(ass,a, point);
        // Now, the type lies atop the stack, and we must pop the ctype out from
        // under it
        size_t cts = pi_stack_size_of(*syn.c_type->ptype);

        // TODO (IMPROVEMENT) this assumes address-size == 64 bits 
        build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);
        build_binary_op(ass, Add, reg(RSP, sz_64), imm32(cts), a, point);
        build_unary_op(ass, Push, reg(RCX, sz_64), a, point);

        data_stack_shrink(env, cts);
        break;
    case SNamedType:
        address_bind_type(syn.named_type.name, env);
        build_binary_op(ass, Mov, reg(RAX, sz_64), imm64(syn.named_type.name.name), a, point);
        build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
        build_binary_op(ass, Mov, reg(RAX, sz_64), imm64(syn.named_type.name.did), a, point);
        build_unary_op(ass, Push, reg(RAX, sz_64), a, point);

        data_stack_grow(env, sizeof(Symbol));
        generate(*(Syntax*)syn.named_type.body, env, target, links, a, point);
        data_stack_shrink(env, sizeof(Symbol));
        gen_mk_named_ty(ass, a, point);
        address_pop(env);
        break;
    case SDistinctType:
        generate(*(Syntax*)syn.distinct_type, env, target, links, a, point);
        gen_mk_distinct_ty(ass, a, point);
        break;
    case SOpaqueType:
        generate(*(Syntax*)syn.opaque_type, env, target, links, a, point);
        gen_mk_opaque_ty(ass, a, point);
        break;
    case STraitType:
        // Generate trait type: first bind relevant variables
        for (size_t i = 0; i < syn.bind_type.bindings.len; i++) {
            address_bind_type(syn.bind_type.bindings.data[i], env);
        }

        // First, malloc enough data for the array:
        generate_tmp_malloc(reg(RAX, sz_64), imm32(syn.trait.fields.len * (sizeof(Symbol) + ADDRESS_SIZE)), ass, a, point);
        build_binary_op(ass, Mov, reg(RCX, sz_64), imm32(0), a, point);


        for (size_t i = 0; i < syn.trait.fields.len; i++) {
            SymPtrCell field = syn.trait.fields.data[i];
            // First, move the field name
            build_binary_op(ass, Mov, sib8(RAX, RCX, 8, 0, sz_64), imm32(field.key.name), a, point);
            build_binary_op(ass, Mov, sib8(RAX, RCX, 8, 8, sz_64), imm32(field.key.did), a, point);

            // Second, generate & move the type (note: stash & pop RCX)
            build_unary_op(ass, Push, reg(RCX, sz_64), a, point);
            build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
            data_stack_grow(env, 2*ADDRESS_SIZE);
            generate(*(Syntax*)field.val, env, target, links, a, point);

            data_stack_shrink(env, 3*ADDRESS_SIZE);
            build_unary_op(ass, Pop, reg(R9, sz_64), a, point);
            build_unary_op(ass, Pop, reg(RAX, sz_64), a, point);
            build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);

            build_binary_op(ass, Mov, sib8(RAX, RCX, 8, 16, sz_64), reg(R9, sz_64), a, point);

            // Now, incremenet index by 2 (to account for trait size!)
            build_binary_op(ass, Add, reg(RCX, sz_64), imm32(3), a, point);
        }

        // Finally, generate function call to make type
        gen_mk_trait_ty(syn.trait.vars, reg(RAX, sz_64), imm32(syn.trait.fields.len), reg(RAX, sz_64), ass, a, point);
        build_unary_op(ass, Push, reg(RAX, sz_64), a, point);

        address_pop_n(syn.trait.vars.len, env);
        break;
    case SReinterpret:
        // reinterpret has no associated codegen
        // generate();
        generate(*syn.reinterpret.body, env, target, links, a, point);
        break;
    case SConvert: {
        if (syn.convert.from_native) {
            void* cfn = *(void**)const_fold(syn.convert.body, env, target, links, a, point);
            void* proc_address = get_instructions(target.code_aux).data;
            proc_address += get_instructions(target.code_aux).len;

            // Generate procedure value (push the address onto the stack)
            AsmResult out = build_binary_op(ass, Mov, reg(RAX, sz_64), imm64((uint64_t)proc_address), a, point);
            backlink_code(target, out.backlink, links);
            build_unary_op(ass, Push, reg(RAX, sz_64), a, point);

            // Now, change the target and the assembler, such that code is now
            // generated in the 'code segment'. Then, generate the function body
            ass = target.code_aux;
            target.target = target.code_aux;

            convert_c_fn(cfn, &syn.convert.body->ptype->c_type, syn.ptype, ass, a, point);
        } else {
            panic(mv_string("Cannot yet convert pico value to c value."));
        }
        break;
    }
    case STypeOf: {
        build_binary_op(ass, Mov, reg(R9, sz_64), imm64((uint64_t)syn.type_of->ptype), a, point);
        build_unary_op(ass, Push, reg(R9, sz_64), a, point);
        data_stack_grow(env, pi_size_of(*syn.ptype));
        break;
    }
    case SDescribe: {
        Environment* base = get_addr_base(env);
        EnvEntry entry = env_lookup(syn.to_describe, base);
        String immediate;

        if (entry.success == Ok) {
          if (entry.is_module) {
              SymbolArray syms = get_exported_symbols(entry.value, a);
              PtrArray lines = mk_ptr_array(syms.len + 8, a);
              {
                  PtrArray moduledesc = mk_ptr_array(2, a);
                  String* module_name = get_name(entry.value);
                  if (module_name) {
                      push_ptr(mk_str_doc(mv_string("Module: "), a), &moduledesc);
                      push_ptr(mk_str_doc(*module_name, a), &moduledesc);
                  } else {
                      push_ptr(mk_str_doc(mv_string("Anonymous Module"), a), &moduledesc);
                  }
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
                          push_ptr(mk_str_doc(*symbol_to_string(symbol), a), &desc);
                      } else {
                          push_ptr(mk_str_doc(*symbol_to_string(symbol), a), &desc);
                          push_ptr(mk_str_doc(mv_string(":"), a), &desc);
                          push_ptr(pretty_type(&mentry->type, a), &desc);
                      }
                      push_ptr(mv_sep_doc(desc, a), &lines);
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
                  PtrArray header = mk_ptr_array(2, a);
                  push_ptr(mk_str_doc(mv_string("Symbol: "), a), &header);
                  push_ptr(mk_str_doc(*symbol_to_string(syn.to_describe), a), &header);
                  push_ptr(mv_sep_doc(header, a), &lines);
              }
              push_ptr(mk_str_doc(mv_string("────────────────────────────────────────────"), a), &lines);
              {
                  PtrArray moduledesc = mk_ptr_array(2, a);
                  String* module_name = get_name(entry.source);
                  if (module_name) {
                      push_ptr(mk_str_doc(mv_string("Source Module: "), a), &moduledesc);
                      push_ptr(mk_str_doc(*module_name, a), &moduledesc);
                  } else {
                      push_ptr(mk_str_doc(mv_string("Source Module is Nameless"), a), &moduledesc);
                  }
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
        AsmResult out = build_binary_op(ass, Mov, reg(RAX, sz_64), imm64(0), a, point);

        // Backlink the data & copy the bytes into the data-segment.
        backlink_data(target, out.backlink, links);
        add_u8_chunk(immediate.bytes, immediate.memsize, target.data_aux);

        build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
        build_unary_op(ass, Push, imm32(immediate.memsize), a, point);

        data_stack_grow(env, pi_stack_size_of(*syn.ptype));
        break;
    }
    case SQuote: {
        // Setup: push the memory address (in data) of the Syntax value into stack.
        AsmResult out = build_binary_op(ass, Mov, reg(RAX, sz_64), imm64(0), a, point);
        backlink_data(target, out.backlink, links);

        // Now copy the entire concrete syntax tree into the data-segment,
        // setting allocators to null
        add_rawtree(syn.quoted, target, links);

        build_binary_op(ass, Sub, reg(RSP, sz_64), imm8(sizeof(RawTree)), a, point);
        for (size_t i = 0; i < sizeof(RawTree); i += sizeof(uint64_t)) {
            build_binary_op(ass, Mov, reg(RCX, sz_64), rref8(RAX, i, sz_64), a, point);
            build_binary_op(ass, Mov, rref8(RSP, i, sz_64), reg(RCX, sz_64), a, point);
        }
        data_stack_grow(env, pi_stack_size_of(*syn.ptype));
        break;
    }
    case SCapture: {
        // Setup: push the memory address (in data) of the Syntax value into stack.
        AsmResult out = build_binary_op(ass, Mov, reg(RAX, sz_64), imm64(0), a, point);
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

        build_binary_op(ass, Sub, reg(RSP, sz_64), imm8(sizeof(RawTree)), a, point);
        for (size_t i = 0; i < sizeof(RawTree); i += sizeof(uint64_t)) {
            build_binary_op(ass, Mov, reg(RCX, sz_64), rref8(RAX, i, sz_64), a, point);
            build_binary_op(ass, Mov, rref8(RSP, i, sz_64), reg(RCX, sz_64), a, point);
        }
        data_stack_grow(env, pi_stack_size_of(*syn.ptype));
        break;
    }
    default: {
        panic(mv_string("Invalid abstract supplied to monomorphic codegen."));
    }
    }

#ifdef DEBUG_ASSERT
    int64_t new_head = debug_get_stack_head(env);
    int64_t diff = old_head - new_head;
    if (diff < 0) panic(mv_string("diff < 0!"));

    // Justification: stack size of is extremely unlikely to be 2^63 bytes
    //  in size!
    if (diff != (int64_t)pi_stack_size_of(*syn.ptype))
        panic(mv_string("address environment constraint violated: expected size of the stack is wrong!"));
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

    generate(*syn, env, gen_target, links, a, point);
    
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

void add_tree_children(RawTreeArray trees, U8Array *arr) {
    for (size_t i = 0; i < trees.len; i++) {
        RawTree tree = trees.data[i];
        // TODO (BUG FEAT): set self .data = NULL, .gpa = NULL
        if (tree.type == RawBranch) {
            add_tree_children(tree.branch.nodes, arr);
        }
    }
}

void backlink_children(const size_t field_offset, size_t base_id, size_t* head, RawTreeArray trees, Target target, InternalLinkData* links) {
    for (size_t i = 0; i < trees.len; i++) {
        RawTree tree = trees.data[i];
        size_t current_idx = base_id + sizeof(RawTree) * i;
        if (tree.type == RawBranch) {
            size_t new_base_id = *head;
            backlink_data_data(target, current_idx , new_base_id + field_offset, links);
            *head += sizeof(RawTree) * tree.branch.nodes.len;
            backlink_children(field_offset, new_base_id, head, tree.branch.nodes, target, links);
        }
    }
}

void add_rawtree(RawTree tree, Target target, InternalLinkData* links) {
    if (tree.type == RawAtom) {
        add_u8_chunk((uint8_t*)&tree, sizeof(RawTree), target.data_aux);
    } else {
        // Start index of 
        size_t start_idx = target.data_aux->len;
        RawTree copy = (RawTree) {
            .type = tree.type,
            .range = tree.range,
            .branch.hint = tree.branch.hint,
            .branch.nodes.data = NULL, // this will be backlinked later
            .branch.nodes.len = tree.branch.nodes.len,
            .branch.nodes.size = tree.branch.nodes.len,
            // TODO; add allocator callbacks!
        };
        // Step 1: Copy in the tree
        add_u8_chunk((uint8_t*)&copy, sizeof(RawTree), target.data_aux);
        add_u8_chunk((uint8_t*)&tree.branch.nodes.data, sizeof(RawTree) * tree.branch.nodes.len, target.data_aux);
        add_tree_children(tree.branch.nodes, target.data_aux);

        // Step 2: Backlink nodes
        // ----------------------
        // The field offset for .branch.nodes.data from the rawtree (note: C
        // does not allow us to calculate this value directly, so we use a
        // little pointer arithmetic cheat to properly calculate it.
        const size_t field_offset = ((size_t)&tree.branch.nodes.data) - ((size_t)&tree);

        size_t head = start_idx + sizeof(RawTree) * tree.branch.nodes.len;
        backlink_data_data(target, start_idx, start_idx + field_offset, links);
        backlink_children(field_offset, start_idx, &head, tree.branch.nodes, target, links);
    }
}

