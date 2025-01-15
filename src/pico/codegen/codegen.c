#include "data/meta/array_impl.h"
#include "platform/signals.h"
#include "platform/machine_info.h"

#include "pico/codegen/codegen.h"
#include "pico/codegen/internal.h"
#include "pico/codegen/polymorphic.h"
#include "pico/binding/address_env.h"

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
    long int diff = lhs.origin_offset - rhs.origin_offset;
    if (diff) return diff;
    return lhs.data_index - rhs.data_index;
}

ARRAY_CMP_IMPL(LinkMetaData, compare_link_meta, link_meta, LinkMeta)

// Implementation details
void generate(Syntax syn, AddressEnv* env, Assembler* ass, LinkData* links, Allocator* a, ErrorPoint* point);
void generate_stack_move(size_t dest_offset, size_t src_offset, size_t size, Assembler* ass, Allocator* a, ErrorPoint* point);
void get_variant_fun(size_t idx, size_t vsize, size_t esize, uint64_t* out, ErrorPoint* point);
size_t calc_variant_size(PtrArray* types);

GenResult generate_expr(Syntax* syn, Environment* env, Assembler* ass, Allocator* a, ErrorPoint* point) {
    AddressEnv* a_env = mk_address_env(env, NULL, a);
    LinkData links = (LinkData) {
        .backlinks = mk_sym_sarr_amap(32, a),
        .gotolinks = mk_sym_sarr_amap(32, a),

        .to_generate = mk_to_gen_array(8, a),
        .bytes = mk_u8_array(128, a),
        .data_meta = mk_link_meta_array(8, a),
        .code_meta = mk_link_meta_array(8, a),
    };
    generate(*syn, a_env, ass, &links, a, point);
    delete_address_env(a_env, a);

    GenResult out;
    out.backlinks = links.backlinks;

    return out;
}

GenResult generate_toplevel(TopLevel top, Environment* env, Assembler* ass, Allocator* a, ErrorPoint* point) {
    LinkData links = (LinkData) {
        .backlinks = mk_sym_sarr_amap(32, a),
        .gotolinks = mk_sym_sarr_amap(32, a),
    };

    switch(top.type) {
    case TLDef: {
        AddressEnv* a_env = mk_address_env(env, &top.def.bind, a);
        generate(*top.def.value, a_env, ass, &links, a, point);
        delete_address_env(a_env, a);
        break;
    }
    case TLExpr: {
        AddressEnv* a_env = mk_address_env(env, NULL, a);
        generate(*top.expr, a_env, ass, &links, a, point);
        delete_address_env(a_env, a);
        break;
    }
    }

    return (GenResult) {
        .backlinks = links.backlinks,
    };
}

void generate(Syntax syn, AddressEnv* env, Assembler* ass, LinkData* links, Allocator* a, ErrorPoint* point) {
    switch (syn.type) {
    case SLitUntypedIntegral: 
        panic(mv_string("Cannot generate monomorphic code for untyped integral!"));
    case SLitTypedIntegral: {
        // Does it fit into 32 bits?
        if (syn.integral.value >= 0x80000000) 
            throw_error(point, mv_string("Codegen: Literals must fit into less than 32 bits"));

        int32_t immediate = (int32_t)syn.integral.value;
        build_unary_op(ass, Push, imm32(immediate), a, point);
        address_stack_grow(env, pi_size_of(*syn.ptype));
        break;
    }
    case SLitBool: {
        int8_t immediate = syn.boolean;
        build_unary_op(ass, Push, imm8(immediate), a, point);
        address_stack_grow(env, pi_size_of(*syn.ptype));
        break;
    }
    case SLitString: {
        String immediate = syn.string; 
        if (immediate.memsize > UINT32_MAX) 
            throw_error(point, mv_string("Codegen: String literal length must fit into less than 32 bits"));

        build_binary_op(ass, Mov, reg(RAX, sz_64), imm64((uint64_t)immediate.bytes), a, point);
        build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
        build_unary_op(ass, Push, imm32(immediate.memsize), a, point);

        address_stack_grow(env, pi_size_of(*syn.ptype));
        break;
    }
    case SVariable: 
    case SAbsVariable: {
        // Lookup the variable in the assembly envionrment
        AddressEntry e = (syn.type == SVariable)
            ? address_env_lookup(syn.variable, env)
            : address_abs_lookup(syn.abvar, env);
        switch (e.type) {
        case ALocalDirect:
            build_unary_op(ass, Push, rref8(RBP, sz_64, e.stack_offset), a, point);
            break;
        case ALocalIndirect:
            panic(mv_string("cannot generate code for local direct"));
            break;
        case AGlobal: {
            PiType indistinct_type = *syn.ptype;
            while (indistinct_type.sort == TDistinct) { indistinct_type = *indistinct_type.distinct.type; }

            // Procedures (inc. polymorphic procedures), Types and types are passed by reference (i.e. they are addresses). 
            if (indistinct_type.sort == TProc || indistinct_type.sort == TAll || indistinct_type.sort == TKind) {
                AsmResult out = build_binary_op(ass, Mov, reg(R9, sz_64), imm64((uint64_t)e.value), a, point);
                backlink_global(syn.variable, out.backlink, links, a);
                build_unary_op(ass, Push, reg(R9, sz_64), a, point);

            // Primitives, Dynamic Vars and instances passed by value, but are guaranteed to take up 64 bits.
            } else if (indistinct_type.sort == TPrim || indistinct_type.sort == TDynamic || indistinct_type.sort == TTraitInstance) {
                AsmResult out = build_binary_op(ass, Mov, reg(RCX, sz_64), imm64((uint64_t)e.value), a, point);
                backlink_global(syn.variable, out.backlink, links, a);
                build_binary_op(ass, Mov, reg(R9, sz_64), rref8(RCX, sz_64, 0), a, point);
                build_unary_op(ass, Push, reg(R9, sz_64), a, point);

            // Structs and Enums are passed by value, and have variable size.
            } else if (indistinct_type.sort == TStruct || indistinct_type.sort == TEnum) {
                size_t value_size = pi_size_of(*syn.ptype);
                AsmResult out = build_binary_op(ass, Mov, reg(RCX, sz_64), imm64((uint64_t)e.value), a, point);
                backlink_global(syn.variable, out.backlink, links, a);

                // Allocate space on the stack for composite type (struct/enum)
                build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(value_size), a, point);

                generate_monomorphic_copy(RSP, RCX, value_size, ass, a, point);
            } else {
                throw_error(point,
                            string_ncat(a, 3,
                                        mv_string("Codegen: Global var '"),
                                        *symbol_to_string(syn.variable),
                                        mv_string("' has unsupported sort")
                                        ));
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
        address_stack_grow(env, pi_size_of(*syn.ptype));
        break;
    }
    case SAll: {
        generate_polymorphic(syn.all.args, *syn.all.body, env, ass, links, a, point);
        break;
    }
    case SProcedure: {
        // Codegen function setup
        build_unary_op(ass, Push, reg(R14, sz_64), a, point);
        build_unary_op(ass, Push, reg(RBP, sz_64), a, point);
        build_binary_op(ass, Mov, reg(RBP, sz_64), reg(RSP, sz_64), a, point);

        // Codegen Procedure Body 
        size_t args_size = 0;
        SymSizeAssoc impl_sizes = mk_sym_size_assoc(syn.procedure.implicits.len, a);
        for (size_t i = 0; i < syn.procedure.implicits.len; i++) {
            size_t arg_size = pi_size_of(*(PiType*)syn.ptype->proc.implicits.data[i]);
            args_size += arg_size;
            sym_size_bind(syn.procedure.implicits.data[i].key , arg_size , &impl_sizes);
        }

        SymSizeAssoc arg_sizes = mk_sym_size_assoc(syn.procedure.args.len, a);
        for (size_t i = 0; i < syn.procedure.args.len; i++) {
            size_t arg_size = pi_size_of(*(PiType*)syn.ptype->proc.args.data[i]);
            args_size += arg_size;
            sym_size_bind(syn.procedure.args.data[i].key , arg_size , &arg_sizes);
        }

        address_start_proc(impl_sizes, arg_sizes, env, a);
        generate(*syn.procedure.body, env, ass, links, a, point);
        address_end_proc(env, a);

        // Codegen function teardown:
        // + restore old RBP & R14 in registers
        // + stash return address
        // + copy result down stack, accounting for
        //   + Return address, old RBP & old RSP
        //   + all arguments
        // + return to stashed address

        // Storage of function output 
        size_t ret_size = pi_size_of(*syn.procedure.body->ptype);
        build_binary_op(ass, Mov, reg(R9, sz_64),  rref8(RBP, sz_64, 16), a, point);
        build_binary_op(ass, Mov, reg(R14, sz_64), rref8(RBP, sz_64, 8), a, point);
        build_binary_op(ass, Mov, reg(RBP, sz_64), rref8(RBP, sz_64, 0), a, point);

        generate_stack_move(args_size + 3 * ADDRESS_SIZE, 0, ret_size, ass, a, point);

        // Pop args
        build_binary_op(ass, Add, reg(RSP, sz_64), imm32(args_size + 3 * ADDRESS_SIZE), a, point);

        // push return address 
        build_unary_op(ass, Push, reg(R9, sz_64), a, point);
        build_nullary_op(ass, Ret, a, point);
        break;
    }
    case SApplication: {
        // Monomorphic Codegen
        if (syn.application.function->ptype->sort == TProc) {
            size_t args_size = 0;
            for (size_t i = 0; i < syn.application.implicits.len; i++) {
                Syntax* arg = (Syntax*) syn.application.implicits.data[i];
                args_size += pi_size_of(*arg->ptype);
                generate(*arg, env, ass, links, a, point);
            }
            for (size_t i = 0; i < syn.application.args.len; i++) {
                Syntax* arg = (Syntax*) syn.application.args.data[i];
                args_size += pi_size_of(*arg->ptype);
                generate(*arg, env, ass, links, a, point);
            }

            // This will push a function pointer onto the stack
            generate(*syn.application.function, env, ass, links, a, point);
        
            // Regular Function Call
            // Pop the function into RCX; call the function
            build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);
            build_unary_op(ass, Call, reg(RCX, sz_64), a, point);
            // Update for popping all values off the stack (also the function itself)
            address_stack_shrink(env, args_size + ADDRESS_SIZE);

            // Update as pushed the final value onto the stac
            address_stack_grow(env, pi_size_of(*syn.ptype));

        // Is a type family 
        } else {
            size_t args_size = 0;
            for (size_t i = 0; i < syn.application.args.len; i++) {
                Syntax* arg = (Syntax*) syn.application.args.data[i];
                args_size += pi_size_of(*arg->ptype);
                generate(*arg, env, ass, links, a, point);
            }

            // push the type onto the stack:
            generate(*syn.application.function, env, ass, links, a, point);

            gen_mk_family_app(syn.application.args.len, ass, a, point);
            // Update for popping all values off the stack (also the function itself)
            address_stack_shrink(env, args_size + ADDRESS_SIZE);

            // Update as pushed the final value onto the stac
            address_stack_grow(env, pi_size_of(*syn.ptype));

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
            build_unary_op(ass, Push, imm32(pi_size_of(*((Syntax*)syn.all_application.types.data[i])->type_val)), a, point);
        }

        // Calculation of offsets:
        // • Remaining offset starts @ sum of ADDRESS_SIZE * 2
        int32_t offset = 0;
        
        for (size_t i = 0; i < syn.all_application.implicits.len; i++) {
            offset += pi_size_of(*((Syntax*)syn.all_application.implicits.data[i])->ptype);
            build_unary_op(ass, Push, imm32(-offset), a, point);
        }
        for (size_t i = 0; i < syn.all_application.args.len; i++) {
            offset += pi_size_of(*((Syntax*)syn.all_application.args.data[i])->ptype);
            build_unary_op(ass, Push, imm32(-offset), a, point);
        }
        //size_t args_size = offset - ADDRESS_SIZE;
        build_binary_op(ass, Sub, reg(RSP, sz_64), imm8(ADDRESS_SIZE), a, point);
        build_unary_op(ass, Push, reg(RBP, sz_64), a, point);

        address_stack_grow(env, ADDRESS_SIZE * (syn.all_application.types.len
                                                + syn.all_application.implicits.len
                                                + syn.all_application.args.len + 2));

        for (size_t i = 0; i < syn.all_application.implicits.len; i++) {
            Syntax* arg = (Syntax*) syn.all_application.implicits.data[i];
            generate(*arg, env, ass, links, a, point);
        }
        for (size_t i = 0; i < syn.all_application.args.len; i++) {
            Syntax* arg = (Syntax*) syn.all_application.args.data[i];
            generate(*arg, env, ass, links, a, point);
        }

        // This will push a function pointer onto the stack
        generate(*syn.application.function, env, ass, links, a, point);

        // Now, calculate what RBP should be 
        // RBP = RSP - (args_size + ADDRESS_SIZE) ;; (ADDRESS_SIZE accounts for function ptr)
        //     = RSP - offset
        build_binary_op(ass, Mov, reg(RBP, sz_64), reg(RSP, sz_64), a, point);
        build_binary_op(ass, Add, reg(RBP, sz_64), imm32(offset + ADDRESS_SIZE), a, point);
        
        // Regular Function Call
        // Pop the function into RCX; call the function
        build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);
        build_unary_op(ass, Call, reg(RCX, sz_64), a, point);
        // Update for popping all values off the stack
        // TODO: make sure this gets done!
        //address_stack_shrink(env, args_size);

        // Update as pushed the final value onto the stack
        address_stack_grow(env, pi_size_of(*syn.ptype));
        break;
            
    }
    case SConstructor: {
        PiType* enum_type = syn.constructor.enum_type->type_val;
        size_t enum_size = pi_size_of(*enum_type);
        size_t variant_size = calc_variant_size(enum_type->enumeration.variants.data[syn.variant.tag].val);

        build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(enum_size - variant_size), a, point);
        build_unary_op(ass, Push, imm32(syn.constructor.tag), a, point);

        address_stack_grow(env, enum_size);
        break;
    }
    case SVariant: {
        const size_t tag_size = sizeof(uint64_t);
        PiType* enum_type = syn.variant.enum_type->type_val;
        size_t enum_size = pi_size_of(*enum_type);
        size_t variant_size = calc_variant_size(enum_type->enumeration.variants.data[syn.variant.tag].val);

        // Make space to fit the (final) variant
        build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(enum_size), a, point);

        // Set the tag
        build_binary_op(ass, Mov, rref8(RSP, sz_64, 0), imm32(syn.constructor.tag), a, point);

        // Generate each argument
        for (size_t i = 0; i < syn.variant.args.len; i++) {
            generate(*(Syntax*)syn.variant.args.data[i], env, ass, links, a, point);
        }

        // Now, move them into the space allocated in reverse order
        PtrArray args = *(PtrArray*)syn.ptype->enumeration.variants.data[syn.variant.tag].val;
        size_t src_stack_offset = variant_size - tag_size;
        size_t dest_stack_offset = variant_size;
        for (size_t i = 0; i < syn.variant.args.len; i++) {
            // We now both the source_offset and dest_offset. These are both
            // relative to the 'bottom' of their respective structures.
            // Therefore, we now need to find their offsets relative to the `top'
            // of the stack.
            
            size_t field_size = pi_size_of(*(PiType*)args.data[syn.variant.args.len - (i + 1)]);
            src_stack_offset -= field_size;

            // Now, move the data.
            generate_stack_move(dest_stack_offset, src_stack_offset, field_size, ass, a, point);

            // Now, increase the amount of data we have used
            dest_stack_offset += field_size;
        }

        // Remove the space occupied by the temporary values 
        build_binary_op(ass, Add, reg(RSP, sz_64), imm32(variant_size - tag_size), a, point);

        // Grow the stack to account for the difference in enum & variant sizes
        address_stack_grow(env, enum_size - variant_size);
        break;
    }
    case SMatch: {
        // Generate code for the value
        Syntax* match_value = syn.match.val;
        size_t enum_size = pi_size_of(*match_value->ptype);
        size_t out_size = pi_size_of(*syn.ptype);

        generate(*match_value, env, ass, links, a, point);

        SizeArray back_positions = mk_size_array(syn.match.clauses.len, a);
        PtrArray back_refs = mk_ptr_array(syn.match.clauses.len, a);

        // For each pattern match, generate two things 
        // 1. A comparison/check that the pattern has been matched
        // 2. A jump to the relevant location
        for (size_t i = 0; i < syn.match.clauses.len; i++) {
            SynClause clause = *(SynClause*)syn.match.clauses.data[i];
            build_binary_op(ass, Cmp, rref8(RSP, sz_64, 0), imm32(clause.tag), a, point);
            AsmResult out = build_unary_op(ass, JE, imm8(0), a, point);
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
            if (body_pos - branch_pos > INT8_MAX) {
                throw_error(point, mk_string("Jump in match too large", a));
            } 

            *branch_ref = (uint8_t)(body_pos - branch_pos);

            SynClause clause = *(SynClause*)syn.match.clauses.data[i];
            PtrArray variant_types = *(PtrArray*)match_value->ptype->enumeration.variants.data[clause.tag].val; 

            // Bind Clause Vars 
            SymSizeAssoc arg_sizes = mk_sym_size_assoc(variant_types.len, a);
            for (size_t i = 0; i < variant_types.len; i++) {
                sym_size_bind(clause.vars.data[i]
                              , pi_size_of(*(PiType*)variant_types.data[i])
                              , &arg_sizes);
            }
            address_bind_enum_vars(arg_sizes, env);

            generate(*clause.body, env, ass, links, a, point);

            // Generate jump to end of false branch to be backlinked later
            AsmResult out = build_unary_op(ass, JMP, imm8(0), a, point);
            push_size(get_pos(ass), &body_positions);
            push_ptr(get_instructions(ass).data + out.backlink, &body_refs);

            address_unbind_enum_vars(env);
            address_stack_shrink(env, out_size);
        }

        // Finally, backlink all jumps from the bodies to the end.
        size_t curr_pos = get_pos(ass);
        for (size_t i = 0; i < body_positions.len; i++) {
            size_t body_pos = body_positions.data[i];
            uint8_t* body_ref = body_refs.data[i];

            if (curr_pos - body_pos > INT8_MAX) {
                throw_error(point, mk_string("Jump in match too large", a));
            } 

            *body_ref = (uint8_t)(curr_pos - body_pos);
        }


        generate_stack_move(out_size + (enum_size - out_size), 0, out_size, ass, a, point);

        build_binary_op(ass, Add, reg(RSP, sz_64), imm32(enum_size), a, point);

        address_stack_shrink(env, enum_size);
        address_stack_grow(env, out_size);
        break;
    }
    case SStructure: {
        // For structures, we have to be careful - this is because the order in
        // which arguments are evaluated is not necessarily the order in which
        // arguments are inserted into the structure.

        // Step 1: Make room on the stack for our struct
        size_t struct_size = pi_size_of(*syn.ptype);
        build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(struct_size), a, point);
        address_stack_grow(env, struct_size);

        // Step 2: evaluate each element/variable binding
        for (size_t i = 0; i < syn.structure.fields.len; i++) {
            generate(*(Syntax*)syn.structure.fields.data[i].val, env, ass, links, a, point);
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
        size_t dest_offset = 0;
        for (size_t i = 0; i < syn.ptype->structure.fields.len; i++) {

            // Find the field in the source & compute offset
            size_t src_offset = 0;
            for (size_t j = 0; j < syn.structure.fields.len; j++) {
                PiType** t = (PiType**)sym_ptr_lookup(syn.structure.fields.data[j].key, syn.ptype->structure.fields);
                if (t) {
                    src_offset += pi_size_of(*((Syntax*)syn.structure.fields.data[j].val)->ptype); 
                } else {
                    throw_error(point, mv_string("Error code-generating for structure: field not found."));
                }

                if (syn.structure.fields.data[j].key == syn.ptype->structure.fields.data[i].key) {
                    break; // offset is correct, end the loop
                }
            }

            // We now both the source_offset and dest_offset. These are both
            // relative to the 'bottom' of their respective structures.
            // Therefore, we now need to find their offsets relative to the `top'
            // of the stack.
            size_t src_stack_offset = struct_size - src_offset;
            size_t dest_stack_offset = struct_size + dest_offset;
            size_t field_size = pi_size_of(*(PiType*)syn.ptype->structure.fields.data[i].val);

            // Now, move the data.
            generate_stack_move(dest_stack_offset, src_stack_offset, field_size, ass, a, point);

            // Compute dest_offset for next loop
            dest_offset += pi_size_of(*(PiType*)syn.ptype->structure.fields.data[i].val);
        }
        // Remove the space occupied by the temporary values 
        build_binary_op(ass, Add, reg(RSP, sz_64), imm32(struct_size), a, point);
        address_stack_shrink(env, struct_size);
        break;
    }
    case SProjector: {
        // First, allocate space on the stack for the value
        size_t out_sz = pi_size_of(*syn.ptype);
        build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(out_sz), a, point);
        address_stack_grow(env, out_sz);

        // Second, generate the structure/instance object
        generate(*syn.projector.val, env, ass, links, a, point);
        size_t src_sz = pi_size_of(*syn.projector.val->ptype);

        // From this point, behaviour depends on whether we are projecting from
        // a structure or from an instance
        if (syn.projector.val->ptype->sort == TStruct) {
            // Now, copy the structure to the destination
            // for this, we need the struct size + offset of field in the struct
            size_t offset = 0;
            for (size_t i = 0; i < syn.projector.val->ptype->structure.fields.len; i++) {
                if (syn.projector.val->ptype->structure.fields.data[i].key == syn.projector.field)
                    break;
                offset += pi_size_of(*(PiType*)syn.projector.val->ptype->structure.fields.data[i].val);
            }

            generate_stack_move(src_sz + out_sz - 0x8, offset, out_sz, ass, a, point);
            // now, remove the original struct from the stack
            build_binary_op(ass, Add, reg(RSP, sz_64), imm32(src_sz), a, point);

            address_stack_shrink(env, src_sz);
        } else {
            // Pop the pointer to the instance from the stack - store in RSI
            address_stack_shrink(env, src_sz);
            build_unary_op(ass, Pop, reg(RSI, sz_64), a, point);

            // Now, calculate offset for field 
            size_t offset = 0;
            for (size_t i = 0; i < syn.projector.val->ptype->instance.fields.len; i++) {
                if (syn.projector.val->ptype->instance.fields.data[i].key == syn.projector.field)
                    break;
                offset += pi_size_of(*(PiType*)syn.projector.val->ptype->instance.fields.data[i].val);
            }
            build_binary_op(ass, Add, reg(RSI, sz_64), imm32(offset), a, point);

            generate_monomorphic_copy(RSP, RSI, out_sz, ass, a, point);
        }
        break;
    }
    case SInstance: {
        /* Instances work as follows:
         * • Instances as values are expected to be passed as pointers and allocated temporarily. 
         * • Non-parametric instances are simply pointers : codegen generates a
         *   malloc and then assigns all values.
         * • Parametric instances are functions (which may be instantiated by
         *   the runtime)
         */

        size_t immediate_sz = 0;
        for (size_t i = 0; i < syn.ptype->instance.fields.len; i++) {
            immediate_sz += pi_size_of(*(PiType*)syn.ptype->instance.fields.data[i].val);
        }
        build_binary_op(ass, Mov, reg(RSI, sz_64), imm32(immediate_sz), a, point);
        generate_tmp_malloc(reg(RAX, sz_64), reg(RSI, sz_64), ass, a, point);
        // build_binary_op(ass, Add, reg(RAX), imm32(immediate_sz), a, point);
        build_binary_op(ass, Mov, reg(RCX, sz_64), reg(RAX, sz_64), a, point);

        // Grow by address size to account for the fact that the for loop
        // keeps a stack of the address, which is updated each iteration.
        address_stack_grow(env, ADDRESS_SIZE);
        build_unary_op(ass, Push, reg(RCX, sz_64), a, point);

        for (size_t i = 0; i < syn.ptype->instance.fields.len; i++) {

            // Generate field
            Syntax* val = syn.instance.fields.data[i].val;
            generate(*val, env, ass, links, a, point);

            size_t offset = pi_size_of(*val->ptype);
            // Retrieve index (ptr) 
            // TODO (BUG) Check offset is < int8_t max.
            build_binary_op(ass, Mov, reg(RCX, sz_64), rref8(RSP, sz_64, offset), a, point);

            generate_monomorphic_copy(RCX, RSP, offset, ass, a, point);

            build_binary_op(ass, Add, reg(RCX, sz_64), imm32(offset), a, point);

            // Pop value from stack
            build_binary_op(ass, Add, reg(RSP, sz_64), imm32(offset), a, point);
            address_stack_shrink(env, offset);

            // Override index with new value
            build_binary_op(ass, Mov, rref8(RSP, sz_64, 0), reg(RCX, sz_64), a, point);
        }

        build_binary_op(ass, Mov, reg(RCX, sz_64), rref8(RSP, sz_64, 0), a, point);
        build_binary_op(ass, Sub, reg(RCX, sz_64), imm32(immediate_sz), a, point);
        build_binary_op(ass, Mov, rref8(RSP, sz_64, 0), reg(RCX, sz_64), a, point);

        // Note: we don't shrink as the final address (on stack) is accounted
        // for by the 'grow' prior to the above for-loop
        break;
    }
    case SDynamic: {
        // Create a new dynamic variable, i.e. call the C function 
        // mk_dynamic_var(size_t size, void* default_val)
        generate(*syn.dynamic, env, ass, links, a, point);

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
        build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#else
#error "unknown ABI"
#endif

        // call function
        build_binary_op(ass, Mov, reg(RAX, sz_64), imm64((uint64_t)mk_dynamic_var), a, point);
        build_unary_op(ass, Call, reg(RAX, sz_64), a, point);

#if ABI == WIN_64 
        build_binary_op(ass, Add, reg(RSP, sz_64), imm32(32), a, point);
#endif
        // Pop value off stack
        build_binary_op(ass, Add, reg(RSP, sz_64), imm32(val_size), a, point);
        build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
        
        address_stack_shrink(env, ADDRESS_SIZE);
        address_stack_grow(env, val_size);
        break;
    }
    case SDynamicUse: {
        generate(*syn.use, env, ass, links, a, point);

        // We now have a dynamic variable: get its' value as ptr

#if ABI == SYSTEM_V_64 
        // arg1 = rdi
        build_unary_op(ass, Pop, reg(RDI, sz_64), a, point);
#elif ABI == WIN_64 
        // arg1 = rcx
        build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);
        build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#else
#error "unknown ABI"
#endif

        // call function
        build_binary_op(ass, Mov, reg(RAX, sz_64), imm64((uint64_t)get_dynamic_val), a, point);
        build_unary_op(ass, Call, reg(RAX, sz_64), a, point);

#if ABI == WIN_64 
        build_binary_op(ass, Add, reg(RSP, sz_64), imm32(32), a, point);
#endif
        // Now, allocate space on stack
        size_t val_size = pi_size_of(*syn.ptype);
        build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(val_size), a, point);
        build_binary_op(ass, Mov, reg(RCX, sz_64), reg(RAX, sz_64), a, point);

        //void generate_monomorphic_copy(Regname dest, Regname src, size_t size, Assembler* ass, Allocator* a, ErrorPoint* point) {
        generate_monomorphic_copy(RSP, RCX, val_size, ass, a, point);
        
        address_stack_shrink(env, ADDRESS_SIZE);
        address_stack_grow(env, val_size);
        break;
    }
    case SDynamicLet: {

        // Step 1: evaluate all dynamic bindings & bind them
        for (size_t i = 0; i < syn.dyn_let_expr.bindings.len; i++) {
            DynBinding* dbind = syn.dyn_let_expr.bindings.data[i];
            size_t bind_size = pi_size_of(*dbind->expr->ptype);
            generate(*dbind->var, env, ass, links, a, point);
            generate(*dbind->expr, env, ass, links, a, point);

            // Copy the value into the dynamic var
            // Currently, the stack looks like:
            // + dynamic-var-index
            //   new-value
            // R15 is the dynamic memory register, move it into RCX
            // Move the index into RAX
            build_binary_op(ass, Mov, reg(RCX, sz_64), reg(R15, sz_64), a, point);
            build_binary_op(ass, Mov, reg(RAX, sz_64), rref8(RSP, sz_64, bind_size), a, point);
            // RCX holds the array - we need to index it with index located in RAX
            build_binary_op(ass, Mov, reg(RCX, sz_64), sib(RCX, RAX, 8, sz_64), a, point);
            // Now we have a pointer to the value stored in RCX, swap it with
            // the value on the stack
            generate_monomorphic_swap(RCX, RSP, bind_size, ass, a, point);
        }

        // Step 2: generate the body
        generate(*syn.let_expr.body, env, ass, links, a, point);
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
            build_binary_op(ass, Mov, reg(RAX, sz_64), rref8(RDX, sz_64, 0), a, point);
            // RCX holds the array - we need to index it with index located in RAX
            build_binary_op(ass, Mov, reg(RCX, sz_64), sib(RCX, RAX, 8, sz_64), a, point);

            // Store ptr to local value to restore in RDX 
            generate_monomorphic_swap(RCX, RDX, bind_size, ass, a, point);
            build_binary_op(ass, Add, reg(RDX, sz_64), imm32(bind_size + ADDRESS_SIZE), a, point);

            offset_size += bind_size + ADDRESS_SIZE;
        }

        // Step 4: cleanup: pop bindings
        generate_stack_move(offset_size, 0, val_size, ass, a, point);
        build_binary_op(ass, Add, reg(RSP, sz_64), imm32(offset_size), a, point);
        address_stack_shrink(env, offset_size);
        break;
    }
    case SLet: {
        size_t bsize = 0;
        for (size_t i = 0; i < syn.let_expr.bindings.len; i++) {
            Syntax* sy = syn.let_expr.bindings.data[i].val;
            generate(*sy, env, ass, links, a, point);
            address_bind_relative(syn.let_expr.bindings.data[i].key, 0, env);
            bsize += pi_size_of(*sy->ptype);
        }
        generate(*syn.let_expr.body, env, ass, links, a, point);
        address_pop_n(syn.let_expr.bindings.len, env);

        generate_stack_move(bsize, 0, pi_size_of(*syn.let_expr.body->ptype), ass, a, point);
        build_binary_op(ass, Add, reg(RSP, sz_64), imm32(bsize), a, point);
        address_stack_shrink(env, bsize);
        address_pop_n(syn.let_expr.bindings.len, env);
        break;
    }
    case SIf: {
        // generate the condition
        generate(*syn.if_expr.condition, env, ass, links, a, point);

        // Pop the bool into R9; compare with 0
        build_unary_op(ass, Pop, reg(R9, sz_64), a, point);
        build_binary_op(ass, Cmp, reg(R9, sz_64), imm32(0), a, point);
        address_stack_shrink(env, pi_size_of((PiType) {.sort = TPrim, .prim = Bool}));

        // ---------- CONDITIONAL JUMP ----------
        // compare the value to 0
        // jump to false branch if equal to 0 -- the immediate 8 is a placeholder
        AsmResult out = build_unary_op(ass, JE, imm8(0), a, point);
        size_t start_pos = get_pos(ass);

        uint8_t* jmp_loc = get_instructions(ass).data + out.backlink;

        // ---------- TRUE BRANCH ----------
        // now, generate the code to run (if true)
        generate(*syn.if_expr.true_branch, env, ass, links, a, point);

        // Generate jump to end of false branch to be backlinked later
        out = build_unary_op(ass, JMP, imm8(0), a, point);

        // calc backlink offset
        size_t end_pos = get_pos(ass);
        if (end_pos - start_pos > INT8_MAX) {
            throw_error(point, mk_string("Jump in conditional too large", a));
        } 

        // backlink
        *jmp_loc = (end_pos - start_pos);
        jmp_loc = get_instructions(ass).data + out.backlink;
        start_pos = get_pos(ass);


        // ---------- FALSE BRANCH ----------
        // Generate code for the false branch
        generate(*syn.if_expr.false_branch, env, ass, links, a, point);

        // calc backlink offset
        end_pos = get_pos(ass);
        if (end_pos - start_pos > INT8_MAX) {
            throw_error(point, mk_string("Jump in conditional too large", a));
        } 
        *jmp_loc = (uint8_t)(end_pos - start_pos);
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
        SymbolArray labels = mk_u64_array(syn.labels.terms.len, a);
        for (size_t i = 0; i < syn.labels.terms.len; i++) 
            push_u64(syn.labels.terms.data[i].key, &labels);

        address_start_labels(labels, env);

        generate(*syn.labels.entry, env, ass, links, a, point);

        SymSizeAssoc label_points = mk_sym_size_assoc(syn.labels.terms.len, a);
        SymSizeAssoc label_jumps = mk_sym_size_assoc(syn.labels.terms.len, a);

        for (size_t i = 0; i < syn.labels.terms.len; i++) {
            SymPtrACell cell = syn.labels.terms.data[i];

            // Mark Label
            size_t pos = get_pos(ass);
            sym_size_bind(cell.key, pos, &label_points);

            generate(*(Syntax*)cell.val, env, ass, links, a, point);
            AsmResult out = build_unary_op(ass, JMP, imm8(0), a, point);
            sym_size_bind(cell.key, out.backlink, &label_jumps);
        }

        size_t label_end = get_pos(ass);

        for (size_t i = 0; i < label_points.len; i++)  {
            Symbol sym = label_points.data[i].key;
            size_t dest = label_points.data[i].val;

            // Step 1: make the end of the label jump to the end of the expression.
            {
                size_t backlink = label_jumps.data[i].val;
                size_t origin = backlink + 1;
                size_t dest = label_end;

                int64_t amt = dest - origin;
                if (amt < INT8_MIN || amt > INT8_MAX) panic(mv_string("Label jump too large!"));
                
                int8_t* loc = (int8_t*) get_instructions(ass).data + backlink;
                *loc = (int8_t) amt;
            }


            // Step 2: fill out each jump to this label.
            SizeArray* arr = sym_sarr_lookup(sym, links->gotolinks);
            if (!arr) panic(mv_string("Can't find size array when backlinking label!"));

            for (size_t i = 0; i < arr->len; i++) {
                size_t backlink = arr->data[i];
                size_t origin = backlink + 1;

                int64_t amt = dest - origin;
                if (amt < INT8_MIN || amt > INT8_MAX) panic(mv_string("Label jump too large!"));
                
                int8_t* loc = (int8_t*) get_instructions(ass).data + backlink;
                *loc = (int8_t) amt;
            }
        }

        address_end_labels(env);
        break;
    }
    case SGoTo: {
        // Generating code for a goto:
        // 1. Backlink the label
        //throw_error(point, mv_string("Codegen is not implemented for this syntactic form: 'go-to'"));
        LabelEntry lble = label_env_lookup(syn.go_to.label, env);
        if (lble.type == Ok) {
            if (lble.stack_offset != 0) {
                // jump up stack
                // TODO handle dynamic variable unbinding (if needed!)
                build_binary_op(ass, Add, reg(RSP, sz_64), imm32(lble.stack_offset), a, point);
            }

            AsmResult out = build_unary_op(ass, JMP, imm8(0), a, point);

            backlink_goto(syn.go_to.label, out.backlink, links, a);
        } else {
            throw_error(point, mv_string("Label not found during codegen!!"));
        }
        break;
    }
    case SWithReset: {
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
        address_stack_grow(env, 3*ADDRESS_SIZE);
        address_bind_relative(syn.with_reset.point_sym, 0, env);

        // Step 3.
        //--------
        generate(*syn.with_reset.expr,env, ass, links, a, point);

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

        address_stack_shrink(env, 3*ADDRESS_SIZE + val_size);
        address_pop(env);

        // Step 5.
        //--------
        out = build_unary_op(ass, JMP, imm8(0), a, point);
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
        address_stack_grow(env, ADDRESS_SIZE + in_val_size);
        address_bind_relative(syn.with_reset.cont_sym, 0, env);
        address_bind_relative(syn.with_reset.in_sym, 8, env);
        
        /* Symbol cont_sym; */
        /* Symbol in_sym; */
        generate(*syn.with_reset.handler, env, ass, links, a, point);
        address_stack_shrink(env, ADDRESS_SIZE + in_val_size);
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
        if (dist > INT8_MAX) {
            throw_error(point, mv_string("Internal error in codegen: jump distance exceeded INT8_MAX"));
        }
        *(get_instructions(ass).data + end_expr_link) = (int8_t) dist;

        break;
    }
    case SResetTo: {
        generate(*syn.reset_to.arg, env, ass, links, a, point);
        generate(*syn.reset_to.point, env, ass, links, a, point);
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
        generate_monomorphic_copy(RSP, R9, asize, ass, a, point);

        // Step 5: Long Jump (call) register
        build_unary_op(ass, Call, reg(RDI, sz_64), a, point);

        // Address environment bookkeeping: shrink the stack appropriately
        address_stack_shrink(env, pi_size_of(*syn.reset_to.arg->ptype) + pi_size_of(*syn.reset_to.point->ptype));
        address_stack_grow(env, pi_size_of(*syn.ptype));

        break;
    }
    case SSequence: {
        size_t binding_size = 0;
        size_t num_bindings = 0;
        size_t last_size = 0;
        for (size_t i = 0; i < syn.sequence.elements.len; i++) {
            SeqElt* elt = syn.sequence.elements.data[i];
            generate(*elt->expr, env, ass, links, a, point);

            size_t sz = pi_size_of(*elt->expr->ptype);
            if (elt->is_binding) {
                num_bindings++;
                binding_size += i + 1 == syn.sequence.elements.len ? 0 : sz;
                address_bind_relative(elt->symbol, 0, env);
            }
            else if (i + 1 != syn.sequence.elements.len) {
                if (sz != 0) {
                    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(sz), a, point);
                }
                address_stack_shrink(env, sz);
            }

            if (i + 1 == syn.sequence.elements.len) {
                last_size = sz;
            }
            
        }
        if (binding_size != 0) {
            generate_stack_move(binding_size, 0, last_size, ass, a, point);
            build_binary_op(ass, Add, reg(RSP, sz_64), imm32(binding_size), a, point);
            address_stack_shrink(env, binding_size);
            address_pop_n(num_bindings, env);
        }
        break;
    }
    case SIs:
        generate(*syn.is.val, env, ass, links, a, point);
        break;
    case SInTo:
        generate(*syn.into.val, env, ass, links, a, point);
        break;
    case SOutOf:
        generate(*syn.out_of.val, env, ass, links, a, point);
        break;
    case SDynAlloc:
        generate(*syn.size, env, ass, links, a, point);

        // The size to allocate will sit atop the stack; swap it with 
        // the pointer to free stack memory
        build_unary_op(ass, Pop, reg(RAX, sz_64), a, point);
        build_unary_op(ass, Push, reg(R14, sz_64), a, point);

        // Now, increment pointer to reserve the stack memory
        build_binary_op(ass, Add, reg(R14, sz_64), reg(RAX, sz_64), a, point);

        break;
    case SCheckedType: {
        build_binary_op(ass, Mov, reg(R9, sz_64), imm64((uint64_t)syn.type_val), a, point);
        build_unary_op(ass, Push, reg(R9, sz_64), a, point);
        address_stack_grow(env, pi_size_of(*syn.ptype));
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
            address_stack_grow(env, 2*ADDRESS_SIZE);
            generate(*arg, env, ass, links, a, point);

            address_stack_shrink(env, 3*ADDRESS_SIZE);
            build_unary_op(ass, Pop, reg(R9, sz_64), a, point);
            build_unary_op(ass, Pop, reg(RAX, sz_64), a, point);
            build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);

            build_binary_op(ass, Mov, sib(RAX, RCX, 8, sz_64), reg(R9, sz_64), a, point);

            // Now, incremenet index by 1
            build_binary_op(ass, Add, reg(RCX, sz_64), imm32(1), a, point);
        }
        // Stash RAX
        build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
        address_stack_grow(env, ADDRESS_SIZE);
        generate(*syn.proc_type.return_type, env, ass, links, a, point);
        address_stack_shrink(env, 2*ADDRESS_SIZE);
        build_unary_op(ass, Pop, reg(R9, sz_64), a, point);
        build_unary_op(ass, Pop, reg(RAX, sz_64), a, point);

        // Finally, generate function call to make type
        gen_mk_proc_ty(reg(RAX, sz_64), imm32(syn.proc_type.args.len), reg(RAX, sz_64), reg(R9, sz_64), ass, a, point);
        build_unary_op(ass, Push, reg(RAX, sz_64), a, point);

        break;
    case SStructType:
        // Generate struct type: for each element of the struct type
        // First, malloc enough data for the array:
        generate_tmp_malloc(reg(RAX, sz_64), imm32(syn.struct_type.fields.len * 2 * ADDRESS_SIZE), ass, a, point);
        build_binary_op(ass, Mov, reg(RCX, sz_64), imm32(0), a, point);

        for (size_t i = 0; i < syn.struct_type.fields.len; i++) {
            SymPtrCell field = syn.struct_type.fields.data[i];
            // First, move the field name
            build_binary_op(ass, Mov, sib8(RAX, RCX, 8, 0, sz_64), imm32(field.key), a, point);

            // Second, generate & move the type (note: stash & pop RCX)
            build_unary_op(ass, Push, reg(RCX, sz_64), a, point);
            build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
            address_stack_grow(env, 2*ADDRESS_SIZE);
            generate(*(Syntax*)field.val, env, ass, links, a, point);

            address_stack_shrink(env, 3*ADDRESS_SIZE);
            build_unary_op(ass, Pop, reg(R9, sz_64), a, point);
            build_unary_op(ass, Pop, reg(RAX, sz_64), a, point);
            build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);

            build_binary_op(ass, Mov, sib8(RAX, RCX, 8, 8, sz_64), reg(R9, sz_64), a, point);

            // Now, incremenet index by 2 (to account for struct size!)
            build_binary_op(ass, Add, reg(RCX, sz_64), imm32(2), a, point);
        }

        // Finally, generate function call to make type
        gen_mk_struct_ty(reg(RAX, sz_64), imm32(syn.struct_type.fields.len), reg(RAX, sz_64), ass, a, point);
        build_unary_op(ass, Push, reg(RAX, sz_64), a, point);

        break;
    case SEnumType:
        // Generate enum type: malloc array for enum
        // First, malloc enough data for the array:
        generate_tmp_malloc(reg(RAX, sz_64), imm32(syn.enum_type.variants.len * 2 * ADDRESS_SIZE), ass, a, point);
        build_binary_op(ass, Mov, reg(RCX, sz_64), imm32(0), a, point);

        for (size_t i = 0; i < syn.enum_type.variants.len; i++) {
            SymPtrCell field = syn.enum_type.variants.data[i];
            // First, move the field name
            build_binary_op(ass, Mov, sib8(RAX, RCX, 8, 0, sz_64), imm32(field.key), a, point);

            // Second, generate & variant (note: stash & pop RCX)
            build_unary_op(ass, Push, reg(RCX, sz_64), a, point);
            build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
            address_stack_grow(env, 2*ADDRESS_SIZE);

            PtrArray variant = *(PtrArray*)field.val;
            generate_tmp_malloc(reg(RAX, sz_64), imm32(variant.len * ADDRESS_SIZE), ass, a, point);
            build_binary_op(ass, Mov, reg(RCX, sz_64), imm32(0), a, point);

            for (size_t i = 0; i < variant.len; i++) {

                build_unary_op(ass, Push, reg(RCX, sz_64), a, point);
                build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
                address_stack_grow(env, 2*ADDRESS_SIZE);

                generate(*(Syntax*)variant.data[i], env, ass, links, a, point);

                address_stack_shrink(env, 3*ADDRESS_SIZE);
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
            address_stack_shrink(env, 2*ADDRESS_SIZE);

            build_binary_op(ass, Mov, sib8(RAX, RCX, 8, 8, sz_64), reg(R9, sz_64), a, point);

            // Now, incremenet index by 2 (to account for ptr + symbol)
            build_binary_op(ass, Add, reg(RCX, sz_64), imm32(2), a, point);
        }

        // Finally, generate function call to make type
        gen_mk_enum_ty(reg(RAX, sz_64), syn.enum_type, reg(RAX, sz_64), ass, a, point);
        build_unary_op(ass, Push, reg(RAX, sz_64), a, point);

        break;
    case SResetType:
        generate(*(Syntax*)syn.reset_type.in, env, ass, links, a, point);
        generate(*(Syntax*)syn.reset_type.out, env, ass, links, a, point);
        gen_mk_reset_ty(ass, a, point);
        address_stack_shrink(env, ADDRESS_SIZE);
        break;
    case SDynamicType:
        generate(*(Syntax*)syn.dynamic_type, env, ass, links, a, point);
        gen_mk_dynamic_ty(ass, a, point);
        break;
    case SAllType:
        // Forall type structure: (array symbol) body
        for (size_t i = 0; i < syn.bind_type.bindings.len; i++) {
            address_bind_type(syn.bind_type.bindings.data[i], env);
        }
        generate(*(Syntax*)syn.bind_type.body, env, ass, links, a, point);
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
        generate(*(Syntax*)syn.bind_type.body, env, ass, links, a, point);
        gen_mk_fam_ty(syn.bind_type.bindings, ass, a, point);
        address_pop_n(syn.bind_type.bindings.len, env);
        break;
    case SDistinctType:
        generate(*(Syntax*)syn.distinct_type, env, ass, links, a, point);
        gen_mk_distinct_ty(ass, a, point);
        break;
    case SOpaqueType:
        generate(*(Syntax*)syn.opaque_type, env, ass, links, a, point);
        gen_mk_opaque_ty(ass, a, point);
        break;
    case STraitType:
        // Generate trait type: first bind relevant variables
        for (size_t i = 0; i < syn.bind_type.bindings.len; i++) {
            address_bind_type(syn.bind_type.bindings.data[i], env);
        }

        // First, malloc enough data for the array:
        generate_tmp_malloc(reg(RAX, sz_64), imm32(syn.trait.fields.len * 2 * ADDRESS_SIZE), ass, a, point);
        build_binary_op(ass, Mov, reg(RCX, sz_64), imm32(0), a, point);


        for (size_t i = 0; i < syn.trait.fields.len; i++) {
            SymPtrCell field = syn.trait.fields.data[i];
            // First, move the field name
            build_binary_op(ass, Mov, sib8(RAX, RCX, 8, 0, sz_64), imm32(field.key), a, point);

            // Second, generate & move the type (note: stash & pop RCX)
            build_unary_op(ass, Push, reg(RCX, sz_64), a, point);
            build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
            address_stack_grow(env, 2*ADDRESS_SIZE);
            generate(*(Syntax*)field.val, env, ass, links, a, point);

            address_stack_shrink(env, 3*ADDRESS_SIZE);
            build_unary_op(ass, Pop, reg(R9, sz_64), a, point);
            build_unary_op(ass, Pop, reg(RAX, sz_64), a, point);
            build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);

            build_binary_op(ass, Mov, sib8(RAX, RCX, 8, 8, sz_64), reg(R9, sz_64), a, point);

            // Now, incremenet index by 2 (to account for trait size!)
            build_binary_op(ass, Add, reg(RCX, sz_64), imm32(2), a, point);
        }

        // Finally, generate function call to make type
        gen_mk_trait_ty(syn.trait.vars, reg(RAX, sz_64), imm32(syn.trait.fields.len), reg(RAX, sz_64), ass, a, point);
        build_unary_op(ass, Push, reg(RAX, sz_64), a, point);

        address_pop_n(syn.trait.vars.len, env);
        break;
    default: {
        panic(mv_string("Invalid abstract supplied to monomorphic codegen."));
    }
    }
}

void generate_stack_move(size_t dest_stack_offset, size_t src_stack_offset, size_t size, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // first, assert that size_t is divisible by 8 ( we use rax for copies )
    if (size % 8 != 0)  {
        throw_error(point, mv_string("Error in generate_stack_copy: expected copy size to be divisible by 8"));
    };

    if ((dest_stack_offset + size) > 255 || (src_stack_offset + size) > 255)  {
        throw_error(point, mv_string("Error in generate_stack_copy: offsets + size must be smaller than 255!"));
    };

    for (size_t i = 0; i < size / 8; i++) {
        build_binary_op(ass, Mov, reg(RAX, sz_64), rref8(RSP, src_stack_offset + (i * 8) , sz_64), a, point);
        build_binary_op(ass, Mov, rref8(RSP, dest_stack_offset + (i * 8), sz_64), reg(RAX, sz_64), a, point);
    }
}

size_t calc_variant_size(PtrArray* types) {
    size_t total = sizeof(uint64_t);
    for (size_t i = 0; i < types->len; i++) {
        total += pi_size_of(*(PiType*)types->data[i]);
    }
    return total;
}
