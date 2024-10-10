#include "platform/signals.h"

#include "pico/codegen/internal.h"
#include "pico/codegen/polymorphic.h"
#include "pico/binding/address_env.h"

// Implementation details
AsmResult generate_polymorphic_i(Syntax syn, AddressEnv* env, Assembler* ass, SymSArrAMap* links, Allocator* a);

AsmResult generate_polymorphic(Syntax syn, AddressEnv* env, Assembler* ass, SymSArrAMap* links, Allocator* a) {
    return generate_polymorphic_i(syn, env, ass, links, a);
}

AsmResult generate_polymorphic_proc(Syntax syn, AddressEnv* env, Assembler* ass, SymSArrAMap* links, Allocator* a) {
}

AsmResult generate_polymorphic_i(Syntax syn, AddressEnv* env, Assembler* ass, SymSArrAMap* links, Allocator* a) {
    AsmResult out;

    switch (syn.type) {
    case SLitI64: {
        // Does it fit into 32 bits?
        if (syn.lit_i64 < 0x80000000 && syn.lit_i64 > -80000001) {
            int32_t immediate = syn.lit_i64;
            out = build_unary_op(ass, Push, imm32(immediate), a);
        } else {
            out.type = Err;
            out.error_message = mk_string("Limitation: Literals must fit into less than 64 bits.", a);
        }
        break;
    }
    case SLitBool: {
        // TODO: squash to smallest size (8 bits)
        int32_t immediate = syn.lit_i64;
        out = build_unary_op(ass, Push, imm32(immediate), a);
        break;
    }
    case SVariable: {
        // Lookup the variable in the assembly envionrment
        AddressEntry e = address_env_lookup(syn.variable, env);
        switch (e.type) {
        case ALocalDirect:
            out = build_unary_op(ass, Push, rref(RBP, e.stack_offset), a);
            break;
        case ALocalIndirect:
            panic(mv_string("Internal Error: Tried to generate monomorphic code for local indirect variable access."));
            break;
        case AGlobal:
            // Use RAX as a temp
            // Note: casting void* to uint64_t only works for 64-bit systems...
            if (syn.ptype->sort == TProc) {
                out = build_binary_op(ass, Mov, reg(RBX), imm64((uint64_t)e.value), a);
                if (out.type == Err) return out;
                backlink_global(syn.variable, out.backlink, links, a);

                out = build_unary_op(ass, Push, reg(RBX), a);
            } else if (syn.ptype->sort == TPrim) {
                out = build_binary_op(ass, Mov, reg(RCX), imm64((uint64_t)e.value), a);
                if (out.type == Err) return out;
                backlink_global(syn.variable, out.backlink, links, a);
                out = build_binary_op(ass, Mov, reg(RBX), rref(RCX, 0), a);
                if (out.type == Err) return out;
                out = build_unary_op(ass, Push, reg(RBX), a);
            } else if (syn.ptype->sort == TKind) {
                out = build_binary_op(ass, Mov, reg(RCX), imm64((uint64_t)e.value), a);
                if (out.type == Err) return out;
                backlink_global(syn.variable, out.backlink, links, a);
            } else if (syn.ptype->sort == TStruct || syn.ptype->sort == TEnum) {
                // This is a global variable, and therefore has a known
                // monomorphic type
                size_t value_size = pi_mono_size_of(*syn.ptype);
                out = build_binary_op(ass, Mov, reg(RCX), imm64((uint64_t)e.value), a);
                if (out.type == Err) return out;
                backlink_global(syn.variable, out.backlink, links, a);

                // Allocate space on the stack for composite type (struct/enum)
                out = build_binary_op(ass, Sub, reg(RSP), imm32(value_size), a);
                if (out.type == Err) return out;

                // copy
                out = generate_monomorphic_copy(RSP, RCX, value_size, ass, a);
                if (out.type == Err) return out;

            } else {
                out.type = Err;
                out.error_message = mv_string("Codegen: Global has unsupported sort: must be Primitive or Proc");
            }
            break;
        case ANotFound:
            out.type = Err;
            String* sym = symbol_to_string(syn.variable);
            String msg = mv_string("Couldn't find variable during codegen: ");
            out.error_message = string_cat(msg, *sym, a);
            break;
        case ATooManyLocals:
            out.type = Err;
            out.error_message = mk_string("Too Many Local variables!", a);
            break;
        }
        break;
    }
    case SProcedure: {
        panic(mv_string("Internal error: cannot generate procedure inside polymorphic code"));
    }
    case SApplication: {
        // Generate the arguments
        for (size_t i = 0; i < syn.application.args.len; i++) {
            Syntax* arg = (Syntax*) syn.application.args.data[i];
            out = generate_polymorphic(*arg, env, ass, links, a);
            if (out.type == Err) return out;
        }

        // This will push a function pointer onto the stack
        out = generate_polymorphic(*syn.application.function, env, ass, links, a);
        if (out.type == Err) return out; 
        
        // Regular Function Call
        // Pop the function into RCX; call the function
        out = build_unary_op(ass, Pop, reg(RCX), a);
        if (out.type == Err) return out;
        out = build_unary_op(ass, Call, reg(RCX), a);
        break;
    }
    case SStructure: {
        panic(mv_string("Not implemented: structure in forall."));
        /*
        // For structures, we have to be careful - this is because the order in
        // which arguments are evaluated is not necessarily the order in which
        // arguments are inserted into the structure.

        // Step 1: Make room on the stack for our struct
        size_t struct_size = pi_runtime_size_of(*syn.ptype);
        out = build_binary_op(ass, Sub, reg(RSP), imm32(struct_size), a);
        if (out.type == Err) return out;

        // Step 2: evaluate each element/variable binding
        for (size_t i = 0; i < syn.structure.fields.len; i++) {
            out = generate_polymorphic(*(Syntax*)syn.structure.fields.data[i].val, env, ass, links, a);
            if (out.type == Err) return out;
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
                    src_offset += pi_runtime_size_of(*((Syntax*)syn.structure.fields.data[j].val)->ptype); 
                } else {
                    out.type = Err;
                    out.error_message = mv_string("Error code-generating for structure: field not found.");
                    return out;
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
            size_t field_size = pi_runtime_size_of(*(PiType*)syn.ptype->structure.fields.data[i].val);

            // Now, move the data.
            out = generate_runtime_stack_move(dest_stack_offset, src_stack_offset, field_size, ass, a);
            if (out.type == Err) return out;

            // Compute dest_offset for next loop
            dest_offset += pi_runtime_size_of(*(PiType*)syn.ptype->structure.fields.data[i].val);
        }
        // Remove the space occupied by the temporary values 
        out = build_binary_op(ass, Add, reg(RSP), imm32(struct_size), a);

        address_stack_grow(env, pi_runtime_size_of(*syn.ptype));
        break;
        */
    }
    case SProjector: {
        panic(mv_string("Not implemented: projector in forall."));
        /*
        // First, allocate space on the stack for the value
        size_t out_sz = pi_runtime_size_of(*syn.ptype);
        out = build_binary_op(ass, Sub, reg(RSP), imm32(out_sz), a);
        if (out.type == Err) return out;

        // Second, generate the structure object
        out = generate(*syn.projector.val, env, ass, links, a);

        // Now, copy the structure to the destination
        // for this, we need the struct size + offset of field in the struct
        size_t struct_sz = pi_runtime_size_of(*syn.projector.val->ptype);
        size_t offset = 0;
        for (size_t i = 0; i < syn.projector.val->ptype->structure.fields.len; i++) {
            if (syn.projector.val->ptype->structure.fields.data[i].key == syn.projector.field)
                break;
            offset += pi_runtime_size_of(*(PiType*)syn.projector.val->ptype->structure.fields.data[i].val);
        }

        out = generate_stack_move(struct_sz + out_sz - 0x8, offset, out_sz, ass, a);
        if (out.type == Err) return out;
        // now, remove the original struct from the stack
        out = build_binary_op(ass, Add, reg(RSP), imm32(struct_sz), a);

        address_stack_shrink(env, struct_sz);
        address_stack_grow(env, out_sz);
        break;
        */
    }
    case SConstructor: {
        panic(mv_string("Not implemented: constructor in forall."));
        /*
        PiType* enum_type = syn.constructor.enum_type->type_val;
        size_t enum_size = pi_runtime_size_of(*enum_type);
        size_t variant_size = calc_variant_size(enum_type->enumeration.variants.data[syn.variant.tag].val);

        out = build_binary_op(ass, Sub, reg(RSP), imm32(enum_size - variant_size), a);
        if (out.type == Err) return out;
        out = build_unary_op(ass, Push, imm32(syn.constructor.tag), a);
        if (out.type == Err) return out;

        address_stack_grow(env, enum_size);
        break;
        */
    }
    case SVariant: {
        panic(mv_string("Not implemented: variant in forall."));
        /*
        const size_t tag_size = sizeof(uint64_t);
        PiType* enum_type = syn.variant.enum_type->type_val;
        size_t enum_size = pi_runtime_size_of(*enum_type);
        size_t variant_size = calc_variant_size(enum_type->enumeration.variants.data[syn.variant.tag].val);

        // Make space to fit the (final) variant
        out = build_binary_op(ass, Sub, reg(RSP), imm32(enum_size), a);
        if (out.type == Err) return out;

        // Set the tag
        out = build_binary_op(ass, Mov, rref(RSP, 0), imm32(syn.constructor.tag), a);
        if (out.type == Err) return out;

        // Generate each argument
        for (size_t i = 0; i < syn.variant.args.len; i++) {
            out = generate(*(Syntax*)syn.variant.args.data[i], env, ass, links, a);
            if (out.type == Err) return out;
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
            
            size_t field_size = pi_runtime_size_of(*(PiType*)args.data[syn.variant.args.len - (i + 1)]);
            src_stack_offset -= field_size;

            // Now, move the data.
            out = generate_stack_move(dest_stack_offset, src_stack_offset, field_size, ass, a);
            if (out.type == Err) return out;

            // Now, increase the amount of data we have used
            dest_stack_offset += field_size;
        }

        // Remove the space occupied by the temporary values 
        out = build_binary_op(ass, Add, reg(RSP), imm32(variant_size - tag_size), a);

        // Grow the stack to account for the difference in enum & variant sizes
        address_stack_grow(env, enum_size - variant_size);
        break;
        */
    }
    case SMatch: {
        panic(mv_string("Not implemented: match in forall."));
        /*
        // Generate code for the value
        Syntax* match_value = syn.match.val;
        size_t enum_size = pi_runtime_size_of(*match_value->ptype);
        size_t out_size = pi_runtime_size_of(*syn.ptype);

        out = generate(*match_value, env, ass, links, a);
        if (out.type == Err) return out;

        SizeArray back_positions = mk_size_array(syn.match.clauses.len, a);
        PtrArray back_refs = mk_ptr_array(syn.match.clauses.len, a);

        // For each pattern match, generate two things 
        // 1. A comparison/check that the pattern has been matched
        // 2. A jump to the relevant location
        for (size_t i = 0; i < syn.match.clauses.len; i++) {
            SynClause clause = *(SynClause*)syn.match.clauses.data[i];
            out = build_binary_op(ass, Cmp, rref(RSP, 0), imm32(clause.tag), a);
            if (out.type == Err) return out;
            out = build_unary_op(ass, JE, imm8(0), a);
            if (out.type == Err) return out;
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
                out.type = Err;
                out.error_message = mk_string("Jump in match too large", a);
                return out;
            } 

            *branch_ref = (uint8_t)(body_pos - branch_pos);

            SynClause clause = *(SynClause*)syn.match.clauses.data[i];
            PtrArray variant_types = *(PtrArray*)match_value->ptype->enumeration.variants.data[clause.tag].val; 

            // Bind Clause Vars 
            SymSizeAssoc arg_sizes = mk_sym_size_assoc(variant_types.len, a);
            for (size_t i = 0; i < variant_types.len; i++) {
                sym_size_bind(clause.vars.data[i]
                              , pi_mono_size_of(*(PiType*)variant_types.data[i])
                              , &arg_sizes);
            }
            address_bind_enum_vars(arg_sizes, env, a);

            out = generate(*clause.body, env, ass, links, a);
            if (out.type == Err) return out;

            // Generate jump to end of false branch to be backlinked later
            out = build_unary_op(ass, JMP, imm8(0), a);
            if (out.type == Err) return out;
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
                out.type = Err;
                out.error_message = mk_string("Jump in match too large", a);
                return out;
            } 

            *body_ref = (uint8_t)(curr_pos - body_pos);
        }


        out = generate_stack_move(out_size + (enum_size - out_size), 0, out_size, ass, a);
        if (out.type == Err) return out;

        out = build_binary_op(ass, Add, reg(RSP), imm32(enum_size), a);

        address_stack_shrink(env, enum_size);
        address_stack_grow(env, out_size);
        break;
        */
    }
    case SLet:
        panic(mv_string("Not implemented: let in forall."));
        out = (AsmResult) {
            .type = Err,
            .error_message = mk_string("No assembler implemented for Let", a)
        };
        break;
    case SIf: {
        panic(mv_string("Not implemented: if in forall."));
        /*
        // generate the condition
        out = generate(*syn.if_expr.condition, env, ass, links, a);
        if (out.type == Err) return out;

        // Pop the bool into RBX; compare with 0
        out = build_unary_op(ass, Pop, reg(RBX), a);
        if (out.type == Err) return out;
        out = build_binary_op(ass, Cmp, reg(RBX), imm32(0), a);
        if (out.type == Err) return out;

        // ---------- CONDITIONAL JUMP ----------
        // compare the value to 0
        // jump to false branch if equal to 0 -- the immediate 8 is a placeholder
        out = build_unary_op(ass, JE, imm8(0), a);
        if (out.type == Err) return out;
        size_t start_pos = get_pos(ass);

        uint8_t* jmp_loc = get_instructions(ass).data + out.backlink;

        // ---------- TRUE BRANCH ----------
        // now, generate the code to run (if true)
        out = generate(*syn.if_expr.true_branch, env, ass, links, a);
        if (out.type == Err) return out;

        // Generate jump to end of false branch to be backlinked later
        out = build_unary_op(ass, JMP, imm8(0), a);
        if (out.type == Err) return out;

        // calc backlink offset
        size_t end_pos = get_pos(ass);
        if (end_pos - start_pos > INT8_MAX) {
            out.type = Err;
            out.error_message = mk_string("Jump in conditional too large", a);
            return out;
        } 

        // backlink
        *jmp_loc = (end_pos - start_pos);
        jmp_loc = get_instructions(ass).data + out.backlink;
        start_pos = get_pos(ass);


        // ---------- FALSE BRANCH ----------
        // Generate code for the false branch
        out = generate(*syn.if_expr.false_branch, env, ass, links, a);
        if (out.type == Err) return out;

        // calc backlink offset
        end_pos = get_pos(ass);
        if (end_pos - start_pos > INT8_MAX) {
            out.type = Err;
            out.error_message = mk_string("Jump in conditional too large", a);
            return out;
        } 
        *jmp_loc = (uint8_t)(end_pos - start_pos);
        break;
        */
    }
    case SCheckedType: {
        out = build_binary_op(ass, Mov, reg(RBX), imm64((uint64_t)syn.type_val), a);
        if (out.type == Err) return out;
        out = build_unary_op(ass, Push, reg(RBX), a);
        break;
    }
    default: {
        out.type = Err;
        out.error_message = mv_string("Unrecognized abstract term in codegen.");
    }
    }

    return out;
}