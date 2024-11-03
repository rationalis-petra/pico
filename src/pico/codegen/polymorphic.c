#include <string.h>

#include "platform/signals.h"
#include "platform/machine_info.h"
#include "platform/machine_info.h"

#include "pico/codegen/polymorphic.h"
#include "pico/codegen/internal.h"
#include "pico/binding/address_env.h"

// Implementation details
void generate_polymorphic_i(Syntax syn, AddressEnv* env, Assembler* ass, LinkData* links, Allocator* a, ErrorPoint* point);
void generate_size_of(Regname dest, PiType* type, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_poly_stack_move(Location dest, Location src, Location size, Assembler* ass, Allocator* a, ErrorPoint* point);

void generate_polymorphic(SymbolArray types, Syntax syn, AddressEnv* env, Assembler* ass, LinkData* links, Allocator* a, ErrorPoint* point) {
    SymbolArray vars;
    Syntax body;
    if (syn.type == SProcedure) {
        vars = mk_u64_array(syn.procedure.args.len, a);
        for (size_t i = 0; i < syn.procedure.args.len; i++) {
            push_u64(syn.procedure.args.data[i].key, &vars);
        }
        body = *syn.procedure.body;
    } else {
        vars = mk_u64_array(0, a);
        body = syn;
    }

    // Polymorphic ABI
    // RBP+  | types
    // RBP+  | arg-ptrs
    // RBP+8 | Space for return address
    // RBP   | old RBP
    // RBP-  | args
    // RSP → Return address

    // Prelude: copy return address to RBP+8
    build_unary_op(ass, Pop, reg(R9), a, point);
    build_binary_op(ass, Mov, rref8(RBP, 8), reg(R9), a, point);

    address_start_poly(types, vars, env, a);

    generate_polymorphic_i(body, env, ass, links, a, point);

    // Codegen function postlude:
    // Stack now looks like:
    // 
    // RBP+ | types + argument offfsets (to discard)
    // RBP+ | return address
    // RBP  | OLD RBP
    // RBP- | args
    // RSP  | output value 
    // Postlude:
    // 1. Push Old RBP & Return Address
    // 2. move output value to start of types
    // 3. Pop Old RBP & Return Address
    // 5. move RSP to end of value & restore old RBP
    // 4. push return address
    // 6. return

    // 1. Stash old RBP & return address
    build_unary_op(ass, Push, rref8(RBP, 8), a, point); 
    build_unary_op(ass, Push, rref8(RBP, 0), a, point); 

    // 2. Move output value to start of types. We do this via a poly stack move,
    // which needs three pieces of info: 
    // 2.1 : The size of the data to be copid: Relatively simple.
    generate_size_of(RAX, body.ptype, env, ass, a, point);

    // 2.2 : The destination address. This is offset + RBP, with offset calculated as:
    //              OLD RBP+RET ADDR | accounts for types       | account for vars
    size_t offset = 2 * ADDRESS_SIZE + ADDRESS_SIZE * types.len + ADDRESS_SIZE * vars.len;
    build_binary_op(ass, Mov, reg(R9), reg(RBP), a, point);
    build_binary_op(ass, Add, reg(R9), imm32(offset), a, point);
    build_binary_op(ass, Sub, reg(R9), reg(RAX), a, point);

    // 2.3: The source address: this is just RSP + 2*ADDRESS_SIZE, as we pushed
    //      the old RBP + Return address 
    build_binary_op(ass, Mov, reg(RDX), reg(RSP), a, point);
    build_binary_op(ass, Add, reg(RDX), imm8(2 * ADDRESS_SIZE), a, point);

    // Note: we push R9 (the new head of stack) so it can be used
    build_unary_op(ass, Push, reg(R9), a, point); 
    
    generate_poly_stack_move(reg(R9), reg(RDX), reg(RAX), ass, a, point);

    // 3. Pop old RBP and return address
    //    Note that these are currently BELOW the top of the stack!
    build_unary_op(ass, Pop, reg(RDX), a, point);
    build_unary_op(ass, Pop, reg(RBP), a, point);
    build_unary_op(ass, Pop, reg(RCX), a, point);

    // 4. Move RSP to end of value & restore old RBP
    build_binary_op(ass, Mov, reg(RSP), reg(RDX), a, point);

    // 5. push return address
    build_unary_op(ass, Push, reg(RCX), a, point); 
    build_nullary_op(ass, Ret, a, point);

    address_end_poly(env, a);
}

void generate_polymorphic_i(Syntax syn, AddressEnv* env, Assembler* ass, LinkData* links, Allocator* a, ErrorPoint* point) {
    switch (syn.type) {
    case SLitUntypedIntegral: {
        panic(mv_string("Cannot generate polymorphic code for untyped integral."));
    }
    case SLitTypedIntegral: {
        // Does it fit into 32 bits?
        if (syn.integral.value < 0x80000000 && syn.integral.value > -80000001) {
            int32_t immediate = syn.integral.value;
            build_unary_op(ass, Push, imm32(immediate), a, point);
        } else {
            throw_error(point, mk_string("Limitation: Literals must fit into less than 64 bits.", a));
        }
        break;
    }
    case SLitBool: {
        int8_t immediate = (int8_t) syn.boolean;
        build_unary_op(ass, Push, imm8(immediate), a, point);
        break;
    }
    case SVariable: {
        // Lookup the variable in the assembly envionrment
        AddressEntry e = address_env_lookup(syn.variable, env);
        switch (e.type) {
        case ALocalDirect:
            build_unary_op(ass, Push, rref8(RBP, e.stack_offset), a, point);
            break;
        case ALocalIndirect:
            // First, we need the size of the variable & allocate space for it on the stack
            generate_size_of(RAX, syn.ptype, env, ass, a, point);
            build_binary_op(ass, Sub, reg(RSP), reg(RAX), a, point);

            // Then, find the location of the variable on the stack 
            // *(RBP + stack offset) = offset2
            // RBP + offset2 = dest (stored here in R9)
            build_binary_op(ass, Mov, reg(R9), rref8(RBP, e.stack_offset), a, point);
            build_binary_op(ass, Add, reg(R9), reg(RBP), a, point); // 
            //build_binary_op(ass, Mov, reg(R9), rref8(R9, 0), a, point);
            //build_binary_op(ass, Add, reg(R9), reg(RBP), a, point);

            generate_poly_stack_move(reg(RSP), reg(R9), reg(RAX), ass, a, point);
            break;
        case AGlobal:
            // Use RAX as a temp
            // Note: casting void* to uint64_t only works for 64-bit systems...
            if (syn.ptype->sort == TProc) {
                AsmResult out = build_binary_op(ass, Mov, reg(R9), imm64((uint64_t)e.value), a, point);
                backlink_global(syn.variable, out.backlink, links, a);

                out = build_unary_op(ass, Push, reg(R9), a, point);
            } else if (syn.ptype->sort == TPrim) {
                AsmResult out = build_binary_op(ass, Mov, reg(RCX), imm64((uint64_t)e.value), a, point);
                backlink_global(syn.variable, out.backlink, links, a);
                build_binary_op(ass, Mov, reg(R9), rref8(RCX, 0), a, point);
                build_unary_op(ass, Push, reg(R9), a, point);
            } else if (syn.ptype->sort == TKind) {
                AsmResult out = build_binary_op(ass, Mov, reg(RCX), imm64((uint64_t)e.value), a, point);
                backlink_global(syn.variable, out.backlink, links, a);
            } else if (syn.ptype->sort == TStruct || syn.ptype->sort == TEnum) {
                // This is a global variable, and therefore has a known
                // monomorphic type
                size_t value_size = pi_mono_size_of(*syn.ptype);
                AsmResult out = build_binary_op(ass, Mov, reg(RCX), imm64((uint64_t)e.value), a, point);
                backlink_global(syn.variable, out.backlink, links, a);

                // Allocate space on the stack for composite type (struct/enum)
                out = build_binary_op(ass, Sub, reg(RSP), imm32(value_size), a, point);

                // copy
                generate_monomorphic_copy(RSP, RCX, value_size, ass, a, point);

            } else {
                throw_error(point, mv_string("Codegen: Global has unsupported sort: must be Primitive or Proc"));
            }
            break;
        case ANotFound: {
            String* sym = symbol_to_string(syn.variable);
            String msg = mv_string("Couldn't find variable during codegen: ");
            throw_error(point, string_cat(msg, *sym, a));
            break;
        }
        case ATooManyLocals:
            throw_error(point, mk_string("Too Many Local variables!", a));
            break;
        }
        break;
    }
    case SAll: {
        throw_error(point, mv_string("Internal error: cannot generate procedure all inside polymorphic code"));
    }
    case SProcedure: {
        throw_error(point, mv_string("Internal error: cannot generate procedure inside polymorphic code"));
    }
    case SApplication: {
        // Generate the arguments
        for (size_t i = 0; i < syn.application.args.len; i++) {
            Syntax* arg = (Syntax*) syn.application.args.data[i];
            generate_polymorphic_i(*arg, env, ass, links, a, point);
        }

        // This will push a function pointer onto the stack
        generate_polymorphic_i(*syn.application.function, env, ass, links, a, point);
        
        // Regular Function Call
        // Pop the function into RCX; call the function
        build_unary_op(ass, Pop, reg(RCX), a, point);
        build_unary_op(ass, Call, reg(RCX), a, point);
        break;
    }
    case SStructure: {
        throw_error(point, mv_string("Not implemented: structure in forall."));
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
        throw_error(point, mv_string("Not implemented: projector in forall."));
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
        throw_error(point, mv_string("Not implemented: constructor in forall."));
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
        throw_error(point, mv_string("Not implemented: variant in forall."));
        /*
        const size_t tag_size = sizeof(uint64_t);
        PiType* enum_type = syn.variant.enum_type->type_val;
        size_t enum_size = pi_runtime_size_of(*enum_type);
        size_t variant_size = calc_variant_size(enum_type->enumeration.variants.data[syn.variant.tag].val);

        // Make space to fit the (final) variant
        out = build_binary_op(ass, Sub, reg(RSP), imm32(enum_size), a);
        if (out.type == Err) return out;

        // Set the tag
        out = build_binary_op(ass, Mov, rref8(RSP, 0), imm32(syn.constructor.tag), a);
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
        throw_error(point, mv_string("Not implemented: match in forall."));
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
            out = build_binary_op(ass, Cmp, rref8(RSP, 0), imm32(clause.tag), a);
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
    case SLet: {
        throw_error(point, mv_string("Not implemented: let in forall."));
        break;
    }
    case SIf: {
        throw_error(point, mv_string("Not implemented: if in forall."));
        /*
        // generate the condition
        out = generate(*syn.if_expr.condition, env, ass, links, a);
        if (out.type == Err) return out;

        // Pop the bool into R9; compare with 0
        out = build_unary_op(ass, Pop, reg(R9), a);
        if (out.type == Err) return out;
        out = build_binary_op(ass, Cmp, reg(R9), imm32(0), a);
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
        build_binary_op(ass, Mov, reg(R9), imm64((uint64_t)syn.type_val), a, point);
        build_unary_op(ass, Push, reg(R9), a, point);
        break;
    }
    default: {
        panic(mv_string("Invalid abstract term in polymorphic codegen."));
    }
    }
}

void generate_size_of(Regname dest, PiType* type, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point) {
    switch (type->sort) {
    case TVar: {
        AddressEntry e = address_env_lookup(type->var, env);
        switch (e.type) {
        case ALocalDirect:
            build_binary_op(ass, Mov, reg(dest), rref8(RBP, e.stack_offset), a, point);
            break;
        case ALocalIndirect:
            panic(mv_string("cannot generate code for local indirect."));
            break;
        break;
        case AGlobal:
            panic(mv_string("Unexpected type variable sort: Global."));
            break;
        case ANotFound: {
            panic(mv_string("Type Variable not found during codegen."));
            break;
        }
        case ATooManyLocals: {
            throw_error(point, mk_string("Too Many Local variables!", a));
            break;
        }
        }
        break;
    }
    default:
        panic(mv_string("Unrecognized type to generate_size_of."));
    }
}

void generate_poly_stack_move(Location dest, Location src, Location size, Assembler* ass, Allocator* a, ErrorPoint* point) {

#if ABI == SYSTEM_V_64
    // memcpy (dest = rdi, src = rsi, size = rdx)
    // copy size into RDX
    build_binary_op(ass, Mov, reg(RDI), dest, a, point);
    build_binary_op(ass, Mov, reg(RSI), src, a, point);
    build_binary_op(ass, Mov, reg(RDX), size, a, point);

#elif ABI == WIN_64
    // memcpy (dest = rcx, src = rdx, size = r8)
    build_binary_op(ass, Mov, reg(RCX), dest, a, point);
    build_binary_op(ass, Mov, reg(RDX), src, a, point);
    build_binary_op(ass, Mov, reg(R8), size, a, point);
    build_binary_op(ass, Sub, reg(RSP), imm32(32), a, point);
#else
#error "Unknown calling convention"
#endif

    // copy memcpy into RCX & call
    build_binary_op(ass, Mov, reg(RAX), imm64((uint64_t)&memcpy), a, point);
    build_unary_op(ass, Call, reg(RAX), a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP), imm32(32), a, point);
#endif
}
