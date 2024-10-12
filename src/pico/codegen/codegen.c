#include "platform/signals.h"
#include "platform/calling_convention.h"

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

// Implementation details
void generate(Syntax syn, AddressEnv* env, Assembler* ass, SymSArrAMap* links, Allocator* a, ErrorPoint* point);
void generate_stack_move(size_t dest_offset, size_t src_offset, size_t size, Assembler* ass, Allocator* a, ErrorPoint* point);
void get_variant_fun(size_t idx, size_t vsize, size_t esize, uint64_t* out, ErrorPoint* point);
size_t calc_variant_size(PtrArray* types);

GenResult generate_expr(Syntax* syn, Environment* env, Assembler* ass, Allocator* a, ErrorPoint* point) {
    AddressEnv* a_env = mk_address_env(env, NULL, a);
    SymSArrAMap backlinks = mk_sym_sarr_amap(16, a);
    generate(*syn, a_env, ass, &backlinks, a, point);
    delete_address_env(a_env, a);

    GenResult out;
    out.backlinks = backlinks;

    return out;
}

GenResult generate_toplevel(TopLevel top, Environment* env, Assembler* ass, Allocator* a, ErrorPoint* point) {
    SymSArrAMap backlinks = mk_sym_sarr_amap(32, a);

    switch(top.type) {
    case TLDef: {
        AddressEnv* a_env = mk_address_env(env, &top.def.bind, a);
        generate(*top.def.value, a_env, ass, &backlinks, a, point);
        delete_address_env(a_env, a);
        break;
    }
    case TLExpr: {
        AddressEnv* a_env = mk_address_env(env, NULL, a);
        generate(*top.expr, a_env, ass, &backlinks, a, point);
        delete_address_env(a_env, a);
        break;
    }
    }

    return (GenResult) {
        .backlinks = backlinks,
    };
}

void generate(Syntax syn, AddressEnv* env, Assembler* ass, SymSArrAMap* links, Allocator* a, ErrorPoint* point) {
    switch (syn.type) {
    case SLitI64: {
        // Does it fit into 32 bits?
        if (syn.lit_i64 < 0x80000000) {
            int32_t immediate = syn.lit_i64;
            build_unary_op(ass, Push, imm32(immediate), a, point);
        } else {
            throw_error(point, mk_string("literals must fit into less than 64 bits", a));
        }
        address_stack_grow(env, pi_size_of(*syn.ptype));
        // push literal onto stack
        break;
    }
    case SLitBool: {
        // TODO: squash to smallest size (8 bits)
        int32_t immediate = syn.lit_i64;
        build_unary_op(ass, Push, imm32(immediate), a, point);
        address_stack_grow(env, pi_size_of(*syn.ptype));
        break;
    }
    case SVariable: {
        // Lookup the variable in the assembly envionrment
        AddressEntry e = address_env_lookup(syn.variable, env);
        switch (e.type) {
        case ALocalDirect:
            build_unary_op(ass, Push, rref(RBP, e.stack_offset), a, point);
            break;
        case ALocalIndirect:
            panic(mv_string("cannot generate code for local direct"));
            break;
        case AGlobal:
            // Use RAX as a temp
            // Note: casting void* to uint64_t only works for 64-bit systems...
            if (syn.ptype->sort == TProc) {
                AsmResult out = build_binary_op(ass, Mov, reg(RBX), imm64((uint64_t)e.value), a, point);
                backlink_global(syn.variable, out.backlink, links, a);

                build_unary_op(ass, Push, reg(RBX), a, point);
            } else if (syn.ptype->sort == TPrim) {
                AsmResult out = build_binary_op(ass, Mov, reg(RCX), imm64((uint64_t)e.value), a, point);
                backlink_global(syn.variable, out.backlink, links, a);
                build_binary_op(ass, Mov, reg(RBX), rref(RCX, 0), a, point);
                build_unary_op(ass, Push, reg(RBX), a, point);
            } else if (syn.ptype->sort == TKind) {
                AsmResult out = build_binary_op(ass, Mov, reg(RCX), imm64((uint64_t)e.value), a, point);
                backlink_global(syn.variable, out.backlink, links, a);
                build_unary_op(ass, Push, reg(RCX), a, point);
            } else if (syn.ptype->sort == TStruct || syn.ptype->sort == TEnum) {
                size_t value_size = pi_size_of(*syn.ptype);
                AsmResult out = build_binary_op(ass, Mov, reg(RCX), imm64((uint64_t)e.value), a, point);
                backlink_global(syn.variable, out.backlink, links, a);

                // Allocate space on the stack for composite type (struct/enum)
                build_binary_op(ass, Sub, reg(RSP), imm32(value_size), a, point);

                // Copy
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
        case ATooManyLocals: {
            throw_error(point, mk_string("Too Many Local variables!", a));
            break;
        }
            address_stack_grow(env, pi_size_of(*syn.ptype));
            break;
        }
        break;
    }
    case SAll: {
        generate_polymorphic(syn.all.args, *syn.all.body, env, ass, links, a, point);
        break;
    }
    case SProcedure: {
        // Codegen function setup
        build_unary_op(ass, Push, reg(RBP), a, point);
        build_binary_op(ass, Mov, reg(RBP), reg(RSP), a, point);

        // Codegen Procedure Body 
        SymSizeAssoc arg_sizes = mk_sym_size_assoc(syn.procedure.args.len, a);
        for (size_t i = 0; i < syn.procedure.args.len; i++) {
            sym_size_bind(syn.procedure.args.data[i].key
                         , pi_size_of(*(PiType*)syn.ptype->proc.args.data[i])
                         , &arg_sizes);
        }

        address_start_proc(arg_sizes, env, a);
        generate(*syn.procedure.body, env, ass, links, a, point);
        address_end_proc(env, a);

        // Codegen function teardown:
        // + store output in RAX
        // + restore rbp to original value
        // + pop all arguments from stack
        // + store return address in RCX
        // + push value (RAX)
        // + push return address (RCX)
        // + return ()
        // TODO: ensure functions work in the context of a composite (large) value  

        // storage of function output 
        build_unary_op(ass, Pop, reg(RAX), a, point);
        build_unary_op(ass, Pop, reg(RBP), a, point);
        // storage of return address
        build_unary_op(ass, Pop, reg(RBX), a, point);
        
        // pop args
        build_binary_op(ass, Add, reg(RSP), imm32(syn.procedure.args.len * 8), a, point);

        // push value
        build_unary_op(ass, Push, reg(RAX), a, point);
        // push return address 
        build_unary_op(ass, Push, reg(RBX), a, point);
        build_nullary_op(ass, Ret, a, point);
        break;
    }
    case SApplication: {
        // Generate the arguments
        size_t args_size = 0;
        for (size_t i = 0; i < syn.application.args.len; i++) {
            Syntax* arg = (Syntax*) syn.application.args.data[i];
            args_size += pi_size_of(*arg->ptype);
            generate(*arg, env, ass, links, a, point);
        }

        // This will push a function pointer onto the stack
        generate(*syn.application.function, env, ass, links, a, point);
        
        // Regular Function Call
        // Pop the function into RCX; call the function
        build_unary_op(ass, Pop, reg(RCX), a, point);
        build_unary_op(ass, Call, reg(RCX), a, point);
        // Update for popping all values off the stack
        address_stack_shrink(env, args_size);

        // Update as pushed the final value onto the stac
        address_stack_grow(env, pi_size_of(*syn.ptype));
        break;
    }
    case SStructure: {
        // For structures, we have to be careful - this is because the order in
        // which arguments are evaluated is not necessarily the order in which
        // arguments are inserted into the structure.

        // Step 1: Make room on the stack for our struct
        size_t struct_size = pi_size_of(*syn.ptype);
        build_binary_op(ass, Sub, reg(RSP), imm32(struct_size), a, point);

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
        build_binary_op(ass, Add, reg(RSP), imm32(struct_size), a, point);

        address_stack_grow(env, pi_size_of(*syn.ptype));
        break;
    }
    case SProjector: {
        // First, allocate space on the stack for the value
        size_t out_sz = pi_size_of(*syn.ptype);
        build_binary_op(ass, Sub, reg(RSP), imm32(out_sz), a, point);

        // Second, generate the structure object
        generate(*syn.projector.val, env, ass, links, a, point);

        // Now, copy the structure to the destination
        // for this, we need the struct size + offset of field in the struct
        size_t struct_sz = pi_size_of(*syn.projector.val->ptype);
        size_t offset = 0;
        for (size_t i = 0; i < syn.projector.val->ptype->structure.fields.len; i++) {
            if (syn.projector.val->ptype->structure.fields.data[i].key == syn.projector.field)
                break;
            offset += pi_size_of(*(PiType*)syn.projector.val->ptype->structure.fields.data[i].val);
        }

        generate_stack_move(struct_sz + out_sz - 0x8, offset, out_sz, ass, a, point);
        // now, remove the original struct from the stack
        build_binary_op(ass, Add, reg(RSP), imm32(struct_sz), a, point);

        address_stack_shrink(env, struct_sz);
        address_stack_grow(env, out_sz);
        break;
    }

    case SConstructor: {
        PiType* enum_type = syn.constructor.enum_type->type_val;
        size_t enum_size = pi_size_of(*enum_type);
        size_t variant_size = calc_variant_size(enum_type->enumeration.variants.data[syn.variant.tag].val);

        build_binary_op(ass, Sub, reg(RSP), imm32(enum_size - variant_size), a, point);
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
        build_binary_op(ass, Sub, reg(RSP), imm32(enum_size), a, point);

        // Set the tag
        build_binary_op(ass, Mov, rref(RSP, 0), imm32(syn.constructor.tag), a, point);

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
        build_binary_op(ass, Add, reg(RSP), imm32(variant_size - tag_size), a, point);

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
            build_binary_op(ass, Cmp, rref(RSP, 0), imm32(clause.tag), a, point);
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
                              , pi_mono_size_of(*(PiType*)variant_types.data[i])
                              , &arg_sizes);
            }
            address_bind_enum_vars(arg_sizes, env, a);

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

        build_binary_op(ass, Add, reg(RSP), imm32(enum_size), a, point);

        address_stack_shrink(env, enum_size);
        address_stack_grow(env, out_size);
        break;
    }
    case SLet:
        throw_error(point, mk_string("No assembler implemented for Let", a));
        break;
    case SIf: {
        // generate the condition
        generate(*syn.if_expr.condition, env, ass, links, a, point);

        // Pop the bool into RBX; compare with 0
        build_unary_op(ass, Pop, reg(RBX), a, point);
        build_binary_op(ass, Cmp, reg(RBX), imm32(0), a, point);
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
        build_unary_op(ass, JMP, imm8(0), a, point);

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
    case SCheckedType: {
        build_binary_op(ass, Mov, reg(RBX), imm64((uint64_t)syn.type_val), a, point);
        build_unary_op(ass, Push, reg(RBX), a, point);
        address_stack_grow(env, pi_size_of(*syn.ptype));
        break;
    }
    default: {
        throw_error(point, mv_string("Unrecognized abstract term in codegen."));
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
        build_binary_op(ass, Mov, reg(RAX), rref(RSP, src_stack_offset + (i * 8) ), a, point);
        build_binary_op(ass, Mov, rref(RSP, dest_stack_offset + (i * 8)), reg(RAX), a, point);
    }
}

size_t calc_variant_size(PtrArray* types) {
    size_t total = sizeof(uint64_t);
    for (size_t i = 0; i < types->len; i++) {
        total += pi_size_of(*(PiType*)types->data[i]);
    }
    return total;
}

size_t calc_max_vars(Syntax syn) {
    switch (syn.type) {
    case SLitI64:
    case SLitBool:
    case SVariable:
        return 0;
    case SProcedure:
        return syn.procedure.args.len + calc_max_vars(*syn.procedure.body);
    case SAll:
        return syn.all.args.len + calc_max_vars(*syn.all.body);
    case SApplication: {
        size_t max = calc_max_vars(*syn.application.function);
        for (size_t i = 0; i < syn.application.args.len; i++) {
            size_t sz = calc_max_vars(*(Syntax*)syn.application.args.data[i]);
            max = max < sz ? sz : max;
        }
        return max;
    }
    case SConstructor:
        return 0;
    case SVariant: {
        size_t max = 0;
        for (size_t i = 0; i < syn.application.args.len; i++) {
            size_t sz = calc_max_vars(*(Syntax*)syn.variant.args.data[i]);
            max = max < sz ? sz : max;
        }
        return max;
    }
    case SMatch: {
        size_t max = calc_max_vars(*syn.match.val); 
        for (size_t i = 0; i < syn.match.clauses.len; i++) {
            SynClause* clause = syn.match.clauses.data[i];
            size_t sz = clause->vars.len + calc_max_vars(*clause->body);
            max = max < sz ? sz : max;
        }
        return max;
    }
    case SStructure: {
        size_t max = 0;
        for (size_t i = 0; i < syn.structure.fields.len; i++) {
            size_t sz = calc_max_vars(*(Syntax*)syn.structure.fields.data[i].val);
            max = max < sz ? sz : max;
        }
        return max;
    }
    case SProjector: 
        return calc_max_vars(*syn.projector.val);
    case SLet:
        return syn.let_expr.bindings.len + calc_max_vars(*syn.let_expr.body);
    case SIf: {
        size_t max = calc_max_vars(*syn.if_expr.condition);
        size_t sz = calc_max_vars(*syn.if_expr.true_branch);
        max = max < sz ? sz : max;
        sz = calc_max_vars(*syn.if_expr.false_branch);
        max = max < sz ? sz : max;
        return max;
    }

    // Types are evaluated at typechecking time, therefore no runtime binds.
    case SProcType:
    case SStructType:
    case SEnumType:
    case SForallType:
    case SExistsType:
    case STypeFamily:
    case SCheckedType:
        return 0;
    case SAnnotation:
        return calc_max_vars(*syn.annotation.val);
    }
}
