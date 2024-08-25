#include <stdarg.h>
#include "pico/codegen/codegen.h"
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

// implementation details
result generate_expr_i(syntax syn, address_env* env, assembler* ass, sym_sarr_amap* links, allocator a);
asm_result generate(syntax syn, address_env* env, assembler* ass, sym_sarr_amap* links, allocator a);
void backlink_global(pi_symbol sym, size_t offset, sym_sarr_amap* links, allocator a);
asm_result generate_stack_move(size_t dest_offset, size_t src_offset, size_t size, assembler* ass, allocator a);
asm_result generate_copy(regname dest, regname src, size_t size, assembler* ass, allocator a);
asm_result get_variant_fun(size_t idx, size_t vsize, size_t esize, uint64_t* out);
size_t calc_variant_size(ptr_array* types);

gen_result generate_expr(syntax syn, environment* env, assembler* ass, allocator a) {
    address_env* a_env = mk_address_env(env, NULL, a);
    sym_sarr_amap backlinks = mk_sym_sarr_amap(16, a);
    result m_err = generate_expr_i(syn, a_env, ass, &backlinks, a);
    delete_address_env(a_env, a);

    gen_result out;
    out.backlinks = backlinks;
    if (m_err.type == Err) {
        out.error_message = m_err.error_message;
    }

    return out;
}

gen_result generate_toplevel(toplevel top, environment* env, assembler* ass, allocator a) {
    result m_err;
    sym_sarr_amap backlinks = mk_sym_sarr_amap(32, a);

    switch(top.type) {
    case TLDef: {
        address_env* a_env = mk_address_env(env, &top.def.bind, a);
        m_err = generate_expr_i(*top.def.value, a_env, ass, &backlinks, a);
        delete_address_env(a_env, a);
        break;
    }
    case TLExpr: {
        address_env* a_env = mk_address_env(env, NULL, a);
        m_err = generate_expr_i(top.expr, a_env, ass, &backlinks, a);
        delete_address_env(a_env, a);
        break;
    }
    }


    gen_result out;
    out.type = m_err.type;
    out.error_message = m_err.error_message;
    out.backlinks = backlinks;
    return out;
}

/* Relevant assembly:
 * 
 */
result generate_expr_i(syntax syn, address_env* env, assembler* ass, sym_sarr_amap *links, allocator a) {
    asm_result generated = generate(syn, env, ass, links, a);

    result out;
    out.type = generated.type;
    if (generated.type == Err)
        out.error_message = generated.error_message;

    return out;
}

asm_result generate(syntax syn, address_env* env, assembler* ass, sym_sarr_amap* links, allocator a) {
    asm_result out;

    switch (syn.type) {
    case SLitI64: {
        // Does it fit into 32 bits?
        if (syn.lit_i64 < 0x80000000) {
            int32_t immediate = syn.lit_i64;
            out = build_unary_op(ass, Push, imm32(immediate), a);
        } else {
            out.type = Err;
            out.error_message = mk_string("literals must fit into less than 64 bits", a);
        }
        // push literal onto stack
        break;
    }
    case SLitBool: {
        // TODO: squash to smallest size (8 bits)
        int32_t immediate = syn.lit_i64;
        out = build_unary_op(ass, Push, imm32(immediate), a);
        break;
    }
    case SType: {
        out = build_binary_op(ass, Mov, reg(RBX), imm64((uint64_t)syn.type_val), a);
        if (out.type == Err) return out;
        out = build_unary_op(ass, Push, reg(RBX), a);
        break;
    }
    case SVariable: {
        // Lookup the variable in the assembly envionrment
        address_entry e = address_env_lookup(syn.variable, env);
        switch (e.type) {
        case ALocal:
            out = build_unary_op(ass, Push, rref(RBP, e.stack_offset), a);
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
                if (syn.ptype->prim == TType) {
                    out = build_unary_op(ass, Push, reg(RCX), a);
                } else {
                    out = build_binary_op(ass, Mov, reg(RBX), rref(RCX, 0), a);
                    if (out.type == Err) return out;
                    out = build_unary_op(ass, Push, reg(RBX), a);
                }
            } else if (syn.ptype->sort == TStruct) {
                size_t struct_size = pi_size_of(*syn.ptype);
                out = build_binary_op(ass, Mov, reg(RCX), imm64((uint64_t)e.value), a);
                if (out.type == Err) return out;
                backlink_global(syn.variable, out.backlink, links, a);

                // Allocate space on the stack for struct
                out = build_binary_op(ass, Sub, reg(RSP), imm32(struct_size), a);
                if (out.type == Err) return out;

                // copy
                out = generate_copy(RSP, RCX, struct_size, ass, a);
                if (out.type == Err) return out;

            } else {
                out.type = Err;
                out.error_message = mv_string("Codegen: Global has unsupported sort: must be Primitive or Proc");
            }
            break;
        case ANotFound:
            out.type = Err;
            string* sym = symbol_to_string(syn.variable);
            string msg = mv_string("Couldn't find variable during codegen: ");
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
        // Codegen function setup
        out = build_unary_op(ass, Push, reg(RBP), a);
        if (out.type == Err) return out;
        out = build_binary_op(ass, Mov, reg(RBP), reg(RSP), a);
        if (out.type == Err) return out;

        // Codegen Procedure Body 
        sym_size_assoc arg_sizes = mk_sym_size_assoc(syn.procedure.args.len, a);
        for (size_t i = 0; i < syn.procedure.args.len; i++) {
            sym_size_bind(syn.procedure.args.data[i]
                         , pi_size_of(*(pi_type*)syn.ptype->proc.args.data[i])
                         , &arg_sizes, a);
        }

        address_fn_vars(arg_sizes, env, a);
        out = generate(*syn.procedure.body, env, ass, links, a);
        if (out.type == Err) return out;
        pop_fn_vars(env);

        // Codegen function teardown:
        // + store output in RAX
        // + restore rbp to original value
        // + pop all arguments from stack
        // + store return address in RCX
        // + push value (RAX)
        // + push return address (RCX)
        // + return ()

        // storage of function output 
        out = build_unary_op(ass, Pop, reg(RAX), a);
        if (out.type == Err) return out;
        out = build_unary_op(ass, Pop, reg(RBP), a);
        if (out.type == Err) return out;
        // storage of return address
        out = build_unary_op(ass, Pop, reg(RBX), a);
        if (out.type == Err) return out;
        
        // pop args
        out = build_binary_op(ass, Add, reg(RSP), imm32(syn.procedure.args.len * 8), a);
        if (out.type == Err) return out;

        // push value
        out = build_unary_op(ass, Push, reg(RAX), a);
        if (out.type == Err) return out;
        // push return address 
        out = build_unary_op(ass, Push, reg(RBX), a);
        if (out.type == Err) return out;
        out = build_nullary_op(ass, Ret, a);
        break;
    }
    case SApplication: {
        // Generate the arguments
        for (size_t i = 0; i < syn.application.args.len; i++) {
            syntax* arg = (syntax*) syn.application.args.data[i];
            out = generate(*arg, env, ass, links, a);
            if (out.type == Err) return out;
        }

        // This will push a function pointer onto the stack
        out = generate(*syn.application.function, env, ass, links, a);
        if (out.type == Err) return out; 
        
        // Regular Function Call
        // Pop the function into RCX; call the function
        out = build_unary_op(ass, Pop, reg(RCX), a);
        if (out.type == Err) return out;
        out = build_unary_op(ass, Call, reg(RCX), a);
        break;
    }

    case SStructure: {
        // For structures, we have to be careful - this is because the order in
        // which arguments are evaluated is not necessarily the order in which
        // arguments are inserted into the structure.

        // Step 1: Make room on the stack for our struct
        size_t struct_size = pi_size_of(*syn.ptype);
        out = build_binary_op(ass, Sub, reg(RSP), imm32(struct_size), a);
        if (out.type == Err) return out;

        // Step 2: evaluate each element/variable binding
        for (size_t i = 0; i < syn.structure.fields.len; i++) {
            out = generate(*(syntax*)syn.structure.fields.data[i].val, env, ass, links, a);
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
                pi_type** t = (pi_type**)sym_ptr_lookup(syn.structure.fields.data[j].key, syn.ptype->structure.fields);
                if (t) {
                    src_offset += pi_size_of(*((syntax*)syn.structure.fields.data[j].val)->ptype); 
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
            size_t field_size = pi_size_of(*(pi_type*)syn.ptype->structure.fields.data[i].val);

            // Now, move the data.
            out = generate_stack_move(dest_stack_offset, src_stack_offset, field_size, ass, a);
            if (out.type == Err) return out;

            // Compute dest_offset for next loop
            dest_offset += pi_size_of(*(pi_type*)syn.ptype->structure.fields.data[i].val);
        }
        // Remove the space occupied by the temporary values 
        out = build_binary_op(ass, Add, reg(RSP), imm32(struct_size), a);

        break;
    }
    case SProjector: {
        // First, allocate space on the stack for the value
        size_t out_sz = pi_size_of(*syn.ptype);
        out = build_binary_op(ass, Sub, reg(RSP), imm32(out_sz), a);
        if (out.type == Err) return out;

        // Second, generate the structure object
        out = generate(*syn.projector.val, env, ass, links, a);

        // Now, copy the structure to the destination
        // for this, we need the struct size + offset of field in the struct
        size_t struct_sz = pi_size_of(*syn.projector.val->ptype);
        size_t offset = 0;
        for (size_t i = 0; i < syn.projector.val->ptype->structure.fields.len; i++) {
            if (syn.projector.val->ptype->structure.fields.data[i].key == syn.projector.field)
                break;
            offset += pi_size_of(*(pi_type*)syn.projector.val->ptype->structure.fields.data[i].val);
        }

        out = generate_stack_move(struct_sz + out_sz - 0x8, offset, out_sz, ass, a);
        if (out.type == Err) return out;
        // now, remove the original struct from the stack
        out = build_binary_op(ass, Add, reg(RSP), imm32(struct_sz), a);
        break;
    }

    case SConstructor: {
        pi_type* enum_type = syn.constructor.enum_type->type_val;
        size_t enum_size = pi_size_of(*enum_type);
        size_t variant_size = calc_variant_size(enum_type->enumeration.variants.data[syn.variant.tag].val);

        out = build_binary_op(ass, Sub, reg(RSP), imm32(enum_size - variant_size), a);
        if (out.type == Err) return out;
        out = build_unary_op(ass, Push, imm32(syn.constructor.tag), a);
        if (out.type == Err) return out;
        break;
    }
    case SVariant: {
        const size_t tag_size = sizeof(uint64_t);
        pi_type* enum_type = syn.variant.enum_type->type_val;
        size_t enum_size = pi_size_of(*enum_type);
        size_t variant_size = calc_variant_size(enum_type->enumeration.variants.data[syn.variant.tag].val);

        // Make space to fit the (final) variant
        out = build_binary_op(ass, Sub, reg(RSP), imm32(enum_size), a);
        if (out.type == Err) return out;

        // Set the tag
        out = build_binary_op(ass, Mov, rref(RSP, 0), imm32(syn.constructor.tag), a);
        if (out.type == Err) return out;

        // Generate each argument
        for (size_t i = 0; i < syn.variant.args.len; i++) {
            out = generate(*(syntax*)syn.variant.args.data[i], env, ass, links, a);
            if (out.type == Err) return out;
        }

        // Now, move them into the space allocated in reverse order

        ptr_array args = *(ptr_array*)syn.ptype->enumeration.variants.data[syn.variant.tag].val;
        size_t src_stack_offset = variant_size - tag_size;
        size_t dest_stack_offset = variant_size;
        for (size_t i = 0; i < syn.variant.args.len; i++) {

            // We now both the source_offset and dest_offset. These are both
            // relative to the 'bottom' of their respective structures.
            // Therefore, we now need to find their offsets relative to the `top'
            // of the stack.
            
            size_t field_size = pi_size_of(*(pi_type*)args.data[syn.variant.args.len - (i + 1)]);
            src_stack_offset -= field_size;

            // Now, move the data.
            out = generate_stack_move(dest_stack_offset, src_stack_offset, field_size, ass, a);
            if (out.type == Err) return out;

            // Now, increase the amount of data we have used
            dest_stack_offset += field_size;
        }
        // Remove the space occupied by the temporary values 
        out = build_binary_op(ass, Add, reg(RSP), imm32(variant_size - tag_size), a);


        break;
    }

    case SMatch:

    case SLet:
        out = (asm_result) {
            .type = Err,
            .error_message = mk_string("Not yet assembled.", a)
        };
        break;
    case SIf: {
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
    }
    default: {
        out.type = Err;
        out.error_message = mv_string("Unrecognized abstract term in codegen.");
    }
    }

    return out;
}

void backlink_global(pi_symbol sym, size_t offset, sym_sarr_amap* links, allocator a) {
    // Step 1: Try lookup or else create & insert 
    size_array* sarr = NULL;
    sarr = sym_sarr_lookup(sym, *links);

    if (!sarr) {
        // create & insert
        sym_sarr_insert(sym, mk_size_array(4, a), links, a);
        sarr = sym_sarr_lookup(sym, *links);
    }

    // Step 2: insert offset into array
    push_size(offset, sarr, a);
}

asm_result generate_copy(regname dest, regname src, size_t size, assembler* ass, allocator a) {
    asm_result out;

    // first, assert that size_t is divisible by 8 ( we use rax for copies )
    if (size % 8 != 0)  {
        out.type = Err;
        out.error_message = mv_string("Error in generate_stack_copy: expected copy size to be divisible by 8");
        return out;
    };

    if (size > 255)  {
        out.type = Err;
        out.error_message = mv_string("Error in generate_copy: copy size must be smaller than 255!");
        return out;
    };

    if (src == RAX || dest == RAX)  {
        out.type = Err;
        out.error_message = mv_string("Error in generate_copy: offsets must be smaller than 255!");
        return out;
    };

    for (size_t i = 0; i < size / 8; i++) {
        out = build_binary_op(ass, Mov, reg(RAX), rref(src, i * 8), a);
        if (out.type == Err) return out;

        out = build_binary_op(ass, Mov, rref(dest, i * 8), reg(RAX), a);
        if (out.type == Err) return out;
    }

    return out;
}

asm_result generate_stack_move(size_t dest_stack_offset, size_t src_stack_offset, size_t size, assembler* ass, allocator a) {
    asm_result out;

    // first, assert that size_t is divisible by 8 ( we use rax for copies )
    if (size % 8 != 0)  {
        out.type = Err;
        out.error_message = mv_string("Error in generate_stack_copy: expected copy size to be divisible by 8");
        return out;
    };

    if ((dest_stack_offset + size) > 255 || (src_stack_offset + size) > 255)  {
        out.type = Err;
        out.error_message = mv_string("Error in generate_stack_copy: offsets + size must be smaller than 255!");
        return out;
    };

    for (size_t i = 0; i < size / 8; i++) {
        out = build_binary_op(ass, Mov, reg(RAX), rref(RSP, src_stack_offset + (i * 8) ), a);
        if (out.type == Err) return out;

        out = build_binary_op(ass, Mov, rref(RSP, dest_stack_offset + (i * 8)), reg(RAX), a);
        if (out.type == Err) return out;
    }

    return out;
}

size_t calc_variant_size(ptr_array* types) {
    size_t total = sizeof(uint64_t);
    for (size_t i = 0; i < types->len; i++) {
        total += pi_size_of(*(pi_type*)types->data[i]);
    }
    return total;
}
