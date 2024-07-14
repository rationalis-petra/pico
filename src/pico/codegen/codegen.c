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
 * • 
 */

// implementation details
result generate_expr_i(syntax syn, address_env* env, assembler* ass, sym_sarr_amap* links, allocator a);
asm_result generate(syntax syn, address_env* env, assembler* ass, sym_sarr_amap* links, allocator a);
void backlink_global(pi_symbol sym, size_t offset, sym_sarr_amap* links, allocator a);

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
    case SProcedure:
        // Codegen function setup
        out = build_unary_op(ass, Push, reg(RBP), a);
        if (out.type == Err) return out;
        out = build_binary_op(ass, Mov, reg(RBP), reg(RSP), a);
        if (out.type == Err) return out;

        // Codegen Procedure Body 
        address_fn_vars(syn.procedure.args, env, a);
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

    case SConstructor:
    case SRecursor:
    case SStructure:
    case SProjector:

    case SLet:
        out.type = Err;
        out.error_message = mk_string("Not yet assembled.", a);
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
