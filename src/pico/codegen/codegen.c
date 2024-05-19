
#include "pico/codegen/codegen.h"
#include "pico/binding/address_env.h"

/* Code Generation Assumptions:
 * • All expressions evaluate to integers or functions 
 * • All functions are just an address to call to (or jmp to for tail calls) 
 * • All terms are well-typed
 */

// 

/* Relevant assembly:
 * 
 */

result generate(syntax syn, address_env* env, assembler* ass, allocator a) {
    result out;

    switch (syn.type) {
    case SLiteral:
        // Does it fit into 32 bits?
        if (syn.data.lit_i64 < 0x80000000) {
            int32_t immediate = syn.data.lit_i64;
            out = build_unary_op(ass, Push, imm32(immediate), a);
        } else {
            out.type = Err;
            out.error_message = mk_string("literals must fit into less than 64 bits", a);
        }
        // push literal onto stack
        break;
    case SVariable: {
        // Lookup the variable in the assembly envionrment
        address_entry e = address_env_lookup(syn.data.variable, env);
        switch (e.type) {
        case ALocal:
            out.type = Err;
            out.error_message = mk_string("Local variables cannot yet be assembled.",a);
            break;
        case AGlobal:
            // Use RAX as a temp
            // Note: casting void* to uint64_t only works for 64-bit systems...
            out = build_binary_op(ass, Mov, reg(RCX), imm64((uint64_t)e.value), a);
            out = build_unary_op(ass, Push, reg(RCX), a);
            break;
        case ANotFound:
            out.type = Err;
            out.error_message = mk_string("Variable not found!", a);
            break;
        case ATooManyLocals:
            out.type = Err;
            out.error_message = mk_string("Too Many Local variables!", a);
            break;
        } 
        break;
    }
    case SFunction:
        out.type = Err;
        out.error_message = mk_string("Functions cannot yet be assembled.",a);
        break;
    case SApplication: {
        // Generate the arguments
        for (size_t i = 0; i < syn.data.application.args.len; i++) {
            syntax* arg = (syntax*) syn.data.application.args.data[i];
            out = generate(*arg, env, ass, a);
            if (out.type == Err) return out;
        }

        // This will push a function pointer onto the stack
        out = generate(*syn.data.application.function, env, ass, a);
        if (out.type == Err) return out; 
        
        // pop the function into RCX; call the function
        out = build_unary_op(ass, Pop, reg(RCX), a);
        if (out.type == Err) return out;
        out = build_unary_op(ass, Call, reg(RCX), a);
        break;
    }

    case SConstructor:
    case SRecursor:
    case SDestructor:
    case SCorecursor:
    case SStructure:
    case SProjector:

    case SLet:
    case SIf:
        out.type = Err;
        out.error_message = mk_string("Not yet assembled.", a);
        break;
    }

    return out;
}
                                                 
result generate_expr(syntax syn, environment* env, assembler* ass, allocator a) {
    address_env* a_env = mk_address_env(env, a);
    result generated = generate(syn, a_env, ass, a);

    if (generated.type == Ok) {
        // genereate a return call
        generated = build_unary_op(ass, Pop, reg(RAX), a);

        if (generated.type == Ok) {
            generated = build_nullary_op(ass, Ret, a);
        }
    }

    delete_address_env(a_env, a);
    return generated;
}

result generate_toplevel(toplevel top, environment* env, assembler* ass, allocator a) {
    result out;
    switch(top.type) {
    case TLDef:
        out = generate_expr(*top.def.value, env, ass, a);
        break;
    case TLExpr:
        out = generate_expr(top.expr, env, ass, a);
        break;
    }
    return out;
}
