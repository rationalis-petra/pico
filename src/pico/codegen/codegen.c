#include "data/array.h"
#include "pico/codegen/codegen.h"

/* Code Generation Assumptions:
 * • All expressions evaluate to integers or functions 
 * • All functions are just an address to call to (or jmp to for tail calls) 
 * • All terms are well-typed
 */

// 
pi_value* static_eval(syntax syn, environment* env) {
    if (syn.type == SVariable) {
        return env_lookup_static(syn.data.variable, env);
    } else {
        return NULL;
    }
};

/* Relevant assembly:
 * 
 */

result generate(syntax syn, environment* env, assembler* ass, allocator a) {
    result out;

    switch (syn.type) {
    case SLiteral:
        // Does it fit into 32 bits?
        if (syn.data.lit_i64 < 0x80000000) {
            // Use RAX as a temp
            int32_t immediate = syn.data.lit_i64;
            out = build_unary_op(ass, Push, imm32(immediate), a);
        } else {
            out.type = Err;
            out.error_message = mk_string("literals must fit into less than 64 bits", a);
        }
        // push literal onto stack
        break;
    case SVariable:
        out.type = Err;
        out.error_message = mk_string("Variables cannot yet be assembled.",a);
        break;
    case SFunction:
        out.type = Err;
        out.error_message = mk_string("Functions cannot yet be assembled.",a);
        break;
    case SApplication: {
        for (size_t i = 0; i < syn.data.application.args.len; i++) {
            syntax* arg = (syntax*) syn.data.application.args.data[i];
            out = generate(*arg, env, ass, a);
            if (out.type == Err) return out;
        }
        // First, do a static check to see if the value is a primitive etc. 
        pi_value* val = static_eval(*syn.data.application.function, env);
        if (val && val->type == VPrimOp) {
            // generate primop code
            // for now, we know it's a binary binop
            out = build_unary_op(ass, Pop, reg(RAX), a);
            if (out.type == Err) return out;
            out = build_unary_op(ass, Pop, reg(RDI), a);
            if (out.type == Err) return out;
            binary_op op;
            switch (val->term.primop) {
            case AddI64:
                op = Add;
                break;
            case SubI64:
                op = Sub;
                break;
            case MulI64:
            case QuotI64:
                out.type = Err;
                out.error_message = mk_string("Mul+Quot cannot yet be assembled", a);
                break;
            }
            if (out.type == Err) return out;
            out = build_binary_op(ass, op, reg (RAX), reg(RDI), a);
            if (out.type == Err) return out;
            out = build_unary_op(ass, Push, reg (RAX), a);
        } else {
            // evaluate the function
            out = generate(*syn.data.application.function, env, ass, a);
            if (out.type == Err) return out;
            // pop the function into RAX; call the function
            out = build_unary_op(ass, Pop, reg(RAX), a);
            if (out.type == Err) return out;
            out = build_unary_op(ass, Call, reg(RAX), a);
        }
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
                                                 
