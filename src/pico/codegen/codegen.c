#include <stdio.h>
#include "data/array.h"
#include "pico/codegen/codegen.h"



/* Relevant assembly:
 * 
 */

asm_result generate(syntax syn, assembler* ass, allocator a) {
    asm_result out;

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
    case SApplication:
        for (size_t i = 0; i < syn.data.application.args.len; i++) {
            syntax* arg = (syntax*) syn.data.application.args.data[i];
            out = generate(*arg, ass, a);
            if (out.type == Err) return out;
        }
        // evaluate the function
        out = generate(*syn.data.application.function, ass, a);
        if (out.type == Err) return out;

        // pop the function into RAX; call the function
        out = build_unary_op(ass, Pop, reg(RAX), a);
        if (out.type == Err) return out;
        out = build_unary_op(ass, Call, reg(RAX), a);
        break;
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
                                                 
