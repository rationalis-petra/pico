#include "pico/eval/expr.h"

eval_result exec_primop2int64(pi_primop_t po, syntax lhs, syntax rhs, environment* env, allocator a) {
    eval_result out;
    // TODO: eval lhs & rhs
    eval_result lhout = eval_expr(lhs, env, a);
    if (lhout.type == Err) {
        return lhout;
    }
    if (lhout.data.out.type != VI64) {
        // clean lhs
        out.type = Err;
        out.data.error_message = mk_string("Primitive operating expects in argument in 1st operand location", a);
        return out;
    }
    eval_result rhout = eval_expr(rhs, env, a);
    if (rhout.type == Err) {
        return rhout;
    }
    if (rhout.data.out.type != VI64) {
        // TODO clean lhs, rhs
        out.type = Err;
        out.data.error_message = mk_string("Primitive operator expects int argument in 2nd operand location", a);
        return out;
    }
    
    uint64_t ilhs = lhout.data.out.term.int_64;
    uint64_t irhs = rhout.data.out.term.int_64;
    out.type = Ok;
    out.data.out.type = VI64;
    switch (po) {
    case AddI64:
        out.data.out.term.int_64 = ilhs + irhs;
        break;
    case SubI64:
        out.data.out.term.int_64 = ilhs - irhs;
        break;
    case MulI64:
        out.data.out.term.int_64 = ilhs * irhs;
        break;
    case QuotI64:
        out.data.out.term.int_64 = ilhs / irhs;
        break;
    }
    return out;
}

eval_result eval_expr(syntax term, environment* env, allocator a) {
    eval_result out;
    switch (term.type) {
    case SLiteral: {
        out.type = Ok;
        out.data.out.type = VI64;
        out.data.out.term.int_64 = term.data.lit_i64;
        break;
    }
    case SVariable: {
        pi_value* ptr = env_lookup(term.data.variable, env);
        if (ptr) {
            out.type = Ok;
            out.data.out = *ptr;
        }
        else {
            out.type = Err;
            out.data.error_message = mk_string("Couldn't find symbol :(", a);
        }
        break;
    }
    case SFunction: {
        out.data.error_message = mk_string("Eval Function not implemented", a);
        break;
    }
    case SApplication: {
        eval_result rfn = eval_expr(*term.data.application.function, env, a);
        if (rfn.type == Err) {
            return rfn;
        }
        if (rfn.data.out.type == VPrimOp) {
            if (term.data.application.args.len != 2) {
                out.type = Err;
                out.data.error_message = mk_string("Expected two arguments to primitive operation", a);
                return out;
            } 
            else {
                syntax* lhs = term.data.application.args.data[0];
                syntax* rhs = term.data.application.args.data[1];
                return exec_primop2int64(rfn.data.out.term.primop, *lhs, *rhs, env, a);
            }
        }
        else {
            out.type = Err;
            out.data.error_message = mk_string("Couldn't invoke destructor on value (bad kind)", a);
        }
        break;
    }

    case SConstructor: {
        out.data.error_message = mk_string("Eval Constructor not implemented", a);
        break;
    }
    case SRecursor: {
        out.type = Err;
        out.data.error_message = mk_string("Eval Recursor not implemented", a);
        break;
    }
    case SDestructor: {
        out.type = Err;
        out.data.error_message = mk_string("Eval Destructor not implemented", a);
        break;
    }
    case SCorecursor: {
        out.type = Err;
        out.data.error_message = mk_string("Eval Corecursor not implemented", a);
        break;
    }
    case SStructure: {
        out.data.error_message = mk_string("Eval Structure not implemented", a);
        break;
    }
    case SProjector: {
        out.data.error_message = mk_string("Eval Projector not implemented", a);
        break;
    }
    case SLet: {
        out.data.error_message = mk_string("Eval Let not implemented", a);
        break;
    }
    case SIf: {
        // TODO: change form integer to boolean!
        eval_result condres = eval_expr(*term.data.if_expr.condition, env, a);
        if (condres.type == Err) {
            return condres;
        }
        if (condres.data.out.type != VI64) {
            out.type = Err;
            out.data.error_message = mk_string("If expects condition to evaluate to integer", a);
        }
        else if (condres.data.out.term.int_64 == 1) {
            out = eval_expr(*term.data.if_expr.true_branch, env, a);
        }
        else if (condres.data.out.term.int_64 == 0) {
            out = eval_expr(*term.data.if_expr.false_branch, env, a);
        }
        else {
            out.type = Err;
            out.data.error_message = mk_string("If expects condition to evaluate to either 0 or 1!", a);
        }
        break;
    }
    }
    return out;
}
