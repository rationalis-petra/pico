#include <string.h>

#include "platform/signals.h"
#include "platform/machine_info.h"
#include "platform/machine_info.h"
#include "components/pretty/string_printer.h"

#include "pico/codegen/backend-direct/polymorphic.h"
#include "pico/codegen/backend-direct/internal.h"
#include "pico/binding/address_env.h"

void generate_polymorphic(SymbolArray types, Syntax syn, AddressEnv* env, Target target, InternalLinkData* links, Allocator* a, ErrorPoint* point) {
    Assembler* ass = target.target;
    BindingArray vars;
    Syntax body;

    size_t args_size = types.len * REGISTER_SIZE;
    if (syn.type == SProcedure) {
        vars = mk_binding_array(syn.procedure.args.len + syn.procedure.implicits.len, a);
        for (size_t i = 0; i < syn.procedure.implicits.len; i++) {
            PiType* impl_ty = syn.ptype->proc.implicits.data[i];
            if (is_variable_for(impl_ty, types)) {
                args_size += ADDRESS_SIZE;
                Binding bind = (Binding) {
                    .sym = syn.procedure.implicits.data[i].key,
                    .size = ADDRESS_SIZE,
                    .is_variable = true,
                };
                push_binding(bind, &vars);
            } else {
                size_t arg_sz = pi_stack_size_of(*impl_ty);
                args_size += arg_sz;
                Binding bind = (Binding) {
                    .sym = syn.procedure.implicits.data[i].key,
                    .size = arg_sz,
                    .is_variable = false,
                };
                push_binding(bind, &vars);
            }
        }
        for (size_t i = 0; i < syn.ptype->proc.args.len; i++) {
            PiType* arg_ty = syn.ptype->proc.args.data[i];
            if (is_variable_for(arg_ty, types)) {
                args_size += ADDRESS_SIZE;
                Binding bind = (Binding) {
                    .sym = syn.procedure.args.data[i].key,
                    .size = ADDRESS_SIZE,
                    .is_variable = true,
                };
                push_binding(bind, &vars);
            } else {
                size_t arg_sz = pi_stack_size_of(*arg_ty);
                args_size += arg_sz;
                Binding bind = (Binding) {
                    .sym = syn.procedure.args.data[i].key,
                    .size = arg_sz,
                    .is_variable = false,
                };
                push_binding(bind, &vars);
            }
        }
        body = *syn.procedure.body;
    } else {
        vars = mk_binding_array(0, a);
        body = syn;
    }

    address_start_poly(types, vars, env, a);

    build_unary_op(Push, reg(RBP, sz_64), ass, a, point);
    build_unary_op(Push, reg(R15, sz_64), ass, a, point);

    build_binary_op(Mov, reg(RBP, sz_64), reg(RSP, sz_64), ass, a, point);

    generate_i(body, env, target, links, a, point);

    // Codegen function postlude:
    // Stack now looks like:
    // 
    // OLD RBP | return address
    // OLD R15 | OLD RBP
    // Arguments...
    // Return Value

    // Considerations:
    // - which stack to return value on? 
    // - always data stack?
    // - always relevant stack?

    if (is_variable_for(body.ptype, types)) {
        // Return on Variable Stack
        // R15 is the 'destination' on the variable stack of a return
        // argument.

        // Store value at the stack head
        generate_stack_size_of(RAX, body.ptype, env, ass, a, point);
        build_binary_op(Mov, reg(R15, sz_64), rref8(RBP, 0, sz_64), ass, a, point);
        build_binary_op(Sub, reg(R15, sz_64), reg(RAX, sz_64), ass, a, point);
        generate_poly_move(reg(R15, sz_64), reg(VSTACK_HEAD, sz_64), reg(RAX, sz_64), ass, a, point);
        build_binary_op(Mov, reg(VSTACK_HEAD, sz_64), reg(R15, sz_64), ass, a, point);

        // Next, restore the old stack bases (variable + static)
        build_binary_op(Mov, reg(R15, sz_64), rref8(RBP, 0, sz_64), ass, a, point);
        build_binary_op(Mov, reg(RBP, sz_64), rref8(RBP, 0x8, sz_64), ass, a, point);

        // Now, copy the return return address 
        build_binary_op(Mov, reg(RDX, sz_64), rref8(RSP, 0x18, sz_64), ass, a, point);

        // destination
        // return val store at (RET + FROM + RBP + R15 + ARGS = 8 + 8 + 8 + 8 ARGS = 0x20 + args)
        // return address = above - 0x8
        build_binary_op(Mov, rref8(RSP, 0x18 + args_size, sz_64), reg(VSTACK_HEAD, sz_64), ass, a, point);
        build_binary_op(Mov, rref8(RSP, 0x10 + args_size, sz_64), reg(RDX, sz_64), ass, a, point);
        build_binary_op(Add, reg(RSP, sz_64), imm8(0x10 + args_size), ass, a, point);
    } else {
        // Return on Data Stack
        // this is much like the steps above, where we copy 
        size_t ret_sz = pi_stack_size_of(*body.ptype);
    
        // Next, restore the old stack bases (variable + static)
        build_binary_op(Mov, reg(R15, sz_64), rref8(RBP, 0, sz_64), ass, a, point);
        build_binary_op(Mov, reg(VSTACK_HEAD, sz_64), rref8(RBP, 0, sz_64), ass, a, point);
        build_binary_op(Mov, reg(RBP, sz_64), rref8(RBP, 0x8, sz_64), ass, a, point);

        // Now, copy the return address into a (safe) register
        build_binary_op(Mov, reg(RBX, sz_64), rref8(RSP, 0x10 + ret_sz, sz_64), ass, a, point);
        generate_stack_move(0x18 + args_size, 0, ret_sz, ass, a, point);

        // Then the return address
        build_binary_op(Mov, rref8(RSP, 0x10 + args_size, sz_64), reg(RBX, sz_64), ass, a, point);
        build_binary_op(Add, reg(RSP, sz_64), imm8(0x10 + args_size), ass, a, point);
    }

    build_nullary_op(Ret, ass, a, point);
    address_end_poly(env, a);
}

// Internal helper functions for movement 
U8Array free_registers(U8Array inputs, Allocator* a);
bool reg_conflict(Location loc, Regname reg) {
    return (loc.type == Dest_Register && loc.reg == reg);
}

void generate_size_of(Regname dest, PiType* type, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point) {
    type = strip_type(type);

    size_t sz; 
    Result_t sz_res = pi_maybe_size_of(*type, &sz);
    if (sz_res == Ok) {
        build_binary_op(Mov, reg(dest, sz_64), imm32(sz), ass, a, point);
    } else {
        switch (type->sort) {
        case TPrim:
        case TProc:
        case TTraitInstance:
            build_binary_op(Mov, reg(dest, sz_64), imm32(pi_size_of(*type)), ass, a, point);
            break;
        case TVar: {
            AddressEntry e = address_env_lookup(type->var, env);
            switch (e.type) {
            case ALocalDirect:
                build_binary_op(Mov, reg(dest, sz_64), rref8(RBP, e.stack_offset, sz_64), ass, a, point);
                build_binary_op(SHR, reg(dest, sz_64), imm8(28), ass, a, point);
                build_binary_op(And, reg(dest, sz_64), imm32(0xFFFFFFF), ass, a, point);
                break;
            case ALocalIndexed:
                panic(mv_string("Cannot generate code for size of local indexed."));
                break;
            case AGlobal:
                panic(mv_string("Unexpected type variable sort: Global."));
                break;
            case ATypeVar:
                panic(mv_string("Unexpected type variable sort: ATypeVar."));
                break;
            case ANotFound: {
                panic(mv_string("Type Variable not found during codegen."));
                break;
            }
            case ATooManyLocals: {
                throw_error(point, mv_cstr_doc("Too Many Local variables!", a));
                break;
            }
            }
            break;
        }
        case TStruct: {
            build_unary_op(Push, imm32(0), ass, a, point);
            for (size_t i = 0; i < type->structure.fields.len; i++) {
                PiType* field_type = type->structure.fields.data[i].val;
                generate_align_of(R8, field_type, env, ass, a, point);
                build_binary_op(Mov, reg(R9, sz_64), rref8(RSP, 0, sz_64), ass, a, point);

                generate_align_to(R9, R8, ass, a, point);
                build_binary_op(Mov, rref8(RSP, 0, sz_64), reg(R9, sz_64), ass, a, point);
        
                generate_size_of(R8, field_type, env, ass, a, point);
                build_binary_op(Add, rref8(RSP, 0, sz_64), reg(R8, sz_64), ass, a, point);
            }
            // pop into the destination register
            build_unary_op(Pop, reg(dest, sz_64), ass, a, point);
            break;
        }
        case TEnum: {
            build_unary_op(Push, imm32(0), ass, a, point);
            for (size_t i = 0; i < type->enumeration.variants.len; i++) {
                PtrArray* types = type->enumeration.variants.data[i].val;

                // Pick the larger of (old size) vs (current size)
                generate_variant_size_of(R9, types, env, ass, a, point);
                build_binary_op(Mov, reg(R10, sz_64), rref8(RSP, 0, sz_64), ass, a, point);
                build_binary_op(Cmp, reg(R9, sz_64), reg(R10, sz_64), ass, a, point);

                // Move R10 into R9 if R9 was below R10
                build_binary_op(CMovB, reg(R9, sz_64), reg(R10, sz_64), ass, a, point);

                // Store R9 on the top of the stack
                build_binary_op(Mov, rref8(RSP, 0, sz_64), reg(R9, sz_64), ass, a, point);
            }
            // pop into the destination register
            build_unary_op(Pop, reg(dest, sz_64), ass, a, point);
            break;
        }
        case TNamed: {
            generate_size_of(dest, type->named.type, env, ass, a, point);
            break;
        }
        default: {
            // TODO BUG: This seems to cause crashes!
            PtrArray nodes = mk_ptr_array(4, a);
            push_ptr(mv_str_doc(mv_string("Unrecognized type to generate_size_of:"), a), &nodes);
            push_ptr(pretty_type(type, a), &nodes);
            Document* message = mk_sep_doc(nodes, a);
            panic(doc_to_str(message, 80, a));
        }
        }
    }
}

void generate_align_of(Regname dest, PiType* type, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point) {
    type = strip_type(type);

    size_t align; 
    Result_t al_res = pi_maybe_align_of(*type, &align);
    if (al_res == Ok) {
        build_binary_op(Mov, reg(dest, sz_64), imm32(align), ass, a, point);
    } else {
        switch (type->sort) {
        case TPrim:
        case TProc:
        case TTraitInstance:
            build_binary_op(Mov, reg(dest, sz_64), imm32(pi_align_of(*type)), ass, a, point);
            break;
        case TVar: {
            // TODO (BUG UB VERY BAD) This is size - not alignment. Alignment
            //   needs to be accounted for!
            AddressEntry e = address_env_lookup(type->var, env);
            switch (e.type) {
            case ALocalDirect:
                build_binary_op(Mov, reg(dest, sz_64), rref8(RBP, e.stack_offset, sz_64), ass, a, point);
                build_binary_op(SHR, reg(dest, sz_64), imm8(56), ass, a, point);
                break;
            case ALocalIndexed:
                panic(mv_string("cannot generate align-of code for local indexed variable."));
                break;
            case AGlobal:
                panic(mv_string("Unexpected type variable sort: Global."));
                break;
            case ATypeVar:
                panic(mv_string("Unexpected type variable sort: ATypeVar."));
                break;
            case ANotFound: {
                panic(mv_string("Type Variable not found during codegen."));
                break;
            }
            case ATooManyLocals: {
                throw_error(point, mv_cstr_doc("Too Many Local variables!", a));
                break;
            }
            }
            break;
        }
        case TStruct: {
            build_unary_op(Push, imm32(0), ass, a, point);
            for (size_t i = 0; i < type->structure.fields.len; i++) {
                PiType* field_type = type->structure.fields.data[i].val;
                generate_align_of(R8, field_type, env, ass, a, point);
                build_binary_op(Mov, reg(R9, sz_64), rref8(RSP, 0, sz_64), ass, a, point);

                generate_align_to(R9, R8, ass, a, point);
                build_binary_op(Mov, rref8(RSP, 0, sz_64), reg(R9, sz_64), ass, a, point);
        
                generate_size_of(R8, field_type, env, ass, a, point);
                build_binary_op(Add, rref8(RSP, 0, sz_64), reg(R8, sz_64), ass, a, point);
            }
            // pop into the destination register
            build_unary_op(Pop, reg(dest, sz_64), ass, a, point);
            break;
        }
        case TEnum: {
            build_unary_op(Push, imm32(0), ass, a, point);
            for (size_t i = 0; i < type->enumeration.variants.len; i++) {
                PtrArray* types = type->enumeration.variants.data[i].val;

                // Pick the larger of (old align) vs (current align)
                generate_variant_align_of(R9, types, env, ass, a, point);
                build_binary_op(Mov, reg(R10, sz_64), rref8(RSP, 0, sz_64), ass, a, point);
                build_binary_op(Cmp, reg(R9, sz_64), reg(R10, sz_64), ass, a, point);

                // Move R10 into R9 if R9 was below R10
                build_binary_op(CMovB, reg(R9, sz_64), reg(R10, sz_64), ass, a, point);

                // Store R9 on the top of the stack
                build_binary_op(Mov, rref8(RSP, 0, sz_64), reg(R9, sz_64), ass, a, point);
            }
            // pop into the destination register
            build_unary_op(Pop, reg(dest, sz_64), ass, a, point);
            break;
        }
        case TNamed: {
            generate_align_of(dest, type->named.type, env, ass, a, point);
            break;
        }
        default: {
            // TODO BUG: This seems to cause crashes!
            PtrArray nodes = mk_ptr_array(4, a);
            push_ptr(mv_str_doc(mv_string("Unrecognized type provided to generate_align_of:"), a), &nodes);
            push_ptr(pretty_type(type, a), &nodes);
            Document* message = mk_sep_doc(nodes, a);
            panic(doc_to_str(message, 80, a));
        }
        }
    }
}

void generate_offset_of(Regname dest, Symbol field, SymAddrPiAMap fields, AddressEnv *env, Assembler *ass, Allocator *a, ErrorPoint *point) {
    build_unary_op(Push, imm8(0), ass, a, point);
    for (size_t i = 0; i < fields.len; i++) {
        if (i != 0) {
            // Align to the new field; can skip if size = 0;
            generate_align_of(R8, (PiType*)fields.data[i].val, env, ass, a, point);
            build_binary_op(Mov, reg(R9, sz_64), rref8(RSP, 0, sz_64), ass, a, point);
            generate_align_to(R9, R8, ass, a, point);
            build_binary_op(Mov, rref8(RSP, 0, sz_64), reg(R9, sz_64), ass, a, point);
        }

        if (symbol_eq(fields.data[i].key, field))
            break;

        // Push the size into RAX; this is then added to the value at
        // the top of the stack  
        generate_size_of(RAX, (PiType*)fields.data[i].val, env, ass, a, point);
        build_binary_op(Add, rref8(RSP, 0, sz_64), reg(RAX, sz_64), ass, a, point);
    }
    build_unary_op(Pop, reg(dest, sz_64), ass, a, point);
}

void generate_variant_size_of(Regname dest, PtrArray* types, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point) {
    size_t total = sizeof(uint64_t);
    // Push size onto stack
    build_unary_op(Push, imm32(total), ass, a, point);
    for (size_t i = 0; i < types->len; i++) {
        generate_align_of(R8, types->data[i], env, ass, a, point);
        build_binary_op(Mov, reg(R9, sz_64), rref8(RSP, 0, sz_64), ass, a, point);

        generate_align_to(R9, R8, ass, a, point);
        build_binary_op(Mov, rref8(RSP, 0, sz_64), reg(R9, sz_64), ass, a, point);
        
        generate_size_of(R8, types->data[i], env, ass, a, point);
        build_binary_op(Add, rref8(RSP, 0, sz_64), reg(R8, sz_64), ass, a, point);
    }

    build_unary_op(Pop, reg(dest, sz_64), ass, a, point);
}

void generate_variant_align_of(Regname dest, PtrArray* types, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point) {
    size_t total = sizeof(uint64_t);
    // Push size onto stack
    build_unary_op(Push, imm32(total), ass, a, point);
    for (size_t i = 0; i < types->len; i++) {
        generate_align_of(R9, types->data[i], env, ass, a, point);
        build_binary_op(Mov, reg(R10, sz_64), rref8(RSP, 0, sz_64), ass, a, point);
        build_binary_op(Cmp, reg(R9, sz_64), reg(R10, sz_64), ass, a, point);

        // Move R10 into R9 if R9 was below R10
        build_binary_op(CMovB, reg(R9, sz_64), reg(R10, sz_64), ass, a, point);

        // Store R9 on the top of the stack
        build_binary_op(Mov, rref8(RSP, 0, sz_64), reg(R9, sz_64), ass, a, point);
    }

    build_unary_op(Pop, reg(dest, sz_64), ass, a, point);
}

void generate_variant_stack_size_of(Regname dest, PtrArray* types, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point) {
    size_t total = sizeof(uint64_t);
    // Push size onto stack
    build_unary_op(Push, imm32(total), ass, a, point);
    for (size_t i = 0; i < types->len; i++) {
        generate_stack_size_of(RAX, types->data[i], env, ass, a, point);
        build_binary_op(Add, rref8(RSP, 0, sz_64), reg(RAX, sz_64), ass, a, point);
    }
    build_unary_op(Pop, reg(dest, sz_64), ass, a, point);
}

void generate_stack_size_of(Regname dest, PiType* type, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point) {
    size_t sz; 
    Result_t sz_res = pi_maybe_stack_size_of(*type, &sz);
    if (sz_res == Ok) {
        build_binary_op(Mov, reg(dest, sz_64), imm32(sz), ass, a, point);
    } else {
        switch (type->sort) {
        case TVar: {
            AddressEntry e = address_env_lookup(type->var, env);
            switch (e.type) {
            case ALocalDirect:
                build_binary_op(Mov, reg(dest, sz_64), rref8(RBP, e.stack_offset, sz_64), ass, a, point);
                build_binary_op(And, reg(dest, sz_64), imm32(0xFFFFFFF), ass, a, point);
                break;
            case ALocalIndexed:
                panic(mv_string("cannot generate code for local indexed."));
                break;
            case AGlobal:
                panic(mv_string("Unexpected type variable sort: Global."));
                break;
            case ATypeVar:
                panic(mv_string("Unexpected type variable sort: ATypeVar."));
                break;
            case ANotFound: {
                panic(mv_string("Type Variable not found during codegen."));
                break;
            }
            case ATooManyLocals: {
                throw_error(point, mv_cstr_doc("Too Many Local variables!", a));
                break;
            }
            }
            break;
        }
        default: {
            U8Array inputs = mk_u8_array(4, a);
            push_u8(dest, &inputs);
            push_u8(RAX, &inputs);
            push_u8(RCX, &inputs);
            push_u8(RDX, &inputs);
            U8Array regs = free_registers(inputs, a);

            generate_size_of(regs.data[0], type, env, ass, a, point);
            build_unary_op(Push, reg(regs.data[0], sz_64), ass, a, point);
            generate_align_of(regs.data[1], type, env, ass, a, point);
            build_unary_op(Pop, reg(regs.data[0], sz_64), ass, a, point);
            generate_align_to(regs.data[0], regs.data[1], ass, a, point);
            build_binary_op(Mov, reg(dest, sz_64), reg(regs.data[0], sz_64), ass, a, point);
        }
        }
    }
}

void generate_pi_type(PiType *type, AddressEnv *env, Assembler *ass, Allocator *a, ErrorPoint *point) {
    if (is_variable_in(type, env)) {
        if (type->sort == TVar) {
            // Optimization
            AddressEntry e = address_env_lookup(type->var, env);
            switch (e.type) {
            case ALocalDirect:
                build_binary_op(Mov, reg(R8, sz_64), rref8(RBP, e.stack_offset, sz_64), ass, a, point);
                build_unary_op(Push, reg(R8, sz_64), ass, a, point);
                break;
            case ATooManyLocals: {
                throw_error(point, mv_cstr_doc("Too Many Local variables!", a));
                break;
            }
            default: {
                panic(mv_string("Invalid address entry sort for runtime type variable."));
            }
            }
        } else {

            // The 'type' looks as follows:
            // | 16 bits | 16 bits     | 32 bits |
            // | align   | stack align | size    |
            generate_align_of(R8, type, env, ass, a, point);
            build_binary_op(SHL, reg(R8, sz_64), imm8(56), ass, a, point);
            build_unary_op(Push, reg(R8, sz_64), ass, a, point);

            // TODO BUG LOGIC Check that R8 < max_uint_28
            generate_size_of(R8, type, env, ass, a, point);
            build_binary_op(Mov, reg(RAX, sz_64), reg(R8, sz_64), ass, a, point);
            build_binary_op(SHL, reg(R8, sz_64), imm8(28), ass, a, point);
            build_binary_op(Or, rref8(RSP, 0, sz_64), reg(R8, sz_64), ass, a, point);

            /* uint64_t result = (align << 56) | (size << 28) | stack_sz; */
            // Now, we use the 'div' instruction with RDX = 0; RAX = size.
            // Remainder stored in RDX
            build_binary_op(Mov, reg(RDX, sz_64), imm32(0), ass, a, point);
            build_binary_op(Mov, reg(RCX, sz_64), imm32(8), ass, a, point);
            build_unary_op(Div, reg(RCX, sz_64), ass, a, point);

            // We need now to if rem == 0 then 0 else 8 - rem 
            // store 8 - rem in RCX (8 is already in rcx from above)
            build_binary_op(Sub, reg(RCX, sz_64), reg(RDX, sz_64), ass, a, point);
            build_binary_op(Cmp, reg(RDX, sz_64), imm8(0), ass, a, point);
            
            build_binary_op(CMovE, reg(RCX, sz_64), reg(RDX, sz_64), ass, a, point);

            // Add this to the original size and binary-or it into the type.
            build_binary_op(Add, reg(R8, sz_64), reg(RCX, sz_64), ass, a, point);
            build_binary_op(Or, rref8(RSP, 0, sz_64), reg(R8, sz_64), ass, a, point);
        }
    } else {
        size_t align = pi_align_of(*type);
        size_t size = pi_size_of(*type);
        size_t stack_sz = pi_stack_align(size);

        // TODO BUG LOGIC Check that stack_sz < max_uint_28
        uint64_t result = (align << 56) | (size << 28) | stack_sz;
        build_binary_op(Mov, reg(RAX, sz_64), imm64(result), ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
    }
    data_stack_grow(env, ADDRESS_SIZE);
}

void generate_align_to(Regname sz_reg, Regname align, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // RDX, RAX, RCX
    if (sz_reg == RAX || sz_reg == RCX || sz_reg == RDX ||
        align == RAX  || align == RCX  || align == RDX) {
        throw_error(point, mv_cstr_doc("Error in generate_align_to: sz or align registers were either RAX, RCX or RDX!", a));
    }

    /* size_t rem = size % 8; */
    /* size_t pad = rem == 0 ? 0 : align - rem; */
    /* return size + pad; */
    // If we assume align is a power of 2, then this also works:
    // size_t pad = ( ~(size & (align - 1)) + 1) & (align - 1)
    // return size + pad;

    // align_reg = align - 1 (what if align = 0?)
    build_binary_op(Sub, reg(align, sz_64), imm8(1), ass, a, point); 

    // start building pad (RCX)
    build_binary_op(Mov, reg(RCX, sz_64), reg(sz_reg, sz_64), ass, a, point); 
    build_binary_op(And, reg(RCX, sz_64), reg(align, sz_64), ass, a, point); 
    build_unary_op(Not, reg(RCX, sz_64), ass, a, point); 
    build_binary_op(Add, reg(RCX, sz_64), imm8(1), ass, a, point); 
    build_binary_op(And, reg(RCX, sz_64), reg(align, sz_64), ass, a, point); 

    // Finally, add size (sz_reg) to padding (RCX)
    build_binary_op(Add, reg(sz_reg, sz_64), reg(RCX, sz_64), ass, a, point); 
}

void generate_poly_move(Location dest, Location src, Location size, Assembler* ass, Allocator* a, ErrorPoint* point) {

#if ABI == SYSTEM_V_64
    if (reg_conflict(src, RDI) || reg_conflict(size, RDI) || reg_conflict(size, RSI)) {
        panic(mv_string("In generate_poly_move: invalid regitser provided to generate_poly_move"));
    }

    // memcpy (dest = rdi, src = rsi, size = rdx)
    // copy size into RDX
    build_binary_op(Mov, reg(RDI, sz_64), dest, ass, a, point);
    build_binary_op(Mov, reg(RSI, sz_64), src, ass, a, point);
    build_binary_op(Mov, reg(RDX, sz_64), size, ass, a, point);

#elif ABI == WIN_64
    if (reg_conflict(src, RCX) || reg_conflict(size, RDX) || reg_conflict(size, R8)) {
        panic(mv_string("In generate_poly_move: invalid regitser provided to generate_poly_move"));
    }

    // memcpy (dest = rcx, src = rdx, size = r8)
    build_binary_op(Mov, reg(RCX, sz_64), dest, ass, a, point);
    build_binary_op(Mov, reg(RDX, sz_64), src, ass, a, point);
    build_binary_op(Mov, reg(R8, sz_64), size, ass, a, point);
#else
#error "Unknown calling convention"
#endif

    generate_c_call(memmove, ass, a, point);
}

void generate_poly_stack_move(Location dest, Location src, Location size, Assembler* ass, Allocator* a, ErrorPoint* point) {

#if ABI == SYSTEM_V_64
    // Check that we don't accidentally overwrite any registers!
    if (reg_conflict(src, RDI) || reg_conflict(size, RDI) || reg_conflict(size, RSI)) {
        panic(mv_string("In generate_poly_stack_move: invalid regitser provided to generate_poly_stack_move"));
    }

    // memmove (dest = rdi, src = rsi, size = rdx)
    // copy size into RDX
    build_binary_op(Mov, reg(RDI, sz_64), dest, ass, a, point);
    build_binary_op(Add, reg(RDI, sz_64), reg(RSP, sz_64), ass, a, point);
    build_binary_op(Mov, reg(RSI, sz_64), src, ass, a, point);
    build_binary_op(Add, reg(RSI, sz_64), reg(RSP, sz_64), ass, a, point);
    build_binary_op(Mov, reg(RDX, sz_64), size, ass, a, point);

#elif ABI == WIN_64
    if (reg_conflict(src, RCX) || reg_conflict(size, RDX) || reg_conflict(size, R8)) {
        panic(mv_string("In generate_poly_stack_move: invalid regitser provided to generate_poly_stack_move"));
    }

    // memmove (dest = rcx, src = rdx, size = r8)
    build_binary_op(Mov, reg(RCX, sz_64), dest, ass, a, point);
    build_binary_op(Add, reg(RCX, sz_64), reg(RSP, sz_64), ass, a, point);
    build_binary_op(Mov, reg(RDX, sz_64), src, ass, a, point);
    build_binary_op(Add, reg(RDX, sz_64), reg(RSP, sz_64), ass, a, point);
    build_binary_op(Mov, reg(R8, sz_64), size, ass, a, point);
#else
#error "Unknown calling convention"
#endif

    generate_c_call(memmove, ass, a, point);
}

void generate_poly_copy_from_base(size_t dest, size_t src, Location size, Assembler* ass, Allocator* a, ErrorPoint* point) {

#if ABI == SYSTEM_V_64
    // Check that we don't accidentally overwrite any registers!
    if (reg_conflict(size, RDI) || reg_conflict(size, RSI)) {
        panic(mv_string("In generate_poly_stack_move: invalid regitser provided to generate_poly_stack_move"));
    }

    // memmove (dest = rdi, src = rsi, size = rdx)
    // copy size into RDX
    build_binary_op(Mov, reg(RDI, sz_64), imm32(dest), ass, a, point);
    build_binary_op(Add, reg(RDI, sz_64), reg(RSP, sz_64), ass, a, point);
    build_binary_op(Mov, reg(RSI, sz_64), imm32(src), ass, a, point);
    build_binary_op(Add, reg(RSI, sz_64), reg(RBP, sz_64), ass, a, point);
    build_binary_op(Mov, reg(RDX, sz_64), size, ass, a, point);

#elif ABI == WIN_64
    if (reg_conflict(size, RDX) || reg_conflict(size, R8)) {
        panic(mv_string("In generate_poly_stack_move: invalid regitser provided to generate_poly_stack_move"));
    }

    // memmove (dest = rcx, src = rdx, size = r8)
    build_binary_op(Mov, reg(RCX, sz_64), imm32(dest), ass, a, point);
    build_binary_op(Add, reg(RCX, sz_64), reg(RSP, sz_64), ass, a, point);
    build_binary_op(Mov, reg(RDX, sz_64), imm32(src), ass, a, point);
    build_binary_op(Add, reg(RDX, sz_64), reg(RBP, sz_64), ass, a, point);
    build_binary_op(Mov, reg(R8, sz_64), size, ass, a, point);
#else
#error "Unknown calling convention"
#endif

    generate_c_call(memmove, ass, a, point);
    
}

U8Array free_registers(U8Array inputs, Allocator* a) {
    // These are registers that are volatile on BOTH architectures (Win64 + System-V)
    static uint8_t vr_data[7] = {RAX, RDX, RCX, R8, R9, R10, R11};
    U8Array volatile_registers = (U8Array) {
        .data = vr_data,
        .len = 7,
        .size = 7,
        .gpa = (Allocator){},
    };

    U8Array out_registers = mk_u8_array(7, a);
    for (size_t i = 0; i < volatile_registers.len; i++) {
        // If the register is not in the input, push it
        if (find_u8(volatile_registers.data[i], inputs) == inputs.len) {
            push_u8(volatile_registers.data[i], &out_registers);
        }
    }
    return out_registers;
}
