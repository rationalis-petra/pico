#include <string.h>

#include "data/stringify.h"

#include "platform/signals.h"
#include "platform/machine_info.h"
#include "platform/machine_info.h"
#include "components/pretty/string_printer.h"

#include "pico/data/error.h"
#include "pico/codegen/backend-direct/polymorphic.h"
#include "pico/codegen/backend-direct/internal.h"
#include "pico/binding/address_env.h"

#define STACK_ALIGN 8

// Implementation details
void generate_polymorphic_i(Syntax syn, AddressEnv* env, Target target, InternalLinkData* links, Allocator* a, ErrorPoint* point);

// Type Info
void generate_pi_type(PiType* type, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_align_to(Regname sz, Regname align, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_stack_size_of(Regname dest, PiType* type, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_offset_of(Regname dest, Symbol field, SymPtrAMap fields, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_variant_size_of(Regname dest, PtrArray* types, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_variant_align_of(Regname dest, PtrArray* types, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_variant_stack_size_of(Regname dest, PtrArray* types, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point);
U8Array free_registers(U8Array inputs, Allocator* a);

// Movement
void generate_poly_move(Location dest, Location src, Location size, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_poly_stack_move(Location dest_offset, Location src_offset, Location size, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_poly_copy_from_base(size_t dest, size_t src, Location size, Assembler* ass, Allocator* a, ErrorPoint* point);

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

    generate_polymorphic_i(body, env, target, links, a, point);

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
        build_binary_op(Sub, reg(R15, sz_64), reg(RAX, sz_64), ass, a, point);
        generate_poly_move(reg(R15, sz_64), reg(R14, sz_64), reg(RAX, sz_64), ass, a, point);
        build_binary_op(Mov, reg(R14, sz_64), reg(R15, sz_64), ass, a, point);

        // Next, restore the old stack bases (variable + static)
        build_binary_op(Mov, reg(R15, sz_64), rref8(RBP, 0, sz_64), ass, a, point);
        build_binary_op(Mov, reg(RBP, sz_64), rref8(RBP, 0x8, sz_64), ass, a, point);

        // Now, copy the return return address 
        build_binary_op(Mov, reg(RDX, sz_64), rref8(RSP, 0x18, sz_64), ass, a, point);

        // destination
        // return val store at (RET + FROM + RBP + R15 + ARGS = 8 + 8 + 8 + 8 ARGS = 0x20 + args)
        // return address = above - 0x8
        build_binary_op(Mov, rref8(RSP, 0x18 + args_size, sz_64), reg(R14, sz_64), ass, a, point);
        build_binary_op(Mov, rref8(RSP, 0x10 + args_size, sz_64), reg(RDX, sz_64), ass, a, point);
        build_binary_op(Add, reg(RSP, sz_64), imm8(0x10 + args_size), ass, a, point);
    } else {
        // Return on Data Stack
        // this is much like the steps above, where we copy 
        size_t ret_sz = pi_stack_size_of(*body.ptype);
    
        // Next, restore the old stack bases (variable + static)
        build_binary_op(Mov, reg(R15, sz_64), rref8(RBP, 0, sz_64), ass, a, point);
        build_binary_op(Mov, reg(R14, sz_64), rref8(RBP, 0, sz_64), ass, a, point);
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

void generate_polymorphic_i(Syntax syn, AddressEnv* env, Target target, InternalLinkData* links, Allocator* a, ErrorPoint* point) {
#ifdef DEBUG_ASSERT
    int64_t old_head = get_stack_head(env);
#endif

    Assembler* ass = target.target;
    switch (syn.type) {
    case SLitUntypedIntegral:
    case SLitTypedIntegral:
    case SLitUntypedFloating: 
    case SLitTypedFloating: 
    case SLitBool: 
    case SLitUnit: 
    case SLitString: 
        // For literals, use same as regular codegen
        generate_i(syn, env, target, links, a, point);
        break;
    case SVariable:
    case SAbsVariable: {
        // Lookup the variable in the assembly envionrment
        AddressEntry e = (syn.type == SVariable)
            ? address_env_lookup(syn.variable, env)
            : address_abs_lookup(syn.abvar, env);
        switch (e.type) {
        case ALocalDirect: {
            // Copy to the current level 
            size_t size = pi_stack_size_of(*syn.ptype);
            data_stack_grow(env, size);
            build_binary_op(Sub, reg(RSP, sz_64), imm32(size), ass, a, point);

            // The size | 7 "rounds up" size to the nearet multiple of eight. 
            // All local variables on the stack are stored with size rounded up
            // to nearest eight, so this allows objects with size, e.g. 5, 12,
            // etc. to have all their data copied  
            size = size | 7;
            if (e.stack_offset + size > INT8_MAX || (e.stack_offset - (int64_t)size) < INT8_MIN) {
                for (size_t i = 0; i < size / 8; i++) {
                    build_binary_op(Mov, reg(RAX, sz_64), rref32(RBP, e.stack_offset + (i * 8) , sz_64), ass, a, point);
                    build_binary_op(Mov, rref32(RSP, (i * 8), sz_64), reg(RAX, sz_64), ass, a, point);
                }
            } else {
                for (size_t i = 0; i < size / 8; i++) {
                    build_binary_op(Mov, reg(RAX, sz_64), rref8(RBP, e.stack_offset + (i * 8) , sz_64), ass, a, point);
                    build_binary_op(Mov, rref8(RSP, (i * 8), sz_64), reg(RAX, sz_64), ass, a, point);
                }
            }
            break;
        }
        case ALocalIndexed:
            // First, we need the size of the variable & allocate space for it on the stack
            // ------------------------------------------------------------------------------------------
            // Store stack size in R9
            generate_stack_size_of(R9, syn.ptype, env, ass, a, point);
            build_binary_op(Sub, reg(VSTACK_HEAD, sz_64), reg(R9, sz_64), ass, a, point);
            build_unary_op(Push, reg(VSTACK_HEAD, sz_64), ass, a, point);
            data_stack_grow(env, ADDRESS_SIZE);

            // Next, find the source location on the variable stack
            // ------------------------------
            build_binary_op(Mov, reg(R8, sz_64), rref8(RBP, e.stack_offset, sz_64), ass, a, point);

            // Finally, move the value from the source to the stack head
            generate_poly_move(reg(VSTACK_HEAD, sz_64), reg(R8, sz_64), reg(R9, sz_64), ass, a, point);
            break;
        case ATypeVar:
            throw_error(point, mv_string("Codegen not implemented for ATypeVar"));
            break;
        case AGlobal: {
            PiType indistinct_type = *strip_type(syn.ptype);

            // Procedures (inc. polymorphic procedures), Types are passed by reference (i.e. they are addresses). 
            // Dynamic Vars and instances pby value, but are guaranteed to take up 64 bits.
            if (indistinct_type.sort == TProc || indistinct_type.sort == TAll || indistinct_type.sort == TKind
                || indistinct_type.sort == TDynamic || indistinct_type.sort == TTraitInstance) {
                AsmResult out = build_binary_op(Mov, reg(R9, sz_64), imm64(*(uint64_t*)e.value), ass, a, point);
                backlink_global(syn.variable, out.backlink, links, a);
                build_unary_op(Push, reg(R9, sz_64), ass, a, point);

            // Primitives are (currently) all <= 64 bits, but may treating them
            // as 64-bits may overflow a allocated portion of memory, so we must
            // be more careful here.
            } else if (indistinct_type.sort == TPrim) {
                size_t prim_size = pi_size_of(indistinct_type);
                AsmResult out;
                /* if (prim_size == 0) { */
                /*     // Do nothing, unit value */
                /* } else */
                if (prim_size == 1) {
                    out = build_binary_op(Mov, reg(R9, sz_64), imm64(*(uint8_t*)e.value), ass, a, point);
                } else if (prim_size == 2) {
                    out = build_binary_op(Mov, reg(R9, sz_64), imm64(*(uint16_t*)e.value), ass, a, point);
                } else if (prim_size == 4) {
                    out = build_binary_op(Mov, reg(R9, sz_64), imm64(*(uint32_t*)e.value), ass, a, point);
                } else if (prim_size == 8) {
                    out = build_binary_op(Mov, reg(R9, sz_64), imm64(*(uint64_t*)e.value), ass, a, point);
                } else {
                    panic(mv_string("Codegen expects globals bound to primitives to have size 1, 2, 4 or 8."));
                }
                backlink_global(syn.variable, out.backlink, links, a);
                build_unary_op(Push, reg(R9, sz_64), ass, a, point);

            // Structs and Enums are passed by value, and have variable size.
            } else if (indistinct_type.sort == TStruct || indistinct_type.sort == TEnum) {
                size_t value_size = pi_size_of(indistinct_type);
                size_t stack_size = pi_stack_align(value_size);
                AsmResult out = build_binary_op(Mov, reg(RCX, sz_64), imm64((uint64_t)e.value), ass, a, point);
                backlink_global(syn.variable, out.backlink, links, a);

                // Allocate space on the stack for composite type (struct/enum)
                build_binary_op(Sub, reg(RSP, sz_64), imm32(stack_size), ass, a, point);

                generate_monomorphic_copy(RSP, RCX, value_size, ass, a, point);
            } else {
                throw_error(point,
                            string_ncat(a, 3,
                                        mv_string("Codegen: Global var '"),
                                        symbol_to_string(syn.variable, a),
                                        mv_string("' has unsupported sort")));
            }
            data_stack_grow(env, pi_stack_size_of(indistinct_type));
            break;
        }
        case ANotFound: {
            String sym = symbol_to_string(syn.variable, a);
            String msg = mv_string("Couldn't find variable during codegen: ");
            throw_error(point, string_cat(msg, sym, a));
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
        bool variable_args = false;
        int64_t arg_base = get_stack_head(env);
        for (size_t i = 0; i < syn.application.implicits.len; i++) {
            Syntax* arg = (Syntax*) syn.application.implicits.data[i];
            variable_args |= is_variable_in(arg->ptype, env);
            generate_polymorphic_i(*arg, env, target, links, a, point);
        }
        for (size_t i = 0; i < syn.application.args.len; i++) {
            Syntax* arg = (Syntax*) syn.application.args.data[i];
            variable_args |= is_variable_in(arg->ptype, env);
            generate_polymorphic_i(*arg, env, target, links, a, point);
        }

        if (!variable_args) {
            // Calculate size to pop
            size_t pop_sz = ADDRESS_SIZE;
            for (size_t i = 0; i < syn.application.implicits.len; i++) {
                PiType* aty = ((Syntax*)syn.application.implicits.data[i])->ptype;
                pop_sz += pi_stack_size_of(*aty);
            }
            for (size_t i = 0; i < syn.application.args.len; i++) {
                PiType* aty = ((Syntax*)syn.application.args.data[i])->ptype;
                pop_sz += pi_stack_size_of(*aty);
            }
            // This will push a function pointer onto the stack
            generate_polymorphic_i(*syn.application.function, env, target, links, a, point);
        
            // Regular Function Call
            // Pop the function into RCX; call the function
            build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);
            build_unary_op(Call, reg(RCX, sz_64), ass, a, point);
            data_stack_shrink(env, pop_sz);
            if (is_variable_in(syn.ptype, env)) {
                // Move value from data to 'variable' stack.
                panic(mv_string("not implemented: variable return from static function in poly code."));
            } else {
                data_stack_grow(env, pi_stack_size_of(*syn.ptype));
            }
        } else {
            generate_polymorphic_i(*syn.application.function, env, target, links, a, point);

            size_t args_size = 0;
            // Move variable args from the data stack to the variable stack
            for (size_t i = 0; i < syn.application.implicits.len; i++) {
                PiType* aty = ((Syntax*)syn.application.implicits.data[i])->ptype;
                if (is_variable_in(aty, env)) {

                    // Unfold argument onto data-stack
                    args_size += ADDRESS_SIZE;
                    generate_stack_size_of(RAX, aty, env, ass, a, point);
                    build_binary_op(Sub, reg(RSP, sz_64), reg(RAX, sz_64), ass, a, point);
                    generate_poly_copy_from_base(0, arg_base - args_size, reg(RAX, sz_64), ass, a, point);
                } else {

                    // Copy down From Above
                    size_t argsz = pi_stack_size_of(*aty);
                    args_size += argsz;
                    build_binary_op(Sub, reg(RSP, sz_64), imm8(argsz), ass, a, point);
                    generate_stack_copy_from_base(0, arg_base - args_size, argsz, ass, a, point);
                }
            }
            for (size_t i = 0; i < syn.application.args.len; i++) {
                PiType* aty = ((Syntax*)syn.application.args.data[i])->ptype;
                if (is_variable_in(aty, env)) {

                    // Unfold argument onto data-stack
                    args_size += ADDRESS_SIZE;;
                    generate_stack_size_of(RAX, aty, env, ass, a, point);
                    build_binary_op(Sub, reg(RSP, sz_64), reg(RAX, sz_64), ass, a, point);
                    build_binary_op(Mov, reg(RDX, sz_64), rref8(RBP, arg_base - args_size, sz_64), ass, a, point);
                    generate_poly_move(reg(RSP, sz_64), reg(RDX, sz_64), reg(RAX, sz_64), ass, a, point);
                } else {

                    // Copy down From Above
                    size_t argsz = pi_stack_size_of(*aty);
                    args_size += argsz;
                    build_binary_op(Sub, reg(RSP, sz_64), imm8(argsz), ass, a, point);
                    generate_stack_copy_from_base(0, arg_base - args_size, argsz, ass, a, point);
                }
            }
            // Account for the function
            args_size += ADDRESS_SIZE;
                
            // Copy down the function itself & call
            build_binary_op(Mov, reg(RCX, sz_64), rref8(RBP, arg_base - args_size, sz_64), ass, a, point);
            build_unary_op(Call, reg(RCX, sz_64), ass, a, point);
            data_stack_shrink(env, args_size);

            if (is_variable_in(syn.ptype, env)) {
                // Move value from data to 'variable' stack.
                generate_stack_size_of(RAX, syn.ptype, env, ass, a, point);
                build_binary_op(Sub, reg(R14, sz_64), reg(RAX, sz_64), ass, a, point);
                build_binary_op(Mov, reg(R9, sz_64), reg(RSP, sz_64), ass, a, point);
                build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
                generate_poly_move(reg(R14, sz_64), reg(R9, sz_64), reg(RAX, sz_64), ass, a, point);

                build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);
                build_binary_op(Add, reg(RSP, sz_64), imm8(args_size), ass, a, point);
                build_binary_op(Add, reg(RSP, sz_64), reg(RAX, sz_64), ass, a, point);
                build_unary_op(Push, reg(R14, sz_64), ass, a, point);
                data_stack_grow(env, ADDRESS_SIZE);
            } else {
                // Regular move up the data-stack
                size_t out_sz = pi_stack_size_of(*syn.ptype);
                generate_stack_move(args_size, 0, out_sz, ass, a, point);
                build_binary_op(Add, reg(RSP, sz_64), imm8(args_size), ass, a, point);
                data_stack_grow(env, pi_stack_size_of(*syn.ptype));
            }
        }
        break;
    }
    case SAllApplication: {
        // Polymorphic Funcall
        build_binary_op(Mov, reg(R15, sz_64), reg(R14, sz_64), ass, a, point);
        size_t static_arg_size = 0;

        SymbolArray type_vars = syn.all_application.function->ptype->binder.vars;
        for (size_t i = 0; i < syn.all_application.types.len; i++) {
            PiType* type = ((Syntax*)syn.all_application.types.data[i])->type_val;
            generate_pi_type(type, env, ass, a, point);
            static_arg_size += ADDRESS_SIZE;
        }

        bool mismatch_variable_args = false;
        PiType* fn_ty = strip_type(syn.all_application.function->ptype);
        if (fn_ty->sort == TAll) { fn_ty = fn_ty->binder.body; }

        for (size_t i = 0; i < syn.all_application.implicits.len; i++) {
            Syntax* arg = (Syntax*) syn.all_application.implicits.data[i];
            mismatch_variable_args |= is_variable_for(arg->ptype, type_vars);
            generate_polymorphic_i(*arg, env, target, links, a, point);
            static_arg_size += is_variable_in(arg->ptype, env) ? ADDRESS_SIZE : pi_stack_size_of(*arg->ptype);
        }

        for (size_t i = 0; i < syn.all_application.args.len; i++) {
            PiType* argty = fn_ty->proc.args.data[i];
            Syntax* arg = (Syntax*) syn.all_application.args.data[i];

            // The argument is variable for for the callee
            if (is_variable_for(argty, type_vars)) {
                // Is it also variable in our context?
                // if not, we need to move to variable stack, otherwise can keep here.
                if (!is_variable_in(arg->ptype, env)) {
                    panic(mv_string("Not implemented copy from static stack to dynamic stack in poly codegen: all_application"));
                }
            } else {
                mismatch_variable_args |= is_variable_in(arg->ptype, env);
            }
            static_arg_size += is_variable_in(arg->ptype, env) ? ADDRESS_SIZE : pi_stack_size_of(*arg->ptype);
            generate_polymorphic_i(*arg, env, target, links, a, point);
        }
        generate_polymorphic_i(*syn.all_application.function, env, target, links, a, point);
        static_arg_size += ADDRESS_SIZE;

        if (mismatch_variable_args) {
            panic(mv_string("Mismatch in variable args: codegen for this scenario not implemented."));
        }

        build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);

        build_unary_op(Call, reg(RCX, sz_64), ass, a, point);
        data_stack_shrink(env, static_arg_size);

        PiType* ty = fn_ty->sort == TProc ? fn_ty->proc.ret : fn_ty;
        bool callee_varstack = is_variable_for(ty, type_vars);
        bool caller_varstack = is_variable_in(syn.ptype, env);

        data_stack_grow(env, caller_varstack ? ADDRESS_SIZE : pi_stack_size_of(*syn.ptype));
        if (callee_varstack != caller_varstack) {
            if (callee_varstack) {
                size_t out_size = pi_stack_size_of(*syn.ptype);
                // Copy from varstack to our stack 
                build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);
                build_binary_op(Sub, reg(RSP, sz_64), imm32(out_size), ass, a, point);
                generate_monomorphic_copy(RSP, RCX, out_size, ass, a, point);

                // Pop value from variable stack
                build_binary_op(Add, reg(R14, sz_64), imm32(out_size), ass, a, point);
            } else {
                panic(mv_string("not implemented: Calling with value on caller varstack and callee static stack"));
            }
        } else {
        }

        break;
    }
    case SExists: {
        not_implemented(mv_string("Poly Direct Codegen for Exists"));
        break; 
    }
    case SUnpack: {
        not_implemented(mv_string("Poly Direct Codegen for Unpack"));
        break; 
    }
    case SStructure: {
        PiType* struct_type = strip_type(syn.ptype);
        if (is_variable_in(syn.ptype, env)) {
            // Reserve space on the stack for the structure to go
            if (syn.structure.base && syn.structure.base->type != SCheckedType) {
                generate_polymorphic_i(*syn.structure.base, env, target, links, a, point);
            } else {
                generate_stack_size_of(RAX, syn.ptype, env, ass, a, point);
                build_binary_op(Sub, reg(R14, sz_64), reg(RAX, sz_64), ass, a, point);
                build_unary_op(Push, reg(R14, sz_64), ass, a, point);
                data_stack_grow(env, ADDRESS_SIZE);
            }
            int64_t head = get_stack_head(env);

            // Generate code for each of the fields (in order)
            for (size_t i = 0; i < struct_type->structure.fields.len; i++)
                generate_polymorphic_i(*(Syntax *)syn.structure.fields.data[i].val, env, target, links, a, point);

            // Copy the field contents into the final struct.
            int64_t source_pos = head - get_stack_head(env);
            build_unary_op(Push, imm8(0), ass, a, point);
            for (size_t i = 0; i < struct_type->structure.fields.len; i++) {
                Symbol field = struct_type->structure.fields.data[i].key;
                PiType* ty = struct_type->structure.fields.data[i].val;

                size_t source_offset = 0;
                for (size_t j = 0; j < syn.structure.fields.len; j++) {
                    if (symbol_eq(field, syn.structure.fields.data[j].key) == 0) break;
                    PiType* field_ty = ((Syntax*)syn.structure.fields.data[j].val)->ptype;
                    source_offset += is_variable_in(field_ty, env) ? ADDRESS_SIZE : pi_stack_size_of(*field_ty);
                }

                // Update destination with alignment
                generate_align_of(R10, ty, env, ass, a, point);
                build_binary_op(Mov, reg(R9, sz_64), rref8(RSP, 0, sz_64), ass, a, point);
                generate_align_to(R9, R10, ass, a, point);
                build_binary_op(Mov, rref8(RSP, 0, sz_64), reg(R9, sz_64), ass, a, point);

                generate_size_of(RAX, ty, env, ass, a, point);
                build_unary_op(Push, reg(RAX, sz_64), ass, a, point);

                // Copy to dest
                build_binary_op(Mov, reg(RSI, sz_64), rref8(RSP, source_pos, sz_64), ass, a, point);
                build_binary_op(Add, reg(RSI, sz_64), rref8(RSP, 0, sz_64), ass, a, point);
                generate_poly_move(reg(RSI, sz_64), rref8(RSP, source_offset, sz_64), reg(RAX, sz_64), ass, a, point);

                // Add to size
                build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);
                build_binary_op(Add, rref8(RSP, 0, sz_64), reg(RAX, sz_64), ass, a, point);
            }
            // Now, copy up the stack
            build_binary_op(Add, reg(RSP, sz_64), imm8(source_pos), ass, a, point);
            data_stack_shrink(env, source_pos);

        } else {
            // The following code is copied exactly from the 'regular' codegen 
            // ------------
            // For structures, we have to be careful - this is because the order in
            // which arguments are evaluated is not necessarily the order in which
            // arguments are inserted into the structure.

            // Step 1: Make room on the stack for our struct (OR, just generate the
            // base struct)
            if (syn.structure.base && syn.structure.base->type != SCheckedType) {
                generate_polymorphic_i(*syn.structure.base, env, target, links, a, point);
            } else {
                size_t struct_size = pi_stack_size_of(*struct_type);
                build_binary_op(Sub, reg(RSP, sz_64), imm32(struct_size), ass, a, point);
                data_stack_grow(env, struct_size);
            }

            // Step 2: evaluate each element/variable binding
            for (size_t i = 0; i < syn.structure.fields.len; i++) {
                generate_polymorphic_i(*(Syntax*)syn.structure.fields.data[i].val, env, target, links, a, point);
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
            size_t source_region_size = 0;
            for (size_t i = 0; i < syn.structure.fields.len; i++) {
                source_region_size += pi_stack_size_of(*((Syntax*)syn.structure.fields.data[i].val)->ptype); 
            }
            size_t src_offset = 0;
            for (size_t i = 0; i < syn.structure.fields.len; i++) {
                // Find the field in the source & compute offset
                size_t dest_offset = 0;
                for (size_t j = 0; j < struct_type->structure.fields.len; j++) {
                    PiType* t = struct_type->structure.fields.data[j].val;
                    dest_offset = pi_size_align(dest_offset, pi_align_of(*t)); 

                    if (symbol_eq(syn.structure.fields.data[i].key, struct_type->structure.fields.data[j].key)) {
                        break; // offset is correct, end the loop
                    }
                    dest_offset += pi_size_of(*t); 
                }

                // We now both the source_offset and dest_offset. These are both
                // relative to the 'bottom' of their respective structures.
                // Therefore, we now need to find their offsets relative to the `top'
                // of the stack.
                size_t field_size = pi_size_of(*((Syntax*)syn.structure.fields.data[i].val)->ptype);
                src_offset += pi_stack_align(field_size);
                size_t src_stack_offset = source_region_size - src_offset;
                size_t dest_stack_offset = source_region_size + dest_offset;

                // Now, move the data.
                generate_stack_move(dest_stack_offset, src_stack_offset, field_size, ass, a, point);
            }

            // Remove the space occupied by the temporary values 
            build_binary_op(Add, reg(RSP, sz_64), imm32(source_region_size), ass, a, point);
            data_stack_shrink(env, source_region_size);
            break;
        }
        break;
    }
    case SProjector: {
        PiType* source_type = strip_type(syn.projector.val->ptype);
        if (source_type->sort == TStruct) {
            // -----------------------------------------------------------------
            //
            //                               STRUCTURE
            //
            // -----------------------------------------------------------------

            if (is_variable_in(syn.projector.val->ptype, env)) {
                panic(mv_string("not (yet) projecting out of variable structs."));
            } else {
                size_t out_sz = pi_stack_size_of(*syn.ptype);
                build_binary_op(Sub, reg(RSP, sz_64), imm32(out_sz), ass, a, point);
                data_stack_grow(env, out_sz);

                // Second, generate the structure/instance object
                generate_polymorphic_i(*syn.projector.val, env, target, links, a, point);
                size_t src_sz = pi_stack_size_of(*source_type);
                // Now, copy the structure to the destination
                // for this, we need the struct size + offset of field in the struct
                size_t offset = 0;
                for (size_t i = 0; i < source_type->structure.fields.len; i++) {
                    size_t align = pi_align_of(*(PiType*)source_type->structure.fields.data[i].val);
                    offset = pi_size_align(offset, align);
                    if (symbol_eq(source_type->structure.fields.data[i].key, syn.projector.field))
                        break;
                    offset += pi_size_of(*(PiType*)source_type->structure.fields.data[i].val);
                }

                generate_stack_move(src_sz, offset, out_sz, ass, a, point);
                // Now, remove the original struct from the stack
                build_binary_op(Add, reg(RSP, sz_64), imm32(src_sz), ass, a, point);

                data_stack_shrink(env, src_sz);
            } 
        } else {
            // -----------------------------------------------------------------
            //
            //                               INSTANCE
            //
            // -----------------------------------------------------------------

            // Second, generate the structure/instance object
            generate_polymorphic_i(*syn.projector.val, env, target, links, a, point);
            // Both instances (passed by reference) and structs (on variable
            // stack) will occupy an address size on the stack.
            size_t src_sz = ADDRESS_SIZE;

            // From this point, behaviour depends on whether we are projecting from
            // a structure or from an instance
            bool field_is_var = is_variable_in(syn.ptype, env);
            bool offset_is_var = false;
            for (size_t i = 0; i < source_type->instance.fields.len; i++) {
                offset_is_var |= is_variable_in(source_type->instance.fields.data[i].val, env);

                // Note: we only break *after* checking the field, as the offset
                //       of a field is dependent on its' alignment
                if (symbol_eq(source_type->instance.fields.data[i].key, syn.projector.field) == 0)
                    break;
            }

            if (!field_is_var && !offset_is_var) {
                // Pop the pointer to the instance from the stack - store in RSI
                data_stack_shrink(env, src_sz);
                build_unary_op(Pop, reg(RSI, sz_64), ass, a, point);

                // Now, calculate offset for field 
                size_t offset = 0;
                for (size_t i = 0; i < source_type->instance.fields.len; i++) {
                    offset = pi_size_align(offset, pi_align_of(*(PiType*)source_type->instance.fields.data[i].val));
                    if (symbol_eq(source_type->instance.fields.data[i].key, syn.projector.field))
                        break;
                    offset += pi_size_of(*(PiType*)source_type->instance.fields.data[i].val);
                }
                build_binary_op(Add, reg(RSI, sz_64), imm32(offset), ass, a, point);

                size_t val_sz = pi_size_of(*syn.ptype);
                size_t val_stack_sz = pi_stack_align(val_sz);
                build_binary_op(Sub, reg(RSP, sz_64), imm32(val_stack_sz), ass, a, point);
                data_stack_grow(env, val_stack_sz);

                generate_monomorphic_copy(RSP, RSI, val_sz, ass, a, point);
            } else if (!field_is_var && offset_is_var) {
                // Now, calculate offset for field 
                generate_offset_of(RDI, syn.projector.field, source_type->instance.fields, env, ass, a, point);

                // Pop the pointer to the instance from the stack - store in RSI
                data_stack_shrink(env, src_sz);
                build_unary_op(Pop, reg(RSI, sz_64), ass, a, point);
                build_binary_op(Add, reg(RSI, sz_64), reg(RDI, sz_64), ass, a, point);

                size_t val_sz = pi_size_of(*syn.ptype);
                size_t val_stack_sz = pi_stack_align(val_sz);
                build_binary_op(Sub, reg(RSP, sz_64), imm32(val_stack_sz), ass, a, point);
                data_stack_grow(env, val_stack_sz);

                generate_monomorphic_copy(RSP, RSI, val_sz, ass, a, point);
            } else {
                // Note: stack remains unchanged; as we popped of instance, but
                //       dynamic stack val ptr

                // Generate field offset and size. 
                generate_offset_of(RDI, syn.projector.field, source_type->instance.fields, env, ass, a, point);
                build_unary_op(Push, reg(RDI, sz_64), ass, a, point);
                generate_size_of(RAX, syn.ptype, env, ass, a, point);
                build_unary_op(Push, reg(RAX, sz_64), ass, a, point);

                build_binary_op(Mov, reg(R9, sz_64), rref8(RSP, 0, sz_64), ass, a, point);
                build_binary_op(Mov, reg(R8, sz_64), imm32(STACK_ALIGN), ass, a, point);
                generate_align_to(R9, R8, ass, a, point);

                build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);
                build_unary_op(Pop, reg(RDI, sz_64), ass, a, point);

                // source
                build_unary_op(Pop, reg(RSI, sz_64), ass, a, point);
                build_binary_op(Add, reg(RSI, sz_64), reg(RDI, sz_64), ass, a, point);

                build_binary_op(Sub, reg(R14, sz_64), reg(RAX, sz_64), ass, a, point);

                build_unary_op(Push, reg(R14, sz_64), ass, a, point);
                generate_poly_move(reg(R14, sz_64), reg(RSI, sz_64), reg(RAX, sz_64), ass, a, point);
            }
        }
        break;
    }
    case SConstructor: {
        not_implemented(mv_string("Polymorphic constructor"));
        PiType* enum_type = strip_type(syn.constructor.enum_type->type_val);

        generate_stack_size_of(RAX, enum_type, env, ass, a, point);
        generate_variant_size_of(RCX, enum_type->enumeration.variants.data[syn.variant.tag].val, env, ass, a, point);

        build_binary_op(Sub, reg(RAX, sz_64), reg(RCX, sz_64), ass, a, point);
        build_binary_op(Sub, reg(RSP, sz_64), reg(RAX, sz_64), ass, a, point);
        build_unary_op(Push, imm32(syn.constructor.tag), ass, a, point);
        break;
    }
    case SVariant: {
        not_implemented(mv_string("Polymorphic variant"));
        // TODO (FEAT BUG): ensure this will correctly handle non-stack aligned
        // enum tags, members and overall enums gracefully.
        const size_t tag_size = sizeof(uint64_t);
        PiType* enum_type = strip_type(syn.variant.enum_type->type_val);
        generate_stack_size_of(RCX, enum_type, env, ass, a, point);

        // Make space to fit the (final) variant
        build_binary_op(Sub, reg(RSP, sz_64), reg(RCX, sz_64), ass, a, point);

        // Set the tag
        build_binary_op(Mov, rref8(RSP, 0, sz_64), imm32(syn.constructor.tag), ass, a, point);

        // Generate each argument
        for (size_t i = 0; i < syn.variant.args.len; i++) {
            generate_polymorphic_i(*(Syntax*)syn.variant.args.data[i], env, target, links, a, point);
        }

        // Generate the variant stack size
        generate_variant_stack_size_of(RCX, enum_type->enumeration.variants.data[syn.variant.tag].val, env, ass, a, point);

        generate_variant_size_of(RAX, enum_type->enumeration.variants.data[syn.variant.tag].val, env, ass, a, point);


        // dest_stack_offset = RAX = variant_size + variant_stack_size - tag_size
        build_binary_op(Add, reg(RAX,sz_64), rref8(INDEX_REGISTER, 0, sz_64), ass, a, point);
        build_binary_op(Sub, reg(RAX,sz_64), imm8(tag_size), ass, a, point);


        // Index Stack shape
        // variant size
        // dest offset
        // src offset

        // Now, move them into the space allocated in reverse order
        PtrArray args = *(PtrArray*)enum_type->enumeration.variants.data[syn.variant.tag].val;

        // Note, as we are reversing the order, we start at the top of the stack (last enum element),
        // which gets copied to the end of the enum
        for (size_t i = 0; i < syn.variant.args.len; i++) {
            // We now have both the source_offset and dest_offset. These are both
            // relative to the 'bottom' of their respective structures.
            // Therefore, we now need to find their offsets relative to the `top'
            // of the stack.
            
            generate_size_of(RAX,args.data[syn.variant.args.len - (i + 1)], env, ass, a, point);
            build_binary_op(Sub, rref8(INDEX_REGISTER, -0x8, sz_64), reg(RAX, sz_64), ass, a, point);

            // RSI = dest offset, R9 = src offset
            build_binary_op(Mov, reg(RSI, sz_64), rref8(INDEX_REGISTER, -0x8, sz_64), ass, a, point);
            build_binary_op(Mov, reg(R9, sz_64), rref8(INDEX_REGISTER, 0, sz_64), ass, a, point);

            generate_poly_stack_move(reg(RSI, sz_64), reg(R9, sz_64), reg(RAX, sz_64), ass, a, point);

            //src_stack_offset += stack_size_of(field_size);
            generate_stack_size_of(RAX,args.data[syn.variant.args.len - (i + 1)], env, ass, a, point);
            build_binary_op(Add, rref8(INDEX_REGISTER, 0, sz_64), reg(RAX, sz_64), ass, a, point);
        }

        build_binary_op(Sub, reg(INDEX_REGISTER, sz_64), imm32(3 * ADDRESS_SIZE), ass, a, point);
        build_binary_op(Mov, reg(RCX, sz_64), rref8(INDEX_REGISTER, 8, sz_64), ass, a, point);
        build_binary_op(Sub, reg(RCX, sz_64), imm32(tag_size), ass, a, point);

        // Remove the space occupied by the temporary values 
        build_binary_op(Add, reg(RSP, sz_64), reg(RCX, sz_64), ass, a, point);
        break;
    }
    case SMatch: {
        not_implemented(mv_string("Polymorphic match"));
        // TODO: check that the match handles stack alignment correctly
        // Generate code for the value
        Syntax* match_value = syn.match.val;
        PiType* enum_type = strip_type(syn.match.val->ptype);

        generate_polymorphic_i(*match_value, env, target, links, a, point);

        SizeArray back_positions = mk_size_array(syn.match.clauses.len, a);
        PtrArray back_refs = mk_ptr_array(syn.match.clauses.len, a);

        // For each pattern match, generate two things 
        // 1. A comparison/check that the pattern has been matched
        // 2. A jump to the relevant location
        for (size_t i = 0; i < syn.match.clauses.len; i++) {
            SynClause clause = *(SynClause*)syn.match.clauses.data[i];
            build_binary_op(Cmp, rref8(RSP, 0, sz_64), imm32(clause.tag), ass, a, point);
            AsmResult out = build_unary_op(JE, imm32(0), ass, a, point);
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

            *branch_ref = (int32_t)(body_pos - branch_pos);

            SynClause clause = *(SynClause*)syn.match.clauses.data[i];
            PtrArray variant_types = *(PtrArray*)enum_type->enumeration.variants.data[clause.tag].val; 

            // Bind Clause Vars 
            // + tag size
            build_binary_op(Add, rref8(INDEX_REGISTER, 0, sz_64), imm8(sizeof(uint64_t)), ass, a, point);
            for (size_t i = 0; i < variant_types.len; i++) {
                address_bind_relative(clause.vars.data[i], 0, env);

                if (i + 1 != variant_types.len) {
                    generate_size_of(RAX, variant_types.data[i], env, ass, a, point);
                    build_binary_op(Add, reg(RAX, sz_64), rref8(INDEX_REGISTER, 0, sz_64), ass, a, point);

                }
            }

            generate_polymorphic_i(*clause.body, env, target, links, a, point);

            build_binary_op(Sub, reg(INDEX_REGISTER, sz_64), imm32(0x8 * variant_types.len), ass, a, point);
            address_pop_n(variant_types.len, env);

            // Generate jump to end of match expression to be backlinked later
            AsmResult out = build_unary_op(JMP, imm32(0), ass, a, point);
            push_size(get_pos(ass), &body_positions);
            push_ptr(get_instructions(ass).data + out.backlink, &body_refs);
        }

        // Finally, backlink all jumps from the bodies to the end.
        size_t curr_pos = get_pos(ass);
        for (size_t i = 0; i < body_positions.len; i++) {
            size_t body_pos = body_positions.data[i];
            uint8_t* body_ref = body_refs.data[i];

            if (curr_pos - body_pos > INT32_MAX) {
                throw_error(point, mk_string("Jump in match too large", a));
            } 

            *body_ref = (int32_t)(curr_pos - body_pos);
        }

        //size_t enum_size = pi_size_of(*match_value->ptype);
        //size_t out_size = pi_size_of(*syn.ptype);
        generate_stack_size_of(RCX, enum_type, env, ass, a, point);

        generate_stack_size_of(RAX, syn.ptype, env, ass, a, point);

        // Now, pop the enum off of the stack!
        generate_poly_stack_move(reg(RCX, sz_64), imm32(0), reg(RAX, sz_64), ass, a, point);

        build_binary_op(Add, reg(RSP, sz_64), rref8(INDEX_REGISTER, -0x8, sz_64), ass, a, point);
        build_binary_op(Sub, reg(INDEX_REGISTER, sz_64), imm32(-0x16), ass, a, point);
        break;
    }
    case SDynamicUse: {
        // TODO: check that the dynamic use handles stack alignment correctly
        generate_polymorphic_i(*syn.use, env, target, links, a, point);

        // We now have a dynamic variable: get its' value as ptr

#if ABI == SYSTEM_V_64 
        // arg1 = rdi
        build_unary_op(Pop, reg(RDI, sz_64), ass, a, point);
#elif ABI == WIN_64 
        // arg1 = rcx
        build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);
#else
#error "unknown ABI"
#endif

        // call function
        generate_c_call(get_dynamic_val, ass, a, point);

        if (is_variable_in(syn.ptype, env)) {
            panic(mv_string("Not yet generating code for polymorphic dynamic use"));
        } else{
            // Now, allocate space on stack
            size_t val_size = pi_size_of(*syn.ptype);
            build_binary_op(Sub, reg(RSP, sz_64), imm32(pi_stack_align(val_size)), ass, a, point);
            build_binary_op(Mov, reg(RCX, sz_64), reg(RAX, sz_64), ass, a, point);

            generate_monomorphic_copy(RSP, RCX, val_size, ass, a, point);
            data_stack_shrink(env, ADDRESS_SIZE);
            data_stack_grow(env, val_size);
        }
        break;
    }
    case SDynamicSet: {
        size_t val_size = pi_size_of(*syn.dynamic_set.new_val->ptype);
        generate_polymorphic_i(*syn.dynamic_set.dynamic, env, target, links, a, point);
        generate_polymorphic_i(*syn.dynamic_set.new_val, env, target, links, a, point);

#if ABI == SYSTEM_V_64 
        // arg1 = rdi, arg2 = rsi
        build_binary_op(Mov, reg(RDI, sz_64), rref8(RSP, val_size, sz_64), ass, a, point);
        build_binary_op(Mov, reg(RSI, sz_64), reg(RSP, sz_64), ass, a, point);
        build_binary_op(Mov, reg(RDX, sz_64), imm32(val_size), ass, a, point);
#elif ABI == WIN_64 
        // arg1 = rcx, arg2 = rdx
        build_binary_op(Mov, reg(RCX, sz_64), rref8(RSP, val_size, sz_64), ass, a, point);
        build_binary_op(Mov, reg(RDX, sz_64), reg(RSP, sz_64), ass, a, point);
        build_binary_op(Mov, reg(R8, sz_64), imm32(val_size), ass, a, point);
#else
#error "unknown ABI"
#endif
        // call function
        generate_c_call(set_dynamic_val, ass, a, point);

        if (is_variable_in(syn.ptype, env)) {
            panic(mv_string("Not yet generating code for polymorphic dynamic set"));
        } else {
            build_binary_op(Add, reg(RSP, sz_64), imm32(val_size + ADDRESS_SIZE), ass, a, point);
            data_stack_shrink(env, ADDRESS_SIZE + val_size);
        }
        break;
    }
    case SLet: {
        size_t bind_sz = ADDRESS_SIZE; 

        build_unary_op(Push, reg(R14, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);

        for (size_t i = 0; i < syn.let_expr.bindings.len; i++) {
            SymPtrCell elt = syn.let_expr.bindings.data[i];
            generate_polymorphic_i(*(Syntax*)elt.val, env, target, links, a, point);

            if (is_variable_in(((Syntax*)elt.val)->ptype, env)) {
                address_bind_relative_index(elt.key, 0, env);
                bind_sz += ADDRESS_SIZE;
            } else {
                size_t stack_sz = pi_stack_size_of(*((Syntax*)elt.val)->ptype);
                address_bind_relative(elt.key, 0, env);
                bind_sz += stack_sz;
            }
        }
        generate_polymorphic_i(*syn.let_expr.body, env, target, links, a, point);

        if (is_variable_in(syn.ptype, env)) {
            generate_stack_size_of(RAX, syn.ptype, env, ass, a, point);

            // Grab the value of R14 we pushed at the beginning - offset bind_sz + stack head 
            build_binary_op(Mov, reg(R14, sz_64), rref8(RSP, bind_sz, sz_64), ass, a, point);
            build_binary_op(Sub, reg(R14, sz_64), reg(RAX, sz_64), ass, a, point);

            build_binary_op(Mov, reg(R9, sz_64), rref8(RSP, 0, sz_64), ass, a, point);

            generate_poly_move(reg(R14, sz_64), reg(R9, sz_64), reg(RAX, sz_64), ass, a, point);

            // Store current index in stack return position
            build_binary_op(Mov, rref8(RSP, bind_sz, sz_64), reg(R14, sz_64), ass, a, point);
            build_binary_op(Add, reg(RSP, sz_64), imm8(bind_sz), ass, a, point);
            data_stack_shrink(env, bind_sz);
        } else {
            size_t stack_sz = pi_stack_size_of(*syn.ptype);
            build_binary_op(Mov, reg(R14, sz_64), rref8(RSP, bind_sz + stack_sz - ADDRESS_SIZE, sz_64), ass, a, point);
            generate_stack_move(bind_sz, 0, stack_sz, ass, a, point);
            build_binary_op(Add, reg(RSP, sz_64), imm8(bind_sz), ass, a, point);
            data_stack_shrink(env, bind_sz);
        }
        break;
    }
    case SIf: {
        // Generate the condition
        generate_polymorphic_i(*syn.if_expr.condition, env, target, links, a, point);

        // Pop the bool into R9; compare with 0
        build_unary_op(Pop, reg(R9, sz_64), ass, a, point);
        build_binary_op(Cmp, reg(R9, sz_64), imm32(0), ass, a, point);
        data_stack_shrink(env, ADDRESS_SIZE);

        // ---------- CONDITIONAL JUMP ----------
        // compare the value to 0
        // jump to false branch if equal to 0 -- the 32-bit immediate is a placeholder
        AsmResult out = build_unary_op(JE, imm32(0), ass, a, point);
        size_t start_pos = get_pos(ass);

        size_t jmp_loc = out.backlink;

        // ---------- TRUE BRANCH ----------
        // now, generate the code to run (if true)
        generate_polymorphic_i(*syn.if_expr.true_branch, env, target, links, a, point);

        // Generate jump to end of false branch to be backlinked later
        out = build_unary_op(JMP, imm32(0), ass, a, point);

        // calc backlink offset
        size_t end_pos = get_pos(ass);
        if (end_pos - start_pos > INT32_MAX) {
            throw_error(point, mk_string("Jump in conditional too large", a));
        } 

        // backlink
        set_i32_backlink(ass, jmp_loc, end_pos - start_pos);
        jmp_loc = out.backlink;
        start_pos = get_pos(ass);

        data_stack_shrink(env, is_variable_in(syn.ptype, env) ? ADDRESS_SIZE : pi_stack_size_of(*syn.ptype));

        // ---------- FALSE BRANCH ----------
        // Generate code for the false branch
        generate_polymorphic_i(*syn.if_expr.false_branch, env, target, links, a, point);

        // calc backlink offset
        end_pos = get_pos(ass);
        if (end_pos - start_pos > INT32_MAX) {
            throw_error(point, mk_string("Jump in conditional too large", a));
        } 
        set_i32_backlink(ass, jmp_loc, end_pos - start_pos);
        break;
    }
    case SLabels: {
        // Labels: The code-generation for labels is as follows: 
        // 1. Push the current variable stack head ($r14)  
        // 2. Add labels to environment
        // 3. Generate expression
        // 4. For each expression:
        //  5.1 Mark Beginning of label expression
        //  6.2 Generate label expressions
        // 7. Backlink all labels.
        
        /* build_unary_op(Push, reg(VSTACK_HEAD, sz_64), ass, a, point); */
        /* data_stack_grow(env, ADDRESS_SIZE); */

        SymbolArray labels = mk_symbol_array(syn.labels.terms.len, a);
        for (size_t i = 0; i < syn.labels.terms.len; i++) 
            push_symbol(syn.labels.terms.data[i].key, &labels);

        address_start_labels(labels, env);

        generate_polymorphic_i(*syn.labels.entry, env, target, links, a, point);

        SymSizeAssoc label_points = mk_sym_size_assoc(syn.labels.terms.len, a);
        SymSizeAssoc label_jumps = mk_sym_size_assoc(syn.labels.terms.len, a);

        size_t out_size = pi_size_of(*syn.ptype);
        for (size_t i = 0; i < syn.labels.terms.len; i++) {
            // Clear the stack offset, either from expression or previous label
            data_stack_shrink(env,out_size);
            SymPtrACell cell = syn.labels.terms.data[i];

            // Mark Label
            size_t pos = get_pos(ass);
            sym_size_bind(cell.key, pos, &label_points);
            
            SynLabelBranch* branch = cell.val;

            // Bind Label Vars 
            size_t arg_total = 0;
            SymSizeAssoc arg_sizes = mk_sym_size_assoc(branch->args.len, a);
            for (size_t i = 0; i < branch->args.len; i++) {
                size_t arg_size = pi_size_of(*(PiType*)branch->args.data[i].val);
                sym_size_bind(branch->args.data[i].key, arg_size, &arg_sizes);
                arg_total += arg_size;
            }

            data_stack_grow(env,arg_total);
            address_bind_label_vars(arg_sizes, env);
            generate_polymorphic_i(*branch->body, env, target, links, a, point);
            address_unbind_label_vars(env);

            LabelEntry lble = label_env_lookup(cell.key, env);
            if (lble.type == Err)
                throw_error(point, mv_string("Label not found during codegen!!"));
            data_stack_shrink(env, arg_total);

            // Copy the result down the stack
            generate_stack_move(arg_total, 0, out_size, ass, a, point);
            build_binary_op(Add, reg(RSP, sz_64), imm8(arg_total), ass, a, point);

            AsmResult out = build_unary_op(JMP, imm32(0), ass, a, point);
            sym_size_bind(cell.key, out.backlink, &label_jumps);
        }

        size_t label_end = get_pos(ass);

        for (size_t i = 0; i < label_points.len; i++)  {
            Symbol sym = label_points.data[i].key;
            size_t dest = label_points.data[i].val;

            // Step 1: make the end of the label jump to the end of the expression.
            {
                size_t backlink = label_jumps.data[i].val;
                size_t origin = backlink + 4; // the + 4 accounts for the 4-byte immediate
                size_t dest = label_end;

                int64_t amt = dest - origin;
                if (amt < INT32_MIN || amt > INT32_MAX) panic(mv_string("Label jump too large!"));
                
                set_i32_backlink(ass, backlink, amt);
            }


            // Step 2: fill out each jump to this label.
            SizeArray* arr = sym_sarr_lookup(sym, links->gotolinks);
            if (!arr) panic(mv_string("Can't find size array when backlinking label!"));

            for (size_t i = 0; i < arr->len; i++) {
                size_t backlink = arr->data[i];
                size_t origin = backlink + 4; // the + 4 accounts for the 4-byte immediate

                int64_t amt = dest - origin;
                if (amt < INT32_MIN || amt > INT32_MAX) panic(mv_string("Label jump too large!"));
                
                set_i32_backlink(ass, backlink, amt);
            }
        }

        address_end_labels(env);
        break;
    }
    case SGoTo: {
        // Generating code for a goto:
        // 1. Generate args
        size_t arg_total = 0;
        for (size_t i = 0; i < syn.go_to.args.len; i++) {
            Syntax* expr = syn.go_to.args.data[i];
            arg_total += pi_stack_size_of(*expr->ptype);
            generate_polymorphic_i(*expr, env, target, links, a, point);
        }

        // 2. Backlink the label
        LabelEntry lble = label_env_lookup(syn.go_to.label, env);
        if (lble.type == Ok) {
            size_t delta = lble.stack_offset - arg_total;
            generate_stack_move(delta, 0, arg_total, ass, a, point);
            if (delta != 0) {
                // Adjust the stack so it has the same value that one would have
                // going into a labels expression.
                // TODO handle dynamic variable unbinding (if needed!)
                build_binary_op(Add, reg(RSP, sz_64), imm32(delta), ass, a, point);
            }

            // Stack sould "pretend" it pushed a value of type syn.ptype and
            // consumed all args. This is so that, e.g. if this go-to is inside
            // a seq or if, the other branches of the if or the rest of the seq
            // generates assuming the correct stack offset.
            data_stack_shrink(env, arg_total);
            data_stack_grow(env, pi_size_of(*syn.ptype));

            AsmResult out = build_unary_op(JMP, imm32(0), ass, a, point);

            backlink_goto(syn.go_to.label, out.backlink, links, a);
        } else {
            throw_error(point, mv_string("Label not found during codegen!!"));
        }
        break;
    }
    case SSequence: {
        size_t bind_sz = ADDRESS_SIZE; 

        build_unary_op(Push, reg(R14, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);

        for (size_t i = 0; i < syn.sequence.elements.len; i++) {
            SeqElt* elt = syn.sequence.elements.data[i];
            generate_polymorphic_i(*elt->expr, env, target, links, a, point);

            if (elt->is_binding && i + 1 != syn.sequence.elements.len) {
                if (is_variable_in(elt->expr->ptype, env)) {
                    address_bind_relative_index(elt->symbol, 0, env);
                    bind_sz += ADDRESS_SIZE;
                } else {
                    size_t stack_sz = pi_stack_size_of(*elt->expr->ptype);
                    address_bind_relative(elt->symbol, 0, env);
                    bind_sz += stack_sz;
                }
            }

            // Remember: We do not pop off the stack if this is
            // a) a binding (hence this is an else) 
            // b) the last value in the sequence (as this value is preserved)
            else if (i + 1 != syn.sequence.elements.len) {
                if (is_variable_in(elt->expr->ptype, env)) {
                    generate_stack_size_of(RAX, elt->expr->ptype, env, ass, a, point);
                    build_binary_op(Add, reg(VSTACK_HEAD, sz_64), reg(RAX, sz_64), ass, a, point);
                    build_binary_op(Add, reg(RSP, sz_64), imm8(ADDRESS_SIZE), ass, a, point);
                    data_stack_shrink(env, ADDRESS_SIZE);
                } else {
                    size_t stack_sz = pi_stack_size_of(*elt->expr->ptype);
                    build_binary_op(Add, reg(RSP, sz_64), imm8(stack_sz), ass, a, point);
                    data_stack_shrink(env, stack_sz);
                }
            }
            
        }
        if (is_variable_in(syn.ptype, env)) {
            generate_stack_size_of(RAX, syn.ptype, env, ass, a, point);

            // Grab the value of R14 we pushed at the beginning - offset bind_sz + stack head 
            build_binary_op(Mov, reg(R14, sz_64), rref8(RSP, bind_sz + ADDRESS_SIZE, sz_64), ass, a, point);
            build_binary_op(Sub, reg(R14, sz_64), reg(RAX, sz_64), ass, a, point);

            build_binary_op(Mov, reg(R9, sz_64), rref8(RSP, 0, sz_64), ass, a, point);

            generate_poly_move(reg(R14, sz_64), reg(R9, sz_64), reg(RAX, sz_64), ass, a, point);

            // Store current index in stack return position
            generate_stack_move(bind_sz, 0, ADDRESS_SIZE, ass, a, point);
            build_binary_op(Add, reg(RSP, sz_64), imm8(bind_sz), ass, a, point);
            data_stack_shrink(env, bind_sz);
        } else {
            size_t stack_sz = pi_stack_size_of(*syn.ptype);
            build_binary_op(Mov, reg(R14, sz_64), rref8(RSP, bind_sz + stack_sz, sz_64), ass, a, point);
            generate_stack_move(bind_sz, 0, stack_sz, ass, a, point);
            build_binary_op(Add, reg(RSP, sz_64), imm8(bind_sz), ass, a, point);
            data_stack_shrink(env, bind_sz);
        }
        break;
    }
    case SIs:
        generate_polymorphic_i(*syn.is.val, env, target, links, a, point);
        break;
    case SInTo:
        generate_polymorphic_i(*syn.into.val, env, target, links, a, point);
        break;
    case SOutOf:
        generate_polymorphic_i(*syn.out_of.val, env, target, links, a, point);
        break;
    case SName:
        generate_polymorphic_i(*syn.name.val, env, target, links, a, point);
        break;
    case SUnName:
        generate_polymorphic_i(*syn.unname, env, target, links, a, point);
        break;
    case SDynAlloc: {
        throw_error(point, mv_string("Not implemented: dynamic allocation in polymorphic code."));
    }
    case SSizeOf: {
        generate_size_of(RAX, syn.size->type_val, env, ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);
        break;
    }
    case SAlignOf: {
        generate_align_of(RAX, syn.size->type_val, env, ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);
        break;
    }
    case SCheckedType: {
        build_binary_op(Mov, reg(R9, sz_64), imm64((uint64_t)syn.type_val), ass, a, point);
        build_unary_op(Push, reg(R9, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);
        break;
    }
    default: {
        panic(mv_string("Invalid abstract term in polymorphic codegen."));
    }
    }

#ifdef DEBUG_ASSERT
    int64_t new_head = get_stack_head(env);
    int64_t diff = old_head - new_head;
    if (diff < 0) panic(mv_string("diff < 0!"));

    size_t stack_sz = is_variable_in(syn.ptype, env) ?
        ADDRESS_SIZE : pi_stack_size_of(*syn.ptype);

    // Justification for cast: stack size of is extremely unlikely to be 2^63 bytes
    //  in size!
    if (diff != (int64_t)stack_sz) {
        String expected = string_u64(stack_sz, a);
        String actual = string_i64(diff, a);
        String message = string_ncat(a, 4,
                                     mv_string("Address environment constraint violated: expected size of the stack is wrong!\nExpected "),
                                     expected, mv_string(" but got "), actual);
        panic(message);
    }
#endif
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
                throw_error(point, mk_string("Too Many Local variables!", a));
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
                throw_error(point, mk_string("Too Many Local variables!", a));
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

void generate_offset_of(Regname dest, Symbol field, SymPtrAMap fields, AddressEnv *env, Assembler *ass, Allocator *a, ErrorPoint *point) {
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
        generate_variant_align_of(R9, types->data[i], env, ass, a, point);
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
                throw_error(point, mk_string("Too Many Local variables!", a));
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
                throw_error(point, mk_string("Too Many Local variables!", a));
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
        throw_error(point, mv_string("Error in generate_align_to: sz or align registers were either RAX, RCX or RDX!"));
    }

    /* size_t rem = size % 8; */
    /* size_t pad = rem == 0 ? 0 : align - rem; */
    /* return size + pad; */

    // We accomplish modulo with IDiv, which stores the remainder (modulo) in RDX 
    build_binary_op(Mov, reg(RAX, sz_64), reg(sz_reg, sz_64), ass, a, point); 
    build_binary_op(Mov, reg(RDX, sz_64), imm32(0), ass, a, point); 
    build_binary_op(Mov, reg(RCX, sz_64), reg(align, sz_64), ass, a, point); 
    build_unary_op(IDiv, reg(RCX, sz_64), ass, a, point); 

    // Now, rem is in RDX

    // Store align - rem in RAX and 0 in RCX
    build_binary_op(Mov, reg(RAX, sz_64), reg(align, sz_64), ass, a, point); 
    build_binary_op(Sub, reg(RAX, sz_64), reg(RDX, sz_64), ass, a, point); 
    build_binary_op(Mov, reg(RCX, sz_64), imm32(0), ass, a, point); 

    // Nowd to the compare (rem == 0) and CMove (asignment base on compare), so
    // the result (pad) is in RDX
    build_binary_op(Cmp, reg(RDX, sz_64), imm32(0), ass, a, point); 
    build_binary_op(CMovE, reg(RAX, sz_64), reg(RCX, sz_64), ass, a, point); 

    // Finally, add size (sz_reg) to padding (RDX)
    build_binary_op(Add, reg(sz_reg, sz_64), reg(RAX, sz_64), ass, a, point); 
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
