#include <string.h>

#include "platform/signals.h"
#include "platform/machine_info.h"
#include "platform/machine_info.h"
#include "components/pretty/string_printer.h"

#include "pico/data/error.h"
#include "pico/codegen/backend-direct/polymorphic.h"
#include "pico/codegen/backend-direct/internal.h"
#include "pico/binding/address_env.h"

// Implementation details
void generate_polymorphic_i(Syntax syn, AddressEnv* env, Target target, InternalLinkData* links, Allocator* a, ErrorPoint* point);

// Type Info
void generate_align_to(Regname sz, Regname align, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_stack_size_of(Regname dest, PiType* type, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_variant_size_of(Regname dest, PtrArray* types, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_variant_align_of(Regname dest, PtrArray* types, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_variant_stack_size_of(Regname dest, PtrArray* types, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point);

// Movement
void generate_poly_move(Location dest, Location src, Location size, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_poly_stack_move(Location dest_offset, Location src_offset, Location size, Assembler* ass, Allocator* a, ErrorPoint* point);

void generate_polymorphic(SymbolArray types, Syntax syn, AddressEnv* env, Target target, InternalLinkData* links, Allocator* a, ErrorPoint* point) {
    Assembler* ass = target.target;
    BindingArray vars;
    Syntax body;

    size_t args_size = 0;
    if (syn.type == SProcedure) {
        vars = mk_binding_array(syn.procedure.args.len + syn.procedure.implicits.len, a);
        for (size_t i = 0; i < syn.procedure.implicits.len; i++) {
            PiType* impl_ty = syn.ptype->proc.implicits.data[i];
            if (is_variable(impl_ty)) {
                args_size += ADDRESS_SIZE;
                Binding bind = (Binding) {
                    .sym = syn.procedure.implicits.data[i].key,
                    .size = ADDRESS_SIZE,
                    .is_variable = true,
                };
                push_binding(bind, &vars);
            } else {
                size_t arg_sz = pi_size_of(*impl_ty);
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
            if (is_variable(arg_ty)) {
                args_size += ADDRESS_SIZE;
                Binding bind = (Binding) {
                    .sym = syn.procedure.args.data[i].key,
                    .size = ADDRESS_SIZE,
                    .is_variable = true,
                };
                push_binding(bind, &vars);
            } else {
                size_t arg_sz = pi_size_of(*arg_ty);
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

    if (is_variable(body.ptype)) {
        // Return on Variable Stack
        // R15 is the 'destination' on the variable stack of a return
        // argument.

        // Store value at the stack head
        generate_stack_size_of(RAX, body.ptype, env, ass, a, point);
        build_binary_op(Sub, reg(R15, sz_64), reg(RAX, sz_64), ass, a, point);
        generate_poly_move(reg(R15, sz_64), reg(R14, sz_64), reg(RAX, sz_64), ass, a, point);
        build_binary_op(Mov, reg(R14, sz_64), reg(R15, sz_64), ass, a, point);

        // Next, restore the old stack bases (variable + static)
        build_binary_op(Mov, reg(RBP, sz_64), rref8(RBP, 0x8, sz_64), ass, a, point);
        build_binary_op(Mov, reg(R15, sz_64), rref8(RBP, 0, sz_64), ass, a, point);

        // Now, copy the return value and return address too
        build_binary_op(Mov, reg(RCX, sz_64), rref8(RSP, 0, sz_64), ass, a, point);
        build_binary_op(Mov, reg(RDX, sz_64), rref8(RSP, 0x18, sz_64), ass, a, point);

        // destination
        // return val (RET + FROM + RBP + R15 + ARGS = 8 + 8 + 8 + 8 ARGS = 0x20 + args)
        // return address = above - 0x8
        build_binary_op(Mov, rref8(RSP, 0x20 + args_size, sz_64), reg(RCX, sz_64), ass, a, point);
        build_binary_op(Mov, rref8(RSP, 0x18 + args_size, sz_64), reg(RDX, sz_64), ass, a, point);
        build_binary_op(Add, reg(RSP, sz_64), imm8(0x18 + args_size), ass, a, point);
    } else {
        // Return on Data Stack
        // this is much like the steps above, where we copy 
        size_t ret_sz = pi_stack_size_of(*body.ptype);
    
        // Next, restore the old stack bases (variable + static)
        build_binary_op(Mov, reg(RBP, sz_64), rref8(RBP, 0x8, sz_64), ass, a, point);
        build_binary_op(Mov, reg(R15, sz_64), rref8(RBP, 0, sz_64), ass, a, point);
        build_binary_op(Mov, reg(R14, sz_64), rref8(RBP, 0, sz_64), ass, a, point);

        // Now, copy the return address into a (safe) register
        build_binary_op(Mov, reg(RBX, sz_64), rref8(RSP, 0x10 + ret_sz, sz_64), ass, a, point);
        generate_stack_move(0x20 + args_size, 0, ret_sz, ass, a, point);

        // Then the return address
        build_binary_op(Mov, rref8(RSP, 0x18 + args_size, sz_64), reg(RBX, sz_64), ass, a, point);
        build_binary_op(Add, reg(RSP, sz_64), imm8(0x18 + args_size), ass, a, point);
    }

    build_nullary_op(Ret, ass, a, point);
    address_end_poly(env, a);
}

void generate_polymorphic_i(Syntax syn, AddressEnv* env, Target target, InternalLinkData* links, Allocator* a, ErrorPoint* point) {
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

            // Procedures (inc. polymorphic procedures), Types and types are passed by reference (i.e. they are addresses). 
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
        not_implemented(mv_string("Polymorphic application"));
        // Generate the arguments
        for (size_t i = 0; i < syn.application.implicits.len; i++) {
            Syntax* arg = (Syntax*) syn.application.implicits.data[i];
            generate_polymorphic_i(*arg, env, target, links, a, point);
        }
        for (size_t i = 0; i < syn.application.args.len; i++) {
            Syntax* arg = (Syntax*) syn.application.args.data[i];
            generate_polymorphic_i(*arg, env, target, links, a, point);
        }

        // This will push a function pointer onto the stack
        generate_polymorphic_i(*syn.application.function, env, target, links, a, point);
        
        // Regular Function Call
        // Pop the function into RCX; call the function
        build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);
        build_unary_op(Call, reg(RCX, sz_64), ass, a, point);
        break;
    }
    case SAllApplication: {
        not_implemented(mv_string("Polymorphic all-application"));
        // Polymorphic Funcall
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
        not_implemented(mv_string("Polymorphic structure"));
        PiType* struct_type = strip_type(syn.ptype);

        // Reserve space on the stack for the structure to go
        if (syn.structure.base && syn.structure.base->type != SCheckedType) {
            generate_polymorphic_i(*syn.structure.base, env, target, links, a, point);
        }    else {
            generate_stack_size_of(RAX, syn.ptype, env, ass, a, point);
            build_binary_op(Sub, reg(RSP, sz_64), reg(RAX, sz_64), ass, a, point);
        }

        // Generate code for each of the fields (in order)
        for (size_t i = 0; i < syn.structure.fields.len; i++)
        {
            generate_polymorphic_i(*(Syntax *)syn.structure.fields.data[i].val, env, target, links, a, point);
        }

        // -------------------------------------------------------------------------
        // 
        // Movement
        // --------
        // The most difficult part of generating a structure is ensuring that
        // all values end up in the "correct" location (i.e. correct part of the
        // stack).
        //
        // We start by calculating two things:
        // 1. The total size of all values just pushed onto the stack. This is
        //    called the source-region-size or source-size.
        // 2. The destination offsets of each field.
        //
        // This is then used to perform a series of smaller moves (one for each
        // field)
        // 
        // -------------------------------------------------------------------------

        for (size_t i = 0; i < struct_type->structure.fields.len; i++) {
            generate_align_of(R8, struct_type->structure.fields.data[i].val, env, ass, a, point);
            build_binary_op(Mov, reg(R9, sz_64), rref8(INDEX_REGISTER, 0, sz_64), ass, a, point);
            generate_align_to(R9, R8, ass, a, point);

            // Update old offset to have alignment
            build_binary_op(Mov, rref8(INDEX_REGISTER, 0, sz_64), reg(R9, sz_64), ass, a, point);


            // Add size to new offset
            generate_size_of(R10, struct_type->structure.fields.data[i].val, env, ass, a, point);
            build_binary_op(Add, rref8(INDEX_REGISTER, 0, sz_64), reg(R10, sz_64), ass, a, point);

            // If this field exists in the asturct, add stack size to source size.
            bool field_source = false;
            for (size_t j = 0; j < syn.structure.fields.len; j++) {
                if (symbol_eq(syn.structure.fields.data[j].key, struct_type->structure.fields.data[i].key)) {
                    field_source = true; 
                    break; // Offset is correct, end the loop
                }
            }

            if (field_source) {
                build_binary_op(Mov, reg(R9, sz_64), imm32(8), ass, a, point);
                generate_align_to(R10, R9, ass, a, point);
                build_binary_op(Add, rref8(INDEX_REGISTER, -8 * (i + 2), sz_64), reg(R10, sz_64), ass, a, point);
            }
        }

        // Align the struct size (top of stack)
        generate_align_of(R8, struct_type, env, ass, a, point);
        build_binary_op(Mov, reg(R9, sz_64), rref8(INDEX_REGISTER, 0, sz_64), ass, a, point);
        generate_align_to(R9, R8, ass, a, point);
        build_binary_op(Mov, rref8(INDEX_REGISTER, 0, sz_64), reg(R9, sz_64), ass, a, point);

        // (elt n + 3) = source_region_size
        // (elt [2, n + 2] control_stack) = dest_offsets, struct size (reversed)
        // (elt 1 control_stack) = struct size
        // (elt 0 control_stack) = source_offset

        const size_t nfields = struct_type->structure.fields.len;
        for (size_t i = 0; i < syn.structure.fields.len; i++) {
            // Find what index field the structure has.
            size_t field_offset_idx = 0; 
            for (size_t j = 0; j < struct_type->structure.fields.len; j++) {
                if (symbol_eq(syn.structure.fields.data[i].key, struct_type->structure.fields.data[j].key)) {
                    field_offset_idx = (nfields - j) + 1; 
                    break; // Offset is correct, end the loop
                }
            }

            // Compute source_offset for this loop (by adding field stack size to current source_offset)
            // For field_offset_idx n, field size = index_stack[n - 1] - index_stack(n)
            build_binary_op(Mov, reg(R9, sz_64), rref8(INDEX_REGISTER, -0x8 * (field_offset_idx - 1), sz_64), ass, a, point);
            build_binary_op(Sub, reg(R9, sz_64), rref8(INDEX_REGISTER, -0x8 * field_offset_idx, sz_64), ass, a, point);
            build_binary_op(Mov, reg(R10, sz_64), imm32(8), ass, a, point);
            generate_align_to(R9, R10, ass, a, point);
            build_binary_op(Add, rref8(INDEX_REGISTER, 0, sz_64), reg(R9, sz_64), ass, a, point);

            // RSI: size_t src_stack_offset = source_region_size - src_offset;
            build_binary_op(Mov, reg(RSI, sz_64), rref8(INDEX_REGISTER, -0x8 * (nfields + 2), sz_64), ass, a, point);
            build_binary_op(Sub, reg(RSI, sz_64), rref8(INDEX_REGISTER, 0x0, sz_64), ass, a, point);
            // RDI: size_t dest_stack_offset = source_region_size + dest_offset;
            build_binary_op(Mov, reg(RDI, sz_64), rref8(INDEX_REGISTER, -0x8 * (nfields + 2), sz_64), ass, a, point);
            build_binary_op(Add, reg(RDI, sz_64), rref8(INDEX_REGISTER, -0x8 * field_offset_idx, sz_64), ass, a, point);

            // Calculate Field size (R9) 
            // For field_offset_idx n, field size = index_stack[n - 1] - index_stack(n)
            build_binary_op(Mov, reg(R9, sz_64), rref8(INDEX_REGISTER, -0x8 * (field_offset_idx - 1), sz_64), ass, a, point);
            build_binary_op(Sub, reg(R9, sz_64), rref8(INDEX_REGISTER, -0x8 * field_offset_idx, sz_64), ass, a, point);

            // Now, move the data.
            generate_poly_stack_move(reg(RDI, sz_64), reg(RSI, sz_64), reg(R9, sz_64), ass, a, point);

        }
        // Lastly, get the source_size from the index stack, and use it to
        // shrink the regular stack.
        // Then, pop all values from the index stack
        build_binary_op(Mov, reg(RCX, sz_64), rref8(INDEX_REGISTER, -0x8 * (nfields + 2), sz_64), ass, a, point);
        build_binary_op(Sub, reg(INDEX_REGISTER, sz_64), imm32(0x8 * (nfields + 3)), ass, a, point);
        build_binary_op(Add, reg(RSP, sz_64), reg(RCX, sz_64), ass, a, point);
        break;
    }
    case SProjector: {
        not_implemented(mv_string("Polymorphic projector"));
        PiType* source_type = strip_type(syn.projector.val->ptype);

        if (source_type->sort == TStruct) {
            generate_polymorphic_i(*syn.projector.val, env, target, links, a, point);
            build_unary_op(Push, imm8(0), ass, a, point);
            // Now, generate a calculation of the field offset 
            for (size_t i = 0; i < source_type->structure.fields.len; i++) {
                if (i != 0) {
                    // Align to the new field; can skip if size = 0;
                    generate_align_of(R8, (PiType*)source_type->structure.fields.data[i].val, env, ass, a, point);
                    build_binary_op(Mov, reg(R9, sz_64), rref8(RSP, 0, sz_64), ass, a, point);
                    generate_align_to(R9, R8, ass, a, point);
                    build_binary_op(Mov, rref8(RSP, 0, sz_64), reg(R9, sz_64), ass, a, point);
                }

                if (symbol_eq(source_type->structure.fields.data[i].key, syn.projector.field))
                    break;

                // Push the size into RAX; this is then added to the value at
                // the top of the stack  
                generate_size_of(RAX, (PiType*)source_type->structure.fields.data[i].val, env, ass, a, point);
                build_binary_op(Add, rref8(RSP, 0, sz_64), reg(RAX, sz_64), ass, a, point);
            }

            // Generate the size of the struct + field
            generate_stack_size_of(RAX, source_type, env, ass, a, point);
            build_unary_op(Push, reg(RAX, sz_64), ass, a, point);

            // Now, RAX = size of field, RCX = struct size, RSI = offset
            generate_stack_size_of(RAX, syn.ptype, env, ass, a, point);
            build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);
            build_unary_op(Pop, reg(RSI, sz_64), ass, a, point);

            // Dest offset = size of struct - size of field
            build_binary_op(Sub, reg(RCX, sz_64), reg(RAX, sz_64), ass, a, point);

            // save dest offset
            build_unary_op(Push, reg(RCX, sz_64), ass, a, point);

            // increment both offsets by 8
            build_binary_op(Add, reg(RCX, sz_64), imm8(8), ass, a, point);
            build_binary_op(Add, reg(RSI, sz_64), imm8(8), ass, a, point);

            // generate poly stack move: 
            // RAX = size
            // RCX = dest offset (from RSP)
            // RSI = src offset  (from RSP)
            generate_poly_stack_move(reg(RCX, sz_64), reg(RSI, sz_64), reg(RAX, sz_64), ass, a, point);

            // Pop the field offset and shrink the stack by that much
            build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);
            build_binary_op(Add, reg(RSP, sz_64), reg(RCX, sz_64),  ass, a, point);

        // Is Instance
        } else {
            generate_polymorphic_i(*syn.projector.val, env, target, links, a, point);

            // Now, calculate offset for field 
            build_unary_op(Push, imm8(0), ass, a, point);
            for (size_t i = 0; i < source_type->instance.fields.len; i++) {
                if (i != 0) {
                    // Align to the new field; can skip if size = 0;
                    generate_align_of(R8, (PiType*)source_type->instance.fields.data[i].val, env, ass, a, point);
                    build_binary_op(Mov, reg(R9, sz_64), rref8(RSP, 0, sz_64), ass, a, point);
                    generate_align_to(R9, R8, ass, a, point);
                    build_binary_op(Mov, rref8(RSP, 0, sz_64), reg(R9, sz_64), ass, a, point);
                }

                if (symbol_eq(source_type->instance.fields.data[i].key, syn.projector.field))
                    break;
                // Push the size into RAX; this is then added to the top of the stack  
                generate_size_of(RAX, (PiType*)source_type->instance.fields.data[i].val, env, ass, a, point);
                build_binary_op(Add, rref8(RSP, 0, sz_64), reg(RAX, sz_64), ass, a, point);
            }

            // Generate the size of the output.
            generate_stack_size_of(RAX, syn.ptype, env, ass, a, point);

            // Pop the offset and instance ptr from the stack
            build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);
            build_unary_op(Pop, reg(RSI, sz_64), ass, a, point);

            // Generate the adderss of the field.
            build_binary_op(Add, reg(RSI, sz_64), reg(RCX, sz_64), ass, a, point);

            // Reserve space on the stack
            build_binary_op(Sub, reg(RSP, sz_64), reg(RAX, sz_64), ass, a, point);

            generate_poly_move(reg(RSP, sz_64), reg(RSI, sz_64), reg(RAX, sz_64), ass, a, point);
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
        not_implemented(mv_string("Polymorphic use"));
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

        // Now, allocate space on stack
        size_t val_size = pi_size_of(*syn.ptype);
        build_binary_op(Sub, reg(RSP, sz_64), imm32(val_size), ass, a, point);
        build_binary_op(Mov, reg(RCX, sz_64), reg(RAX, sz_64), ass, a, point);

        generate_monomorphic_copy(RSP, RCX, val_size, ass, a, point);
        break;
    }
    case SDynamicSet: {
        not_implemented(mv_string("Polymorphic set"));
        // TODO: convert this to 'true' polymorphic code
        size_t val_size = pi_size_of(*syn.dynamic_set.new_val->ptype);
        generate_polymorphic_i(*syn.dynamic_set.dynamic, env, target, links, a, point);
        generate_polymorphic_i(*syn.dynamic_set.new_val, env, target, links, a, point);

#if ABI == SYSTEM_V_64 
        // arg1 = rdi, arg2 = rsi, arg3 = RDX
        build_binary_op(Mov, reg(RDI, sz_64), rref8(RSP, val_size, sz_64), ass, a, point);
        build_binary_op(Mov, reg(RSI, sz_64), reg(RSP, sz_64), ass, a, point);
        build_binary_op(Mov, reg(RDX, sz_64), imm32(val_size), ass, a, point);
#elif ABI == WIN_64 
        // arg1 = rcx, arg2 = rdx, arg3 = R8
        build_binary_op(Mov, reg(RCX, sz_64), rref8(RSP, val_size, sz_64), ass, a, point);
        build_binary_op(Mov, reg(RDX, sz_64), reg(RSP, sz_64), ass, a, point);
        build_binary_op(Mov, reg(R8, sz_64), imm32(val_size), ass, a, point);
#else
#error "unknown ABI"
#endif
        generate_c_call(set_dynamic_val, ass, a, point);

        build_binary_op(Add, reg(RSP, sz_64), imm32(val_size + ADDRESS_SIZE), ass, a, point);
        break;
    }
    case SLet: {
        size_t bind_sz = ADDRESS_SIZE; 

        build_unary_op(Push, reg(R14, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);

        for (size_t i = 0; i < syn.let_expr.bindings.len; i++) {
            SymPtrCell elt = syn.let_expr.bindings.data[i];
            generate_polymorphic_i(*(Syntax*)elt.val, env, target, links, a, point);

            if (is_variable(((Syntax*)elt.val)->ptype)) {
                address_bind_relative_index(elt.key, 0, env);
                bind_sz += ADDRESS_SIZE;
            } else {
                size_t stack_sz = pi_stack_size_of(*((Syntax*)elt.val)->ptype);
                address_bind_relative(elt.key, 0, env);
                bind_sz += stack_sz;
            }
        }
        generate_polymorphic_i(*syn.let_expr.body, env, target, links, a, point);

        if (is_variable(syn.ptype)) {
            generate_stack_size_of(RAX, syn.ptype, env, ass, a, point);

            // Grab the value of R14 we pushed at the beginning - offset bind_sz + stack head 
            build_binary_op(Mov, reg(R14, sz_64), rref8(RSP, bind_sz, sz_64), ass, a, point);
            build_binary_op(Sub, reg(R14, sz_64), reg(RAX, sz_64), ass, a, point);

            build_binary_op(Mov, reg(RCX, sz_64), rref8(RSP, 0, sz_64), ass, a, point);

            generate_poly_move(reg(R14, sz_64), reg(RCX, sz_64), reg(RAX, sz_64), ass, a, point);

            // Store current index in stack return position
            generate_stack_move(bind_sz, 0, ADDRESS_SIZE, ass, a, point);
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
        not_implemented(mv_string("Polymorphic if"));
        // Generate the condition
        generate_polymorphic_i(*syn.if_expr.condition, env, target, links, a, point);

        // Pop the bool into R9; compare with 0
        build_unary_op(Pop, reg(R9, sz_64), ass, a, point);
        build_binary_op(Cmp, reg(R9, sz_64), imm32(0), ass, a, point);

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
        not_implemented(mv_string("Polymorphic labels"));
        // Labels: The code-generation for labels is as follows: 
        // 1. Add labels to environment
        // 2. Generate expression
        // 3. For each expression:
        //  3.1 Mark Beginning of label expression
        //  3.2 Generate label expressions
        // 4. Backlink all labels.
        SymbolArray labels = mk_symbol_array(syn.labels.terms.len, a);
        for (size_t i = 0; i < syn.labels.terms.len; i++) 
            push_symbol(syn.labels.terms.data[i].key, &labels);

        // Push the current value of $RSP onto the indexing stack

        address_start_labels(labels, env);

        generate_polymorphic_i(*syn.labels.entry, env, target, links, a, point);

        SymSizeAssoc label_points = mk_sym_size_assoc(syn.labels.terms.len, a);
        SymSizeAssoc label_jumps = mk_sym_size_assoc(syn.labels.terms.len, a);

        for (size_t i = 0; i < syn.labels.terms.len; i++) {
            SymPtrACell cell = syn.labels.terms.data[i];

            // Mark Label
            size_t pos = get_pos(ass);
            sym_size_bind(cell.key, pos, &label_points);
            
            SynLabelBranch* branch = cell.val;

            // Bind Label Vars 
            // Note: the index stack
            SymSizeAssoc arg_sizes = mk_sym_size_assoc(branch->args.len, a);
            for (size_t i = 0; i < branch->args.len; i++) {
                sym_size_bind(branch->args.data[i].key, i, &arg_sizes);
            }

            // We assume that they have been pushed onto the stack by whatever
            // jumped us in here!
            address_bind_label_vars(arg_sizes, env);
            generate_polymorphic_i(*branch->body, env, target, links, a, point);
            address_unbind_label_vars(env);

            LabelEntry lble = label_env_lookup(cell.key, env);
            if (lble.type == Err)
                throw_error(point, mv_string("Label not found during codegen!!"));

            // TODO (possible bug: use Sub or Add and + or - ?)
            build_binary_op(Sub, reg(INDEX_REGISTER, sz_64), imm32((lble.stack_offset + 1) * ADDRESS_SIZE), ass, a, point);

            // Copy the result down the stack. To do this, we need
            // the size (RAX)
            // The destination (initial RSP - size)
            // The source offset (0)
            generate_size_of(RAX, syn.ptype, env, ass, a, point);
            build_binary_op(Mov, reg(R10, sz_64), rref8(INDEX_REGISTER, 0, sz_64), ass, a, point);

            build_binary_op(Sub, reg(R10, sz_64), reg(RAX, sz_64), ass, a, point);
            generate_poly_stack_move(reg(R10, sz_64), imm32(0), reg(RAX, sz_64), ass, a, point);

            // Ensure the stack is at the same point for the next iteration! 
            build_binary_op(Sub, reg(RSP, sz_64), reg(RAX, sz_64), ass, a, point);

            AsmResult out = build_unary_op(JMP, imm32(0), ass, a, point);
            sym_size_bind(cell.key, out.backlink, &label_jumps);
        }

        // The final iteration will have shrunk the stack "too much", so add the
        // result back in.

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
        not_implemented(mv_string("Polymorphic go-to"));
        // Generating code for a goto:
        // 1. Generate args
        for (size_t i = 0; i < syn.go_to.args.len; i++) {
            Syntax* expr = syn.go_to.args.data[i];

            // Store the size in the index stack
            // (this will later be used to index the input variable to the label)
            generate_stack_size_of(RAX, expr->ptype, env, ass, a, point);

            generate_polymorphic_i(*expr, env, target, links, a, point);
        }

        // 2. Backlink the label
        LabelEntry lble = label_env_lookup(syn.go_to.label, env);
        if (lble.type == Ok) {
            int64_t delta = (lble.stack_offset - syn.go_to.args.len) * 8;
            int64_t rsp_offset = lble.stack_offset * 8;

            // Sum the n prior entries in the stack
            // While doing so, update each stack entry to be an offset from the
            // (final) RSP 
            if (((int64_t)syn.go_to.args.len) * -8 < INT8_MIN) {
                throw_error(point, mv_string("Codegen error: too many arguments to label"));
            }
            build_binary_op(Mov, reg(R9, sz_64), imm32(0), ass, a, point);
            for (int64_t i = 0; i < (int64_t)syn.go_to.args.len; i++) {
                build_binary_op(Mov, reg(R8, sz_64), reg(R9, sz_64), ass, a, point);
                build_binary_op(Add, reg(R9, sz_64), rref8(INDEX_REGISTER, -i * 8, sz_64), ass, a, point);
                build_binary_op(Mov, rref8(INDEX_REGISTER, -i * 8, sz_64), reg(R8, sz_64), ass, a, point);
            }

            // Adjust the stack so it has the same value that one would have
            // going into a labels expression.
            // TODO handle dynamic variable unbinding (if needed!)

            // Get the dest offset by taking the original RSP and
            // subtracting the new RSP and the size 
            build_binary_op(Mov, reg(R11, sz_64), reg(INDEX_REGISTER, sz_64), ass, a, point);
            build_binary_op(Sub, reg(R11, sz_64), imm32(rsp_offset), ass, a, point);
            build_binary_op(Mov, reg(R10, sz_64), rref8(R11, 0, sz_64), ass, a, point);
            build_binary_op(Sub, reg(R10, sz_64), reg(RSP, sz_64), ass, a, point);
            build_binary_op(Sub, reg(R10, sz_64), reg(R9, sz_64), ass, a, point);

            // Save the dest_offset (We will add this to RSP after the copy)

            generate_poly_stack_move(reg(R10, sz_64), imm32(0), reg(R9, sz_64), ass, a, point);

            build_binary_op(Add, reg(RSP, sz_64), reg(R10, sz_64), ass, a, point);

            // Now, update the index stack to handle the new offsets 
            for (int64_t i = 0; i < (int64_t)syn.go_to.args.len; i++) {
                build_binary_op(Add, rref8(INDEX_REGISTER, -i * 8, sz_64), reg(RSP, sz_64), ass, a, point);
            }

            // Now, shrink the address stack appropriately 
            build_binary_op(Sub, reg(INDEX_REGISTER, sz_64), imm32(delta), ass, a, point);

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
                if (is_variable(elt->expr->ptype)) {
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
                if (is_variable(elt->expr->ptype)) {
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
        if (is_variable(syn.ptype)) {
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
        break;
    }
    case SAlignOf: {
        generate_align_of(RAX, syn.size->type_val, env, ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        break;
    }
    case SCheckedType: {
        build_binary_op(Mov, reg(R9, sz_64), imm64((uint64_t)syn.type_val), ass, a, point);
        build_unary_op(Push, reg(R9, sz_64), ass, a, point);
        break;
    }
    default: {
        panic(mv_string("Invalid abstract term in polymorphic codegen."));
    }
    }
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
