#include <string.h>

#include "platform/signals.h"
#include "platform/machine_info.h"
#include "platform/machine_info.h"
#include "pretty/string_printer.h"

#include "pico/codegen/polymorphic.h"
#include "pico/codegen/internal.h"
#include "pico/binding/address_env.h"

// Implementation details
void generate_polymorphic_i(Syntax syn, AddressEnv* env, Target target, InternalLinkData* links, Allocator* a, ErrorPoint* point);

// type info
void generate_size_of(Regname dest, PiType* type, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_align_of(Regname dest, PiType* type, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_align_to(Regname sz, Regname align, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_stack_size_of(Regname dest, PiType* type, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_variant_size_of(Regname dest, PtrArray* types, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point);

// movement
void generate_poly_move(Location dest, Location src, Location size, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_poly_stack_move(Location dest_offset, Location src_offset, Location size, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_index_stack_move(size_t dest_offset, size_t src_offset, size_t size, Assembler* ass, Allocator* a, ErrorPoint* point);

// index stack
void generate_index_push(Location src, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_index_pop(Location dest, Assembler* ass, Allocator* a, ErrorPoint* point);

void generate_polymorphic(SymbolArray types, Syntax syn, AddressEnv* env, Target target, InternalLinkData* links, Allocator* a, ErrorPoint* point) {
    Assembler* ass = target.target;
    SymbolArray vars;
    Syntax body;
    if (syn.type == SProcedure) {
        vars = mk_symbol_array(syn.procedure.args.len + syn.procedure.implicits.len, a);
        for (size_t i = 0; i < syn.procedure.implicits.len; i++) {
            push_symbol(syn.procedure.implicits.data[i].key, &vars);
        }
        for (size_t i = 0; i < syn.procedure.args.len; i++) {
            push_symbol(syn.procedure.args.data[i].key, &vars);
        }
        body = *syn.procedure.body;
    } else {
        vars = mk_symbol_array(0, a);
        body = syn;
    }

    // Polymorphic ABI
    // RBP+  | types
    // RBP+  | arg-ptrs
    // RBP+8 | Space for return address
    // RBP   | old RBP
    // RBP-  | args
    // RSP → Return address

    // Prelude: copy return address to RBP+8
    build_unary_op(ass, Pop, reg(R9, sz_64), a, point);
    build_binary_op(ass, Mov, rref8(RBP, 8, sz_64), reg(R9, sz_64), a, point);

    address_start_poly(types, vars, env, a);

    generate_polymorphic_i(body, env, target, links, a, point);

    // Codegen function postlude:
    // Stack now looks like:
    // 
    // RBP+ | types + argument offfsets (to discard)
    // RBP+ | return address
    // RBP  | OLD RBP
    // RBP- | args
    // RSP  | output value 
    // Postlude:
    // 1. Push Old RBP & Return Address
    // 2. move output value to start of types
    // 3. Pop Old RBP & Return Address
    // 5. move RSP to end of value & restore old RBP
    // 4. push return address
    // 6. return

    // 1. Stash old RBP & return address
    build_unary_op(ass, Push, rref8(RBP, 8, sz_64), a, point); 
    build_unary_op(ass, Push, rref8(RBP, 0, sz_64), a, point); 

    // 2. Move output value to start of types. We do this via a poly stack move,
    // which needs three pieces of info: 
    // 2.1 : The size of the data to be copid: Relatively simple.
    generate_stack_size_of(RAX, body.ptype, env, ass, a, point);

    // 2.2 : The destination address. This is offset + RBP, with offset calculated as:
    //              OLD RBP+RET ADDR | accounts for types       | account for vars
    size_t offset = 2 * ADDRESS_SIZE + ADDRESS_SIZE * types.len + ADDRESS_SIZE * vars.len;
    build_binary_op(ass, Mov, reg(R9, sz_64), reg(RBP, sz_64), a, point);
    build_binary_op(ass, Add, reg(R9, sz_64), imm32(offset), a, point);
    build_binary_op(ass, Sub, reg(R9, sz_64), reg(RAX, sz_64), a, point);

    // 2.3: The source address: this is just RSP + 2*ADDRESS_SIZE, as we pushed
    //      the old RBP + Return address 
    build_binary_op(ass, Mov, reg(RDX, sz_64), reg(RSP, sz_64), a, point);
    build_binary_op(ass, Add, reg(RDX, sz_64), imm8(2 * ADDRESS_SIZE), a, point);

    // Note: We push R9 (the new head of stack) so it can be used
    build_unary_op(ass, Push, reg(R9, sz_64), a, point); 
    
    generate_poly_move(reg(R9, sz_64), reg(RDX, sz_64), reg(RAX, sz_64), ass, a, point);

    // 3. Pop old RBP and return address
    //    Note that these are currently BELOW the top of the stack!
    build_unary_op(ass, Pop, reg(RDX, sz_64), a, point);
    build_unary_op(ass, Pop, reg(RBP, sz_64), a, point);
    build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);

    // 4. Move RSP to end of value & restore old RBP
    build_binary_op(ass, Mov, reg(RSP, sz_64), reg(RDX, sz_64), a, point);

    // 5. push return address
    build_unary_op(ass, Push, reg(RCX, sz_64), a, point); 
    build_nullary_op(ass, Ret, a, point);

    address_end_poly(env, a);
}

void generate_polymorphic_i(Syntax syn, AddressEnv* env, Target target, InternalLinkData* links, Allocator* a, ErrorPoint* point) {
    Assembler* ass = target.target;
    switch (syn.type) {
    case SLitUntypedIntegral: {
        panic(mv_string("Cannot generate polymorphic code for untyped integral."));
    }
    case SLitTypedIntegral: {
        // Does it fit into 32 bits?
        if (syn.integral.value < 0x80000000 && syn.integral.value > -80000001) {
            int32_t immediate = syn.integral.value;
            build_unary_op(ass, Push, imm32(immediate), a, point);
        } else {
            throw_error(point, mk_string("Limitation: Literals must fit into less than 64 bits.", a));
        }
        break;
    }
    case SLitBool: {
        int8_t immediate = (int8_t) syn.boolean;
        build_unary_op(ass, Push, imm8(immediate), a, point);
        break;
    }
    case SLitString: {
        String immediate = syn.string; 
        if (immediate.memsize > UINT32_MAX) 
            throw_error(point, mv_string("Codegen: String literal length must fit into less than 32 bits"));

        // Push the u8
        AsmResult out = build_binary_op(ass, Mov, reg(RAX, sz_64), imm64((uint64_t)(target.data_aux->data + target.data_aux->len)), a, point);

        // Backlink the data & copy the bytes into the data-segment.
        backlink_data(target, out.backlink, links);
        add_u8_chunk(immediate.bytes, immediate.memsize, target.data_aux);

        build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
        build_unary_op(ass, Push, imm32(immediate.memsize), a, point);
        break;
    }
    case SVariable:
    case SAbsVariable: {
        // Lookup the variable in the assembly envionrment
        AddressEntry e = (syn.type == SVariable)
            ? address_env_lookup(syn.variable, env)
            : address_abs_lookup(syn.abvar, env);
        switch (e.type) {
        case ALocalDirect:
            throw_error(point, mv_string("Codegen not implemented for Local Direct variables"));
            break;
        case ALocalIndirect:
            // First, we need the size of the variable & allocate space for it on the stack
            // ------------------------------
            // Store stack size in R9
            generate_stack_size_of(R9, syn.ptype, env, ass, a, point);

            // Subtract stack size
            build_binary_op(ass, Sub, reg(RSP, sz_64), reg(R9, sz_64), a, point);

            // Then, find the location of the variable on the stack 
            // *(RBP + stack offset) = offset2
            // RBP + offset2 = dest (stored here in R9)
            build_binary_op(ass, Mov, reg(R8, sz_64), rref8(RBP, e.stack_offset, sz_64), a, point);
            // We need to stack-align the value!

            build_binary_op(ass, Add, reg(R8, sz_64), reg(RBP, sz_64), a, point); // 

            generate_poly_move(reg(RSP, sz_64), reg(R8, sz_64), reg(R9, sz_64), ass, a, point);
            break;
        case ALocalIndexed:
            // First, we need the size of the variable & allocate space for it on the stack
            // ------------------------------
            // Store stack size in R9
            generate_stack_size_of(R9, syn.ptype, env, ass, a, point);

            // Subtract stack size
            build_binary_op(ass, Sub, reg(RSP, sz_64), reg(R9, sz_64), a, point);

            // Then, find the location of the variable
            build_binary_op(ass, Mov, reg(R8, sz_64), rref8(R13, e.stack_offset, sz_64), a, point);

            // Finally, move the value from the source to the stack.
            generate_poly_move(reg(RSP, sz_64), reg(R8, sz_64), reg(R9, sz_64), ass, a, point);
            break;
        case ATypeVar:
            throw_error(point, mv_string("Codegen not implemented for ATypeVar"));
            break;
        case AGlobal: {
            PiType indistinct_type = *strip_type(syn.ptype);

            // Use RAX as a temp
            // Note: casting void* to uint64_t only works for 64-bit systems...
            if (indistinct_type.sort == TProc || indistinct_type.sort == TAll || indistinct_type.sort == TKind
                || indistinct_type.sort == TPrim || indistinct_type.sort == TDynamic || indistinct_type.sort == TTraitInstance) {
                AsmResult out = build_binary_op(ass, Mov, reg(R8, sz_64), imm64(*(uint64_t*)e.value), a, point);
                backlink_global(syn.variable, out.backlink, links, a);
                build_unary_op(ass, Push, reg(R8, sz_64), a, point);
            } else if (indistinct_type.sort == TStruct || indistinct_type.sort == TEnum) {
                size_t value_size = pi_size_of(*syn.ptype);
                AsmResult out = build_binary_op(ass, Mov, reg(RCX, sz_64), imm64((uint64_t)e.value), a, point);
                backlink_global(syn.variable, out.backlink, links, a);

                // Allocate space on the stack for composite type (struct/enum)
                build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(value_size), a, point);

                generate_monomorphic_copy(RSP, RCX, value_size, ass, a, point);
            } else {
                throw_error(point,
                            string_ncat(a, 3,
                                        mv_string("Codegen: Global var '"),
                                        *symbol_to_string(syn.variable),
                                        mv_string("' has unsupported sort")));
            }
            break;
        }
        case ANotFound: {
            String* sym = symbol_to_string(syn.variable);
            String msg = mv_string("Couldn't find variable during codegen: ");
            throw_error(point, string_cat(msg, *sym, a));
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
        build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);
        build_unary_op(ass, Call, reg(RCX, sz_64), a, point);
        break;
    }
    case SAllApplication: {
        // Polymorphic Funcall
        // Step1: reserve space for types. 
        // Recall the setup
        // > Types
        // > Argument Offsets
        // > Old RBP
        // > 64-bit space
        // > arguments (in order)

        for (size_t i = 0; i < syn.all_application.types.len; i++) {
            PiType* type = ((Syntax*)syn.all_application.types.data[i])->type_val;
            if (type->sort == TVar) {
                // Optimization
                AddressEntry e = address_env_lookup(type->var, env);
                switch (e.type) {
                case ALocalDirect:
                    build_binary_op(ass, Mov, reg(R8, sz_64), rref8(RBP, e.stack_offset, sz_64), a, point);
                    build_unary_op(ass, Push, reg(R8, sz_64), a, point);
                    break;
                case ALocalIndirect:
                    panic(mv_string("All Application: Cannot generate code where type var is local indirect."));
                    break;
                case ALocalIndexed:
                    panic(mv_string("All Application: Cannot generate code where type var is local indexed."));
                    break;
                case AGlobal:
                    panic(mv_string("All Application: Cannot generate code where type var is local global."));
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
            } else {
                // The 'type' looks as follows:
                // | 16 bits | 16 bits     | 32 bits |
                // | align   | stack align | size    |
                generate_align_of(R8, type, env, ass, a, point);
                build_binary_op(ass, SHL, reg(R8, sz_64), imm8(56), a, point);
                build_unary_op(ass, Push, reg(R8, sz_64), a, point);

                // TODO BUG LOGIC Check that R8 < max_uint_28
                generate_size_of(R8, type, env, ass, a, point);
                build_binary_op(ass, Mov, reg(RAX, sz_64), reg(R8, sz_64), a, point);
                build_binary_op(ass, SHL, reg(R8, sz_64), imm8(28), a, point);
                build_binary_op(ass, Or, rref8(RSP, 0, sz_64), reg(R8, sz_64), a, point);

                /* uint64_t result = (align << 56) | (size << 28) | stack_sz; */
                // Now, we use the 'div' instruction with RDX = 0; RAX = size.
                // Remainder stored in RDX
                build_binary_op(ass, Mov, reg(RDX, sz_64), imm32(0), a, point);
                build_binary_op(ass, Mov, reg(RCX, sz_64), imm32(8), a, point);
                build_unary_op(ass, Div, reg(RCX, sz_64), a, point);

                // We need now to if rem == 0 then 0 else 8 - rem 
                // store 8 - rem in RCX (8 is already in rcx from above)
                build_binary_op(ass, Sub, reg(RCX, sz_64), reg(RDX, sz_64), a, point);
                build_binary_op(ass, Cmp, reg(RDX, sz_64), imm8(0), a, point);
            
                build_binary_op(ass, CMovE, reg(RCX, sz_64), reg(RDX, sz_64), a, point);

                // Add this to the original size and binary-or it into the type.
                build_binary_op(ass, Add, reg(R8, sz_64), reg(RCX, sz_64), a, point);
                build_binary_op(ass, Or, rref8(RSP, 0, sz_64), reg(R8, sz_64), a, point);
            }
        }

        // Calculation of offsets:
        // • Remaining offset starts 0
        // Increment by the (stack) size of each argument, push thisoffset
        build_unary_op(ass, Push, imm32(0), a, point);
        
        for (size_t i = 0; i < syn.all_application.implicits.len; i++) {
            generate_stack_size_of(RAX, ((Syntax*)syn.all_application.implicits.data[i])->ptype, env, ass, a, point);

            // Pop current offset, subtract stack size, push new offset 2x 
            build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);
            build_binary_op(ass, Sub, reg(RCX, sz_64), reg(RAX, sz_64), a, point);

            // generate_stack_size_of(RAX, syn.ptype, env, ass, a, point);
            build_unary_op(ass, Push, reg(RCX, sz_64), a, point);
            build_unary_op(ass, Push, reg(RCX, sz_64), a, point);
        }
        for (size_t i = 0; i < syn.all_application.args.len; i++) {
            generate_stack_size_of(RAX, ((Syntax*)syn.all_application.args.data[i])->ptype, env, ass, a, point);

            // Pop current offset, subtract stack size, push new offset 2x 
            build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);
            build_binary_op(ass, Sub, reg(RCX, sz_64), reg(RAX, sz_64), a, point);

            // generate_stack_size_of(RAX, syn.ptype, env, ass, a, point);
            build_unary_op(ass, Push, reg(RCX, sz_64), a, point);
            build_unary_op(ass, Push, reg(RCX, sz_64), a, point);
        }
        // Pop the 'spare' RCX from the stack, stash the offset in the 'RCX stack'
        build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);
        generate_index_push(reg(RCX, sz_64), ass, a, point);
        index_stack_grow(env, 1);

        // Reserve for return address
        build_binary_op(ass, Sub, reg(RSP, sz_64), imm8(ADDRESS_SIZE), a, point);
        // Store "Old" RBP (RBP to restore)
        build_unary_op(ass, Push, reg(RBP, sz_64), a, point);

        for (size_t i = 0; i < syn.all_application.implicits.len; i++) {
            Syntax* arg = (Syntax*) syn.all_application.implicits.data[i];
            generate_polymorphic_i(*arg, env, target, links, a, point);

        }
        for (size_t i = 0; i < syn.all_application.args.len; i++) {
            Syntax* arg = (Syntax*) syn.all_application.args.data[i];
            generate_polymorphic_i(*arg, env, target, links, a, point);
        }

        // This will push a function pointer onto the stack
        generate_polymorphic_i(*syn.application.function, env, target, links, a, point);

        // Now, calculate what RBP should be 
        // RBP = RSP - (args_size + ADDRESS_SIZE) ;; (ADDRESS_SIZE accounts for function ptr)
        //     = RSP + (offset + ADDRESS_SIZE)
        build_binary_op(ass, Mov, reg(RBP, sz_64), reg(RSP, sz_64), a, point);
        // Mov RBP, offset + ADDRESS_SIZE
        // mov offset into rcx

        generate_index_pop(reg(RCX, sz_64), ass, a, point);
        index_stack_shrink(env, 1);
        // add offset to RBP. Note that we use 'sub' because RCX = -offset, so
        // RBP - RCX = RBP - (- Offset) = RBP + Offset
        build_binary_op(ass, Sub, reg(RBP, sz_64), reg(RCX, sz_64), a, point);
        // add address size to RBP
        build_binary_op(ass, Add, reg(RBP, sz_64), imm32(ADDRESS_SIZE), a, point);
        
        // Regular Function Call
        // Pop the function into RCX; call the function
        build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);
        build_unary_op(ass, Call, reg(RCX, sz_64), a, point);
        break;
    }
    case SStructure: {
        PiType* struct_type = strip_type(syn.ptype);

        // Reserve space on the stack for the structure to go
        generate_stack_size_of(RAX, syn.ptype, env, ass, a, point);
        build_binary_op(ass, Sub, reg(RSP, sz_64), reg(RAX, sz_64), a, point);

        // Generate code for each of the fields (in order)
        for (size_t i = 0; i < syn.structure.fields.len; i++)
        {
            generate_polymorphic_i(*(Syntax *)syn.structure.fields.data[i].val, env, target, links, a, point);
        }

        // -------------------------------------------------------------------------
        // Movement
        // The most difficult part of generating a structure is ensuring that
        // all values end up in the "correct" location (i.e. correct part of the stack). 
        //
        // The first step is to calculate the total size of all values pushed to the
        // stack. This is called the source-region-size or source-size
        // 
        // This is then used to perform a series of smaller moves (one for each field)
        // -------------------------------------------------------------------------
        generate_index_push(imm32(0), ass, a, point);
        index_stack_grow(env, 1);
        for (size_t i = 0; i < struct_type->structure.fields.len; i++) {
            generate_size_of(RAX, struct_type->structure.fields.data[i].val, env, ass, a, point);
            build_binary_op(ass, Add, rref8(R13, 0, sz_64), reg(RAX, sz_64), a, point);
        }

        generate_index_push(imm32(0), ass, a, point);
        index_stack_grow(env, 1);
        // (elt 1 control_stack) = source_region_size
        // (elt 0 control_stack) = dest_offset (= 0)
        for (size_t i = 0; i < struct_type->structure.fields.len; i++) {
            generate_index_push(imm32(0), ass, a, point);
            index_stack_grow(env, 1);
            // (elt 2 control_stack) = source_region_size
            // (elt 1 control_stack) = dest_offset
            // (elt 0 control_stack) = source_offset
            for (size_t j = 0; j < syn.structure.fields.len; j++) {
                PiType** t = (PiType**)sym_ptr_lookup(syn.structure.fields.data[j].key, struct_type->structure.fields);
                if (t) {
                    // source_offset = source_offset + stack_size_of(field)
                    generate_stack_size_of(RAX, ((Syntax *)syn.structure.fields.data[j].val)->ptype, env, ass, a, point);
                    build_binary_op(ass, Add, rref8(R13, 0, sz_64), reg(RAX, sz_64), a, point);
                } else {
                    throw_error(point, mv_string("Error code-generating for structure: field not found."));
                }

                if (symbol_eq(syn.structure.fields.data[j].key, struct_type->structure.fields.data[i].key)) {
                    break; // offset is correct, end the loop
                }
            }


            // RSI: size_t src_stack_offset = source_region_size - src_offset;
            build_binary_op(ass, Mov, reg(RSI, sz_64), rref8(R13, -0x10, sz_64), a, point);
            build_binary_op(ass, Sub, reg(RSI, sz_64), rref8(R13, 0x0, sz_64), a, point);
            // RDI: size_t dest_stack_offset = source_region_size + dest_offset;
            build_binary_op(ass, Mov, reg(RDI, sz_64), rref8(R13, -0x10, sz_64), a, point);
            build_binary_op(ass, Add, reg(RDI, sz_64), rref8(R13, -0x8, sz_64), a, point);

            // replace stack_offset with field_size
            generate_size_of(R9, struct_type->structure.fields.data[i].val, env, ass, a, point);
            build_binary_op(ass, Mov, rref8(R13, 0, sz_64), reg(R9, sz_64), a, point);

            // Now, move the data.
            generate_poly_stack_move(reg(RDI, sz_64), reg(RSI, sz_64), reg(R9, sz_64), ass, a, point);

            // Compute dest_offset for next loop (by adding field size to dest_offset)
            generate_index_pop(reg(RCX, sz_64), ass, a, point);
            index_stack_shrink(env, 1);
            build_binary_op(ass, Add, rref8(R13, 0, sz_64), reg(RCX, sz_64), a, point);
        }
        // Lastly, pop the dest_offset and source_size from the control stack,
        // and shrink the 'regular' stack
        build_binary_op(ass, Sub, reg(R13, sz_64), imm32(0x8), a, point);
        generate_index_pop(reg(RCX, sz_64), ass, a, point);
        index_stack_shrink(env, 2);

        build_binary_op(ass, Add, reg(RSP, sz_64), reg(RCX, sz_64), a, point);
        break;
    }
    case SProjector: {
        PiType* source_type = strip_type(syn.projector.val->ptype);

        if (source_type->sort == TStruct) {
            generate_polymorphic_i(*syn.projector.val, env, target, links, a, point);
            build_unary_op(ass, Push, imm8(0), a, point);
            // Now, generate a calculation of the field offset 
            for (size_t i = 0; i < source_type->structure.fields.len; i++) {
                if (i != 0) {
                    // Align to the new field; can skip if size = 0;
                    generate_align_of(R8, (PiType*)source_type->structure.fields.data[i].val, env, ass, a, point);
                    build_binary_op(ass, Mov, reg(R9, sz_64), rref8(RSP, 0, sz_64), a, point);
                    generate_align_to(R9, R8, ass, a, point);
                    build_binary_op(ass, Mov, rref8(RSP, 0, sz_64), reg(R9, sz_64), a, point);
                }

                if (symbol_eq(source_type->structure.fields.data[i].key, syn.projector.field))
                    break;

                // Push the size into RAX; this is then added to the value at
                // the top of the stack  
                generate_size_of(RAX, (PiType*)source_type->structure.fields.data[i].val, env, ass, a, point);
                build_binary_op(ass, Add, rref8(RSP, 0, sz_64), reg(RAX, sz_64), a, point);
            }

            // Generate the size of the struct + field
            generate_stack_size_of(RAX, source_type, env, ass, a, point);
            build_unary_op(ass, Push, reg(RAX, sz_64), a, point);

            // Now, RAX = size of field, RCX = struct size, RSI = offset
            generate_stack_size_of(RAX, syn.ptype, env, ass, a, point);
            build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);
            build_unary_op(ass, Pop, reg(RSI, sz_64), a, point);

            // Dest offset = size of struct - size of field
            build_binary_op(ass, Sub, reg(RCX, sz_64), reg(RAX, sz_64), a, point);

            // save dest offset
            build_unary_op(ass, Push, reg(RCX, sz_64), a, point);

            // increment both offsets by 8
            build_binary_op(ass, Add, reg(RCX, sz_64), imm8(8), a, point);
            build_binary_op(ass, Add, reg(RSI, sz_64), imm8(8), a, point);

            // generate poly stack move: 
            // RAX = size
            // RCX = dest offset (from RSP)
            // RSI = src offset  (from RSP)
            generate_poly_stack_move(reg(RCX, sz_64), reg(RSI, sz_64), reg(RAX, sz_64), ass, a, point);

            // Pop the field offset and shrink the stack by that much
            build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);
            build_binary_op(ass, Add, reg(RSP, sz_64), reg(RCX, sz_64),  a, point);

        // Is Instance
        } else {
            generate_polymorphic_i(*syn.projector.val, env, target, links, a, point);

            // Now, calculate offset for field 
            build_unary_op(ass, Push, imm8(0), a, point);
            for (size_t i = 0; i < source_type->instance.fields.len; i++) {
                if (i != 0) {
                    // Align to the new field; can skip if size = 0;
                    generate_align_of(R8, (PiType*)source_type->instance.fields.data[i].val, env, ass, a, point);
                    build_binary_op(ass, Mov, reg(R9, sz_64), rref8(RSP, 0, sz_64), a, point);
                    generate_align_to(R9, R8, ass, a, point);
                    build_binary_op(ass, Mov, rref8(RSP, 0, sz_64), reg(R9, sz_64), a, point);
                }

                if (symbol_eq(source_type->instance.fields.data[i].key, syn.projector.field))
                    break;
                // Push the size into RAX; this is then added to the top of the stack  
                generate_size_of(RAX, (PiType*)source_type->instance.fields.data[i].val, env, ass, a, point);
                build_binary_op(ass, Add, rref8(RSP, 0, sz_64), reg(RAX, sz_64), a, point);
            }

            // Generate the size of the output.
            generate_stack_size_of(RAX, syn.ptype, env, ass, a, point);

            // Pop the offset and instance ptr from the stack
            build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);
            build_unary_op(ass, Pop, reg(RSI, sz_64), a, point);

            // Generate the adderss of the field.
            build_binary_op(ass, Add, reg(RSI, sz_64), reg(RCX, sz_64), a, point);

            // Reserve space on the stack
            build_binary_op(ass, Sub, reg(RSP, sz_64), reg(RAX, sz_64), a, point);

            generate_poly_move(reg(RSP, sz_64), reg(RSI, sz_64), reg(RAX, sz_64), ass, a, point);
        }
        break;
    }
    case SConstructor: {
        PiType* enum_type = strip_type(syn.constructor.enum_type->type_val);

        generate_stack_size_of(RAX, enum_type, env, ass, a, point);
        generate_variant_size_of(RCX, enum_type->enumeration.variants.data[syn.variant.tag].val, env, ass, a, point);

        build_binary_op(ass, Sub, reg(RAX, sz_64), reg(RCX, sz_64), a, point);
        build_binary_op(ass, Sub, reg(RSP, sz_64), reg(RAX, sz_64), a, point);
        build_unary_op(ass, Push, imm32(syn.constructor.tag), a, point);
        break;
    }
    case SVariant: {
        // TODO (FEAT BUG): ensure this will correctly handle non-stack aligned
        // enum tags, members and overall enums gracefully.
        const size_t tag_size = sizeof(uint64_t);
        PiType* enum_type = strip_type(syn.variant.enum_type->type_val);
        generate_stack_size_of(RCX, enum_type, env, ass, a, point);

        // Make space to fit the (final) variant
        build_binary_op(ass, Sub, reg(RSP, sz_64), reg(RCX, sz_64), a, point);

        // Set the tag
        build_binary_op(ass, Mov, rref8(RSP, 0, sz_64), imm32(syn.constructor.tag), a, point);

        // Generate each argument
        for (size_t i = 0; i < syn.variant.args.len; i++) {
            generate_polymorphic_i(*(Syntax*)syn.variant.args.data[i], env, target, links, a, point);
        }

        // Generate the variant size
        generate_variant_size_of(RCX, enum_type->enumeration.variants.data[syn.variant.tag].val, env, ass, a, point);

        generate_index_push(reg(RCX, sz_64), ass, a, point);
        index_stack_grow(env, 1);

        // TODO (BUG) : update so we add variant size & variant stack size!
        // dest_stack_offset = RAX = (2 * variant_size) - tag_size
        build_binary_op(ass, Mov, reg(RAX,sz_64), reg(RCX,sz_64), a, point);
        build_binary_op(ass, SHR, reg(RAX,sz_64), imm8(1), a, point);
        build_binary_op(ass, Sub, reg(RAX,sz_64), imm8(tag_size), a, point);

        generate_index_push(reg(RCX, sz_64), ass, a, point);
        generate_index_push(imm32(0), ass, a, point);
        index_stack_grow(env, 2);

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
            // dest_stack_offset -= field_size
            build_binary_op(ass, Sub, rref8(R13, -0x8, sz_64), reg(RAX, sz_64), a, point);

            // RSI = dest offset, R9 = src offset
            build_binary_op(ass, Mov, reg(RSI, sz_64), rref8(R13, -0x8, sz_64), a, point);
            build_binary_op(ass, Mov, reg(R9, sz_64), rref8(R13, 0, sz_64), a, point);

            generate_poly_stack_move(reg(RSI, sz_64), reg(R9, sz_64), reg(RAX, sz_64), ass, a, point);

            //src_stack_offset += stack_size_of(field_size);
            generate_stack_size_of(RAX,args.data[syn.variant.args.len - (i + 1)], env, ass, a, point);
            build_binary_op(ass, Add, rref8(R13, 0, sz_64), reg(RAX, sz_64), a, point);
        }

        build_binary_op(ass, Sub, reg(R13, sz_64), imm32(3 * ADDRESS_SIZE), a, point);
        build_binary_op(ass, Mov, reg(RCX, sz_64), rref8(R13, 8, sz_64), a, point);
        build_binary_op(ass, Sub, reg(RCX, sz_64), imm32(tag_size), a, point);

        // Remove the space occupied by the temporary values 
        build_binary_op(ass, Add, reg(RSP, sz_64), reg(RCX, sz_64), a, point);
        index_stack_grow(env, 3);

        break;
    }
    case SMatch: {
        throw_error(point, mv_string("Not implemented: match in forall."));
    }
    case SLet: {
        throw_error(point, mv_string("Not implemented: let in forall."));
        break;
    }
    case SIf: {
        // Generate the condition
        generate_polymorphic_i(*syn.if_expr.condition, env, target, links, a, point);

        // Pop the bool into R9; compare with 0
        build_unary_op(ass, Pop, reg(R9, sz_64), a, point);
        build_binary_op(ass, Cmp, reg(R9, sz_64), imm32(0), a, point);

        // ---------- CONDITIONAL JUMP ----------
        // compare the value to 0
        // jump to false branch if equal to 0 -- the 32-bit immediate is a placeholder
        AsmResult out = build_unary_op(ass, JE, imm32(0), a, point);
        size_t start_pos = get_pos(ass);

        int32_t* jmp_loc = (int32_t*)(get_instructions(ass).data + out.backlink);

        // ---------- TRUE BRANCH ----------
        // now, generate the code to run (if true)
        generate_polymorphic_i(*syn.if_expr.true_branch, env, target, links, a, point);

        // Generate jump to end of false branch to be backlinked later
        out = build_unary_op(ass, JMP, imm32(0), a, point);

        // calc backlink offset
        size_t end_pos = get_pos(ass);
        if (end_pos - start_pos > INT32_MAX) {
            throw_error(point, mk_string("Jump in conditional too large", a));
        } 

        // backlink
        *jmp_loc = end_pos - start_pos;
        jmp_loc = (int32_t*)(get_instructions(ass).data + out.backlink);
        start_pos = get_pos(ass);


        // ---------- FALSE BRANCH ----------
        // Generate code for the false branch
        generate_polymorphic_i(*syn.if_expr.false_branch, env, target, links, a, point);

        // calc backlink offset
        end_pos = get_pos(ass);
        if (end_pos - start_pos > INT32_MAX) {
            throw_error(point, mk_string("Jump in conditional too large", a));
        } 
        *jmp_loc = end_pos - start_pos;
        break;
    }
    case SLabels: {
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
        generate_index_push(reg(RSP, sz_64), ass, a, point);
        index_stack_grow(env, 1);

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
            index_stack_grow(env, branch->args.len);
            address_bind_label_vars(arg_sizes, env);
            generate_polymorphic_i(*branch->body, env, target, links, a, point);
            address_unbind_label_vars(env);

            LabelEntry lble = label_env_lookup(cell.key, env);
            if (lble.type == Err)
                throw_error(point, mv_string("Label not found during codegen!!"));

            index_stack_shrink(env, branch->args.len + 1);
            // TODO (possible bug: use Sub or Add and + or - ?)
            build_binary_op(ass, Sub, reg(R13, sz_64), imm32((lble.stack_offset + 1) * ADDRESS_SIZE), a, point);

            // Copy the result down the stack. To do this, we need
            // the size (RAX)
            // The destination (initial RSP - size)
            // The source offset (0)
            generate_size_of(RAX, syn.ptype, env, ass, a, point);
            build_binary_op(ass, Mov, reg(R10, sz_64), rref8(R13, 0, sz_64), a, point);
            generate_index_push(reg(RAX, sz_64), ass, a, point);

            build_binary_op(ass, Sub, reg(R10, sz_64), reg(RAX, sz_64), a, point);
            generate_poly_stack_move(reg(R10, sz_64), imm32(0), reg(RAX, sz_64), ass, a, point);

            // Ensure the stack is at the same point for the next iteration! 
            generate_index_pop(reg(RAX, sz_64), ass, a, point);
            generate_index_pop(reg(RSP, sz_64), ass, a, point);
            build_binary_op(ass, Sub, reg(RSP, sz_64), reg(RAX, sz_64), a, point);

            AsmResult out = build_unary_op(ass, JMP, imm32(0), a, point);
            sym_size_bind(cell.key, out.backlink, &label_jumps);
        }

        // The final iteration will have shrunk the stack "too much", so add the
        // result back in.
        index_stack_grow(env, 1);

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
                
                int8_t* loc_byte = (int8_t*) get_instructions(ass).data + backlink;
                int32_t* loc = (int32_t*)loc_byte;
                *loc = (int32_t) amt;
            }


            // Step 2: fill out each jump to this label.
            SizeArray* arr = sym_sarr_lookup(sym, links->gotolinks);
            if (!arr) panic(mv_string("Can't find size array when backlinking label!"));

            for (size_t i = 0; i < arr->len; i++) {
                size_t backlink = arr->data[i];
                size_t origin = backlink + 4; // the + 4 accounts for the 4-byte immediate

                int64_t amt = dest - origin;
                if (amt < INT32_MIN || amt > INT32_MAX) panic(mv_string("Label jump too large!"));
                
                int8_t* loc_byte = (int8_t*) get_instructions(ass).data + backlink;
                int32_t* loc = (int32_t*)loc_byte;
                *loc = (int32_t) amt;
            }
        }

        address_end_labels(env);
        break;
    }
    case SGoTo: {
        // Generating code for a goto:
        // 1. Generate args
        for (size_t i = 0; i < syn.go_to.args.len; i++) {
            Syntax* expr = syn.go_to.args.data[i];

            // Store the size in the index stack
            // (this will later be used to index the input variable to the label)
            generate_stack_size_of(RAX, expr->ptype, env, ass, a, point);
            generate_index_push(reg(RAX, sz_64), ass, a, point);
            index_stack_grow(env, 1);

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
            build_binary_op(ass, Mov, reg(R9, sz_64), imm32(0), a, point);
            for (int64_t i = 0; i < (int64_t)syn.go_to.args.len; i++) {
                build_binary_op(ass, Mov, reg(R8, sz_64), reg(R9, sz_64), a, point);
                build_binary_op(ass, Add, reg(R9, sz_64), rref8(R13, -i * 8, sz_64), a, point);
                build_binary_op(ass, Mov, rref8(R13, -i * 8, sz_64), reg(R8, sz_64), a, point);
            }

            // Adjust the stack so it has the same value that one would have
            // going into a labels expression.
            // TODO handle dynamic variable unbinding (if needed!)

            // Get the dest offset by taking the original RSP and
            // subtracting the new RSP and the size 
            build_binary_op(ass, Mov, reg(R11, sz_64), reg(R13, sz_64), a, point);
            build_binary_op(ass, Sub, reg(R11, sz_64), imm32(rsp_offset), a, point);
            build_binary_op(ass, Mov, reg(R10, sz_64), rref8(R11, 0, sz_64), a, point);
            build_binary_op(ass, Sub, reg(R10, sz_64), reg(RSP, sz_64), a, point);
            build_binary_op(ass, Sub, reg(R10, sz_64), reg(R9, sz_64), a, point);

            // Save the dest_offset (We will add this to RSP after the copy)
            generate_index_push(reg(R10, sz_64), ass,  a, point);

            generate_poly_stack_move(reg(R10, sz_64), imm32(0), reg(R9, sz_64), ass, a, point);

            generate_index_pop(reg(R10, sz_64), ass,  a, point);
            build_binary_op(ass, Add, reg(RSP, sz_64), reg(R10, sz_64), a, point);

            // Now, update the index stack to handle the new offsets 
            for (int64_t i = 0; i < (int64_t)syn.go_to.args.len; i++) {
                build_binary_op(ass, Add, rref8(R13, -i * 8, sz_64), reg(RSP, sz_64), a, point);
            }

            // Now, shrink the address stack appropriately 
            generate_index_stack_move(delta, 0, syn.go_to.args.len * 8, ass, a, point);
            build_binary_op(ass, Sub, reg(R13, sz_64), imm32(delta), a, point);

            AsmResult out = build_unary_op(ass, JMP, imm32(0), a, point);

            backlink_goto(syn.go_to.label, out.backlink, links, a);

            index_stack_shrink(env, syn.go_to.args.len);
        } else {
            throw_error(point, mv_string("Label not found during codegen!!"));
        }
        break;
    }
    case SSequence: {
        size_t num_bindings = 0;

        // Store the (current) RSP on top of the stack.
        generate_index_push(reg(RSP, sz_64), ass, a, point);
        index_stack_grow(env, 1);
        for (size_t i = 0; i < syn.sequence.elements.len; i++) {
            SeqElt* elt = syn.sequence.elements.data[i];
            generate_polymorphic_i(*elt->expr, env, target, links, a, point);

            if (elt->is_binding && i + 1 != syn.sequence.elements.len) {
                num_bindings++;
                generate_index_push(reg(RSP, sz_64), ass, a, point);
                index_stack_grow(env, 1);
                address_bind_relative(elt->symbol, 0, env);
            }

            // Remember: We do not pop off the stack if this is
            // a) a binding (hence this is an else) 
            // b) the last value in the sequence (as this value is preserved)
            else if (i + 1 != syn.sequence.elements.len) {
                generate_size_of(RAX, elt->expr->ptype, env, ass, a, point);
                build_binary_op(ass, Add, reg(RSP, sz_64), reg(RAX, sz_64), a, point);
            }
            
        }
        if (num_bindings != 0) {
            // The goal is to pop all bindings from the stack, while preserving
            // the topmost value.

            // Store size of value in RAX
            generate_size_of(RAX, syn.ptype, env, ass, a, point);

            // Pop all local bindings, leaving the old RSP on top of the index stack
            build_binary_op(ass, Sub, reg(R13, sz_64), imm32(ADDRESS_SIZE * num_bindings), a, point);

            // Move the old RSP into R8
            build_binary_op(ass, Mov, reg(R8, sz_64), rref8(R13, 0, sz_64), a, point);

            // We want the Dest offset RSP as (destined rsp - current rsp) = (old rsp - size - current rsp)
            build_binary_op(ass, Sub, reg(R8, sz_64), reg(RAX, sz_64), a, point);
            build_binary_op(ass, Sub, reg(R8, sz_64), reg(RSP, sz_64), a, point);

            // Store the dest offset in R13
            build_binary_op(ass, Mov, rref8(R13, 0, sz_64), reg(R8, sz_64), a, point);

            index_stack_shrink(env, num_bindings);
            address_pop_n(num_bindings, env);

            generate_poly_stack_move(reg(R8, sz_64), imm32(0), reg(RAX, sz_64), ass, a, point);

            // Pop offset from index stack & shrink data/control stack accordingly
            generate_index_pop(reg(R8, sz_64), ass, a, point);
            index_stack_shrink(env, 1);
            build_binary_op(ass, Add, reg(RSP, sz_64), reg(R8, sz_64), a, point);
        } else {
            // Pop old RSP from index stack 
            build_binary_op(ass, Sub, reg(R13, sz_64), imm32(ADDRESS_SIZE), a, point);
            index_stack_shrink(env, 1);
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
        build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
        break;
    }
    case SAlignOf: {
        generate_align_of(RAX, syn.size->type_val, env, ass, a, point);
        build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
        break;
    }
    case SCheckedType: {
        build_binary_op(ass, Mov, reg(R9, sz_64), imm64((uint64_t)syn.type_val), a, point);
        build_unary_op(ass, Push, reg(R9, sz_64), a, point);
        break;
    }
    default: {
        panic(mv_string("Invalid abstract term in polymorphic codegen."));
    }
    }
}

void generate_size_of(Regname dest, PiType* type, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point) {
    type = strip_type(type);
    switch (type->sort) {
    case TPrim:
    case TProc:
    case TTraitInstance:
        build_binary_op(ass, Mov, reg(dest, sz_64), imm32(pi_size_of(*type)), a, point);
        break;
    case TVar: {
        AddressEntry e = address_env_lookup(type->var, env);
        switch (e.type) {
        case ALocalDirect:
            build_binary_op(ass, Mov, reg(dest, sz_64), rref8(RBP, e.stack_offset, sz_64), a, point);
            build_binary_op(ass, SHR, reg(dest, sz_64), imm8(28), a, point);
            build_binary_op(ass, And, reg(dest, sz_64), imm32(0xFFFFFFF), a, point);
            break;
        case ALocalIndirect:
            panic(mv_string("Cannot generate code for size of local indirect."));
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
    default: {
        // TODO BUG: This seems to cause crashes!
        PtrArray nodes = mk_ptr_array(4, a);
        push_ptr(mv_str_doc(mv_string("Unrecognized type to generate_size_of:"), a), &nodes);
        push_ptr(pretty_type(type, a), &nodes);
        Document* message = mk_sep_doc(nodes, a);
        panic(doc_to_str(message, a));
    }
    }
}

void generate_align_of(Regname dest, PiType* type, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point) {
    switch (type->sort) {
    case TPrim:
    case TProc:
    case TTraitInstance:
        build_binary_op(ass, Mov, reg(dest, sz_64), imm32(pi_align_of(*type)), a, point);
        break;
    case TVar: {
        // TODO (BUG UB VERY BAD) This is size - not alignment. Alignment
        //   needs to be accounted for!
        AddressEntry e = address_env_lookup(type->var, env);
        switch (e.type) {
        case ALocalDirect:
            build_binary_op(ass, Mov, reg(dest, sz_64), rref8(RBP, e.stack_offset, sz_64), a, point);
            build_binary_op(ass, SHR, reg(dest, sz_64), imm8(56), a, point);
            break;
        case ALocalIndirect:
            panic(mv_string("Cannot generate align-of code for local indirect variable."));
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
    default: {
        // TODO BUG: This seems to cause crashes!
        PtrArray nodes = mk_ptr_array(4, a);
        push_ptr(mv_str_doc(mv_string("Unrecognized type provided to generate_align_of:"), a), &nodes);
        push_ptr(pretty_type(type, a), &nodes);
        Document* message = mk_sep_doc(nodes, a);
        panic(doc_to_str(message, a));
    }
    }
}

void generate_variant_size_of(Regname dest, PtrArray* types, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // Tag size
    size_t sz = sizeof(uint64_t);
    for (size_t i = 0; i < types->len; i++) {
      size_t out;
      Result_t sz_res = pi_maybe_size_of(*(PiType*)types->data[i], &out);
      if (sz_res == Ok) {
          sz += sz_res;
      } else {
          panic(mv_string("Not yet implemeted: codegen for generat_variant_size_of"));
      }
    }
    build_binary_op(ass, Mov, reg(dest, sz_64), imm32(sz), a, point);
}

void generate_stack_size_of(Regname dest, PiType* type, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point) {
    size_t sz; 
    Result_t sz_res = pi_maybe_size_of(*type, &sz);
    if (sz_res == Ok) {
        build_binary_op(ass, Mov, reg(dest, sz_64), imm32(sz), a, point);
    } else {
        if (type->sort == TVar) {
            AddressEntry e = address_env_lookup(type->var, env);
            switch (e.type) {
            case ALocalDirect:
                build_binary_op(ass, Mov, reg(dest, sz_64), rref8(RBP, e.stack_offset, sz_64), a, point);
                build_binary_op(ass, And, reg(dest, sz_64), imm32(0xFFFFFFF), a, point);
                break;
            case ALocalIndirect:
                panic(mv_string("cannot generate code for local indirect."));
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
        } else {
            // TODO BUG: This seems to cause crashes!
            PtrArray nodes = mk_ptr_array(4, a);
            push_ptr(mv_str_doc(mv_string("Unrecognized type provided to generate_stack_size_of:"), a), &nodes);
            push_ptr(pretty_type(type, a), &nodes);
            Document* message = mk_sep_doc(nodes, a);
            panic(doc_to_str(message, a));
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
    build_binary_op(ass, Mov, reg(RAX, sz_64), reg(sz_reg, sz_64), a, point); 
    build_binary_op(ass, Mov, reg(RDX, sz_64), imm32(0), a, point); 
    build_binary_op(ass, Mov, reg(RCX, sz_64), reg(align, sz_64), a, point); 
    build_unary_op(ass, IDiv, reg(RCX, sz_64), a, point); 

    // Now, rem is in RDX

    // Store align - rem in RAX and 0 in RCX
    build_binary_op(ass, Mov, reg(RAX, sz_64), reg(align, sz_64), a, point); 
    build_binary_op(ass, Sub, reg(RAX, sz_64), reg(RDX, sz_64), a, point); 
    build_binary_op(ass, Mov, reg(RCX, sz_64), imm32(0), a, point); 

    // Nowd to the compare (rem == 0) and CMove (asignment base on compare), so
    // the result (pad) is in RDX
    build_binary_op(ass, Cmp, reg(RDX, sz_64), imm32(0), a, point); 
    build_binary_op(ass, CMovE, reg(RAX, sz_64), reg(RCX, sz_64), a, point); 

    // Finally, add size (sz_reg) to padding (RDX)
    build_binary_op(ass, Add, reg(sz_reg, sz_64), reg(RAX, sz_64), a, point); 

    //build_binary_op(ass, Mov, reg(sz_reg, sz_64), reg(R9, sz_64), a, point); 
}

void generate_poly_move(Location dest, Location src, Location size, Assembler* ass, Allocator* a, ErrorPoint* point) {

#if ABI == SYSTEM_V_64
    // memcpy (dest = rdi, src = rsi, size = rdx)
    // copy size into RDX
    build_binary_op(ass, Mov, reg(RDI, sz_64), dest, a, point);
    build_binary_op(ass, Mov, reg(RSI, sz_64), src, a, point);
    build_binary_op(ass, Mov, reg(RDX, sz_64), size, a, point);

#elif ABI == WIN_64
    // memcpy (dest = rcx, src = rdx, size = r8)
    build_binary_op(ass, Mov, reg(RCX, sz_64), dest, a, point);
    build_binary_op(ass, Mov, reg(RDX, sz_64), src, a, point);
    build_binary_op(ass, Mov, reg(R8, sz_64), size, a, point);
    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#else
#error "Unknown calling convention"
#endif

    // copy memcpy into RCX & call
    build_binary_op(ass, Mov, reg(RAX, sz_64), imm64((uint64_t)&memcpy), a, point);
    build_unary_op(ass, Call, reg(RAX, sz_64), a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(32), a, point);
#endif
}

void generate_poly_stack_move(Location dest, Location src, Location size, Assembler* ass, Allocator* a, ErrorPoint* point) {

#if ABI == SYSTEM_V_64
    // memcpy (dest = rdi, src = rsi, size = rdx)
    // copy size into RDX
    build_binary_op(ass, Mov, reg(RDI, sz_64), dest, a, point);
    build_binary_op(ass, Add, reg(RDI, sz_64), reg(RSP, sz_64), a, point);
    build_binary_op(ass, Mov, reg(RSI, sz_64), src, a, point);
    build_binary_op(ass, Add, reg(RSI, sz_64), reg(RSP, sz_64), a, point);
    build_binary_op(ass, Mov, reg(RDX, sz_64), size, a, point);

#elif ABI == WIN_64
    // memcpy (dest = rcx, src = rdx, size = r8)
    build_binary_op(ass, Mov, reg(RCX, sz_64), dest, a, point);
    build_binary_op(ass, Add, reg(RCX, sz_64), reg(RSP, sz_64), a, point);
    build_binary_op(ass, Mov, reg(RDX, sz_64), src, a, point);
    build_binary_op(ass, Add, reg(RDX, sz_64), reg(RSP, sz_64), a, point);
    build_binary_op(ass, Mov, reg(R8, sz_64), size, a, point);
#else
#error "Unknown calling convention"
#endif

    generate_c_call(memmove, ass, a, point);
}

void generate_index_stack_move(size_t dest_stack_offset, size_t src_stack_offset, size_t size, Assembler* ass, Allocator* a, ErrorPoint* point) {
    if (size== 0 || dest_stack_offset == src_stack_offset) return; // nothing to do
    if (size % 8 != 0) { panic(mv_string("index stack movement is always expected to be 8-byte aligned")); }

    if ((dest_stack_offset + size) > 127 || (src_stack_offset + size) > 127)  {
        // TODO: check to ensure offsets don't exceed 32 bit max

#if ABI == SYSTEM_V_64
        // stack_move (dest = rdi, src = rsi, size = rdx)
        // copy size into RDX
        build_binary_op(ass, Mov, reg(RDI, sz_64), imm32(dest_stack_offset), a, point);
        build_binary_op(ass, Add, reg(RDI, sz_64), reg(R13, sz_64), a, point);
        build_binary_op(ass, Mov, reg(RSI, sz_64), imm32(src_stack_offset), a, point);
        build_binary_op(ass, Add, reg(RSI, sz_64), reg(R13, sz_64), a, point);
        build_binary_op(ass, Mov, reg(RDX, sz_64), imm32(size), a, point);

#elif ABI == WIN_64
        // stack_move (dest = rcx, src = rdx, size = r8)
        build_binary_op(ass, Mov, reg(RCX, sz_64), imm32(dest_stack_offset), a, point);
        build_binary_op(ass, Add, reg(RCX, sz_64), reg(R13, sz_64), a, point);
        build_binary_op(ass, Mov, reg(RDX, sz_64), imm32(src_stack_offset), a, point);
        build_binary_op(ass, Add, reg(RDX, sz_64), reg(R13, sz_64), a, point);
        build_binary_op(ass, Mov, reg(R8, sz_64), imm32(size), a, point);
#else
#error "Unknown calling convention"
#endif

        generate_c_call(memmove, ass, a, point);
    } else  {
        // Using 8-bit immediate is ok

        for (size_t i = 0; i < size / 8; i++) {
            build_binary_op(ass, Mov, reg(RAX, sz_64), rref8(R13, -src_stack_offset - (i * 8) , sz_64), a, point);
            build_binary_op(ass, Mov, rref8(R13, -dest_stack_offset - (i * 8), sz_64), reg(RAX, sz_64), a, point);
        }
    }
}

void generate_index_push(Location src, Assembler* ass, Allocator* a, ErrorPoint* point) {
    build_binary_op(ass, Add, reg(R13, sz_64), imm8(8), a, point);
    build_binary_op(ass, Mov, rref8(R13, 0, sz_64), src, a, point);
}

void generate_index_pop(Location dest, Assembler* ass, Allocator* a, ErrorPoint* point) {
    build_binary_op(ass, Mov, dest, rref8(R13, 0, sz_64), a, point);
    build_binary_op(ass, Sub, reg(R13, sz_64), imm8(8), a, point);
}
