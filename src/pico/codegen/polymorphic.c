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
void generate_size_of(Regname dest, PiType* type, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_align_of(Regname dest, PiType* type, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_align_to(Regname sz, Regname align, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_stack_size_of(Regname dest, PiType* type, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_poly_move(Location dest, Location src, Location size, Assembler* ass, Allocator* a, ErrorPoint* point);

void generate_polymorphic(SymbolArray types, Syntax syn, AddressEnv* env, Target target, InternalLinkData* links, Allocator* a, ErrorPoint* point) {
    Assembler* ass = target.target;
    SymbolArray vars;
    Syntax body;
    if (syn.type == SProcedure) {
        vars = mk_u64_array(syn.procedure.args.len + syn.procedure.implicits.len, a);
        for (size_t i = 0; i < syn.procedure.implicits.len; i++) {
            push_u64(syn.procedure.implicits.data[i].key, &vars);
        }
        for (size_t i = 0; i < syn.procedure.args.len; i++) {
            push_u64(syn.procedure.args.data[i].key, &vars);
        }
        body = *syn.procedure.body;
    } else {
        vars = mk_u64_array(0, a);
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

    // Note: we push R9 (the new head of stack) so it can be used
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
    case SVariable: {
        // Lookup the variable in the assembly envionrment
        AddressEntry e = address_env_lookup(syn.variable, env);
        switch (e.type) {
        case ALocalDirect:
            // TODO (UB BUG): this won't work for values > 64 bits wide!
            throw_error(point, mv_string("Codegen not implemented for Local Direct variables"));
            break;
        case ALocalIndirect:
            // First, we need the size of the variable & allocate space for it on the stack
            // ------------------------------
            // Store stack size in R8
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
        case ATypeVar:
            throw_error(point, mv_string("Codegen not implemented for ATypeVar"));
            break;
        case AGlobal:
            // Use RAX as a temp
            // Note: casting void* to uint64_t only works for 64-bit systems...
            if (syn.ptype->sort == TProc) {
                AsmResult out = build_binary_op(ass, Mov, reg(R9, sz_64), imm64((uint64_t)e.value), a, point);
                backlink_global(syn.variable, out.backlink, links, a);

                out = build_unary_op(ass, Push, reg(R9, sz_64), a, point);
            } else if (syn.ptype->sort == TPrim) {
                AsmResult out = build_binary_op(ass, Mov, reg(RCX, sz_64), imm64((uint64_t)e.value), a, point);
                backlink_global(syn.variable, out.backlink, links, a);
                build_binary_op(ass, Mov, reg(R9, sz_64), rref8(RCX, 0, sz_64), a, point);
                build_unary_op(ass, Push, reg(R9, sz_64), a, point);
            } else if (syn.ptype->sort == TKind) {
                AsmResult out = build_binary_op(ass, Mov, reg(RCX, sz_64), imm64((uint64_t)e.value), a, point);
                backlink_global(syn.variable, out.backlink, links, a);
            } else if (syn.ptype->sort == TStruct || syn.ptype->sort == TEnum) {
                // This is a global variable, and therefore has a known monomorphic type
                size_t value_size = pi_size_of(*syn.ptype);
                AsmResult out = build_binary_op(ass, Mov, reg(RCX, sz_64), imm64((uint64_t)e.value), a, point);
                backlink_global(syn.variable, out.backlink, links, a);

                // Allocate space on the stack for composite type (struct/enum)
                out = build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(value_size), a, point);

                // copy
                generate_monomorphic_copy(RSP, RCX, value_size, ass, a, point);

            } else {
                throw_error(point, mv_string("Codegen: Global has unsupported sort: must be Primitive or Proc"));
            }
            break;
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
        throw_error(point, mv_string("Internal error: cannot generate all application inside polymorphic code"));
    }
    case SStructure: {
        throw_error(point, mv_string("Not implemented: structure in forall."));
    }
    case SProjector: {
        if (syn.projector.val->ptype->sort == TStruct) {
            panic(mv_string("Projector not implemented for structures in polymorphic function"));
        } else {
            generate_polymorphic_i(*syn.projector.val, env, target, links, a, point);

            // Now, calculate offset for field 
            build_unary_op(ass, Push, imm8(0), a, point);
            for (size_t i = 0; i < syn.projector.val->ptype->instance.fields.len; i++) {
                if (i != 0) {
                    // Align to the new field; can skip if size = 0;
                    generate_align_of(R8, (PiType*)syn.projector.val->ptype->instance.fields.data[i].val, env, ass, a, point);
                    build_binary_op(ass, Mov, reg(R9, sz_64), rref8(RSP, 0, sz_64), a, point);
                    generate_align_to(R9, R8, ass, a, point);
                    build_binary_op(ass, Mov, rref8(RSP, 0, sz_64), reg(R9, sz_64), a, point);
                }

                if (syn.projector.val->ptype->instance.fields.data[i].key == syn.projector.field)
                    break;
                // Push the size into RAX; this is then added to the top of the stack  
                generate_size_of(RAX, (PiType*)syn.projector.val->ptype->instance.fields.data[i].val, env, ass, a, point);
                build_binary_op(ass, Add, rref8(RSP, 0, sz_64), reg(RAX, sz_64), a, point);
            }

            // Generate the size of the output.
            generate_stack_size_of(RAX, syn.ptype, env, ass, a, point);

            // Pop the index and instance ptr from the stack
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
        throw_error(point, mv_string("Not implemented: constructor in forall."));
    }
    case SVariant: {
        throw_error(point, mv_string("Not implemented: variant in forall."));
    }
    case SMatch: {
        throw_error(point, mv_string("Not implemented: match in forall."));
    }
    case SLet: {
        throw_error(point, mv_string("Not implemented: let in forall."));
        break;
    }
    case SIf: {
        throw_error(point, mv_string("Not implemented: if in forall."));
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
            panic(mv_string("cannot generate code for local indirect."));
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
            panic(mv_string("cannot generate code for local indirect."));
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

void generate_stack_size_of(Regname dest, PiType* type, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point) {
    switch (type->sort) {
    case TPrim:
    case TProc:
    case TTraitInstance:
        build_binary_op(ass, Mov, reg(dest, sz_64), imm32(8), a, point);
        break;
    case TVar: {
        AddressEntry e = address_env_lookup(type->var, env);
        switch (e.type) {
        case ALocalDirect:
            build_binary_op(ass, Mov, reg(dest, sz_64), rref8(RBP, e.stack_offset, sz_64), a, point);
            build_binary_op(ass, And, reg(dest, sz_64), imm32(0xFFFFFFF), a, point);
            break;
        case ALocalIndirect:
            panic(mv_string("cannot generate code for local indirect."));
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
