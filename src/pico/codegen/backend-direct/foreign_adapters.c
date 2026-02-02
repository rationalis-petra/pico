#include "platform/signals.h"
#include "platform/machine_info.h"
#include "data/stringify.h"
#include "components/pretty/string_printer.h"

#include "pico/codegen/backend-direct/internal.h"
#include "pico/codegen/backend-direct/foreign_adapters.h"

#if ABI == SYSTEM_V_64 
// -----------------------------------------------------------------------------
// System V Specific stuff
// -----------------------------------------------------------------------------

typedef enum : uint8_t {
    SysVInteger,
    SysVSSE,
    SysVSSEUp,
    SysVx87,
    SysVx87Up,
    SysVComplex_x87,
    SysVNoClass,
    SysVMemory,
} SysVArgClass;

U8Array system_v_arg_classes(CType* type, Allocator* a);

SysVArgClass merge_sysv_classes(SysVArgClass c1, SysVArgClass c2) {
    // a) If both classes are equal, this is the resulting class
    if (c1 == c2) {
        return c1;
    }

    // b) If one of the classes is NoClass, this other is the resulting class
    if (c1 == SysVNoClass) {
        return c2;
    } else if (c2 == SysVNoClass) {
        return c1;
    }

    // c) If one of the classes is Memory, the result is Memory
    if (c1 == SysVMemory || c2 == SysVMemory) {
        return SysVMemory;
    }

    // d) If one of the classes is Integer, the result is Integer
    if (c1 == SysVInteger || c2 == SysVInteger) {
        return SysVInteger;
    }
    // e) If one of the classes is X87, X87UP, COMPLEX_X87, Memory is
    // the resulting class.
    if (c1 == SysVx87 || c1 == SysVx87Up || c1 == SysVComplex_x87
        || c2 == SysVx87 || c2 == SysVx87Up || c2 == SysVComplex_x87) {
        return SysVMemory; 
    }

    // Otherwise, SSE is used
    return SysVSSE;
}

void populate_sysv_words(U8Array* out, size_t offset, CType* type, Allocator* a) {
    // We probably want to recurse and merge arrays from subclasses
    switch (type->sort) {
    case CSVoid:
        // Do nothing, merge with class 'None'
        break;
    case CSPrimInt:
        switch (type->prim.prim) {
        case CChar:
        case CShort:
        case CInt:
        case CLong:
        case CLongLong:
            out->data[offset / 8] = merge_sysv_classes(SysVInteger, out->data[offset / 8]);
        }
        break;
    case CSFloat:
    case CSDouble:
        out->data[offset / 8] = merge_sysv_classes(SysVSSE, out->data[offset / 8]);
        break;
    case CSPtr:
    case CSProc:
            out->data[offset / 8] = merge_sysv_classes(SysVInteger, out->data[offset / 8]);
        break;
    case CSIncomplete:
        panic(mv_string("incomplete type does not have arg class"));
        break;
    case CSStruct: {
        for (size_t i = 0; i < type->structure.fields.len; i++) {
            // Consider
            // struct {struct x : int} {struct y : int} -> (x, y) occupy one
            // eightbyte (x & y have alignment 4) 
            // struct {struct x : int} {struct y : int z : long} -> (x, y) each
            // occupy a separate eightbyte (the second struct now has alignment 8)

            // So, to determine which eightbyte a particular component is in, we
            // need size and alignment
            CType field_ty = type->structure.fields.data[i].val;
            size_t field_sz = c_size_of(field_ty);
            size_t field_al = c_align_of(field_ty);
            offset = c_size_align(offset, field_al);
            
            // We need now to figure out get the eightbytes
            populate_sysv_words(out, offset, &field_ty, a);

            offset += field_sz; 
        }
        break;
    }
    case CSUnion:
        for (size_t i = 0; i < type->cunion.fields.len; i++) {
            CType* field_ty = type->cunion.fields.data[i].val;
            populate_sysv_words(out, offset, field_ty, a);
        }
        break;
    case CSCEnum:
        out->data[offset / 8] = merge_sysv_classes(SysVInteger, out->data[offset / 8]);
        break;
    }
}

U8Array system_v_arg_classes(CType* type, Allocator* a) {
    U8Array out = mk_u8_array(8, a);
    bool use_memory = false;
    size_t type_size = c_size_of(*type);
    if (type_size <= 64) {
        // We start by calculating the number of words (eightbytes) we need.
        // This is the divisor (rounded up) of type_size and 8, as words are eightbytes.
        // to ensure that there is a round up, we add 7 to the numerator, as
        // c integer division by default rounds down. 
        size_t num_words = (type_size + 7) / 8;
        for (size_t i = 0; i < num_words; i++) {
            push_u8(SysVNoClass, &out);
        }
        populate_sysv_words(&out, 0, type, a);
    } 

    // Post merger cleanup:
    // a) if one of the classes is Memory, the whole argument is passed in memory
    for (size_t i = 0; i < out.len; i++) {
      if (out.data[i] == SysVMemory) {
          use_memory = true;
      };
    }
    // TODO b) if x87up is not preceeded by x87, the whole argument is passed in memory 
    // c) If the size of the aggregate exceeds two eightbytes and the first eightbyte isn’t
    //    SSE or any other eightbyte isn’t SSEUP, the whole argument is passed in memory
    if (type_size > 16) {
        if (out.data[0] != SysVSSE) use_memory = true;
        for (size_t i = 1; i < out.len; i++) {
            if (out.data[i] != SysVSSEUp) use_memory = true;
        }
    }
    // TODO d) if SSEup is not preceeded by SSE, the whole argument is passed in memory 

    // If anything set the entire class to memory; then push memory
    if (use_memory) {
        out.len = 0;
        push_u8(SysVMemory, &out);
    }
    return out;
}

#elif ABI == WIN_64 

typedef enum {
    Win64None,
    Win64Floating,
    Win64Integer,
    Win64SmallAggregate, // <= 64 bits
    Win64LargeAggregate,
    Win64M128, // __m128
} Win64ArgClass;

Win64ArgClass win_64_arg_class(CType* type) {
    switch (type->sort) {
    case CSVoid:
        return Win64None;
    case CSPrimInt:
        switch (type->prim.prim) {
        case CChar:
        case CShort:
        case CInt:
        case CLong:
        case CLongLong:
            return Win64Integer;
        }
        break;
    case CSFloat:
    case CSDouble:
        return Win64Floating;
    case CSPtr:
    case CSProc:
        return Win64Integer;
    case CSIncomplete:
        panic(mv_string("Incomplete type does not have arg class"));
    case CSStruct: 
    case CSUnion: {
        size_t aggregate_sz = c_size_of(*type);
        if (aggregate_sz <= 8) {
            return Win64SmallAggregate;
        } else {
            return Win64LargeAggregate;
        }
    }
    case CSCEnum:
        return Win64Integer;
        break;
    }
    panic(mv_string("Invalid c type provided to win_64_arg_class."));
}
#endif

void bd_convert_c_fn(void* cfn, CType* ctype, PiType* ptype, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // This function is for converting a c function (assumed platform default
    // ABI) into a pico function. This means that it assumes that all arguments
    // are pushed on the stack in forward order (last at top).
    if (ctype->sort != CSProc || ptype->sort != TProc) {
        panic(mv_string("convert_c_fun requires types to be functions."));
    }
    if (ctype->proc.args.len != ptype->proc.args.len) {
        String message = string_ncat(a, 4,
                                     mv_string("convert_c_fun requires functions to have same number of args. \n     C Type had : "),
                                     string_u64(ctype->proc.args.len, a),
                                     mv_string("\n Relic Type had : "),
                                     string_u64(ptype->proc.args.len, a));
        panic(message);
    }

    if (!bd_can_reinterpret(ctype->proc.ret, ptype->proc.ret)) {
        // TODO (IMPROVEMENT): Move this check/assert to debug builds?
        PtrArray nodes = mk_ptr_array(4, a);
        push_ptr(mv_cstr_doc("Attempted to do invalid conversion of function return types -", a), &nodes);
        push_ptr(pretty_ctype(ctype->proc.ret, a), &nodes);
        push_ptr(mv_cstr_doc("and", a), &nodes);
        push_ptr(pretty_type(ptype->proc.ret, a), &nodes);
        push_ptr(mv_cstr_doc("are not equal.", a), &nodes);
        panic(doc_to_str(mk_hsep_doc(nodes, a), 120, a));
    }

    // We need knowledge of the stack layout of the input arguments when
    // constructing the function call: We know that in pico args are pushed
    // left-to-right, meaning the last argument is on the bottom of the stack
    // (offset 8). This means we need to go through thelist backwards!
    U64Array arg_offsets = mk_u64_array(ctype->proc.args.len + 1, a);
    arg_offsets.len = arg_offsets.size;
    uint64_t offset = ADDRESS_SIZE; // To account for return address
    for (size_t i = 0; i < ctype->proc.args.len; i++) {
        size_t idx = arg_offsets.len - (i + 1);
        arg_offsets.data[idx] = offset;
        offset += pi_stack_size_of(*(PiType*)ptype->proc.args.data[idx - 1]);

        if (!bd_can_reinterpret(&ctype->proc.args.data[i].val, ptype->proc.args.data[i])) {
            // TODO (IMPROVEMENT): Move this check/assert to debug builds?
            PtrArray nodes = mk_ptr_array(4, a);
            push_ptr(mv_cstr_doc("Attempted to do invalid conversion of function argument types -", a), &nodes);
            push_ptr(pretty_ctype(&ctype->proc.args.data[i].val, a), &nodes);
            push_ptr(mv_cstr_doc("and", a), &nodes);
            push_ptr(pretty_type(ptype->proc.args.data[i], a), &nodes);
            push_ptr(mv_cstr_doc("are not equal.", a), &nodes);
            panic(doc_to_str(mk_hsep_doc(nodes, a), 120, a));
        }
    }
    arg_offsets.data[0] = offset;

#if ABI == SYSTEM_V_64
    // Notes: 
    // R14, RBP, RSP are all callee-saved registers in System V, as they are in Pico.
    // This means that we don't have to take care of saving/restoring their
    // values as part of the function call.

    // If class is Memory, we need to allocate space on the stack as if this
    // were the first argument to the function!
    U64Array in_memory_args = mk_u64_array(ctype->proc.args.len, a); 
    size_t input_area_size = 0;

    const int max_integer_registers = 6;
    const Regname integer_registers[6] = {RDI, RSI, RDX, RCX, R8, R9};
    unsigned char current_integer_register = 0;

    const int max_float_registers = 8;
    const Regname float_registers[8] = {XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7};
    unsigned char current_float_register = 0;

    U8Array return_classes = system_v_arg_classes(ctype->proc.ret, a);
    size_t return_arg_size = pi_stack_align(c_size_of(*ctype->proc.ret));

    // Check if return arg in memory, if so then the return arg takes the spot of
    // a 'hidden' extra argument - specifically, the first, with the ptr being
    // passed in RDI.
    bool pass_return_in_memory =
        (return_classes.len == 1 && return_classes.data[0] == SysVMemory)
        || (return_classes.len > 2);
    if (pass_return_in_memory) {
        input_area_size += return_arg_size;
        current_integer_register++;
    }

    for (size_t i = 0; i < ctype->proc.args.len; i++) {
        CType c_arg = ctype->proc.args.data[i].val;
        
        // Get the classes associated with an argument.  
        // If there are multiple classes, each class in the array corresponds to an eightbyte of the argument.
        U8Array classes = system_v_arg_classes(&c_arg, a);

        unsigned char saved_integer_register = current_integer_register;
        unsigned char saved_float_register = current_integer_register;
        size_t assembler_pos = get_pos(ass);
        bool pass_in_memory = classes.len == 0;

        for (size_t j = 0; j < classes.len; j++) {
          switch (classes.data[j]) {
          case SysVInteger: {
              if (current_integer_register >= max_integer_registers) {
                  // TODO: move ptr to value into address
                  set_pos(ass, assembler_pos);
                  current_integer_register = saved_integer_register;
                  pass_in_memory = true; // this breaks us out of the loop
              } else {
                  Regname next_reg = integer_registers[current_integer_register++];
                  // I8 max = 127
                  // Explanation - copy into current register
                  // copy from RSP + current offset + return address offset + eightbyte_index
                  if (arg_offsets.data[i + 1] > 127) {
                      build_binary_op(Mov, reg(next_reg, sz_64),
                                      rref32(RSP, arg_offsets.data[i + 1] + 0x8 * j, sz_64),
                                      ass, a, point);
                  } else {
                      build_binary_op(Mov, reg(next_reg, sz_64),
                                      rref8(RSP, arg_offsets.data[i + 1] + 0x8 * j, sz_64),
                                      ass, a, point);
                  }
              }
              break;
          }
          case SysVSSE: {
              if (current_float_register >= max_float_registers) {
                  // TODO: move ptr to value into address
                  set_pos(ass, assembler_pos);
                  current_float_register = saved_float_register;
                  pass_in_memory = true; // this breaks us out of the loop
              } else {
                  Regname next_reg = float_registers[current_float_register++];
                  // I8 max = 127
                  if (arg_offsets.data[i + 1] > 127) {
                      throw_error(point, mv_cstr_doc("bd_convert_c_fn: arg offset exeeds I8 max.", a));
                  }
                  // Explanation - copy into current register
                  // copy from RSP + current offset + return address offset + eightbyte_index
                  build_binary_op(MovSD, reg(next_reg, sz_64),
                                  rref8(RSP, arg_offsets.data[i + 1] + 0x8 * j, sz_64),
                                  ass, a, point);
              }
              break;
          }
          case SysVSSEUp:
              panic(mv_string("Not yet implemented: passing arg of class SSE UP"));
              break;
          case SysVx87:
              panic(mv_string("Not yet implemented: passing arg of class x87"));
              break;
          case SysVx87Up:
              panic(mv_string("Not yet implemented: passing arg of class x87 Up"));
              break;
          case SysVComplex_x87:
              panic(mv_string("Not yet implemented: passing arg of class Complex Up"));
              break;
          case SysVNoClass:
              panic(mv_string("Internal Error: argument should not have No Class"));
              break;
          case SysVMemory:
              set_pos(ass, assembler_pos);
              current_integer_register = saved_integer_register;
              current_float_register = saved_float_register;
              pass_in_memory = true; // this breaks us out of the loop
              break;
          }
          if (pass_in_memory) break;
        }

        if (pass_in_memory) {
            push_u64(i, &in_memory_args);
            input_area_size += pi_stack_align(c_size_of(c_arg));
        }

        sdelete_u8_array(classes);
    }

    // Use RBX as an indexing register, to point to the 'base'
    build_binary_op(Mov, reg(RBX, sz_64), reg(RSP, sz_64), ass, a, point);

    // The stack shall be aligned either on 16 bytes, (32 bytes if __m256 is used) or
    // 64 bytes (if __m512 is used). The alignment is to the 'end of input
    // area', i.e. the stack must be (16/32/64) - byte aligned immediately BEFORE the call.  
    // TODO (BUG LOGIC): Determine which stack alignment should be used

    const size_t expected_stack_align = 16;
    size_t needed_stack_offset = input_area_size % expected_stack_align;
    needed_stack_offset = expected_stack_align - needed_stack_offset;

    // Step 1. Ensure there is at least 8 bytes of space (to store the offset)
    build_binary_op(Sub, reg(RSP, sz_64), imm32(8), ass, a, point);
    // Get the bottom byte of RSP (store in RAX), which is the info we need to
    // perform (16-byte) alignment.
    build_binary_op(Mov, reg(RAX, sz_64), reg(RSP, sz_64), ass, a, point);
    build_binary_op(And, reg(RAX, sz_64), imm8(0xf), ass, a, point);

    // Do some fancy stuff
    build_binary_op(Mov, reg(R9, sz_64), imm32(needed_stack_offset), ass, a, point);
    build_binary_op(Sub, reg(R9, sz_64), reg(RAX, sz_64), ass, a, point);
    build_binary_op(And, reg(R9, sz_64), imm8(0xf), ass, a, point);
    build_binary_op(Sub, reg(RSP, sz_64), reg(R9, sz_64), ass, a, point);
    build_binary_op(Mov, rref8(RSP, 0, sz_64), reg(R9, sz_64), ass, a, point);

    if (pass_return_in_memory) {
        // pass in memory - reserve space on stack:
        build_binary_op(Sub, reg(RSP, sz_64), imm32(return_arg_size), ass, a, point);
        build_binary_op(Mov, reg(integer_registers[0], sz_64), reg(RSP, sz_64), ass, a, point);
    }

    for (size_t i = 0; i < in_memory_args.len; i++) {
        // TODO (BUG) - this needs reversing maybe??
        // pass in memory - reserve space on stack:
        size_t arg_idx = in_memory_args.data[in_memory_args.len - (i + 1)];
        size_t arg_size = arg_offsets.data[arg_idx] - arg_offsets.data[arg_idx + 1];
        build_binary_op(Sub, reg(RSP, sz_64), imm32(arg_size), ass, a, point);
        build_binary_op(Mov, reg(R10, sz_64), reg(RBX, sz_64), ass, a, point);
        build_binary_op(Add, reg(R10, sz_64), imm32(arg_offsets.data[arg_idx + 1]), ass, a, point);
        generate_monomorphic_copy(RSP, R10, arg_size, ass, a, point);
    }

    // Call function
    build_binary_op(Mov, reg(RBX, sz_64), imm64((uint64_t)cfn), ass, a, point);
    build_unary_op(Call, reg(RBX, sz_64), ass, a, point);

    if (pass_return_in_memory) {
        //        Stack currently looks like
        //        <Args provided to Pico Fn ...>
        //        [Return Address]
        //        <Align Padding ...>
        //        [Align Adjust]
        // RSP -- <Input Area...>
        // unadjusted offset to be at beginning of args provided to pico fn - return_arg_size (but ignore padding)
        //   0x10 = Return address + Align adjust
        //   However, arg_offsets include an extra + 0x8 to account for return
        //        address, so we subtract 0x8 from 0x10 to get 0x8 
        size_t unadjusted_offset = 0x8 + input_area_size + arg_offsets.data[0] - return_arg_size;

        if (input_area_size - return_arg_size > INT8_MAX || return_arg_size > INT8_MAX) {
            panic(mv_string("Either input area - return arg or return arg exceeded INT8 maximum"));
        }
        // in RBX, store the stack alignment adjust (i.e. the size of the align padding)
        if (input_area_size <= INT8_MAX) {
            build_binary_op(Mov, reg(RBX, sz_64), rref8(RSP, input_area_size, sz_64), ass, a, point);
        } else {
            build_binary_op(Mov, reg(RCX, sz_64), imm32(input_area_size), ass, a, point);
            build_binary_op(Add, reg(RCX, sz_64), reg(RSP, sz_64), ass, a, point);
            build_binary_op(Mov, reg(RBX, sz_64), rref8(RCX, 0, sz_64), ass, a, point);
        }

        // TODO: check usages of input area size: may be > 127

        // Use to to calculate the location of the return address (store in RCX)
        build_binary_op(Mov, reg(RCX, sz_64), reg(RSP, sz_64), ass, a, point);
        if (input_area_size <= INT8_MAX) {
            build_binary_op(Add, reg(RCX, sz_64), imm8(input_area_size), ass, a, point);
        } else {
            build_binary_op(Mov, reg(RSI, sz_64), imm32(input_area_size), ass, a, point);
            build_binary_op(Add, reg(RCX, sz_64), reg(RSI, sz_64), ass, a, point);
        }
        build_binary_op(Add, reg(RCX, sz_64), reg(RBX, sz_64), ass, a, point); // align adjust
        // RCX = ret_addr = [RCX + sizeof(align adjust) = 0x8]
        build_binary_op(Mov, reg(RCX, sz_64), rref8(RCX, 0x8, sz_64), ass, a, point);  

        // To copy the value, first store target address (END of where return value is copied) in RDX 
        build_binary_op(Mov, reg(RDX, sz_64), reg(RSP, sz_64), ass, a, point);
        if (unadjusted_offset <= INT8_MAX) {
            build_binary_op(Add, reg(RDX, sz_64), imm32(unadjusted_offset), ass, a, point);
        } else {
            build_binary_op(Mov, reg(RSI, sz_64), imm32(unadjusted_offset), ass, a, point);
            build_binary_op(Add, reg(RDX, sz_64), reg(RSI, sz_64), ass, a, point);
        }
        build_binary_op(Add, reg(RDX, sz_64), reg(RBX, sz_64), ass, a, point);

        // Now, generate a stack copy targeting RDX
        // TODO: check for not overfloating imm8 maximum
        build_binary_op(Add, reg(RSP, sz_64), imm32(input_area_size - return_arg_size), ass, a, point);
        generate_stack_copy(RDX, return_arg_size, ass, a, point);

        // Then, the remainder of memory (sans the return arg) from the stack.
        build_binary_op(Add, reg(RSP, sz_64), imm32(return_arg_size), ass, a, point);
        build_binary_op(Add, reg(RSP, sz_64), reg(RBX, sz_64), ass, a, point);
        build_binary_op(Add, reg(RSP, sz_64), imm32(0x8 + arg_offsets.data[0] - return_arg_size), ass, a, point);

        // Push return address
        build_unary_op(Push, reg(RCX, sz_64), ass, a, point);
    } else {
        // Pop all memory arguments (both pico and c)
        build_binary_op(Add, reg(RSP, sz_64), imm32(input_area_size), ass, a, point);
        build_binary_op(Add, reg(RSP, sz_64), rref8(RSP, 0, sz_64), ass, a, point);
        build_binary_op(Add, reg(RSP, sz_64), imm8(8), ass, a, point);

        build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);

        // The -0x8 accounts for the fact that we just popped the return address! 
        build_binary_op(Add, reg(RSP, sz_64), imm32(arg_offsets.data[0] - 0x8), ass, a, point);

        // Now, push registers onto stack
        size_t current_int_return_register = return_classes.len - 1;
        const Regname return_integer_registers[2] = {RAX, RDX};
        size_t current_float_return_register = return_classes.len - 1;
        const Regname return_float_registers[2] = {XMM0, XMM1};
        if (return_classes.len > 2) panic(mv_string("Generating foreign adapter for System-V: Expected at most 2 return classes"));
        for (size_t ctr = 0; ctr < return_classes.len; ctr++) {
            size_t i = return_classes.len - (ctr + 1);
            switch (return_classes.data[i]) {
            case SysVInteger: {
                Regname next_reg = return_integer_registers[current_int_return_register--];
                // Push from registers into memory
                build_unary_op(Push, reg(next_reg, sz_64), ass,a, point);
                break;
            }
            case SysVSSE: {
                Regname next_reg = return_float_registers[current_float_return_register--];
                // Push from registers into memory
                build_binary_op(Sub, reg(RSP, sz_64), imm8(8), ass,a, point);
                build_binary_op(MovSD, rref8(RSP, 0, sz_64), reg(next_reg, sz_64), ass,a, point);
                break;
            }
            case SysVSSEUp:
                panic(mv_string("Not yet implemented: passing arg of class SSE UP"));
                break;
            case SysVx87:
                panic(mv_string("Not yet implemented: passing arg of class x87"));
                break;
            case SysVx87Up:
                panic(mv_string("Not yet implemented: passing arg of class x87 Up"));
                break;
            case SysVComplex_x87:
                panic(mv_string("Not yet implemented: passing arg of class Complex Up"));
                break;
            case SysVNoClass:
                panic(mv_string("Internal Error: argument should not have No Class"));
                break;
            case SysVMemory:
                break;
            } 
        }
        build_unary_op(Push, reg(RCX, sz_64), ass, a, point);
    }
    build_nullary_op(Ret, ass, a, point);

    sdelete_u8_array(return_classes);
    sdelete_u64_array(in_memory_args);


#elif ABI == WIN_64
    const Regname integer_registers[4] = {RCX, RDX, R8, R9};
    const Regname float_registers[4] = {XMM0, XMM1, XMM2, XMM3};
    size_t current_register = 0;

    const Win64ArgClass return_class = win_64_arg_class(ctype->proc.ret);
    const bool pass_in_memory = return_class == Win64LargeAggregate || return_class == Win64LargeAggregate;
    const size_t return_arg_size = pi_stack_align(c_size_of(*(CType*)ctype->proc.ret));
    size_t input_area_size = 0;

    // Calculate input area size:
    if (pass_in_memory) {
        input_area_size += pi_stack_align(return_arg_size);
    }

    // skip the first 4 arguments as they go in registers
    for (size_t i = 4; i < ctype->proc.args.len; i++) {
        Win64ArgClass class = win_64_arg_class(&ctype->proc.args.data[i].val);
        if (class == Win64LargeAggregate) {
            input_area_size += ADDRESS_SIZE;
        } else {
            input_area_size += pi_stack_align(c_size_of(ctype->proc.args.data[i].val));
        }
    }

    // Use RBX as an indexing register, to point to the 'base' (before runtime offsets are added)
    build_binary_op(Mov, reg(RBX, sz_64), reg(RSP, sz_64), ass, a, point);

    const size_t expected_stack_align = 16;
    size_t needed_stack_offset = input_area_size % expected_stack_align;
    needed_stack_offset = expected_stack_align - needed_stack_offset;

    // Step 1. Ensure there is at least 8 bytes of space (to store the offset)
    build_binary_op(Sub, reg(RSP, sz_64), imm32(8), ass, a, point);
    // Get the bottom byte of RSP (store in RAX), which is the info we need to
    // perform (16-byte) alignment.
    build_binary_op(Mov, reg(RAX, sz_64), reg(RSP, sz_64), ass, a, point);
    build_binary_op(And, reg(RAX, sz_64), imm8(0xf), ass, a, point);

    // Do some fancy stuff
    build_binary_op(Mov, reg(R9, sz_64), imm32(needed_stack_offset), ass, a, point);
    build_binary_op(Sub, reg(R9, sz_64), reg(RAX, sz_64), ass, a, point);
    build_binary_op(And, reg(R9, sz_64), imm8(0xf), ass, a, point);
    build_binary_op(Sub, reg(RSP, sz_64), reg(R9, sz_64), ass, a, point);
    build_binary_op(Mov, rref8(RSP, 0, sz_64), reg(R9, sz_64), ass, a, point);


    // Check for return arg/space
    if (pass_in_memory) {
        Regname next_reg = integer_registers[current_register++];
        build_binary_op(Sub, reg(RSP, sz_64), imm8(pi_stack_align(c_size_of(*ctype->proc.ret))), ass, a, point);
        build_binary_op(Mov, reg(next_reg, sz_64), reg(RSP, sz_64), ass, a, point);
    }

    // Note: for Win 64 ABI, arguments are push left-to-right, meaning the
    // rightmost argument is at the bottom of the stack. 
    for (size_t i = 0; i < ctype->proc.args.len; i++) {
        if (current_register < 4)
        {
            Win64ArgClass class = win_64_arg_class(&ctype->proc.args.data[i].val);
            switch (class)
            {
            case Win64None: // Do nothing!
                break;
            case Win64Floating:
                Regname next_reg = float_registers[current_register++];
                build_binary_op(MovSD, reg(next_reg, sz_64),
                                rref8(RBX, arg_offsets.data[i + 1], sz_64),
                                ass, a, point);
                break;
            case Win64Integer:
            case Win64SmallAggregate:
            {
                Regname next_reg = integer_registers[current_register++];
                uint64_t offset = arg_offsets.data[i + 1];
                if (offset < INT8_MAX)
                {
                    build_binary_op(Mov, reg(next_reg, sz_64),
                                    rref8(RBX, offset, sz_64),
                                    ass, a, point);
                } else {
                  build_binary_op(Mov, reg(next_reg, sz_64),
                                  rref32(RBX, offset, sz_64),
                                  ass, a, point);
              }
              break;
          }
          case Win64LargeAggregate: {
              Regname next_reg = integer_registers[current_register++];
              // In the case of large aggregates being passed in registers,
              // we actually just pass a pointer in their place, so we can
              // simply place the stack location of the argument in next_reg

              // TODO (IMPROVEMENT): replace with LEA instruction.
              // TODO (BUG): ensure pointer is 16-byte aligned
              build_binary_op(Mov, reg(next_reg, sz_64), reg(RBX, sz_64), ass, a, point);
              if (arg_offsets.data[i+1] > INT8_MAX) {
                  build_binary_op(Add, reg(next_reg, sz_64), imm32(arg_offsets.data[i + 1]), ass, a, point);
              } else {
                  build_binary_op(Add, reg(next_reg, sz_64), imm8(arg_offsets.data[i + 1]), ass, a, point);
              }
              break;
          }
          case Win64M128: {
              throw_error(point, mv_cstr_doc("Not implemented: register arg of class Win64 __m128 ", a));
          }
          }
      } else {
          Win64ArgClass class = win_64_arg_class(&ctype->proc.args.data[i].val);

          // We need to reverse the order of the arguments pushed on the, where the usual 
          // formula is (len - 1) - i. However, we only start reversing when i = 4, meaning
          // we want j to be 4 when i is at (proc.args.len - 1). Therefore, we add 4 to the 
          // total output, giving (len - 1) - i + 4 = (len + 3) - i
          size_t j = (ctype->proc.args.len + 3) - i;
          if (class != Win64LargeAggregate)
          {
              // This is NOT a large aggregate, and can be passed normally
              build_binary_op(Mov, reg(R10, sz_64), reg(RBX, sz_64), ass, a, point);
              build_binary_op(Add, reg(R10, sz_64), imm8(arg_offsets.data[j + 1]), ass, a, point);

              // TODO (IMPROVEMENT) what if different sizes?
              // Copy the argument to the bottom of the stack.
              // TOOD (BUG): ensure value is stack-aligned
              size_t arg_size = arg_offsets.data[j] - arg_offsets.data[j + 1];
              build_binary_op(Sub, reg(RSP, sz_64), imm8(arg_size), ass, a, point);
              generate_monomorphic_copy(RSP, R10, arg_size, ass, a, point);
          } else {
            // This *is* a large aggregate - pass a pointer to the argument!
            // TODO (BUG): ensure pointer is 16-byte aligned
            build_binary_op(Mov, reg(R10, sz_64), reg(RBX, sz_64), ass, a, point);
            build_binary_op(Add, reg(R10, sz_64), imm8(arg_offsets.data[j + 1]), ass, a, point);
            build_unary_op(Push, reg(R10, sz_64), ass, a, point);
        }
      }
    }
    
    // Call function
    build_binary_op(Sub, reg(RSP, sz_64), imm8(32), ass, a, point);
    build_binary_op(Mov, reg(RBX, sz_64), imm64((uint64_t)cfn), ass, a, point);
    build_unary_op(Call, reg(RBX, sz_64), ass, a, point);
    build_binary_op(Add, reg(RSP, sz_64), imm8(32), ass, a, point);

    // Unwind: 
    if (pass_in_memory) {
        //        Stack currently looks like
        //        <Args provided to Pico Fn ...>
        //        [Return Address]
        //        <Align Padding ...>
        //        [Align Adjust]
        // RSP -- <Input Area...>
        // unadjusted offset to be at beginning of args provided to pico fn - return_arg_size (but ignore padding)
        //   0x10 = Return address + Align adjust
        //   However, arg_offsets include an extra + 0x8 to account for return
        //        address, so we subtract 0x8 from 0x10 to get 0x8 
        size_t unadjusted_offset = 0x8 + input_area_size + arg_offsets.data[0] - return_arg_size;

        if (input_area_size - return_arg_size > INT8_MAX || return_arg_size > INT8_MAX) {
            panic(mv_string("Either input area - return arg or return arg exceeded INT8 maximum"));
        }
        // in RBX, store the stack alignment adjust (i.e. the size of the align padding)
        if (input_area_size <= INT8_MAX) {
            build_binary_op(Mov, reg(RBX, sz_64), rref8(RSP, input_area_size, sz_64), ass, a, point);
        } else {
            build_binary_op(Mov, reg(R8, sz_64), reg(RSP, sz_64), ass, a, point);
            build_binary_op(Add, reg(R8, sz_64), imm32(input_area_size), ass, a, point);
            build_binary_op(Mov, reg(RBX, sz_64), rref8(R8, 0, sz_64), ass, a, point);
        }

        // Use to to calculate the location of the return address (store in RCX)
        build_binary_op(Mov, reg(RCX, sz_64), reg(RSP, sz_64), ass, a, point);
        if (input_area_size <= INT8_MAX) {
            build_binary_op(Add, reg(RCX, sz_64), imm8(input_area_size), ass, a, point);
        } else {
            build_binary_op(Mov, reg(R8, sz_64), imm32(input_area_size), ass, a, point);
            build_binary_op(Add, reg(RCX, sz_64), reg(R8, sz_64), ass, a, point);
        }
        build_binary_op(Add, reg(RCX, sz_64), reg(RBX, sz_64), ass, a, point); // align adjust
        // RCX = ret_addr = [RCX + sizeof(align adjust) = 0x8]
        build_binary_op(Mov, reg(RCX, sz_64), rref8(RCX, 0x8, sz_64), ass, a, point);  

        // To copy the value, first store target address (END of where return value is copied) in RDX 
        build_binary_op(Mov, reg(RDX, sz_64), reg(RSP, sz_64), ass, a, point);
        build_binary_op(Add, reg(RDX, sz_64), imm32(unadjusted_offset), ass, a, point);
        build_binary_op(Add, reg(RDX, sz_64), reg(RBX, sz_64), ass, a, point);

        // Then, update RSP to point to the return arg.
        build_binary_op(Add, reg(RSP, sz_64), imm32(input_area_size - return_arg_size), ass, a, point);

        // Now, generate a stack copy targeting RDX
        generate_stack_copy(RDX, return_arg_size, ass, a, point);

        // Then, pop all memory (sans the return arg) from the stack.
        build_binary_op(Add, reg(RSP, sz_64), imm32(return_arg_size), ass, a, point);
        build_binary_op(Add, reg(RSP, sz_64), reg(RBX, sz_64), ass, a, point);
        build_binary_op(Add, reg(RSP, sz_64), imm32(0x8 + arg_offsets.data[0] - return_arg_size), ass, a, point);

        // Push return address
        build_unary_op(Push, reg(RCX, sz_64), ass, a, point);

    } else {
        // Pop all memory arguments (both pico and c)
        build_binary_op(Add, reg(RSP, sz_64), imm32(input_area_size), ass, a, point);
        build_binary_op(Add, reg(RSP, sz_64), rref8(RSP, 0, sz_64), ass, a, point);
        build_binary_op(Add, reg(RSP, sz_64), imm32(0x8), ass, a, point);

        // Pop return address
        build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);

        // The offsets account for the return address (which we just popped!), therefore subtract 0x8
        build_binary_op(Add, reg(RSP, sz_64), imm32(arg_offsets.data[0] - 0x8), ass, a, point);

        // Now, push result onto stack
        if (return_arg_size > 0)
        {
            // Check if is floating or not
            if (ctype->proc.ret->sort == CSFloat) {
                build_binary_op(Sub, reg(RSP, sz_64), imm8(8), ass, a, point);
                build_binary_op(MovSS, rref8(RSP, 0, sz_32), reg(XMM0, sz_32), ass, a, point);
            } else if (ctype->proc.ret->sort == CSDouble) {
                build_binary_op(Sub, reg(RSP, sz_64), imm8(8), ass, a, point);
                build_binary_op(MovSD, rref8(RSP, 0, sz_64), reg(XMM0, sz_64), ass, a, point);
            } else {
                build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
            }
        }

        // Push return address
        build_unary_op(Push, reg(RCX, sz_64), ass, a, point);
    }
    build_nullary_op(Ret, ass, a, point);

#else
#error "convert_c_fun not implemented for unknonw arch"
#endif

    sdelete_u64_array(arg_offsets);
}

bool bd_can_convert(CType *ctype, PiType *ptype) {
    ptype = strip_type(ptype);

    if (ctype->sort == CSProc && ptype->sort == TProc) {
        if (ctype->proc.args.len != ptype->proc.args.len) {
            return false;
        }

        for (size_t i = 0; i < ctype->proc.args.len; i++) {
            if (!bd_can_reinterpret(&ctype->proc.args.data[i].val, ptype->proc.args.data[i])) {
                return false;
            }
        }
        if (!bd_can_reinterpret(ctype->proc.ret, ptype->proc.ret)) {
            return false;
        }

        return true;
    }

    return bd_can_reinterpret(ctype, ptype);
}

bool can_reinterpret_prim(CPrimInt ctype, PrimType ptype) {
#if ABI == SYSTEM_V_64
    switch (ptype) {
    case Unit:  {
        return false;
    }
    case Bool:  {
        return ctype.prim == CChar;
    }
    case Address: {
        // Address comparisons need to be caught earlier.
        // this is fine to enforce as a contract, as the only caller of this
        // function is can_reinterpret (no risk of many callers being confused
        // by overly complex contract.)
        return false;
    }
    case Int_64: {
        return (ctype.prim == CLong || ctype.prim == CLongLong) &&
            (ctype.is_signed == Signed || ctype.is_signed == Unspecified); 
    }
    case Int_32: {
        return ctype.prim == CInt &&
            (ctype.is_signed == Signed || ctype.is_signed == Unspecified); 
    }
    case Int_16: {
        return ctype.prim == CShort &&
            (ctype.is_signed == Signed || ctype.is_signed == Unspecified); 
    }
    case Int_8: {
        return ctype.prim == CChar && // TODO: check if char signed by default
            (ctype.is_signed == Signed || ctype.is_signed == Unspecified); 
    }
    case UInt_64: {
        return (ctype.prim == CLong || ctype.prim == CLongLong) && ctype.is_signed == Unsigned; 
    }
    case UInt_32: {
        return ctype.prim == CInt && ctype.is_signed == Unsigned; 
    }
    case UInt_16: {
        return ctype.prim == CShort && ctype.is_signed == Unsigned;
    }
    case UInt_8: {
        return ctype.prim == CChar && ctype.is_signed == Unsigned;
    }
    case Float_32: 
    case Float_64: 
    case TFormer:
        // TODO (FEATURE): check for enum?
        return false;
    case TMacro:  {
        return false;
    }
    }
#elif ABI == WIN_64
    switch (ptype) {
    case Unit:  {
        return false;
    }
    case Bool:  {
        // TODO (check for signedness?)
        return ctype.prim == CChar;
    }
    case Address: {
        // Address comparisons need to be caught earlier.
        // this is fine to enforce as a contract, as the only caller of this
        // function is can_reinterpret (no risk of many callers being confused
        // by overly complex contract.)
        return false;
    }
    case Int_64: {
        return ctype.prim == CLongLong &&
            (ctype.is_signed == Signed || ctype.is_signed == Unspecified); 
    }
    case Int_32: {
        return (ctype.prim == CInt || ctype.prim == CLong) &&
            (ctype.is_signed == Signed || ctype.is_signed == Unspecified); 
    }
    case Int_16: {
        return ctype.prim == CShort &&
            (ctype.is_signed == Signed || ctype.is_signed == Unspecified); 
    }
    case Int_8: {
        return ctype.prim == CChar && // TODO: check if char signed by default
            (ctype.is_signed == Signed || ctype.is_signed == Unspecified); 
    }
    case UInt_64: {
        return ctype.prim == CLongLong && ctype.is_signed == Unsigned; 
    }
    case UInt_32: {
        return (ctype.prim == CInt || ctype.prim == CLong) && ctype.is_signed == Unsigned; 
    }
    case UInt_16: {
        return ctype.prim == CShort && ctype.is_signed == Unsigned;
    }
    case UInt_8: {
        return ctype.prim == CChar && ctype.is_signed == Unsigned;
    }
    case Float_32:
    case Float_64:
    case TFormer:
        // TODO (FEATURE): check for enum?
        return false;
    case TMacro:  {
        return false;
    }
    }
#else
#error "can_reinterpret_prim not implemented for unknonw arch"
#endif
    // TODO (FEATURE): as this can be invoked by user code with user values, 
    //   perhaps abort on invalid type is too harsh? (throw or return false?) 
    //   perhaps depends on debug state?
    panic(mv_string("Invalid prim provided to can_reinterpret_type"));
}

bool bd_can_reinterpret(CType* ctype, PiType* ptype) {
    // C doesn't have a concept of distinct types, so filter those out. 
    // TODO (BUG LOGIC): possibly don't allow opaque to be converted unless
    //                   we are in the source module
    // TODO (FEATURE): check for well-formedness of types in debug mode?
    ptype = strip_type(ptype);

    switch (ptype->sort) {
    case TPrim: {
        if (ctype->sort == CSPrimInt) {
            return can_reinterpret_prim(ctype->prim, ptype->prim);
        } else if (ctype->sort == CSDouble) {
            // if the ctype could be converted to/from void*
            return ptype->sort == TPrim || ptype->prim == Float_64;
        } else if (ctype->sort == CSFloat) {
            // if the ctype could be converted to/from void*
            return ptype->sort == TPrim || ptype->prim == Float_32;
        } else if (ptype->prim == Address) {
            // if the ctype could be converted to/from void*
            return ctype->sort == CSPtr || ctype->sort == CSProc;
        } else if (ctype->sort == CSVoid && ptype->prim == Unit) {
            return true;
        } else {
            return false;
        }
    }
    case TProc: {
        if (ctype->sort != CSProc) return false;
        if (ptype->proc.implicits.len != 0) return false;
        if (ctype->proc.args.len != ptype->proc.args.len) return false;

        for (size_t i = 0; i < ptype->proc.args.len; i++) {
            if (!bd_can_reinterpret(&ctype->proc.args.data[i].val, ptype->proc.args.data[i]))
                return false;
        }
        return bd_can_reinterpret(ctype->proc.ret, ptype->proc.ret);
    }
    case TStruct: {
        if (ptype->structure.fields.len != ctype->structure.fields.len) {
            return false;
        }
        if (ptype->structure.packed) {
            // TODO (FEATURE): add support for packed c structs.
            return false;
        }

        for (size_t i = 0; i < ptype->structure.fields.len; i++) {
          if (!bd_can_reinterpret(&ctype->structure.fields.data[i].val,
                               ptype->structure.fields.data[i].val)) {
              return false;
          }
        }
        return true;
    }
    case TEnum: {
        // If it's a "simple enum" (all variants have 0 args), check against the tag
        if (ctype->sort == CSPrimInt) {
            // Check against UInt 64
            if (!can_reinterpret_prim(ctype->prim, UInt_64)) return false;
            for (size_t i = 0; i < ptype->enumeration.variants.len; i++) {
                PtrArray* variant = ptype->enumeration.variants.data[i].val;
                if (variant->len != 0) return false;
            }
            return true;
        }

        // TODO: compare 0-member enums with actual c enums/ints.
        if (ctype->sort != CSStruct) return false;
        else if (ctype->structure.fields.len != 2) return false;

        // check that the 0th struct field is reinterpretable as a 64-bit int
        // TODO (FEATURE): change tag size based on number of enum vals 
        PiType tag_type = (PiType) { .sort = TPrim, .prim = UInt_64 };
        if (!bd_can_reinterpret(&ctype->structure.fields.data[0].val, &tag_type)) return false;
        
        CType* cunion = &ctype->structure.fields.data[1].val;


        if (cunion->sort != CSUnion || cunion->cunion.fields.len != ptype->enumeration.variants.len) {
            // Special case: consider what an `Enum :none [:some A]` might look lie
            // in C: struct Option { tag present; A val; }. We thus make an
            // exception for cases where the enum has only one value branch.


            size_t selected_index = 0;
            size_t number_populated_branches = 0;
            for (size_t i = 0; i < ptype->enumeration.variants.len; i++) {
                PtrArray* variant = ptype->enumeration.variants.data[i].val;
                if (variant->len != 0) {
                    number_populated_branches++;
                    selected_index = i;
                }
                if (number_populated_branches > 1) return false;
            }

            
            PtrArray* variant = ptype->enumeration.variants.data[selected_index].val;
            if (variant->len != 1) return false;
            return bd_can_reinterpret(cunion, variant->data[0]);
        }

        // TODO (FEATURE): Add ability for C type to not need union/struct if
        // variants empty.

        for (size_t i = 0; i < cunion->cunion.fields.len; i++) {
            PtrArray* variant = ptype->enumeration.variants.data[i].val;
            if (variant->len == 1) {
                if (!bd_can_reinterpret(cunion->cunion.fields.data[i].val, variant->data[0]))
                    return false;
            } else {
                CType* var_struct = cunion->cunion.fields.data[i].val;
                if (var_struct->sort != CSStruct || var_struct->structure.fields.len != variant->len)
                    return false;

                for (size_t j = 0; j < variant->len; j++) {
                    if (!bd_can_reinterpret(&var_struct->structure.fields.data[j].val, variant->data[j]))
                        return false;
                }
            }

        }
        return true;
    }
    case TSealed:
        return true;

    case TReset:
    case TResumeMark:
    case TDynamic:
    case TNamed:
    case TDistinct:
    case TTrait:
    case TTraitInstance:
    case TCType:
    case TVar:
    case TAll:
    case TCApp:
    case TFam:
    case TKind:
    case TConstraint:
    case TUVar:
        return false;
    default:
        panic(mv_string("invalid types provided to bd_can_reinterpret"));
    }
}
