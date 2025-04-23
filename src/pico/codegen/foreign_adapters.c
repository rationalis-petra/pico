#include "platform/signals.h"
#include "platform/machine_info.h"

#include "pico/codegen/internal.h"
#include "pico/codegen/foreign_adapters.h"

#if ABI == SYSTEM_V_64 
// -----------------------------------------------------------------------------
// System V Specific stuff
// -----------------------------------------------------------------------------

typedef enum {
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
        panic(mv_string("void type does not have arg class"));
        break;
    case CSPrim:
        switch (type->prim.prim) {
        case CChar:
        case CShort:
        case CInt:
        case CLong:
        case CLongLong:
            out->data[offset / 8] = merge_sysv_classes(SysVInteger, out->data[offset / 8]);
        }
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
            CType* field_ty = type->structure.fields.data[i].val;
            size_t field_sz = c_size_of(*field_ty);
            size_t field_al = c_align_of(*field_ty);
            offset = c_size_align(offset, field_al);
            
            // We need now to figure out get the eightbytes
            populate_sysv_words(out, offset, field_ty, a);

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
    } else {
        // Larger than 8 words (eightbytes), therefore class is memory
        push_u8(SysVMemory, &out);
    }

    // TODO (FEATURE BUG): Add the other merge steps.
    for (size_t i = 0; i < out.len; i++) {
      if (out.data[i] == SysVMemory) {
          out.len = 0;
      };
    }

    return out;
}

#elif ABI == WIN_64 

typedef enum {
    Win64Floating,
    Win64Integer,
    Win64SmallAggregate, // <= 64 bits
    Win64LargeAggregate,
    Win64M128, // __m128
} Win64ArgClass;

Win64ArgClass win_64_arg_class(CType* type) {
    switch (type->sort) {
    case CSVoid:
        panic(mv_string("Void type does not have arg class"));
        break;
    case CSPrim:
        switch (type->prim.prim) {
        case CChar:
        case CShort:
        case CInt:
        case CLong:
        case CLongLong:
            return Win64Integer;
        }
        break;
    case CSPtr:
    case CSProc:
        return Win64Integer;
        break;
    case CSIncomplete:
        panic(mv_string("Incomplete type does not have arg class"));
        break;
    case CSStruct: 
    case CSUnion: {
        size_t aggregate_sz = c_size_of(*type);
        if (aggregate_sz <= 8) {
            return Win64SmallAggregate;
        } else {
            return Win64LargeAggregate;
        }
        break;
        case CSCEnum:
            return Win64Integer;
            break;
    }
    }
    panic(mv_string("Invalid c type provided to win_64_arg_class."));
}
#endif

void convert_c_fn(void* cfn, CType* ctype, PiType* ptype, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // This function is for converting a c function (assumed platform default
    // ABI) into a pico function. This means that it assumes that all arguments
    // are pushed on the stack in forward order (last at top).
    if (ctype->sort != CSProc || ptype->sort != TProc) {
        panic(mv_string("convert_c_fun requires types to be functions."));
    }
    if (ctype->proc.args.len != ptype->proc.args.len) {
        panic(mv_string("convert_c_fun requires functions to have same number of args."));
    }

    if (!can_reinterpret(ctype->proc.ret, ptype->proc.ret)) {
        // TODO (IMPROVEMENT): Move this check/assert to debug builds?
        panic(mv_string("Attempted to do invalid conversion of function return types!"));
    }

    // We need knowledge of the stack layout of the input arguments when
    // constructing the function call: We know that pico are pushed
    // left-to-right, meaning the last argument is on the bottom of the stack
    // (offset 0). 
    U64Array arg_offsets = mk_u64_array(ctype->proc.args.len + 1, a);
    uint64_t offset = ADDRESS_SIZE; // To account for return address
    for (size_t i = 0; i < ptype->proc.args.len; i++) {
        push_u64(offset, &arg_offsets); 
        size_t idx = ctype->proc.args.len - (i + 1);
        offset += pi_size_of(*(PiType*)ptype->proc.args.data[idx]);
    }
    push_u64(offset, &arg_offsets); 

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
    const Regname integer_registers[8] = {RDI, RSI, RDX, RCX, R8, R9};
    unsigned char current_integer_register = 0;

    U8Array return_classes = system_v_arg_classes(ctype->proc.ret, a);
    size_t return_arg_size = c_size_of(*ctype->proc.ret);

    // Check if return arg in memory, if so then the return arg takes the spot of
    // a 'hidden' extra argument - specifically, the first, with the ptr being
    // passed in RDI.
    bool pass_return_in_memory = (return_classes.len == 0) || (return_classes.len > 2);
    if (pass_return_in_memory) {
        input_area_size += return_arg_size;
        current_integer_register++;
    }

    for (size_t i = 0; i < ctype->proc.args.len; i++) {
        CType* c_arg = ctype->proc.args.data[i].val;
        PiType* p_arg = ptype->proc.args.data[i];
        if (!can_reinterpret(c_arg, p_arg)) {
            // TODO (IMPROVEMENT): Move this check/assert to debug builds?
            panic(mv_string("Attempted to do invalid conversion"));
        }
        
        // Get the classes associated with an argument.  
        // If there are multiple classes, each class in the array corresponds to an eightbyte of the argument.
        U8Array classes = system_v_arg_classes(c_arg, a);

        unsigned char saved_integer_register = current_integer_register;
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
                  if (arg_offsets.data[i] > 127) {
                      throw_error(point, mv_string("convert_c_fn: arg offset exeeds I8 max."));
                  }
                  // Explanation - copy into current register
                  // copy from RSP + current offset + return address offset + eightbyte_index
                  build_binary_op(ass, Mov, reg(next_reg, sz_64),
                                  rref8(RSP, arg_offsets.data[i] + 0x8 * j, sz_64),
                                  a, point);
              }
              break;
          }
          case SysVSSE:
              panic(mv_string("Not yet implemented: passing arg of class SSE"));
              break;
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
              pass_in_memory = true; // this breaks us out of the loop
              break;
          }
          if (pass_in_memory) break;
        }

        if (pass_in_memory) {
            push_u64(i, &in_memory_args);
            input_area_size += c_size_of(*c_arg);
        }

        sdelete_u8_array(classes);
    }

    // Use RBX as an indexing register, to point to the 'base'
    build_binary_op(ass, Mov, reg(RBX, sz_64), reg(RSP, sz_64), a, point);

    // The stack shall be aligned either on 16 bytes, (32 bytes if __m256 is used) or
    // 64 bytes (if __m512 is used). The alignment is to the 'end of input
    // area', i.e. the stack must be (16/32/64) - byte aligned immediately BEFORE the call.  
    // TODO (BUG LOGIC): Determine which stack alignment should be used
    const size_t expected_stack_align = 16;
    size_t needed_stack_offset = (input_area_size + 0x8) % expected_stack_align;
    needed_stack_offset = needed_stack_offset == 0 ? 0 : expected_stack_align - needed_stack_offset;

    // Check if the stack is aligned (and align if not). This is done as follows:
    // Check the offset of the stack (RSP)
    // If this is equal to the needed offset (input 
    build_binary_op(ass, Mov, reg(RAX, sz_64), reg(RSP, sz_64), a, point);
    build_binary_op(ass, Mov, reg(R10, sz_64), imm64(expected_stack_align), a, point);
    build_binary_op(ass, Mov, reg(R11, sz_64), imm64(needed_stack_offset), a, point);

    // The div stores the remainder in RDX, which has the 1st argument and so needs saving
    build_binary_op(ass, Mov, reg(R12, sz_64), reg(RDX, sz_64), a, point);
    build_nullary_op(ass, CQO, a, point);
    build_unary_op(ass, IDiv, reg(R10, sz_64), a, point);

    build_binary_op(ass, Sub, reg(R11, sz_64), reg(RDX, sz_64), a, point);
    build_binary_op(ass, Mov, reg(RDX, sz_64), reg(R11, sz_64), a, point);
    build_unary_op(ass, Neg, reg(R11, sz_64), a, point);

    // Was RDX < R10? 
    build_binary_op(ass, CMovL, reg(R11, sz_64), reg(RDX, sz_64), a, point);
    
    build_unary_op(ass, Push, reg(R11, sz_64), a, point);
    build_binary_op(ass, Sub, reg(RSP, sz_64), reg(R11, sz_64), a, point);

    // Restore value of RDX
    build_binary_op(ass, Mov, reg(RDX, sz_64), reg(R12, sz_64), a, point);

    for (size_t i = 0; i < in_memory_args.len; i++) {
        // pass in memory - reserve space on stack:
        size_t arg_idx = in_memory_args.data[i];
        size_t arg_size = arg_offsets.data[arg_idx + 1] - arg_offsets.data[arg_idx];
        build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(arg_size), a, point);
        build_binary_op(ass, Mov, reg(R10, sz_64), reg(RBX, sz_64), a, point);
        build_binary_op(ass, Add, reg(R10, sz_64), imm32(arg_offsets.data[arg_idx]), a, point);
        generate_monomorphic_copy(RSP, R10, arg_size, ass, a, point);
    }

    if (pass_return_in_memory) {
        // pass in memory - reserve space on stack:
        build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(return_arg_size), a, point);
        build_binary_op(ass, Mov, reg(integer_registers[0], sz_64), reg(RSP, sz_64), a, point);
    }

    // Call function
    build_binary_op(ass, Mov, reg(RBX, sz_64), imm64((uint64_t)cfn), a, point);
    build_unary_op(ass, Call, reg(RBX, sz_64), a, point);

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
        size_t unadjusted_offset = 0x8 + input_area_size + arg_offsets.data[arg_offsets.len -  1] - return_arg_size;

        // in RBX, store the stack alignment adjust (i.e. the size of the align padding)
        build_binary_op(ass, Mov, reg(RBX, sz_64), rref8(RSP, input_area_size, sz_64), a, point);

        // Use to to calculate the location of the return address (store in RCX)
        build_binary_op(ass, Mov, reg(RCX, sz_64), reg(RSP, sz_64), a, point);
        build_binary_op(ass, Add, reg(RCX, sz_64), imm8(input_area_size), a, point);
        build_binary_op(ass, Add, reg(RCX, sz_64), reg(RBX, sz_64), a, point); // align adjust
        // RCX = ret_addr = [RCX + sizeof(align adjust) = 0x8]
        build_binary_op(ass, Mov, reg(RCX, sz_64), rref8(RCX, 0x8, sz_64), a, point);  

        // To copy the value, first store target address (END of where return value is copied) in RDX 
        build_binary_op(ass, Mov, reg(RDX, sz_64), reg(RSP, sz_64), a, point);
        build_binary_op(ass, Add, reg(RDX, sz_64), imm32(unadjusted_offset), a, point);
        build_binary_op(ass, Add, reg(RDX, sz_64), reg(RBX, sz_64), a, point);

        // Now, generate a stack copy targeting RDX
        generate_stack_copy(RDX, return_arg_size, ass, a, point);

        // Then, pop all memory (sans the return arg) from the stack.
        build_binary_op(ass, Add, reg(RSP, sz_64), imm32(input_area_size), a, point);
        build_binary_op(ass, Add, reg(RSP, sz_64), reg(RBX, sz_64), a, point);
        build_binary_op(ass, Add, reg(RSP, sz_64), imm32(0x8 + arg_offsets.data[arg_offsets.len -  1] - return_arg_size), a, point);

        // Push return address
        build_unary_op(ass, Push, reg(RCX, sz_64), a, point);
    } else {
        // Pop all memory arguments (both pico and c)
        build_binary_op(ass, Add, reg(RSP, sz_64), imm32(input_area_size), a, point);
        build_binary_op(ass, Add, reg(RSP, sz_64), rref8(RSP, 0, sz_64), a, point);
        build_binary_op(ass, Add, reg(RSP, sz_64), imm32(0x8 + arg_offsets.data[arg_offsets.len -  1]), a, point);

        // Now, push registers onto stack
        size_t current_return_register = 0;
        const Regname return_integer_registers[2] = {RAX, RDX};
        size_t assembler_pos = get_pos(ass);
        for (size_t i = 0; i < return_classes.len; i++) {
            switch (return_classes.data[i]) {
            case SysVInteger: {
                if (current_return_register >= 2) {
                    set_pos(ass, assembler_pos);
                    pass_return_in_memory = true; // this breaks us out of the loop
                } else {
                    Regname next_reg = return_integer_registers[current_return_register++];
                    // I8 max = 127
                    if (arg_offsets.data[i] > 127) {
                        throw_error(point, mv_string("convert_c_fn: arg offset exeeds I8 max."));
                    }
                    // Push from registers into memory
                    build_unary_op(ass, Push, reg(next_reg, sz_64), a, point);
                }
                break;
            }
            case SysVSSE:
                panic(mv_string("Not yet implemented: passing arg of class SSE"));
                break;
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
            if (pass_return_in_memory) break;
        }
    }
    build_nullary_op(ass, Ret, a, point);

    sdelete_u8_array(return_classes);
    sdelete_u64_array(in_memory_args);
        

#elif ABI == WIN_64
    const Regname integer_registers[4] = {RCX, RDX, R8, R9};
    size_t current_register = 0;

    const Win64ArgClass return_class = win_64_arg_class(ctype->proc.ret);
    const bool pass_in_memory = return_class == Win64LargeAggregate || return_class == Win64LargeAggregate;
    const size_t return_arg_size = c_size_of(*(CType*)ctype->proc.ret);
    size_t input_area_size = 0;

    // Calculate input area size:
    if (pass_in_memory) {
        input_area_size += return_arg_size;
    }
    for (size_t i = 0; i < ctype->proc.args.len; i++) {
      if (i < 4) {
          Win64ArgClass class = win_64_arg_class(ctype->proc.args.data[i].val);
          if (class == Win64LargeAggregate || class == Win64LargeAggregate) {
              input_area_size += c_size_of(*(CType*)ctype->proc.args.data[i].val);
          }
      } else {
          input_area_size += c_size_of(*(CType*)ctype->proc.args.data[i].val);
      }
    }

    // Use RBX as an indexing register, to point to the 'base' (before runtime offsets are added)
    build_binary_op(ass, Mov, reg(RBX, sz_64), reg(RSP, sz_64), a, point);

    const size_t expected_stack_align = 16;
    size_t needed_stack_offset = (input_area_size + 0x8) % expected_stack_align;
    needed_stack_offset = needed_stack_offset == 0 ? 0 : expected_stack_align - needed_stack_offset;
    // Check if the stack is aligned (and align if not). This is done as follows:
    // Check the offset of the stack (RSP)
    // If this is equal to the needed offset (input 
    build_binary_op(ass, Mov, reg(RAX, sz_64), reg(RSP, sz_64), a, point);
    build_binary_op(ass, Mov, reg(R10, sz_64), imm64(expected_stack_align), a, point);
    build_binary_op(ass, Mov, reg(R11, sz_64), imm64(needed_stack_offset), a, point);

    build_nullary_op(ass, CQO, a, point);
    build_unary_op(ass, IDiv, reg(R10, sz_64), a, point);

    build_binary_op(ass, Sub, reg(R11, sz_64), reg(RDX, sz_64), a, point);
    build_binary_op(ass, Mov, reg(RDX, sz_64), reg(R11, sz_64), a, point);
    build_unary_op(ass, Neg, reg(R11, sz_64), a, point);

    // Was RDX < R10? 
    build_binary_op(ass, CMovL, reg(R11, sz_64), reg(RDX, sz_64), a, point);
    
    build_unary_op(ass, Push, reg(R11, sz_64), a, point);
    build_binary_op(ass, Sub, reg(RSP, sz_64), reg(R11, sz_64), a, point);

    // Check for return arg/space
    if (pass_in_memory) {
        Regname next_reg = integer_registers[current_register++];
        build_binary_op(ass, Sub, reg(RSP, sz_64), imm8(c_size_of(*ctype->proc.ret)), a, point);
        build_binary_op(ass, Mov, reg(next_reg, sz_64), reg(RSP, sz_64), a, point);
    }

    // Note: for Win 64 ABI, arguments are push left-to-right, meaning the
    // rightmost argument is at the bottom of the stack. 
    for (size_t i = 0; i < ctype->proc.args.len; i++) {
      if (i < 4) {
          Win64ArgClass class = win_64_arg_class(ctype->proc.args.data[i].val);
          switch (class) {
          case Win64Floating:
              throw_error(point, mv_string("Not implemented: register arg of class Win64 float "));
          case Win64Integer:
          case Win64SmallAggregate: {
              Regname next_reg = integer_registers[current_register++];
              build_binary_op(ass, Mov, reg(next_reg, sz_64),
                              rref8(RBX, arg_offsets.data[i], sz_64),
                              a, point);
              break;
          }
          case Win64LargeAggregate: {
              Regname next_reg = integer_registers[current_register++];
              // For windows, arguments are already on the stack (?), so just point them to the correct offset!
              build_binary_op(ass, Mov, reg(next_reg, sz_64), reg(RBX, sz_64), a, point);
              build_binary_op(ass, Add, reg(next_reg, sz_64), imm8(arg_offsets.data[i]), a, point);

              // TODO (IMPROVEMENT) what if different sizes?
              size_t arg_size = arg_offsets.data[i + 1] - arg_offsets.data[i];
              build_binary_op(ass, Sub, reg(RSP, sz_64), imm8(arg_size), a, point);
              generate_monomorphic_copy(RSP, next_reg, arg_size, ass, a, point);
              build_binary_op(ass, Mov, reg(next_reg, sz_64), reg(RSP, sz_64), a, point);
              break;
          }
          case Win64M128: {
              throw_error(point, mv_string("Not implemented: register arg of class Win64 __m128 "));
          }
          }
      } else {
          Regname next_reg = integer_registers[current_register++];
          build_binary_op(ass, Mov, reg(next_reg, sz_64), reg(RBX, sz_64), a, point);
          build_binary_op(ass, Add, reg(next_reg, sz_64), imm8(arg_offsets.data[i]), a, point);
      }
    }
    
    // Call function
    build_binary_op(ass, Mov, reg(RBX, sz_64), imm64((uint64_t)cfn), a, point);
    build_unary_op(ass, Call, reg(RBX, sz_64), a, point);

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
        size_t unadjusted_offset = 0x8 + input_area_size + arg_offsets.data[arg_offsets.len -  1] - return_arg_size;

        // in RBX, store the stack alignment adjust (i.e. the size of the align padding)
        build_binary_op(ass, Mov, reg(RBX, sz_64), rref8(RSP, input_area_size, sz_64), a, point);

        // Use to to calculate the location of the return address (store in RCX)
        build_binary_op(ass, Mov, reg(RCX, sz_64), reg(RSP, sz_64), a, point);
        build_binary_op(ass, Add, reg(RCX, sz_64), imm8(input_area_size), a, point);
        build_binary_op(ass, Add, reg(RCX, sz_64), reg(RBX, sz_64), a, point); // align adjust
        // RCX = ret_addr = [RCX + sizeof(align adjust) = 0x8]
        build_binary_op(ass, Mov, reg(RCX, sz_64), rref8(RCX, 0x8, sz_64), a, point);  

        // To copy the value, first store target address (END of where return value is copied) in RDX 
        build_binary_op(ass, Mov, reg(RDX, sz_64), reg(RSP, sz_64), a, point);
        build_binary_op(ass, Add, reg(RDX, sz_64), imm32(unadjusted_offset), a, point);
        build_binary_op(ass, Add, reg(RDX, sz_64), reg(RBX, sz_64), a, point);

        // Now, generate a stack copy targeting RDX
        generate_stack_copy(RDX, return_arg_size, ass, a, point);

        // Then, pop all memory (sans the return arg) from the stack.
        build_binary_op(ass, Add, reg(RSP, sz_64), imm32(input_area_size), a, point);
        build_binary_op(ass, Add, reg(RSP, sz_64), reg(RBX, sz_64), a, point);
        build_binary_op(ass, Add, reg(RSP, sz_64), imm32(0x8 + arg_offsets.data[arg_offsets.len -  1] - return_arg_size), a, point);

        // Push return address
        build_unary_op(ass, Push, reg(RCX, sz_64), a, point);
    } else {
        // Pop all memory arguments (both pico and c)
        build_binary_op(ass, Add, reg(RSP, sz_64), imm32(input_area_size), a, point);
        build_binary_op(ass, Add, reg(RSP, sz_64), rref8(RSP, 0, sz_64), a, point);
        build_binary_op(ass, Add, reg(RSP, sz_64), imm32(0x8 + arg_offsets.data[arg_offsets.len -  1]), a, point);

        // Pop return address
        build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);

        // Now, registers onto stack
        build_unary_op(ass, Push, reg(RAX, sz_64), a, point);

        // Push return address
        build_unary_op(ass, Push, reg(RCX, sz_64), a, point);

    }
    build_nullary_op(ass, Ret, a, point);

#else
#error "convert_c_fun not implemented for unknonw arch"
#endif

    sdelete_u64_array(arg_offsets);
}

bool can_convert(CType *ctype, PiType *ptype) {
    if (ctype->sort == CSProc && ptype->sort == TProc) {
        if (ctype->proc.args.len != ptype->proc.args.len) {
            return false;
        }

        for (size_t i = 0; i < ctype->proc.args.len; i++) {
            if (!can_reinterpret(ctype->proc.args.data[i].val, ptype->proc.args.data[i])) {
                return false;
            }
        }
        if (!can_reinterpret(ctype->proc.ret, ptype->proc.ret)) {
            return false;
        }

        return true;
    }

    return can_reinterpret(ctype, ptype);
}

bool can_reinterpret_prim(CPrim ctype, PrimType ptype) {
#if (ABI == SYSTEM_V_64) || (ABI == WIN_64)
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
        return ctype.prim == CLong &&
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
        return ctype.prim == CLong && ctype.is_signed == Unsigned; 
    }
    case UInt_32: {
        return ctype.prim == CInt && ctype.is_signed == Unsigned; 
    }
    case UInt_16: {
        return ctype.prim == CInt && ctype.is_signed == Unsigned;
    }
    case UInt_8: {
        return ctype.prim == CChar && ctype.is_signed == Unsigned;
    }
    case TFormer:  {
        // TODO (FEATURE): check for enum?
        return false;
    }
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

bool can_reinterpret(CType* ctype, PiType* ptype) {
    // TODO (BUG/FEATURE): algorithm should depend on

    // C doesn't have a concept of distinct types, so filter those out. 
    // TODO (BUG LOGIC): possibly don't allow opaque to be converted unless
    // TODO (FEATURE): check for well-formedness of types in debug mode?
    while (ptype->sort == TDistinct) ptype = ptype->distinct.type;

    switch (ptype->sort) {
    case TPrim: {
        if (ctype->sort == CSPrim) {
            return can_reinterpret_prim(ctype->prim, ptype->prim);
        }
        else if (ctype->sort == CSPtr && ptype->prim == Address) {
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
            if (!can_reinterpret(ctype->proc.args.data[i].val, ptype->proc.args.data[i]))
                return false;
        }
        return can_reinterpret(ctype->proc.ret, ptype->proc.ret);
    }
    case TStruct: {
        if (ptype->structure.fields.len != ctype->structure.fields.len) {
            return false;
        }

        for (size_t i = 0; i < ptype->structure.fields.len; i++) {
          if (!can_reinterpret(ctype->structure.fields.data[i].val,
                               ptype->structure.fields.data[i].val)) {
              return false;
          }
        }
        return true;
    }
    case TEnum: {
        // TODO: compare 0-member enums with actual c enums/ints.
        if (ctype->sort != CSStruct) return false;
        if (ctype->structure.fields.len != 2) return false;

        // check that the 0th struct field is reinterpretable as a 64-bit int
        // TODO (FEATURE): change tag size based on number of enum vals 
        PiType tag_type = (PiType) { .sort = TPrim, .prim = UInt_64 };
        if (!can_reinterpret(ctype->structure.fields.data[0].val, &tag_type)) return false;
            
        CType* cunion = ctype->structure.fields.data[1].val;
        if (cunion->sort != CSUnion || cunion->cunion.fields.len != ptype->enumeration.variants.len) return false;
        // TODO (FEATURE): add ability for c type to not need union/struct if
        // variants empty.

        for (size_t i = 0; i < cunion->cunion.fields.len; i++) {
            PtrArray* variant = ptype->enumeration.variants.data[i].val;
            // TODO: allow if union type is struct!
            if (variant->len != 1) return false;

            if (!can_reinterpret(cunion->cunion.fields.data[i].val, variant->data[0]))
                return false;
        }
        return true;
    }


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
    case TExists:
    case TCApp:
    case TFam:
    case TKind:
    case TConstraint:
    case TUVar:
    case TUVarDefaulted:
        return false;
    default:
        panic(mv_string("invalid types provided to can_reinterpret"));
    }
}
