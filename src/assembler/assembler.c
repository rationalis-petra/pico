#include <inttypes.h>
#include <stdio.h>

#include "assembler/assembler.h"
#include "data/binary.h"
#include "data/array.h"

/* Personal Notes/hints
 * 
 * Instruction Format
 *
 * | Prefixes    | Opcode    | Mod R/M | SIB     | Displacement | Immediate
 * | 1 byte each | 1-3 bytes | ?1 byte | ?1 byte | 1,2,4 bytes  | 1,2,4 bytes
 * 
 *   Mod R/M                     SIB
 * 7    6 5      3 2    0     7      6 5     3 2     0
 * | Mod | Reg/Op | R/M |     | Scale | Index | Base |
 *
 * REX Prefixes
 * | Field Name | Bit Position | Definition
 * | -          | 7:4          | 0100
 * | W          | 3            | 1 = 64-bit
 * |            |              | 0 = operand size determined by CS.D
 * | R          | 2            | Extension of the ModR/M reg field
 * | X          | 1            | Etension of the SIB index field
 * | B          | 0            | Extension of any of ModR/M,SIB or Opcode Reg
 * 
 */ 

struct assembler {
    u8_array instructions;
    allocator allocator;
};

void clear_assembler(assembler* assembler) {
    assembler->instructions.len = 0;
}
                                            
assembler* mk_assembler(allocator a) {
    assembler* out = (assembler*)mem_alloc(sizeof(assembler), a);
    out->instructions = mk_u8_array(1024, a);
    out->allocator = a;
    return out;
}

u8_array get_instructions(assembler* ass) {
    return ass->instructions;
}

size_t get_pos(assembler* ass) {
    return ass->instructions.len;
}

void delete_assembler (assembler* ass) {
    sdelete_u8_array(ass->instructions, ass->allocator);
    mem_free(ass, ass->allocator);
}

document* pretty_assembler(assembler* assembler, allocator a) {
    ptr_array nodes = mk_ptr_array(4 + assembler->instructions.len, a);

    for (size_t i = 0; i < assembler->instructions.len; i++) {
        int len = snprintf(NULL, 0, "%02x", assembler->instructions.data[i]) + 1;
        char* str = (char*)mem_alloc(sizeof(char) * len, a);
        snprintf(str, len, "%02" PRIx8, assembler->instructions.data[i]);
        document* arg = mv_str_doc(mv_string(str), a);

        push_ptr(arg, &nodes, a);
    }

    return mv_sep_doc(nodes, a);
}



location reg(regname reg) {
    location out;
    out.type = Register;
    out.reg = reg;
    return out;
}

location rref(regname name, int8_t offset) {
    location out;
    out.type = Deref;
    out.reg = name;
    out.immediate_32 = offset;
    return out;
}

location imm8(int8_t immediate) {
    location out;
    out.type = Immediate8;
    out.immediate_8 = immediate;
    return out;
}
location imm16(int16_t immediate) {
    location out;
    out.type = Immediate16;
    out.immediate_16 = immediate;
    return out;
}

location imm32(int32_t immediate) {
    location out;
    out.type = Immediate32;
    out.immediate_32 = immediate;
    return out;
}

location imm64(int64_t immediate) {
    location out;
    out.type = Immediate64;
    out.immediate_64 = immediate;
    return out;
}


string nullary_op_name(nullary_op op) {
    switch (op) {
    case Ret:
        return mv_string("Ret");
    default: 
        return mv_string("unknown-nullary-op");
    }
}

string unary_op_name(unary_op op) {
    switch (op) {
    case Call:
        return mv_string("Call");
    case Push:
        return mv_string("Push");
    case Pop: 
        return mv_string("Pop");
    case JE:
        return mv_string("JE");
    case JNE:
        return mv_string("JNE");
    case JMP:
        return mv_string("JMP");
    case SetE:
        return mv_string("SetE");
    case SetL:
        return mv_string("SetL");
    case SetG:
        return mv_string("SetG");
    default: 
        return mv_string("unknown-unary-op");
    }
}

string binary_op_name(binary_op op) {
    switch (op) {
    case Add:
        return mv_string("Add");
    case Sub:
        return mv_string("Sub");
    case Cmp:
        return mv_string("Cmp");
    case And:
        return mv_string("And");
    case Or:
        return mv_string("Or");
    case LShift:
        return mv_string("LShift");
    case RShift:
        return mv_string("RShift");
    case Mov:
        return mv_string("Mov");
    default: 
        return mv_string("unknown-binary-op");
    }
}

//------------------------------------------------------------------------------
// Assembly utilities/implementation
//------------------------------------------------------------------------------

// Return: the ModR/M byte for a single register src/dest
uint8_t modrm_reg(regname r2) {
    return 0b11010000 | (r2 & 0b111); 
}

// encode register in r/m field;
void modrm_reg_rm(uint8_t* modrm_byte, regname reg) {
    *modrm_byte |= reg & 0b111;
}

void modrm_reg_rm_rex(uint8_t* modrm_byte, uint8_t* rex_byte,regname reg) {
    if (reg & 010) set_bit(rex_byte, 2);
    *modrm_byte |= reg & 0b111;
}

// encode mod 
void modrm_reg_mod(uint8_t* modrm_byte, uint8_t mod) {
    *modrm_byte |= (mod & 0b11) << 6;
}

// See: page 112 of Intel Vol 2.
uint8_t modrm_reg_imm(regname r2) {
    return 0b11000000 | (r2 & 0b111); 
}


// Return: the ModR/M byte for a register deref
uint8_t modrm_mem(location mem) {
    return 0b01110000 + (mem.reg & 0b111); 
}

uint8_t encode_reg_reg(regname r1, regname r2, uint8_t* rex_byte) {
    // R1 (dest) is encoded in ModR/M Reg + REX.R
    // R2 (src)  is encoded in ModR/M R/M + REX.B

    // set REX.R, Rex.B if needed 
    if (r1 & 010) set_bit(rex_byte, 0);
    if (r2 & 010) set_bit(rex_byte, 2);

    return 0b11000000 | ((r2 & 0b111 ) << 3) | (r1 & 0b111); 
}

uint8_t encode_reg_mem(regname r1, location mem, uint8_t* rex_byte) {
    // R1 (dest) is encoded in ModR/M Reg + REX.R
    // R2 (src)  is encoded in ModR/M R/M + REX.B

    // set REX.R, Rex.B if needed 
    if (r1 & 010) set_bit(rex_byte, 0);
    if (mem.reg & 010) set_bit(rex_byte, 2);

    return 0b00000000 | ((mem.reg & 0b111 ) << 3) | (r1 & 0b111); 
}

void reg_in_opcode(regname r, uint8_t* opcode_byte, uint8_t* rex_byte) {
    // set REX.B if needed
    if (r & 010) set_bit(rex_byte, 0);
    // store regname in opcode
    *opcode_byte += r & 0b111;
}

void opcode_in_reg(uint8_t* modrm_byte, uint8_t opcode) {
    // store opcode extension in reg bits
    *modrm_byte |= (opcode & 0b111) << 3;
}

asm_result build_binary_op(assembler* assembler, binary_op op, location dest, location src, allocator err_allocator) {
    allocator a = assembler->allocator;
    // Most paths are successful, so default assume the operation succeeded.
    asm_result out;
    out.type = Ok;

    // Note: the logic is simplified for now as we assume 32-bit immediates and
    // 64-bit registers. This means we always use REX byte :) 
    bool use_rex = true;
    uint8_t rex_byte = 0b01000000; // default: W,R,X,B = 0

    uint8_t opcode;

    bool use_mod_rm_byte = false;
    uint8_t mod_rm_byte;

    uint8_t num_immediate_bytes = 0;
    uint8_t immediate_bytes[8];

    // Switch based on the location type.
    switch (dest.type) {
    case Register:
        switch (src.type) {
        case Register:
            use_mod_rm_byte = true;
            mod_rm_byte = encode_reg_reg(src.reg, dest.reg, &rex_byte);
            set_bit(&rex_byte, 3); // Set REX.W
            switch (op) {
            case Add: opcode = 0x03; break;
            case Sub: opcode = 0x2B; break;
            case And: opcode = 0x23; break;
            case Or:  opcode = 0x0B; break;
            case Cmp: opcode = 0x3B; break;

            case Mov:  opcode = 0x8B; break;
            default:
                out.type = Err;
                out.error_message = mk_string("Reg/Reg for this Op is not implemented", err_allocator);
            }
            break;
            
        case Deref:
            use_mod_rm_byte = true;
            mod_rm_byte = encode_reg_mem(dest.reg, src, &rex_byte);
            set_bit(&rex_byte, 3); // Set REX.W
            switch (op)  {
            case Mov:
                opcode =  0x8B; 
                break;
            default:
                out.type = Err;
                out.error_message = mk_string("Register/Deref not implemented", err_allocator);
            }
            break;
        case Immediate8:
        case Immediate16:
            out.type = Err;
            out.error_message = mk_string("Immediate 16/32 not implemented", err_allocator);
            break;

        case Immediate32:
            switch (op) {
            case Add: {
                // TODO: opcode + rd(w)
                // TODO: + rd io
                opcode = 0x81;

                set_bit(&rex_byte, 3); // Set REX.W
                use_mod_rm_byte = true;
                mod_rm_byte = modrm_reg_imm(dest.reg);  // /0 id

                num_immediate_bytes = 4;
                uint8_t* bytes = (uint8_t*) &src.immediate_32;
                for (uint8_t i = 0; i < num_immediate_bytes; i++) {
                    immediate_bytes[i] = bytes[i];
                }
                break;
            }
            case Sub: {
                // TODO: opcode + rd(w)
                // TODO: + rd io
                opcode = 0x81;

                set_bit(&rex_byte, 3); // Set REX.W
                use_mod_rm_byte = true;
                mod_rm_byte = modrm_reg_imm(dest.reg);  // /0 id

                num_immediate_bytes = 4;
                uint8_t* bytes = (uint8_t*) &src.immediate_32;
                for (uint8_t i = 0; i < num_immediate_bytes; i++) {
                    immediate_bytes[i] = bytes[i];
                }
                break;
            }
            case Mov: {
                // TODO: opcode + rd(w)
                // TODO: + rd io
                opcode = 0x81;
                set_bit(&rex_byte, 3); // Set REX.W
                reg_in_opcode(dest.reg, &opcode, &rex_byte);

                num_immediate_bytes = 4;
                uint8_t* bytes = (uint8_t*) &src.immediate_32;
                for (uint8_t i = 0; i < num_immediate_bytes; i++) {
                    immediate_bytes[i] = bytes[i];
                }
                break;
            }
            case Cmp: {
                // encode 
                use_rex = true;
                use_mod_rm_byte = true;

                opcode = 0x81;
                set_bit(&rex_byte, 3); // Set REX.W
                opcode_in_reg(&mod_rm_byte, 7);
                modrm_reg_rm(&mod_rm_byte, dest.reg);
                // Why 01??
                modrm_reg_mod(&mod_rm_byte, 0b11);

                num_immediate_bytes = 4;
                uint8_t* bytes = (uint8_t*) &src.immediate_32;
                for (uint8_t i = 0; i < num_immediate_bytes; i++) {
                    immediate_bytes[i] = bytes[i];
                }
                break;
            }
            default: {
                out.type = Err;
                out.error_message = string_cat(mv_string("This operand does not support 32-bit immediates: "),
                                               binary_op_name(op),
                                               err_allocator);
            }
            }
            break;
            
            case Immediate64:
                switch (op) {
                case Mov:
                    opcode = 0xB8;
                    set_bit(&rex_byte, 3); // Set REX.W
                    reg_in_opcode(dest.reg, &opcode, &rex_byte);

                    num_immediate_bytes = 8;
                    uint8_t* bytes = (uint8_t*) &src.immediate_64;
                    for (uint8_t i = 0; i < num_immediate_bytes; i++) {
                        immediate_bytes[i] = bytes[i];
                    }
                    break;
                    break;
                default:
                    out.type = Err;
                    out.error_message = mk_string("This operand does not support 64-bit immmediates", err_allocator);
                }
            }
            break;

    case Deref:
        switch (src.type) {
        case Register:
            out.type = Err;
            out.error_message = mk_string("Deref/register pair not implemented", err_allocator);
            break;
            
        case Deref:
            out.type = Err;
            out.error_message = mk_string("Cannot use two dereferences as a source/destination pair", err_allocator);
            break;

        case Immediate8:
        case Immediate16:
        case Immediate32:
        case Immediate64:
            out.type = Err;
            out.error_message = mk_string("Deref/Immediate not implemented", err_allocator);
        }
        break;

    case Immediate8:
    case Immediate16:
    case Immediate32:
    case Immediate64:
        out.type = Err;
        out.error_message = mk_string("Cannot use an immediate as a destination register!", err_allocator);
        break;
    }
    if (out.type == Err) return out;


    u8_array* instructions = &assembler->instructions;
    // Finally, write out the opcode into the assembler
    if (use_rex) push_u8(rex_byte, instructions, a);
    push_u8(opcode, instructions, a);
    if (use_mod_rm_byte) push_u8(mod_rm_byte, instructions, a);

    if (num_immediate_bytes != 0)
        out.backlink = instructions->len;
    for (uint8_t i = 0; i < num_immediate_bytes; i++) {
        push_u8(immediate_bytes[i], &assembler->instructions, a);
    }

    return out;
}

asm_result build_unary_op(assembler* assembler, unary_op op, location loc, allocator err_allocator) {
    allocator a = assembler->allocator;
    asm_result out;
    out.type = Ok;
    bool use_rex_byte = false;
    uint8_t rex_byte = 0b01000000;

    bool use_prefix_byte = false;
    uint8_t prefix_byte;

    bool use_mod_rm_byte = false;
    uint8_t mod_rm_byte = 0;

    uint8_t num_immediate_bytes = 0;
    uint8_t immediate_bytes[4];
     
    if (loc.type == Register && (loc.reg & 0b1000)) {
        use_prefix_byte = true;
        prefix_byte = 0x41;
    }

    uint8_t opcode;
    switch (op) {
    case Call:
        switch (loc.type) {
        case Register:
            opcode = 0xff;
            use_mod_rm_byte = true;
            mod_rm_byte = modrm_reg(loc.reg);
            break;
        default:
            out.type = Err;
            out.error_message = mk_string("Push for non register locations not implemented", err_allocator);
        }
        break;
    case Pop:
        switch (loc.type) {
        case Register:
            opcode = 0x58 + (loc.reg & 0b111);
            break;
        default:
            out.type = Err;
            out.error_message = mk_string("Pop for non register locations not implemented", err_allocator);
            break;
        }
        break;
    case Push:
        switch (loc.type) {
        case Register:
            opcode = 0x50 + (loc.reg & 0b111);
            break;
        case Deref:
            // use r/m encoding: ff /6 Modr/m(r)
            opcode = 0xff;
            use_mod_rm_byte = true;
            mod_rm_byte = modrm_mem(loc);
            num_immediate_bytes = 1;
            immediate_bytes[0] = loc.immediate_32;
            break;
        case Immediate32: {
            // TODO: optionally shrink immediate
            uint8_t* bytes = (uint8_t*) &loc.immediate_32;
            if (loc.immediate_32 <= 256) {
                opcode = 0x6A;
                num_immediate_bytes = 1;
            } else if (loc.immediate_32 <= 256 * 256) {
                opcode = 0x68;
                num_immediate_bytes = 2;
            } else {
                opcode = 0x68;
                num_immediate_bytes = 4;
            }
            for (uint8_t i = 0; i < num_immediate_bytes; i++) {
                immediate_bytes[i] = bytes[i];
            }
            break;
        }
        default:
            out.type = Err;
            out.error_message = mk_string("Push for register dereference not implemented", err_allocator);
        }
        break;
        // conditional jumps
    case JE:
        opcode = 0x74;
        if (loc.type != Immediate8) {
            out.type = Err;
            out.error_message = mk_string("JE requires 8-bit immediate", err_allocator);
        }
        num_immediate_bytes = 1;
        immediate_bytes[0] = loc.immediate_8;
        break;
    case JNE:
        opcode = 0x75;
        if (loc.type != Immediate8) {
            out.type = Err;
            out.error_message = mk_string("JNE requires 8-bit immediate", err_allocator);
        }
        num_immediate_bytes = 1;
        immediate_bytes[0] = loc.immediate_8;
        break;

    case JMP:
        opcode = 0xEB;
        switch (loc.type) {
        case Immediate8:
            num_immediate_bytes = 1;
            immediate_bytes[0] = loc.immediate_8;
            break;
        default:

            out.type = Err;
            out.error_message = mk_string("JMP requires 8-bit immediate", err_allocator);
            return out;
        }
        break;

    case SetE:
        //use_rex_byte = true;
        use_prefix_byte = true;
        use_mod_rm_byte = true;

        mod_rm_byte |= 0b11000000;
        prefix_byte = 0x0f;
        opcode = 0x94;
        switch (loc.type) {
        case Register:
            modrm_reg_rm_rex(&mod_rm_byte, &rex_byte, loc.reg);
            break;
        default:

            out.type = Err;
            out.error_message = mk_string("SetE requires a register argument", err_allocator);
            return out;
        }
        break;
    case SetL:
        //use_rex_byte = true;
        use_prefix_byte = true;
        use_mod_rm_byte = true;

        mod_rm_byte |= 0b11000000;
        prefix_byte = 0x0f;
        opcode = 0x9C;
        switch (loc.type) {
        case Register:
            modrm_reg_rm_rex(&mod_rm_byte, &rex_byte, loc.reg);
            break;
        default:

            out.type = Err;
            out.error_message = mk_string("SetE requires a register argument", err_allocator);
            return out;
        }
        break;
    case SetG:
        //use_rex_byte = true;
        use_prefix_byte = true;
        use_mod_rm_byte = true;

        mod_rm_byte |= 0b11000000;
        prefix_byte = 0x0f;
        opcode = 0x9F;
        switch (loc.type) {
        case Register:
            modrm_reg_rm_rex(&mod_rm_byte, &rex_byte, loc.reg);
            break;
        default:

            out.type = Err;
            out.error_message = mk_string("SetE requires a register argument", err_allocator);
            return out;
        }
        break;
    }
    if (out.type == Err) return out;

    u8_array* instructions = &assembler->instructions;
    if (use_rex_byte) {
        push_u8(rex_byte, instructions, a);
    }
    if (use_prefix_byte) {
        push_u8(prefix_byte, instructions, a);
    }
    push_u8(opcode, instructions, a);
    if (use_mod_rm_byte) {
        push_u8(mod_rm_byte,instructions, a);
    }
    if (num_immediate_bytes != 0)
        out.backlink = instructions->len;
    for (uint8_t i = 0; i < num_immediate_bytes; i++) {
        push_u8(immediate_bytes[i], instructions, a);
    }
    return out;
}

asm_result build_nullary_op(assembler* assembler, nullary_op op, allocator err_allocator) {
    allocator a = assembler->allocator;
    asm_result out;
    out.type = Ok;

    uint8_t opcode;
    switch (op) {
    case Ret:
        opcode = 0xC3;
        break;
    }
    u8_array* instructions = &assembler->instructions;
    push_u8(opcode, instructions, a);
    return out;
}
