#include "assembler/assembler.h"
#include "data/binary.h"

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

assembler* mk_assembler(allocator a) {
    assembler* out = (assembler*)mem_alloc(sizeof(assembler), a);
    *out = mk_u8_array(256, a);
    return out;
}

location reg(regname reg) {
    location out;
    out.type = Register;
    out.reg = reg;
    return out;
}

location rref(regname name, uint8_t offset) {
    location out;
    out.type = Deref;
    out.immediate = offset;
    return out;
}

location imm32(uint32_t immediate) {
    location out;
    out.type = Immediate;
    out.immediate = immediate;
    return out;
}

// Return: the ModR/M byte
uint8_t encode_reg_reg(regname r1, regname r2, uint8_t* rex_byte) {
    // R1 (dest) is encoded in ModR/M Reg + REX.R
    // R2 (src)  is encoded in ModR/M R/M + REX.B

    // set REX.R, Rex.B if needed 
    if (r1 & 010) set_bit(rex_byte, 0);
    if (r2 & 010) set_bit(rex_byte, 2);

    return 0b11000000 | ((r2 & 0b111 ) << 3) | (r1 & 0b111); 
}

asm_result build_unary_op(assembler* assembler, unary_op op, location loc, allocator a) {
    asm_result out;
    out.succ = true;
    bool use_prefix_byte = false;
    uint8_t prefix_byte;

    uint8_t opcode;
    switch (op) {
    case Pop:
        switch (loc.type) {
        case Register:
            if (loc.reg & 0b1000) {
                use_prefix_byte = true;
                prefix_byte = 0x41;
            }
            opcode = 0x58 + (loc.reg & 0b111);
            break;
        default:
            out.succ = false;
            out.msg = mk_string("Pop for non register locations not implemented", a);
            break;
        }
        break;
    case Push:
        switch (loc.type) {
        case Register:
            if (loc.reg & 0b1000) {
                use_prefix_byte = true;
                prefix_byte = 0x41;
            }
            opcode = 0x50 + (loc.reg & 0b111);
            break;
        default:
            out.succ = false;
            out.msg = mk_string("Push for non register locations not implemented", a);
        }
        break;
    }

    if (use_prefix_byte) {
        push_u8(prefix_byte, assembler, a);
    }
    push_u8(opcode, assembler, a);
    return out;
}

asm_result build_binary_op(assembler* assembler, binary_op op, location dest, location src, allocator a) {
    // Most paths are successful, so default assume the operation succeeded.
    asm_result out;
    out.succ = true;

    // For addition spec - p
    // For subtract spec - p1407 Vol 2 Intel Manual

    // Note: the logic is simplified for now as we assume 32-bit immediates and
    // 64-bit registers. This means we always use REX :) 
    bool use_rex = true;
    bool use_mod_rm_byte = false;
    uint8_t rex_byte = 0b01000000; // default: W,R,X,B = 0
    uint8_t opcode;
    uint8_t mod_rm_byte;
    //uint8_t sib_byte;

    // Switch based on the location type.
    switch (dest.type) {
    case Register:
        switch (src.type) {
        case Register:
            use_mod_rm_byte = true;
            mod_rm_byte = encode_reg_reg(dest.reg, src.reg, &rex_byte);
            set_bit(&rex_byte, 3);
            switch (op) {
            case Add: opcode = 0x01; break;
            case Sub: opcode = 0x29; break;
            case And: opcode = 0x21; break;
            case Or:  opcode = 0x09; break;
            }
            break;
            
        case Deref:

        case Immediate:
        }

    case Deref:
        switch (src.type) {
        case Register:
            break;
            
        case Deref:
            out.succ = false;
            out.msg = mk_string("Cannot use two dereferences as a source/destination pair", a);
            break;

        case Immediate:

        }
        break;

    case Immediate:
        out.succ = false;
        out.msg = mk_string("Cannot use an immediate as a destination register!", a);
        break;
    }
    // Finally, write out the opcode into the assembler
    if (use_rex) push_u8(rex_byte, assembler, a);
    push_u8(opcode, assembler, a);
    if (use_mod_rm_byte) push_u8(mod_rm_byte, assembler, a);

    return out;
}

