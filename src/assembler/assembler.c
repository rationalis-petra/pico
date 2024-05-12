#include <sys/mman.h>
#include <unistd.h>

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

void clear_assembler(assembler* assembler) {
    assembler->len = 0;
}
                                            
void make_executable(assembler* assembler) {
    mprotect(assembler->data, 256, PROT_EXEC);
}
void make_writable (assembler* assembler) {
    mprotect(assembler->data, 256, PROT_WRITE);
}

assembler* mk_assembler(allocator a) {
    assembler* out = (assembler*)mem_alloc(sizeof(assembler), a);
    out->len = 0;
    out->size = getpagesize();
    void* memory = mmap(NULL, out->size, PROT_EXEC | PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_FILE | MAP_PRIVATE, -1, 0);
    out->data = memory;
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
uint8_t modrm_reg(regname r2) {
    return 0b11010000 + (r2 & 0b111); 
}

uint8_t encode_reg_reg(regname r1, regname r2, uint8_t* rex_byte) {
    // R1 (dest) is encoded in ModR/M Reg + REX.R
    // R2 (src)  is encoded in ModR/M R/M + REX.B

    // set REX.R, Rex.B if needed 
    if (r1 & 010) set_bit(rex_byte, 0);
    if (r2 & 010) set_bit(rex_byte, 2);

    return 0b11000000 | ((r2 & 0b111 ) << 3) | (r1 & 0b111); 
}

result build_binary_op(assembler* assembler, binary_op op, location dest, location src, allocator a) {
    // Most paths are successful, so default assume the operation succeeded.
    result out;
    out.type = Ok;

    // Note: the logic is simplified for now as we assume 32-bit immediates and
    // 64-bit registers. This means we always use REX byte :) 
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
            out.type = Err;
            out.error_message = mk_string("Cannot use two dereferences as a source/destination pair", a);
            break;

        case Immediate:

        }
        break;

    case Immediate:
        out.type = Err;
        out.error_message = mk_string("Cannot use an immediate as a destination register!", a);
        break;
    }
    // Finally, write out the opcode into the assembler
    if (use_rex) push_u8(rex_byte, assembler, a);
    push_u8(opcode, assembler, a);
    if (use_mod_rm_byte) push_u8(mod_rm_byte, assembler, a);

    return out;
}

result build_unary_op(assembler* assembler, unary_op op, location loc, allocator a) {
    result out;
    out.type = Ok;
    bool use_prefix_byte = false;
    uint8_t prefix_byte;

    bool use_mod_rm_byte = false;
    uint8_t mod_rm_byte;

    uint8_t num_immediate_bytes = 0;
    uint8_t immediate_bytes[4];
     
    if (loc.type == Register && (loc.reg & 0b1000)) {
        use_prefix_byte = true;
        prefix_byte = 0x41;
    }

    uint8_t opcode;
    switch (op) {
    case Pop:
        switch (loc.type) {
        case Register:
            opcode = 0x58 + (loc.reg & 0b111);
            break;
        default:
            out.type = Err;
            out.error_message = mk_string("Pop for non register locations not implemented", a);
            break;
        }
        break;
    case Push:
        switch (loc.type) {
        case Register:
            opcode = 0x50 + (loc.reg & 0b111);
            break;
        case Immediate: {
            // TODO: optionally shrink immediate
            uint8_t* bytes = (uint8_t*) &loc.immediate;
            if (loc.immediate <= 256) {
                opcode = 0x6A;
                num_immediate_bytes = 1;
            } else if (loc.immediate <= 256 * 256) {
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
            out.error_message = mk_string("Push for register dereference not implemented", a);
        }
        break;
    case Call:
        switch (loc.type) {
        case Register:
            opcode = 0xff;
            use_mod_rm_byte = true;
            mod_rm_byte = modrm_reg(loc.reg);
            break;
        default:
            out.type = Err;
            out.error_message = mk_string("Push for non register locations not implemented", a);
        }
        break;
    }

    if (use_prefix_byte) {
        push_u8(prefix_byte, assembler, a);
    }
    push_u8(opcode, assembler, a);
    if (use_mod_rm_byte) {
        push_u8(mod_rm_byte, assembler, a);
    }
    for (uint8_t i = 0; i < num_immediate_bytes; i++) {
        push_u8(immediate_bytes[i], assembler, a);
    }
    return out;
}

result build_nullary_op(assembler* assembler, nullary_op op, allocator a) {
    result out;
    out.type = Ok;

    uint8_t opcode;
    switch (op) {
    case Ret:
        opcode = 0xC3;
        break;
    }
    push_u8(opcode, assembler, a);
    return out;
}
