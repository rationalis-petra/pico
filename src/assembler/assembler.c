#include <inttypes.h>
#include <stdio.h>

#include "platform/signals.h"
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

struct Assembler {
    U8Array instructions;
    Allocator* gpa;
};

void clear_assembler(Assembler* assembler) {
    assembler->instructions.len = 0;
}
                                            
Assembler* mk_assembler(Allocator* a) {
    Assembler* out = (Assembler*)mem_alloc(sizeof(Assembler), a);
    out->instructions = mk_u8_array(1024, a);
    out->gpa = a;
    return out;
}

U8Array get_instructions(Assembler* ass) {
    return ass->instructions;
}

size_t get_pos(Assembler* ass) {
    return ass->instructions.len;
}

void delete_assembler (Assembler* ass) {
    sdelete_u8_array(ass->instructions);
    mem_free(ass, ass->gpa);
}

Document* pretty_assembler(Assembler* assembler, Allocator* a) {
    PtrArray nodes = mk_ptr_array(4 + assembler->instructions.len, a);

    for (size_t i = 0; i < assembler->instructions.len; i++) {
        int len = snprintf(NULL, 0, "%02x", assembler->instructions.data[i]) + 1;
        char* str = (char*)mem_alloc(sizeof(char) * len, a);
        snprintf(str, len, "%02" PRIx8, assembler->instructions.data[i]);
        Document* arg = mv_str_doc(mv_string(str), a);

        push_ptr(arg, &nodes);
    }

    return mv_sep_doc(nodes, a);
}

Location reg(Regname reg) {
    return (Location) {
      .type = Register,
      .sz = sz_64,
      .reg = reg,
    };
}

Location ref(Regname name) {
    return (Location) {
      .type = Deref,
      .reg = name,
      .disp_sz = 0,
    };
}

Location rref8(Regname name, int8_t offset) {
    return (Location) {
        .type = Deref,
        .sz = sz_64,
        .reg = name,
        .disp_sz = 1,
        .disp_8 = offset,
    };
}

Location rref32(Regname name, int32_t offset) {
    return (Location) {
        .type = Deref,
        .sz = sz_64,
        .reg = name,
        .disp_sz = 4,
        .disp_32 = offset,
    };
}

Location sib(Regname base, Regname index, uint8_t scale) {
    return (Location) {
        .type = Deref,
        .sz = sz_64,
        .reg = base,
        .index = index,
        .is_scale = true,
        .scale = scale,
    };
}

Location sib8(Regname base, Regname index, uint8_t scale, int8_t displacement) {
    return (Location) {
        .type = Deref,
        .sz = sz_64,
        .reg = base,
        .index = index,
        .is_scale = true,
        .scale = scale,

        .disp_sz = 1, 
        .disp_8 = displacement,
    };
}

Location sib_32(Regname base, Regname index, uint8_t scale, int32_t displacement) {
    return (Location) {
        .type = Deref,
        .sz = sz_64,
        .reg = base,
        .index = index,
        .is_scale = true,
        .scale = scale,
        .disp_sz = 4, 
        .disp_32 = displacement,
    };
}

Location imm8(int8_t immediate) {
    return (Location) {
      .type = Immediate,
      .sz = sz_8,
      .immediate_8 = immediate,
    };
}
Location imm16(int16_t immediate) {
    return (Location) {
      .type = Immediate,
      .sz = sz_16,
      .immediate_16 = immediate,
    };
}

Location imm32(int32_t immediate) {
    return (Location) {
       .type = Immediate,
       .sz = sz_32,
       .immediate_32 = immediate,
    };
}

Location imm64(int64_t immediate) {
    return (Location) {
      .type = Immediate,
      .sz = sz_64,
      .immediate_64 = immediate,
    };
}

typedef enum EncOrder {
    RM,
    MR,
    MI,
    OI,
} EncOrder;

typedef struct {
    bool valid;
    bool use_rex_byte;
    uint8_t init_rex_byte;
    bool use_modrm_byte;
    uint8_t num_immediate_bytes;
    EncOrder order;

    bool has_opcode_ext;
} BinaryTableEntry;

static BinaryTableEntry binary_table[256];

static uint8_t binary_opcode_table[Binary_Op_Count][256][2];

uint8_t bindex(Dest_t dest_ty, LocationSize dest_sz, Dest_t src_ty, LocationSize src_sz) {
    return dest_ty | (dest_sz << 2) | (src_ty << 4) | (src_sz << 6) ;
};

void build_binary_table() {
    // populate the table with invalid entries
    for (size_t i = 0; i < 256; i++) {
        binary_table[i].valid = false;
    }

    // r/m64, imm8-64
    binary_table[bindex(Register, sz_64, Immediate, sz_8)] = (BinaryTableEntry){
        .valid = true,
        .use_rex_byte = true,
        .init_rex_byte = 0b01001000, // REX.W
        .use_modrm_byte = true,
        .num_immediate_bytes = 1,
        .order = MI,
        .has_opcode_ext = true,
    };
    binary_table[bindex(Register, sz_64, Immediate, sz_32)] = (BinaryTableEntry){
        .valid = true,
        .use_rex_byte = true,
        .init_rex_byte = 0b01001000, // REX.W
        .use_modrm_byte = true,
        .num_immediate_bytes = 4,
        .order = MI,
        .has_opcode_ext = true,
    };
    binary_table[bindex(Deref, sz_64, Immediate, sz_8)] = (BinaryTableEntry){
        .valid = true,
        .use_rex_byte = true,
        .init_rex_byte = 0b01001000, // REX.W
        .use_modrm_byte = true,
        .num_immediate_bytes = 1,
        .order = MI,
        .has_opcode_ext = true,
    };
    binary_table[bindex(Deref, sz_64, Immediate, sz_32)] = (BinaryTableEntry){
        .valid = true,
        .use_rex_byte = true,
        .init_rex_byte = 0b01001000, // REX.W
        .use_modrm_byte = true,
        .num_immediate_bytes = 4,
        .order = MI,
        .has_opcode_ext = true,
    };

    // r/m64, r64
    binary_table[bindex(Register, sz_64, Register, sz_64)] = (BinaryTableEntry){
        .valid = true,
        .use_rex_byte = true,
        .init_rex_byte = 0b01001000, // REX.W
        .use_modrm_byte = true,
        .num_immediate_bytes = 0,
        .order = MR,
        .has_opcode_ext = false,
    };
    binary_table[bindex(Deref, sz_64, Register, sz_64)] = (BinaryTableEntry){
        .valid = true,
        .use_rex_byte = true,
        .init_rex_byte = 0b01001000, // REX.W
        .use_modrm_byte = true,
        .num_immediate_bytes = 0,
        .order = MR,
        .has_opcode_ext = false,
    };


    // r64, r/m64
    binary_table[bindex(Register, sz_64, Register, sz_64)] = (BinaryTableEntry){
        .valid = true,
        .use_rex_byte = true,
        .init_rex_byte = 0b01001000, // REX.W
        .use_modrm_byte = true,
        .num_immediate_bytes = 0,
        .order = RM,
        .has_opcode_ext = false,
    };
    binary_table[bindex(Register, sz_64, Deref, sz_64)] = (BinaryTableEntry){
        .valid = true,
        .use_rex_byte = true,
        .init_rex_byte = 0b01001000, // REX.W
        .use_modrm_byte = true,
        .num_immediate_bytes = 0,
        .order = RM,
        .has_opcode_ext = false,
    };

    // r64, imm64
    binary_table[bindex(Register, sz_64, Immediate, sz_64)] = (BinaryTableEntry){
        .valid = true,
        .use_rex_byte = true,
        .init_rex_byte = 0b01001000, // REX.W
        .use_modrm_byte = false,
        .num_immediate_bytes = 8,
        .order = OI,
        .has_opcode_ext = false,
    };
}

void build_binary_opcode_table() {
    for (size_t i = 0; i < 256 * Binary_Op_Count; i++) {
        binary_opcode_table[i % Binary_Op_Count][i/Binary_Op_Count][0] = 0x90;
        binary_opcode_table[i % Binary_Op_Count][i/Binary_Op_Count][1] = 0x09;
    }

    // Add
    // r/m64, imm8 & imm64
    binary_opcode_table[Add][bindex(Register, sz_64, Immediate, sz_8)][0] = 0x83;
    binary_opcode_table[Add][bindex(Register, sz_64, Immediate, sz_8)][1] = 0x0;
    binary_opcode_table[Add][bindex(Register, sz_64, Immediate, sz_32)][0] = 0x81;
    binary_opcode_table[Add][bindex(Register, sz_64, Immediate, sz_32)][1] = 0x0;
    binary_opcode_table[Add][bindex(Deref, sz_64, Immediate, sz_8)][0] = 0x83;
    binary_opcode_table[Add][bindex(Deref, sz_64, Immediate, sz_8)][1] = 0x0;
    binary_opcode_table[Add][bindex(Deref, sz_64, Immediate, sz_32)][0] = 0x81;
    binary_opcode_table[Add][bindex(Deref, sz_64, Immediate, sz_32)][1] = 0x0;

    // r/m64, r64
    binary_opcode_table[Add][bindex(Register, sz_64, Register, sz_64)][0] = 0x01;
    binary_opcode_table[Add][bindex(Deref, sz_64, Register, sz_64)][0] = 0x01;

    // r64, r/m64
    binary_opcode_table[Add][bindex(Register, sz_64, Register, sz_64)][0] = 0x03;
    binary_opcode_table[Add][bindex(Register, sz_64, Deref, sz_64)][0] = 0x03;

    // Sub
    // r/m64, imm8 & imm64
    binary_opcode_table[Sub][bindex(Register, sz_64, Immediate, sz_8)][0] = 0x83;
    binary_opcode_table[Sub][bindex(Register, sz_64, Immediate, sz_8)][1] = 0x05;
    binary_opcode_table[Sub][bindex(Register, sz_64, Immediate, sz_32)][0] = 0x81;
    binary_opcode_table[Sub][bindex(Register, sz_64, Immediate, sz_32)][1] = 0x05;
    binary_opcode_table[Sub][bindex(Deref, sz_64, Immediate, sz_8)][0] = 0x83;
    binary_opcode_table[Sub][bindex(Deref, sz_64, Immediate, sz_8)][1] = 0x05;
    binary_opcode_table[Sub][bindex(Deref, sz_64, Immediate, sz_32)][0] = 0x81;
    binary_opcode_table[Sub][bindex(Deref, sz_64, Immediate, sz_32)][1] = 0x05;

    // r/m64, r64
    binary_opcode_table[Sub][bindex(Register, sz_64, Register, sz_64)][0] = 0x29;
    binary_opcode_table[Sub][bindex(Deref, sz_64, Register, sz_64)][0] = 0x29;

    // r64, r/m64
    binary_opcode_table[Sub][bindex(Register, sz_64, Register, sz_64)][0] = 0x2B;
    binary_opcode_table[Sub][bindex(Register, sz_64, Deref, sz_64)][0] = 0x2B;

    // Cmp
    // r/m64, imm8 & imm64
    binary_opcode_table[Cmp][bindex(Register, sz_64, Immediate, sz_8)][0] = 0x83;
    binary_opcode_table[Cmp][bindex(Register, sz_64, Immediate, sz_8)][1] = 0x07;
    binary_opcode_table[Cmp][bindex(Register, sz_64, Immediate, sz_32)][0] = 0x81;
    binary_opcode_table[Cmp][bindex(Register, sz_64, Immediate, sz_32)][1] = 0x07;
    binary_opcode_table[Cmp][bindex(Deref, sz_64, Immediate, sz_8)][0] = 0x83;
    binary_opcode_table[Cmp][bindex(Deref, sz_64, Immediate, sz_8)][1] = 0x07;
    binary_opcode_table[Cmp][bindex(Deref, sz_64, Immediate, sz_32)][0] = 0x81;
    binary_opcode_table[Cmp][bindex(Deref, sz_64, Immediate, sz_32)][1] = 0x07;

    // r/m64, r64
    binary_opcode_table[Cmp][bindex(Register, sz_64, Register, sz_64)][0] = 0x39;
    binary_opcode_table[Cmp][bindex(Deref, sz_64, Register, sz_64)][0] = 0x39;

    // r64, r/m64
    binary_opcode_table[Cmp][bindex(Register, sz_64, Register, sz_64)][0] = 0x3B;
    binary_opcode_table[Cmp][bindex(Register, sz_64, Deref, sz_64)][0] = 0x3B;

    // ------------------
    //  Logic
    // ------------------
    // And
    // r/m64, imm8 & imm64
    binary_opcode_table[And][bindex(Register, sz_64, Immediate, sz_8)][0] = 0x83;
    binary_opcode_table[And][bindex(Register, sz_64, Immediate, sz_8)][1] = 0x04;
    binary_opcode_table[And][bindex(Register, sz_64, Immediate, sz_32)][0] = 0x81;
    binary_opcode_table[And][bindex(Register, sz_64, Immediate, sz_32)][1] = 0x04;
    binary_opcode_table[And][bindex(Deref, sz_64, Immediate, sz_8)][0] = 0x83;
    binary_opcode_table[And][bindex(Deref, sz_64, Immediate, sz_8)][1] = 0x04;
    binary_opcode_table[And][bindex(Deref, sz_64, Immediate, sz_32)][0] = 0x81;
    binary_opcode_table[And][bindex(Deref, sz_64, Immediate, sz_32)][1] = 0x04;

    // r/m64, r64
    binary_opcode_table[And][bindex(Register, sz_64, Register, sz_64)][0] = 0x21;
    binary_opcode_table[And][bindex(Deref, sz_64, Register, sz_64)][0] = 0x21;

    // r64, r/m64
    binary_opcode_table[And][bindex(Register, sz_64, Register, sz_64)][0] = 0x23;
    binary_opcode_table[And][bindex(Register, sz_64, Deref, sz_64)][0] = 0x23;

    // Or
    // r/m64, imm8 & imm64
    binary_opcode_table[Or][bindex(Register, sz_64, Immediate, sz_8)][0] = 0x83;
    binary_opcode_table[Or][bindex(Register, sz_64, Immediate, sz_8)][1] = 0x01;
    binary_opcode_table[Or][bindex(Register, sz_64, Immediate, sz_32)][0] = 0x81;
    binary_opcode_table[Or][bindex(Register, sz_64, Immediate, sz_32)][1] = 0x01;
    binary_opcode_table[Or][bindex(Deref, sz_64, Immediate, sz_8)][0] = 0x83;
    binary_opcode_table[Or][bindex(Deref, sz_64, Immediate, sz_8)][1] = 0x01;
    binary_opcode_table[Or][bindex(Deref, sz_64, Immediate, sz_32)][0] = 0x81;
    binary_opcode_table[Or][bindex(Deref, sz_64, Immediate, sz_32)][1] = 0x01;

    // r/m64, r64
    binary_opcode_table[Or][bindex(Register, sz_64, Register, sz_64)][0] = 0x09;
    binary_opcode_table[Or][bindex(Deref, sz_64, Register, sz_64)][0] = 0x09;

    // r64, r/m64
    binary_opcode_table[Or][bindex(Register, sz_64, Register, sz_64)][0] = 0x0B;
    binary_opcode_table[Or][bindex(Register, sz_64, Deref, sz_64)][0] = 0x0B;

    // ------------------
    //  Bit Manipulation
    // ------------------
    // Shift Left
    // r/m64, imm8 
    binary_opcode_table[LShift][bindex(Register, sz_64, Immediate, sz_8)][0] = 0xC1;
    binary_opcode_table[LShift][bindex(Register, sz_64, Immediate, sz_8)][1] = 0x04;
    binary_opcode_table[LShift][bindex(Deref, sz_64, Immediate, sz_8)][0] = 0xC1;
    binary_opcode_table[LShift][bindex(Deref, sz_64, Immediate, sz_8)][1] = 0x04;

    // Shift Right
    // r/m64, imm8
    binary_opcode_table[RShift][bindex(Register, sz_64, Immediate, sz_8)][0] = 0xD3;
    binary_opcode_table[RShift][bindex(Register, sz_64, Immediate, sz_8)][1] = 0x05;
    binary_opcode_table[RShift][bindex(Deref, sz_64, Immediate, sz_8)][0] = 0xD3;
    binary_opcode_table[RShift][bindex(Deref, sz_64, Immediate, sz_8)][1] = 0x05;

    // ------------------
    //  Memory
    // ------------------
    //Mov,   // p 769.
    // r64, imm64
    binary_opcode_table[Mov][bindex(Register, sz_64, Immediate, sz_64)][0] = 0xB8;
    // r/m64, imm32
    binary_opcode_table[Mov][bindex(Register, sz_64, Immediate, sz_32)][0] = 0xC7;
    binary_opcode_table[Mov][bindex(Register, sz_64, Immediate, sz_32)][1] = 0x00;
    binary_opcode_table[Mov][bindex(Deref, sz_64, Immediate, sz_32)][0] = 0xC7;
    binary_opcode_table[Mov][bindex(Deref, sz_64, Immediate, sz_32)][1] = 0x00;

    // r/m64, r64
    binary_opcode_table[Mov][bindex(Register, sz_64, Register, sz_64)][0] = 0x89;
    binary_opcode_table[Mov][bindex(Deref, sz_64, Register, sz_64)][0] = 0x89;

    // r64, r/m64
    binary_opcode_table[Mov][bindex(Register, sz_64, Register, sz_64)][0] = 0x8B;
    binary_opcode_table[Mov][bindex(Register, sz_64, Deref, sz_64)][0] = 0x8B;

    //Lea,   // p 705.
    // Lea is much more limited in how it works - operand 1 is always a register
    //      and operand 2 is a "Deref"
    // r64, r/m64
    binary_opcode_table[LEA][bindex(Register, sz_64, Deref, sz_64)][0] = 0x8D;
}

uint8_t modrm_rm(uint8_t reg_bits)  { return (reg_bits & 0b111); }
uint8_t modrm_reg(uint8_t reg_bits) { return (reg_bits & 0b111) << 3; }
uint8_t modrm_mod(uint8_t reg_bits) { return (reg_bits & 0b11) << 6; }

uint8_t sib_base(uint8_t base_bits) { return (base_bits & 0b111 ); }
uint8_t sib_index(uint8_t index_bits) { return (index_bits & 0b111) << 3; }
uint8_t sib_ss(uint8_t ss_bits) { return (ss_bits & 0b11) << 6; }

uint8_t rex_rm_ext(uint8_t bit) { return (bit & 0b1); }
uint8_t rex_sb_ext(uint8_t bit) { return (bit & 0b1) << 1; }
uint8_t rex_reg_ext(uint8_t bit) { return (bit & 0b1) << 2; }


#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
AsmResult build_binary_op(Assembler* assembler, BinaryOp op, Location dest, Location src, Allocator* err_allocator, ErrorPoint* point) {
    BinaryTableEntry be = binary_table[bindex(dest.type, dest.sz, src.type, src.sz)];
    if (!be.valid) {
        throw_error(point, mv_string("Invalid binary table entry."));
    }

    uint8_t rex_byte = be.init_rex_byte;
    uint8_t opcode_byte = 0;
    uint8_t modrm_byte = 0;

    bool use_sib_byte = false;
    uint8_t sib_byte = 0;

    uint8_t num_disp_bytes = 0;
    uint8_t disp_bytes [4];

    /* uint8_t num_imm_bytes = 0; */
    /* uint8_t imm_bytes [8]; */

    // Step1: Opcode
    opcode_byte = binary_opcode_table[op][bindex(dest.type, dest.sz, src.type, src.sz)][0];
    if (opcode_byte == 0x90) {
        throw_error(point, mv_string("Invalid binary opcode"));
    }
    if (be.has_opcode_ext) {
        uint8_t ext_byte = binary_opcode_table[op][bindex(dest.type, dest.sz, src.type, src.sz)][1]; 
        modrm_byte |= modrm_reg(ext_byte);
        if (ext_byte == 0x09) {
            throw_error(point, mv_string("Invalid binary opcode extension"));
        }
    }

    // Step 2: Determine Operand Encoding type
    // Store the r/m op
    if (be.use_modrm_byte) {
        Location rm_loc; 
        Location reg_loc;

        if (be.order == MR || be.order == MI) {
            rm_loc = dest;
            reg_loc = src;
        }
        else if (be.order == RM) {
            rm_loc = src;
            reg_loc = dest;
        } else {
            throw_error(point, mv_string("Unrecognized binary op operand order encoding"));
        }

        // Step 3: R/M encoding (most complex)
        // Store the R/M location
        switch (rm_loc.type) {
        case Register:
            if (rm_loc.reg == RIP) {
                throw_error(point, mv_string("Using RIP as a register is invalid"));
            }
            // Simplest : mod = 11, rm = register 
            modrm_byte |= modrm_mod(0b11);
            modrm_byte |= modrm_rm(rm_loc.reg);
            rex_byte |= rex_rm_ext((rm_loc.reg & 0b1000) >> 3); 
            break;
            
        case Deref:
            if (rm_loc.disp_sz == 0)  {
                modrm_byte |= modrm_mod(0b00);
            }
            else if (rm_loc.disp_sz == 1)  {
                modrm_byte |= modrm_mod(0b01);
                num_disp_bytes = 1;
                disp_bytes[0] = rm_loc.disp_bytes[0];

            } else if (rm_loc.disp_sz == 4) {
                if (rm_loc.reg != RIP) {
                    modrm_byte |= modrm_mod(0b10);
                }
                num_disp_bytes = 4;
                for (uint8_t i = 0; i < 4; i++) {
                    disp_bytes[i]  = rm_loc.disp_bytes[i];
                }
            } else {
                throw_error(point, mv_string("Bad displacement size: not 0, 1 or 4"));
            }

            // Now the register
            // Step1: check for Index Register (implies SIB byte)
            if (rm_loc.is_scale) {
                use_sib_byte = true;

                // set modrm byte reg = 0b100
                modrm_byte |= modrm_rm(0b100);

                if (rm_loc.scale == 1)  {
                    sib_byte |= sib_ss(0b00);
                } else if (rm_loc.scale == 2)  {
                    sib_byte |= sib_ss(0b01);
                } else if (rm_loc.scale == 4) {
                    sib_byte |= sib_ss(0b10);
                } else if (rm_loc.scale == 8) {
                    sib_byte |= sib_ss(0b11);
                } else {
                    throw_error(point, mv_string("Bad scale: not 0, 1, 4 or 8"));
                }

                // Base should be register 
                sib_byte |= sib_index(rm_loc.index);
                sib_byte |= sib_base(rm_loc.reg);

            // Here, we guarantee no index register exists
            } else if (rm_loc.reg == RIP) {
                if (rm_loc.disp_sz != 4) {
                    throw_error(point, mv_string("RIP-relative addressing reqiures 32-bit displacement!"));
                }
                // modrm_mod = 00, so no need to do anything here
                modrm_byte |= modrm_rm(RIP);

            // Special Case #1: we need to use SIB byte
            // Check if we need to use SIB byte
            } else if ((0b111 & rm_loc.reg) == RSP)  {
                // Using RSP - necessary to use SIB byte
                modrm_byte |= modrm_rm(RSP);
            
                use_sib_byte = true;
                sib_byte |= sib_ss(0b00);
                sib_byte |= sib_index(0b100);
                sib_byte |= sib_base(RSP);
                rex_byte |= rex_sb_ext((rm_loc.reg & 0b1000) >> 3);

            // Special Case #2: RBP MUST have a displacement - the encoding 
            // for 'RBP without displacement' is just a 'disp32' 
            } else if (((0b111 & rm_loc.reg) == RBP)) {
                // As usual...
                modrm_byte |= modrm_rm(RBP);
                rex_byte |= rex_rm_ext((rm_loc.reg & 0b1000) >> 3);

                // If there is no displacement, update to 8-bit displacement of 0
                if (rm_loc.disp_sz == 0)  {
                    modrm_byte |= modrm_mod(0b01);
                    num_disp_bytes = 1;
                    disp_bytes[0] = 0;
                }

            } else {
                // Simple encoding
                modrm_byte |= modrm_rm(rm_loc.reg);
                rex_byte |= rex_rm_ext((rm_loc.reg & 0b1000) >> 3); 
            }
            break;
        case Immediate:
            throw_error(point, mv_string("Internal error in build_binary_op: rm_loc is immediate."));
            break;
        }

        // Step 4: Reg encoding
        // Store the Reg location
        // TODO (INVESTIGATE): what if reg_loc is not register? (seems inserting
        //    throw results in spurious errors)
        if (reg_loc.type == Register) {
            rex_byte |= rex_reg_ext((reg_loc.reg & 0b1000) >> 3); 
            modrm_byte |= modrm_reg(reg_loc.reg & 0b111);
        }
    } else {
        if (be.order == OI)  {
            opcode_byte |= (dest.reg & 0b111);
            rex_byte |= rex_rm_ext((dest.reg & 0b1000) >> 3);
        } else {
            throw_error(point, mv_string("Unrecognized binary op operand order encoding"));
        }
    }

    // Step 5: write bytes
    AsmResult out = {.backlink = 0};
    U8Array* instructions = &assembler->instructions;
    if (be.use_rex_byte)
        push_u8(rex_byte, instructions);

    // opcode
    push_u8(opcode_byte, instructions);

    if (be.use_modrm_byte)
        push_u8(modrm_byte, instructions);

    if (use_sib_byte)
        push_u8(sib_byte, instructions);
    
    if (num_disp_bytes != 0) 
        out.backlink = instructions->len;
        
    for (uint8_t i = 0; i < num_disp_bytes; i++)
        push_u8(disp_bytes[i], &assembler->instructions);

    if (be.num_immediate_bytes != 0)
        out.backlink = instructions->len;
    for (uint8_t i = 0; i < be.num_immediate_bytes; i++)
        push_u8(src.immediate_bytes[i], &assembler->instructions);

    return out;
}
#pragma GCC diagnostic pop

void modrm_reg_rm_rex(uint8_t* modrm_byte, uint8_t* rex_byte, Regname reg) {
    if (reg & 0b1000) set_bit(rex_byte, 0);
    *modrm_byte |= reg & 0b111;
}


// Old-style instruction encoding
// 

// Return: the ModR/M byte for a single register src/dest
uint8_t modrm_reg_old(Regname r2) {
    return 0b11010000 | (r2 & 0b111); 
}

uint8_t modrm_mem(Location mem) {
    return 0b01110000 + (mem.reg & 0b111); 
}

typedef enum UnaryEncOrder {
    I,
    M,
    O,
} UnaryEncOrder;

typedef struct {
    bool valid;
    bool use_modrm_byte;
    uint8_t num_immediate_bytes;
    UnaryEncOrder order;
} UnaryTableEntry;

typedef struct {
    // A prefix (if present)
    uint8_t opcode_prefix;
    // The opcode 
    uint8_t opcode;
    // The portion of the opcode to stash in the mod/rm byte 
    uint8_t opcode_modrm;
    // the initial value of the REX byte. As 0 is an invalid REX byte, set to 0
    // if not used!
    uint8_t init_rex_byte; 
} UnaryOpEntry;

// 128 - 2 bits for Dest (3 possibilities: Register/Deref/Immediate)
//     - 2 bits for size (4 possibiliyies: 8, 16, 32, 64)
// total = 4 bits, 2^4 = 16
#define UNARY_TABLE_SIZE 16
static UnaryTableEntry unary_table[UNARY_TABLE_SIZE];

static UnaryOpEntry unary_opcode_table[Unary_Op_Count][UNARY_TABLE_SIZE];

uint8_t uindex(Dest_t dest_ty, LocationSize dest_sz) {
    return dest_ty | (dest_sz << 2);
};

void build_unary_table() {
    // populate the table with invalid entries
    for (size_t i = 0; i < UNARY_TABLE_SIZE; i++) {
        binary_table[i].valid = false;
    }

    // r/m64
    unary_table[uindex(Register, sz_64)] = (UnaryTableEntry){
        .valid = true,
        .use_modrm_byte = true,
        .num_immediate_bytes = 0,
        .order = M,
    };
    unary_table[uindex(Deref, sz_64)] = (UnaryTableEntry){
        .valid = true,
        .use_modrm_byte = true,
        .num_immediate_bytes = 0,
        .order = M,
    };

    unary_table[uindex(Immediate, sz_8)] = (UnaryTableEntry){
        .valid = true,
        .use_modrm_byte = false,
        .num_immediate_bytes = 1,
        .order = I,
    };
    unary_table[uindex(Immediate, sz_16)] = (UnaryTableEntry){
        .valid = true,
        .use_modrm_byte = false,
        .num_immediate_bytes = 2,
        .order = I,
    };
    unary_table[uindex(Immediate, sz_32)] = (UnaryTableEntry){
        .valid = true,
        .use_modrm_byte = false,
        .num_immediate_bytes = 4,
        .order = I,
    };
}

void build_unary_opcode_table() {
    for (size_t i = 0; i < UNARY_TABLE_SIZE * Unary_Op_Count; i++) {
        unary_opcode_table[i % Unary_Op_Count][i/Unary_Op_Count] = (UnaryOpEntry) {
            .opcode_prefix = 0x0, // 0 indicates no prefix
            .opcode = 0x90, // 0x90 indicates invalid opcode
            .opcode_modrm = 0x9, // 9 indicates no modrm extension, as it takes 4 bits (1001)
            .init_rex_byte = 0x0, // 0 indicates no rex byte
        };
    }

    // Call
    // r/m64 only! 
    unary_opcode_table[Call][uindex(Register, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xFF, .opcode_modrm = 0x2};
    unary_opcode_table[Call][uindex(Deref, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xFF, .opcode_modrm = 0x2};

    // Push
    // r/m 64, imm8,32
    unary_opcode_table[Push][uindex(Register, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xFF, .opcode_modrm = 0x6};
    unary_opcode_table[Push][uindex(Deref, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xFF, .opcode_modrm = 0x6};

    unary_opcode_table[Push][uindex(Immediate, sz_8)] =
        (UnaryOpEntry) {.opcode = 0x6A,};
    // 16-bit pushes get extended to 32??
    /* unary_opcode_table[Push][uindex(Immediate, sz_16)] = */
    /*     (UnaryOpEntry) {.opcode = 0x68, .opcode_modrm = 0x6, .init_rex_byte = 0x0}; */
    unary_opcode_table[Push][uindex(Immediate, sz_32)] =
        (UnaryOpEntry) {.opcode = 0x68,};

    unary_opcode_table[Pop][uindex(Register, sz_64)] =
        (UnaryOpEntry) {.opcode = 0x8F,};
    unary_opcode_table[Pop][uindex(Deref, sz_64)] =
        (UnaryOpEntry) {.opcode = 0x8F,};


    // ------------------
    //  Jumps
    // ------------------
    // jumps are imm/8, and imm/32 (64-bit mode doesn't support 16-bit jumps)
    unary_opcode_table[JE][uindex(Immediate, sz_8)] =
        (UnaryOpEntry) {.opcode = 0x74,};
    unary_opcode_table[JE][uindex(Immediate, sz_32)] =
        (UnaryOpEntry) {.opcode_prefix = 0x8F, .opcode = 0x84,};

    unary_opcode_table[JNE][uindex(Immediate, sz_8)] =
        (UnaryOpEntry) {.opcode = 0x75,};
    unary_opcode_table[JNE][uindex(Immediate, sz_32)] =
        (UnaryOpEntry) {.opcode_prefix = 0x8F, .opcode = 0x85,};

    unary_opcode_table[JMP][uindex(Immediate, sz_8)] =
        (UnaryOpEntry) {.opcode = 0xEB,};
    unary_opcode_table[JMP][uindex(Immediate, sz_32)] =
        (UnaryOpEntry) {.opcode = 0xE9, };

    unary_opcode_table[JMP][uindex(Register, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xFF, .opcode_modrm = 0x4,};
    unary_opcode_table[JMP][uindex(Deref, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xFF, .opcode_modrm = 0x4,};

    // ------------------------
    //  Set Byte based on flag 
    // ------------------------
    // TODO (BUG): in the future, change this to sz_8, as only sets r/m 8!
    unary_opcode_table[SetE][uindex(Register, sz_64)] =
        (UnaryOpEntry) {.opcode_prefix = 0x0F, .opcode = 0x94,};
    // TODO (BUG): when sz/8 becomes available for Deref, enable this!
    /* unary_opcode_table[SetE][uindex(Deref, sz_64)] = */
    /*     (UnaryOpEntry) {.opcode = 0xFF, .opcode_modrm = 0x4,}; */

    unary_opcode_table[SetL][uindex(Register, sz_64)] =
        (UnaryOpEntry) {.opcode_prefix = 0x0F, .opcode = 0x9C,};
    unary_opcode_table[SetG][uindex(Register, sz_64)] =
        (UnaryOpEntry) {.opcode_prefix = 0x0F, .opcode = 0x9F,};

    // ------------------
    //  Arithmetic
    // ------------------

    unary_opcode_table[Mul][uindex(Register, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x04, .init_rex_byte = 0b01001000, /*REX.W*/};
    unary_opcode_table[Mul][uindex(Deref, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x04, .init_rex_byte = 0b01001000, /*REX.W*/};

    unary_opcode_table[Div][uindex(Register, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x06, .init_rex_byte = 0b01001000, /*REX.W*/};
    unary_opcode_table[Div][uindex(Deref, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x06, .init_rex_byte = 0b01001000, /*REX.W*/};

    unary_opcode_table[IMul][uindex(Register, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x05, .init_rex_byte = 0b01001000, /*REX.W*/};
    unary_opcode_table[Mul][uindex(Deref, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x05, .init_rex_byte = 0b01001000, /*REX.W*/};

    unary_opcode_table[IDiv][uindex(Register, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x07, .init_rex_byte = 0b01001000, /*REX.W*/};
    unary_opcode_table[IDiv][uindex(Deref, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x07, .init_rex_byte = 0b01001000, /*REX.W*/};
}

AsmResult build_unary_op(Assembler* assembler, UnaryOp op, Location loc, Allocator* err_allocator, ErrorPoint* point) {
    if (loc.type == Register && loc.reg == RIP) {
        throw_error(point, mv_string("RIP-relative addressing not supported for unary operations!"));
    }

    UnaryTableEntry ue = unary_table[uindex(loc.type, loc.sz)];
    if (!ue.valid) {
        throw_error(point, mv_string("Invalid unary table entry."));
    }
    UnaryOpEntry uoe = unary_opcode_table[op][uindex(loc.type, loc.sz)];

    uint8_t rex_byte = uoe.init_rex_byte;
    uint8_t opcode_byte = uoe.opcode;
    uint8_t modrm_byte = 0;

    bool use_sib_byte = false;
    uint8_t sib_byte = 0;

    uint8_t num_disp_bytes = 0;
    uint8_t disp_bytes [4];

    /* uint8_t num_imm_bytes = 0; */
    /* uint8_t imm_bytes [8]; */

    // Step1: Opcode
    if (opcode_byte == 0x90) {
        throw_error(point, mv_string("Invalid unary opcode"));
    }
    if (uoe.opcode_modrm != 0x9) {
        modrm_byte |= modrm_reg(uoe.opcode_modrm);
    }

    // Step 2: Determine Operand Encoding type
    // Store the r/m op
    if (ue.use_modrm_byte) {
   
        // Step 3: R/M encoding (most complex)
        // Store the R/M location
        switch (loc.type) {
        case Register:
            // simplest : mod = 11, rm = register 
            modrm_byte |= modrm_mod(0b11);
            modrm_byte |= modrm_rm(loc.reg);

            // If the register portion is R9-R15, we need to add REX!
            if (loc.reg & 0b1000) {
                //rex_byte |= 0b01001000; // REX.W
                rex_byte |= 0b01000000; // REX.W
                rex_byte |= rex_rm_ext((loc.reg & 0b1000) >> 3); 
            }
            break;
            
        case Deref:
            // If the register portion is R9-R15, we need to add REX!
            if (loc.reg & 0b1000) {
                //rex_byte |= 0b01001000; // REX.W
                rex_byte |= 0b01000000; // REX.W
                rex_byte |= rex_rm_ext((loc.reg & 0b1000) >> 3); 
            }

            if (loc.disp_sz == 0)  {
                modrm_byte |= modrm_mod(0b00);
            }
            else if (loc.disp_sz == 1)  {
                modrm_byte |= modrm_mod(0b01);
                num_disp_bytes = 1;
                disp_bytes[0] = loc.disp_bytes[0];

            } else if (loc.disp_sz == 4) {
                if (loc.reg != RIP) {
                    modrm_byte |= modrm_mod(0b10);
                }
                num_disp_bytes = 4;
                for (uint8_t i = 0; i < 4; i++) {
                    disp_bytes[i]  = loc.disp_bytes[i];
                }
            } else {
                throw_error(point, mv_string("Bad displacement size: not 0, 1 or 4"));
            }

            // Now the register
            if (loc.reg == RIP) {
                if (loc.disp_sz != 4) {
                    throw_error(point, mv_string("RIP-relative addressing reqiures 32-bit displacement!"));
                }
                // modrm_mod = 00, so no need to do anything here
                modrm_byte |= modrm_rm(RIP);
            } else if ((0b111 & loc.reg) == RSP)  {
                // Using RSP - necessary to use SIB byte
                modrm_byte |= modrm_rm(RSP);
            
                use_sib_byte = true;
                sib_byte |= sib_ss(0b00);
                sib_byte |= sib_index(0b100);
                sib_byte |= sib_base(RSP);
                rex_byte |= rex_sb_ext((loc.reg & 0b1000) >> 3);

            } else if (((0b111 & loc.reg) == RBP)) {
                // As usual...
                modrm_byte |= modrm_rm(RBP);
                rex_byte |= rex_rm_ext((loc.reg & 0b1000) >> 3);

                // If there is no displacement, update to 8-bit displacement of 0
                if (loc.disp_sz == 0)  {
                    modrm_byte |= modrm_mod(0b01);
                    num_disp_bytes = 1;
                    disp_bytes[0] = 0;
                }

            } else {
                // Simple encoding
                modrm_byte |= modrm_rm(loc.reg);
                rex_byte |= rex_rm_ext((loc.reg & 0b1000) >> 3); 
            }
            break;
        case Immediate:
            throw_error(point, mv_string("Internal error in unary_binary_op: r/m loc is immediate."));
            break;
        }

    } else {
        if (ue.order == O)  {
            opcode_byte |= (loc.reg & 0b111);
            // TODO (INVESTIGATE) is this correct?
            rex_byte |= rex_rm_ext((loc.reg & 0b1000) >> 3);
        } else if (ue.order == I) {
            // TODO (INVESTIGATE): do nothing here?
        } else {
            throw_error(point, mv_string("Unrecognized unary op operand order encoding"));
        }
    }

    // Step 5: write bytes
    AsmResult out = {.backlink = 0};
    U8Array* instructions = &assembler->instructions;
    if (rex_byte)
        push_u8(rex_byte, instructions);

    // TODO (BUG INVESITGATE) check if REX or prefix goes first!
    if (uoe.opcode_prefix)
        push_u8(uoe.opcode_prefix, instructions);

    // opcode
    push_u8(opcode_byte, instructions);

    if (ue.use_modrm_byte)
        push_u8(modrm_byte, instructions);

    if (use_sib_byte)
        push_u8(sib_byte, instructions);
    
    if (num_disp_bytes != 0) 
        out.backlink = instructions->len;
        
    for (uint8_t i = 0; i < num_disp_bytes; i++)
        push_u8(disp_bytes[i], &assembler->instructions);

    if (ue.num_immediate_bytes != 0)
        out.backlink = instructions->len;
    for (uint8_t i = 0; i < ue.num_immediate_bytes; i++)
        push_u8(loc.immediate_bytes[i], &assembler->instructions);

    return out;
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
AsmResult build_nullary_op(Assembler* assembler, NullaryOp op, Allocator* err_allocator, ErrorPoint* point) {
    uint8_t opcode;
    switch (op) {
    case Ret:
        opcode = 0xC3;
        break;
    }
    U8Array* instructions = &assembler->instructions;
    push_u8(opcode, instructions);
    AsmResult out = {.backlink = 0};
    return out;
}
#pragma GCC diagnostic pop


void asm_init() {
    build_unary_table();
    build_unary_opcode_table();

    build_binary_table();
    build_binary_opcode_table();
}
