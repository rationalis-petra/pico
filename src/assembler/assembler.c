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

Location rref(Regname name, int8_t offset) {
    return (Location) {
      .type = Deref,
      .sz = sz_64,
      .reg = name,
      .disp_sz = 1,
      .disp_8 = offset,
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

/* Encoding relevant to binary operations:
 * • ModRm byte:  
 */

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
            // simplest : mod = 11, rm = register 
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

            } else if (rm_loc.disp_sz == 4 ){
                modrm_byte |= modrm_mod(0b10);
                for (uint8_t i = 0; i < 4; i++) {
                    disp_bytes[i]  = rm_loc.disp_bytes[i];
                }
            } else {
                throw_error(point, mv_string("Bad displacement size: not 0, 1 or 4"));
            }
            if ((0b111 & rm_loc.reg) == RSP)  {
                // Using RSP - necessary to use SIB byte
                modrm_byte |= modrm_rm(RSP);
            
                use_sib_byte = true;
                sib_byte |= sib_ss(0b00);
                sib_byte |= sib_index(0b100);
                sib_byte |= sib_base(RSP);
                rex_byte |= rex_sb_ext((rm_loc.reg & 0b1000) >> 3);

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
        case Immediate:
            // ??: proably error
            break;
        }

        // Step 4: Reg encoding
        // Store the Reg location
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
    U8Array* instructions = &assembler->instructions;
    if (be.use_rex_byte)
        push_u8(rex_byte, instructions);

    // opcode
    push_u8(opcode_byte, instructions);

    if (be.use_modrm_byte)
        push_u8(modrm_byte, instructions);

    if (use_sib_byte)
        push_u8(sib_byte, instructions);
    
    for (uint8_t i = 0; i < num_disp_bytes; i++)
        push_u8(disp_bytes[i], &assembler->instructions);

    AsmResult out = {.backlink = 0};
    if (be.num_immediate_bytes != 0)
        out.backlink = instructions->len;
    for (uint8_t i = 0; i < be.num_immediate_bytes; i++)
        push_u8(src.immediate_bytes[i], &assembler->instructions);

    return out;
}
#pragma GCC diagnostic pop

void modrm_reg_rm_rex(uint8_t* modrm_byte, uint8_t* rex_byte, Regname reg) {
    if (reg & 010) set_bit(rex_byte, 2);
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

AsmResult build_unary_op(Assembler* assembler, UnaryOp op, Location loc, Allocator* err_allocator, ErrorPoint* point) {
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
            mod_rm_byte = modrm_reg_old(loc.reg);
            break;
        default:
            throw_error(point, mk_string("Push for non register locations not implemented", err_allocator));
        }
        break;
    case Pop:
        switch (loc.type) {
        case Register:
            opcode = 0x58 + (loc.reg & 0b111);
            break;
        default:
            throw_error(point, mk_string("Pop for non register locations not implemented", err_allocator));
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
            immediate_bytes[0] = loc.disp_8;
            break;
        case Immediate: {
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
            throw_error(point, mk_string("Push for register dereference not implemented", err_allocator));
        }
        break;
        // conditional jumps
    case JE:
        opcode = 0x74;
        if (loc.type != Immediate && loc.sz != sz_8) {
            throw_error(point, mk_string("JE requires 8-bit immediate", err_allocator));
        }
        num_immediate_bytes = 1;
        immediate_bytes[0] = loc.immediate_8;
        break;
    case JNE:
        opcode = 0x75;
        if (loc.type != Immediate && loc.sz != sz_8) {
            throw_error(point, mk_string("JNE requires 8-bit immediate", err_allocator));
        }
        num_immediate_bytes = 1;
        immediate_bytes[0] = loc.immediate_8;
        break;

    case JMP:
        opcode = 0xEB;
        switch (loc.type) {
        case Immediate:
            num_immediate_bytes = 1;
            immediate_bytes[0] = loc.immediate_8;
            break;
        default:
            throw_error(point, mk_string("JMP requires 8-bit immediate", err_allocator));
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
            throw_error(point, mk_string("SetE requires a register argument", err_allocator));
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
            throw_error(point, mk_string("SetE requires a register argument", err_allocator));
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
            throw_error(point, mk_string("SetE requires a register argument", err_allocator));
        }
        break;
    }

    U8Array* instructions = &assembler->instructions;
    if (use_rex_byte) {
        push_u8(rex_byte, instructions);
    }
    if (use_prefix_byte) {
        push_u8(prefix_byte, instructions);
    }
    push_u8(opcode, instructions);
    if (use_mod_rm_byte) {
        push_u8(mod_rm_byte,instructions);
    }

    AsmResult out = {.backlink = 0};
    if (num_immediate_bytes != 0)
        out.backlink = instructions->len;
    for (uint8_t i = 0; i < num_immediate_bytes; i++) {
        push_u8(immediate_bytes[i], instructions);
    }
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
    build_binary_table();
    build_binary_opcode_table();
}
