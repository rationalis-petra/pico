#include <inttypes.h>
#include <stdio.h>

#include "data/binary.h"
#include "data/array.h"
#include "platform/signals.h"

#include "assembler/assembler.h"
#include "pretty/document.h"
#include "pretty/standard_types.h"
#include "pretty/string_printer.h"

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
 *    Implementation Notes
 * --------------------------------
 * Overview
 * ---------
 * For details of the instruction encoding, refer to the Intel Manuals for
 * Software developers (Intel 64 and IA-32). These can be found at
 * https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html
 * the list of all available instructions can be found in Volume 2, and
 * instructions may reference the relevant page of this volume.
 * 
 * Nullary Operations
 * ------------------
 * 
 * Unary Operations
 * ------------------
 *
 * Binary Operations
 * ------------------
 *  Referring to, for example, the add instruction (p142), we see there are many
 *  different encodings for different operand pairs. There is Add r64, r/m64, 
 *  Add r/m32 imm8, etc. The possible encodable instruction pairs across all
 *  instructions is much smaller than the set of possible pairs. For example,  
 *  there is no pair imm8, imm16 or rm8, imm64. For this reason, the supported pairs
 *  are encoded into their own enum type: BinOpType. This type is setup as a
 *  flag, with each occupying a unique bit.
 * 
 *  Then, each operation (Add, Sub, etc.) is given a unique table in binary_opcode_tables
 *  the table indicates which operand pairs are supported with a BinOpType
 *  variable, and has an array of opcodes (one for each type). 
 * 
 *  To calculate the index of an in the array, do as follows: 
 *   1. Take the entry type, e.g. RM16_Imm8 = 0100
 *   2. Take the supported types, e.g. 1101 = R16_Imm16 | R16_Imm8 | R8_Imm8
 *   3. Consider all bits less than the value: 0100 - 1 = 0011
 *   4. Binary And this with the supported types - 0011 & 1101 = 1
 *   5. Count the bits in this number - index = 1
 * 
 */

typedef enum : uint32_t {
    R8_Imm8     = 0b1,
    RM8_Imm8    = 0b10,
    RM16_Imm8   = 0b100,
    R16_Imm16   = 0b1000,
    RM16_Imm16  = 0b10000,
    RM32_Imm8   = 0b100000,
    R32_Imm32   = 0b1000000,
    RM32_Imm32  = 0b10000000,
    R64_Imm8    = 0b100000000,
    R64_Imm32   = 0b1000000000,
    R64_Imm64   = 0b10000000000,
    RM64_Imm8   = 0b100000000000,
    RM64_Imm32  = 0b1000000000000,
    RM8_R8      = 0b10000000000000,
    RM16_R16    = 0b100000000000000,
    RM32_R32    = 0b1000000000000000,
    RM64_R64    = 0b10000000000000000,
    R8_RM8      = 0b100000000000000000,
    R16_RM16    = 0b1000000000000000000,
    R32_RM32    = 0b10000000000000000000,
    R64_RM64    = 0b100000000000000000000,
    XMM32_XMM32 = 0b1000000000000000000000,
    XMM32_M32   = 0b10000000000000000000000,
    M32_XMM32   = 0b100000000000000000000000,
    XMM64_XMM64 = 0b1000000000000000000000000,
    XMM64_M64   = 0b10000000000000000000000000,
    M64_XMM64   = 0b100000000000000000000000000,
} BinOpType; 

struct Assembler {
    U8Array instructions;
    CPUFeatureFlags flags; 
    Allocator* gpa;
};

CPUFeatureFlags current_cpu_feature_flags() {
    return SSE;
}

void clear_assembler(Assembler* assembler) {
    assembler->instructions.len = 0;
    for (size_t i = 0; i < assembler->instructions.len; i++) {
        assembler->instructions.data[i] = 0x90;
    }
}
                                            
Assembler* mk_assembler(CPUFeatureFlags flags, Allocator* a) {
    Assembler* out = (Assembler*)mem_alloc(sizeof(Assembler), a);
    const size_t initial_capacity = 4096;

    *out = (Assembler) {
        .instructions = mk_u8_array(initial_capacity, a),
        .flags = flags,
        .gpa = a,
    };

    for (size_t i = 0; i < initial_capacity; i++) {
        out->instructions.data[i] = 0x90;
    }
    return out;
}

U8Array get_instructions(Assembler* ass) {
    return ass->instructions;
}

size_t get_pos(Assembler* assembler) {
    return assembler->instructions.len;
}

void set_pos(Assembler *assembler, size_t pos) {
    assembler->instructions.len = pos;
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
        Document* arg = mv_cstr_doc(str, a);

        push_ptr(arg, &nodes);
    }

    return mv_hsep_doc(nodes, a);
}

Location reg(Regname reg, LocationSize sz) {
    return (Location) {
      .type = Dest_Register,
      .sz = sz,
      .reg = reg,
    };
}

Location ref(Regname name, LocationSize sz) {
    return (Location) {
      .type = Dest_Deref,
      .reg = name,
      .sz = sz,
      .disp_sz = 0,
    };
}

Location rref8(Regname name, int64_t offset, LocationSize sz) {
#ifdef DEBUG_ASSERT
  if (offset > INT8_MAX || offset < INT8_MIN) {
      panic(mv_string("offset out of bounds"));
  }
#endif
    return (Location) {
        .type = Dest_Deref,
        .sz = sz,
        .reg = name,
        .disp_sz = 1,
        .disp_8 = (int8_t)offset,
    };
}

Location rref32(Regname name, int32_t offset, LocationSize sz) {
    return (Location) {
        .type = Dest_Deref,
        .sz = sz,
        .reg = name,
        .disp_sz = 4,
        .disp_32 = offset,
    };
}

Location sib(Regname base, Regname index, int64_t scale, LocationSize sz) {
#ifdef DEBUG_ASSERT
    if (scale < INT8_MIN || scale > INT8_MAX)
        panic(mv_string("scale in sib exceeds int8_t bounds"));
#endif
    return (Location) {
        .type = Dest_Deref,
        .sz = sz,
        .reg = base,
        .index = index,
        .is_scale = true,
        .scale = (int8_t)scale,
    };
}

Location sib8(Regname base, Regname index, int64_t scale, int64_t displacement, LocationSize sz) {
#ifdef DEBUG_ASSERT
    if (scale < INT8_MIN || scale > INT8_MAX)
        panic(mv_string("scale in sib8 exceeds int8_t bounds"));
    if (displacement < INT8_MIN || displacement > INT8_MAX)
        panic(mv_string("displacement in sib8 exceeds int8_t bounds"));
#endif
    return (Location) {
        .type = Dest_Deref,
        .sz = sz,
        .reg = base,
        .index = index,
        .is_scale = true,
        .scale = (int8_t)scale,

        .disp_sz = 1, 
        .disp_8 = (int8_t)displacement,
    };
}

Location sib_32(Regname base, LocationSize sz, Regname index, int64_t scale, int32_t displacement) {
#ifdef DEBUG_ASSERT
    if (scale < INT32_MIN || scale > INT32_MAX)
        panic(mv_string("scale in sib32 exceeds int32_t bounds"));
#endif
    return (Location) {
        .type = Dest_Deref,
        .sz = sz,
        .reg = base,
        .index = index,
        .is_scale = true,
        .scale = (int32_t)scale,
        .disp_sz = 4, 
        .disp_32 = (int32_t)displacement,
    };
}

Location imm8(int64_t immediate) {
#ifdef DEBUG_ASSERT
  if (immediate > INT8_MAX || immediate < INT8_MIN) {
      panic(mv_string("immediate out of bounds"));
  }
#endif
    return (Location) {
      .type = Dest_Immediate,
      .sz = sz_8,
      .immediate_8 = (int8_t)immediate,
    };
}
Location imm16(int16_t immediate) {
    return (Location) {
      .type = Dest_Immediate,
      .sz = sz_16,
      .immediate_16 = immediate,
    };
}

Location imm32(int32_t immediate) {
    return (Location) {
       .type = Dest_Immediate,
       .sz = sz_32,
       .immediate_32 = immediate,
    };
}

Location imm64(int64_t immediate) {
    return (Location) {
      .type = Dest_Immediate,
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
    // Validity
    bool valid;
    // Prefixes
    bool use_size_override_prefix;
    bool use_rex_byte;
    uint8_t init_rex_byte;

    // Body of the instruction
    bool use_modrm_byte;
    uint8_t num_immediate_bytes;

    EncOrder order;

    bool has_opcode_ext;
} BinaryTableEntry;

static BinaryTableEntry binary_table[256];

typedef struct {
    uint8_t b1;
    uint8_t b2;
    uint8_t b3;
    uint8_t reg_ext;
} BinOpBytes;

typedef struct {
    uint32_t supported;
    BinOpBytes* entries;
    CPUFeatureFlags flags;
} BinaryOpTable;

static BinaryOpTable binary_opcode_tables[Binary_Op_Count];

uint8_t bindex(Dest_t dest_ty, LocationSize dest_sz, Dest_t src_ty, LocationSize src_sz) {
    return dest_ty | (dest_sz << 2) | (src_ty << 4) | (src_sz << 6) ;
};

void build_binary_table() {
    // populate the table with invalid entries
    for (size_t i = 0; i < 256; i++) {
        binary_table[i].valid = false;
    }

    // r/m64, imm8-64
    binary_table[bindex(Dest_Register, sz_64, Dest_Immediate, sz_8)] = (BinaryTableEntry){
        .valid = true,
        .use_size_override_prefix = false,
        .use_rex_byte = true,
        .init_rex_byte = 0b01001000, // REX.W
        .use_modrm_byte = true,
        .num_immediate_bytes = 1,
        .order = MI,
        .has_opcode_ext = true,
    };
    binary_table[bindex(Dest_Register, sz_64, Dest_Immediate, sz_32)] = (BinaryTableEntry){
        .valid = true,
        .use_size_override_prefix = false,
        .use_rex_byte = true,
        .init_rex_byte = 0b01001000, // REX.W
        .use_modrm_byte = true,
        .num_immediate_bytes = 4,
        .order = MI,
        .has_opcode_ext = true,
    };
    binary_table[bindex(Dest_Deref, sz_64, Dest_Immediate, sz_8)] = (BinaryTableEntry){
        .valid = true,
        .use_size_override_prefix = false,
        .use_rex_byte = true,
        .init_rex_byte = 0b01001000, // REX.W
        .use_modrm_byte = true,
        .num_immediate_bytes = 1,
        .order = MI,
        .has_opcode_ext = true,
    };
    binary_table[bindex(Dest_Deref, sz_64, Dest_Immediate, sz_32)] = (BinaryTableEntry){
        .valid = true,
        .use_size_override_prefix = false,
        .use_rex_byte = true,
        .init_rex_byte = 0b01001000, // REX.W
        .use_modrm_byte = true,
        .num_immediate_bytes = 4,
        .order = MI,
        .has_opcode_ext = true,
    };

    // r/m64, r64
    binary_table[bindex(Dest_Register, sz_64, Dest_Register, sz_64)] = (BinaryTableEntry){
        .valid = true,
        .use_size_override_prefix = false,
        .use_rex_byte = true,
        .init_rex_byte = 0b01001000, // REX.W
        .use_modrm_byte = true,
        .num_immediate_bytes = 0,
        .order = MR,
        .has_opcode_ext = false,
    };
    binary_table[bindex(Dest_Deref, sz_64, Dest_Register, sz_64)] = (BinaryTableEntry){
        .valid = true,
        .use_size_override_prefix = false,
        .use_rex_byte = true,
        .init_rex_byte = 0b01001000, // REX.W
        .use_modrm_byte = true,
        .num_immediate_bytes = 0,
        .order = MR,
        .has_opcode_ext = false,
    };


    // r64, r/m64
    binary_table[bindex(Dest_Register, sz_64, Dest_Register, sz_64)] = (BinaryTableEntry){
        .valid = true,
        .use_size_override_prefix = false,
        .use_rex_byte = true,
        .init_rex_byte = 0b01001000, // REX.W
        .use_modrm_byte = true,
        .num_immediate_bytes = 0,
        .order = RM,
        .has_opcode_ext = false,
    };
    binary_table[bindex(Dest_Register, sz_64, Dest_Deref, sz_64)] = (BinaryTableEntry){
        .valid = true,
        .use_size_override_prefix = false,
        .use_rex_byte = true,
        .init_rex_byte = 0b01001000, // REX.W
        .use_modrm_byte = true,
        .num_immediate_bytes = 0,
        .order = RM,
        .has_opcode_ext = false,
    };

    // r64, imm64
    binary_table[bindex(Dest_Register, sz_64, Dest_Immediate, sz_64)] = (BinaryTableEntry){
        .valid = true,
        .use_size_override_prefix = false,
        .use_rex_byte = true,
        .init_rex_byte = 0b01001000, // REX.W
        .use_modrm_byte = false,
        .num_immediate_bytes = 8,
        .order = OI,
        .has_opcode_ext = false,
    };

    // m32, r32
    binary_table[bindex(Dest_Deref, sz_32, Dest_Register, sz_32)] = (BinaryTableEntry){
        .valid = true,
        .use_size_override_prefix = false,
        .use_rex_byte = false,
        .use_modrm_byte = true,
        .num_immediate_bytes = 0,
        .order = MR,
        .has_opcode_ext = false,
    };

    // r32, r/m32
    binary_table[bindex(Dest_Register, sz_32, Dest_Register, sz_32)] = (BinaryTableEntry){
        .valid = true,
        .use_size_override_prefix = false,
        .use_rex_byte = false,
        .use_modrm_byte = true,
        .num_immediate_bytes = 0,
        .order = RM,
        .has_opcode_ext = false,
    };
    binary_table[bindex(Dest_Register, sz_32, Dest_Deref, sz_32)] = (BinaryTableEntry){
        .valid = true,
        .use_size_override_prefix = false,
        .use_rex_byte = false,
        .use_modrm_byte = true,
        .num_immediate_bytes = 0,
        .order = RM,
        .has_opcode_ext = false,
    };

    // r32, imm32
    binary_table[bindex(Dest_Register, sz_32, Dest_Immediate, sz_32)] = (BinaryTableEntry){
        .valid = true,
        .use_size_override_prefix = false,
        .use_rex_byte = false,
        .use_modrm_byte = true,
        .num_immediate_bytes = 4,
        .order = MI,
        .has_opcode_ext = false,
    };

    // m16, r16
    binary_table[bindex(Dest_Deref, sz_16, Dest_Register, sz_16)] = (BinaryTableEntry){
        .valid = true,
        .use_size_override_prefix = true,
        .use_rex_byte = false,
        .use_modrm_byte = true,
        .num_immediate_bytes = 0,
        .order = MR,
        .has_opcode_ext = false,
    };
    // r16, r/m16
    binary_table[bindex(Dest_Register, sz_16, Dest_Register, sz_16)] = (BinaryTableEntry){
        .valid = true,
        .use_size_override_prefix = true,
        .use_rex_byte = false,
        .use_modrm_byte = true,
        .num_immediate_bytes = 0,
        .order = RM,
        .has_opcode_ext = false,
    };
    binary_table[bindex(Dest_Register, sz_16, Dest_Deref, sz_16)] = (BinaryTableEntry){
        .valid = true,
        .use_size_override_prefix = true,
        .use_rex_byte = false,
        .use_modrm_byte = true,
        .num_immediate_bytes = 0,
        .order = RM,
        .has_opcode_ext = false,
    };

    // r16, imm16
    binary_table[bindex(Dest_Register, sz_16, Dest_Immediate, sz_16)] = (BinaryTableEntry){
        .valid = true,
        .use_size_override_prefix = true,
        .use_rex_byte = false,
        .use_modrm_byte = true,
        .num_immediate_bytes = 2,
        .order = MI,
        .has_opcode_ext = false,
    };

    // m8, r8
    binary_table[bindex(Dest_Deref, sz_8, Dest_Register, sz_8)] = (BinaryTableEntry){
        .valid = true,
        .use_size_override_prefix = false,
        .use_rex_byte = true,
        .init_rex_byte = 0b01000000, // REX
        .use_modrm_byte = true,
        .num_immediate_bytes = 0,
        .order = MR,
        .has_opcode_ext = false,
    };

    // r8, r/m8
    binary_table[bindex(Dest_Register, sz_8, Dest_Register, sz_8)] = (BinaryTableEntry){
        .valid = true,
        .use_size_override_prefix = false,
        .use_rex_byte = true,
        .init_rex_byte = 0b01000000, // REX
        .use_modrm_byte = true,
        .num_immediate_bytes = 0,
        .order = RM,
        .has_opcode_ext = false,
    };
    binary_table[bindex(Dest_Register, sz_8, Dest_Deref, sz_8)] = (BinaryTableEntry){
        .valid = true,
        .use_size_override_prefix = false,
        .use_rex_byte = true,
        .init_rex_byte = 0b01000000, // REX
        .use_modrm_byte = true,
        .num_immediate_bytes = 0,
        .order = RM,
        .has_opcode_ext = false,
    };

    // r8, imm8
    binary_table[bindex(Dest_Register, sz_8, Dest_Immediate, sz_8)] = (BinaryTableEntry){
        .valid = true,
        .use_size_override_prefix = false,
        .use_rex_byte = true,
        .init_rex_byte = 0b01000000, // REX
        .use_modrm_byte = true,
        .num_immediate_bytes = 1,
        .order = MI,
        .has_opcode_ext = false,
    };

}

void add_op_ext(uint8_t op, uint8_t ext, BinOpType type, uint32_t supported, BinOpBytes* ops) {
#ifdef DEBUG_ASSERT
    if (!(type & supported)) {
        panic(mv_string("Error in setup: unsupported binop type."));
    }
#endif
    size_t idx = __builtin_popcount(supported & (type - 1));
    ops[idx] = (BinOpBytes) {.b1 = op, .b2 = 0x90, .b3 = 0x90, .reg_ext = ext};
}

void add_op(uint8_t op, BinOpType type, uint32_t supported, BinOpBytes* ops) {
#ifdef DEBUG_ASSERT
    if (!(type & supported)) {
        panic(mv_string("Error in setup: unsupported binop type."));
    }
#endif
    size_t idx = __builtin_popcount(supported & (type - 1));
    ops[idx] = (BinOpBytes) {.b1 = op, .b2 = 0x90, .b3 = 0x90, .reg_ext = 0x9};
}

void add_op2(uint8_t op1, uint8_t op2, BinOpType type, uint32_t supported, BinOpBytes* ops) {
#ifdef DEBUG_ASSERT
    if (!(type & supported)) {
        panic(mv_string("Error in setup: unsupported binop type."));
    }
#endif
    size_t idx = __builtin_popcount(supported & (type - 1));
    ops[idx] = (BinOpBytes) {.b1 = op1, .b2 = op2, .b3 = 0x90, .reg_ext = 0x9};
}

void add_op3(uint8_t op1, uint8_t op2, uint8_t op3, BinOpType type, uint32_t supported, BinOpBytes* ops) {
#ifdef DEBUG_ASSERT
    if (!(type & supported)) {
        panic(mv_string("Error in setup: unsupported binop type."));
    }
#endif
    size_t idx = __builtin_popcount(supported & (type - 1));
    ops[idx] = (BinOpBytes) {.b1 = op1, .b2 = op2, .b3 = op3, .reg_ext = 0x9};
}

void build_binary_opcode_tables() {
    for (size_t i = 0; i < Binary_Op_Count; i++) {
        binary_opcode_tables[i] = (BinaryOpTable){0, NULL, 0};
    }

    {   // Add Operation. Source - Intel Manual Vol 2. p142
        static uint32_t sup = RM64_Imm8 | RM64_Imm32 | RM64_R64 | R64_RM64 | R32_RM32 | R16_RM16 | R8_RM8;
        static BinOpBytes ops[7];
        add_op_ext(0x83, 0x0, RM64_Imm8, sup, ops);
        add_op_ext(0x81, 0x0, RM64_Imm32, sup, ops);

        add_op(0x01, RM64_R64, sup, ops);
        add_op(0x03, R64_RM64, sup, ops);

        add_op(0x03, R32_RM32, sup, ops);
        add_op(0x03, R16_RM16, sup, ops);
        add_op(0x02, R8_RM8, sup, ops);
        binary_opcode_tables[Add] = (BinaryOpTable) {
            .supported = sup,
            .entries = ops,
        };
    }

    {   // Sub Operation. Source - Intel Manual Vol 2. p1407
        static uint32_t sup = RM64_Imm8 | RM64_Imm32 | RM64_R64 | R64_RM64 | R32_RM32 | R16_RM16 | R8_RM8;
        static BinOpBytes ops[7];
        add_op_ext(0x83, 0x5, RM64_Imm8, sup, ops);
        add_op_ext(0x81, 0x5, RM64_Imm32, sup, ops);

        add_op(0x29, RM64_R64, sup, ops);
        add_op(0x2B, R64_RM64, sup, ops);

        add_op(0x2B, R32_RM32, sup, ops);
        add_op(0x2B, R16_RM16, sup, ops);
        add_op(0x2A, R8_RM8, sup, ops);
        binary_opcode_tables[Sub] = (BinaryOpTable) {
            .supported = sup,
            .entries = ops,
        };
    }

    {   // Cmp Operation. Source - Intel Manual Vol 2. p1407
        static uint32_t sup = RM64_Imm8 | RM64_Imm32 | RM64_R64 | R64_RM64 | R32_RM32 | R16_RM16 | R8_RM8;
        static BinOpBytes ops[7];
        add_op_ext(0x83, 0x7, RM64_Imm8, sup, ops);
        add_op_ext(0x81, 0x7, RM64_Imm32, sup, ops);

        add_op(0x39, RM64_R64, sup, ops);
        add_op(0x3B, R64_RM64, sup, ops);

        add_op(0x3B, R32_RM32, sup, ops);
        add_op(0x3B, R16_RM16, sup, ops);
        add_op(0x3A, R8_RM8, sup, ops);
        binary_opcode_tables[Cmp] = (BinaryOpTable) {
            .supported = sup,
            .entries = ops,
        };
    }

    // ------------------
    //  Logic
    // ------------------

    {   // And Operation. Source - Intel Manual Vol 2. p1407
        static uint32_t sup = RM64_Imm8 | RM64_Imm32 | RM64_R64 | R64_RM64 | R8_RM8;
        static BinOpBytes ops[5];
        add_op_ext(0x83, 0x4, RM64_Imm8, sup, ops);
        add_op_ext(0x81, 0x4, RM64_Imm32, sup, ops);

        add_op(0x21, RM64_R64, sup, ops);
        add_op(0x23, R64_RM64, sup, ops);

        add_op(0x22, R8_RM8, sup, ops);
        binary_opcode_tables[And] = (BinaryOpTable) {
            .supported = sup,
            .entries = ops,
        };
    }

    {   // Or Operation. Source - Intel Manual Vol 2. p902
        static uint32_t sup = RM64_Imm8 | RM64_Imm32 | RM64_R64 | R64_RM64 | R8_RM8;
        static BinOpBytes ops[5];
        add_op_ext(0x83, 0x1, RM64_Imm8, sup, ops);
        add_op_ext(0x81, 0x1, RM64_Imm32, sup, ops);

        add_op(0x09, RM64_R64, sup, ops);
        add_op(0x0B, R64_RM64, sup, ops);

        add_op(0x0A, R8_RM8, sup, ops);
        binary_opcode_tables[Or] = (BinaryOpTable) {
            .supported = sup,
            .entries = ops,
        };
    }

    {   // XOr Operation. Source - Intel Manual Vol 2. p2236
        static uint32_t sup = RM64_Imm8 | RM64_Imm32 | RM64_R64 | R64_RM64 | R8_RM8;
        static BinOpBytes ops[5];
        add_op_ext(0x83, 0x6, RM64_Imm8, sup, ops);
        add_op_ext(0x81, 0x6, RM64_Imm32, sup, ops);

        add_op(0x31, RM64_R64, sup, ops);
        add_op(0x33, R64_RM64, sup, ops);

        add_op(0x32, R8_RM8, sup, ops);
        binary_opcode_tables[Xor] = (BinaryOpTable) {
            .supported = sup,
            .entries = ops,
        };
    }

    // ------------------
    //  Bit Manipulation
    // ------------------
    {   // Shift Left. Source - Intel Manual Vol 2. ??
        static uint32_t sup = RM64_Imm8;
        static BinOpBytes ops[1];
        add_op_ext(0xC1, 0x4, RM64_Imm8, sup, ops);
        binary_opcode_tables[SHL] = (BinaryOpTable) {
            .supported = sup,
            .entries = ops,
        };
    }

    {   // Shift Right. Source - Intel Manual Vol 2. ??
        static uint32_t sup = RM64_Imm8;
        static BinOpBytes ops[1];
        add_op_ext(0xC1, 0x5, RM64_Imm8, sup, ops);
        binary_opcode_tables[SHR] = (BinaryOpTable) {
            .supported = sup,
            .entries = ops,
        };
    }

    // ------------------
    //  Memory
    // ------------------

    {   // Move. Source - Intel Manual Vol 2. 769
      static uint32_t sup = R64_Imm64 | RM64_Imm32 | RM64_R64 | R64_RM64 |
                            RM32_R32 | R32_RM32 | RM16_R16 | R16_RM16 | RM8_R8 |
                            R8_RM8 | RM32_Imm32 | RM16_Imm16 | R8_Imm8;
        static BinOpBytes ops[14];
        add_op(0xB8, R64_Imm64, sup, ops);
        add_op_ext(0xC7, 0x0, RM64_Imm32, sup, ops);
        add_op_ext(0xC7, 0x0, RM64_Imm32, sup, ops);

        add_op(0x89, RM64_R64, sup, ops);
        add_op(0x8B, R64_RM64, sup, ops);
        add_op(0x89, RM32_R32, sup, ops);
        add_op(0x8B, R32_RM32, sup, ops);
        add_op(0x89, RM16_R16, sup, ops);
        add_op(0x8B, R16_RM16, sup, ops);
        add_op(0x88, RM8_R8, sup, ops);
        add_op(0x8A, R8_RM8, sup, ops);

        add_op_ext(0xC7, 0x0, RM32_Imm32, sup, ops);
        add_op_ext(0xC7, 0x0, RM16_Imm16, sup, ops);
        add_op(0xC6, R8_Imm8, sup, ops);
        binary_opcode_tables[Mov] = (BinaryOpTable) {
            .supported = sup,
            .entries = ops,
        };
    }

    {   // Move Float. Source - Intel Manual Vol 2. 857
        static uint32_t sup = XMM32_XMM32 | M32_XMM32 | XMM32_M32;
        static BinOpBytes ops[3];
        add_op3(0xF3, 0x0F, 0x10, XMM32_XMM32, sup, ops);
        add_op3(0xF3, 0x0F, 0x10, XMM32_M32, sup, ops);
        add_op3(0xF3, 0x0F, 0x11, M32_XMM32, sup, ops);

        binary_opcode_tables[MovSS] = (BinaryOpTable) {
            .supported = sup,
            .entries = ops,
        };
    }

    {   // Load Effective Address. Source - Intel Manual Vol 2. 705
        // Lea is much more limited in how it works - operand 1 is always a register
        //      and operand 2 is a memory location.
        // TODO (BUG): This restriction is (will be) enforced elsewhere by a
        //   check_special_conditions() function for binary operations
        static uint32_t sup = R64_RM64;
        static BinOpBytes ops[1];
        add_op(0x8D, R64_RM64, sup, ops);
        binary_opcode_tables[LEA] = (BinaryOpTable) {
            .supported = sup,
            .entries = ops,
        };
    }


    // -------------------
    //  Conditional Moves
    // -------------------
    // CMoves, p 285.

    {   // Move if equal.
        static uint32_t sup = R64_RM64;
        static BinOpBytes ops[1];
        add_op2(0x0F, 0x44, R64_RM64, sup, ops);
        binary_opcode_tables[CMovE] = (BinaryOpTable) {
            .supported = sup,
            .entries = ops,
        };
    }

    {   // Move if below.
        static uint32_t sup = R64_RM64;
        static BinOpBytes ops[1];
        add_op2(0x0F, 0x42, R64_RM64, sup, ops);
        binary_opcode_tables[CMovB] = (BinaryOpTable) {
            .supported = sup,
            .entries = ops,
        };
    }

    {   // Move if above.
        static uint32_t sup = R64_RM64;
        static BinOpBytes ops[1];
        add_op2(0x0F, 0x47, R64_RM64, sup, ops);
        binary_opcode_tables[CMovA] = (BinaryOpTable) {
            .supported = sup,
            .entries = ops,
        };
    }

    {   // Move if less.
        static uint32_t sup = R64_RM64;
        static BinOpBytes ops[1];
        add_op2(0x0F, 0x4C, R64_RM64, sup, ops);
        binary_opcode_tables[CMovL] = (BinaryOpTable) {
            .supported = sup,
            .entries = ops,
        };
    }

    {   // Move if Greater.
        static uint32_t sup = R64_RM64;
        static BinOpBytes ops[1];
        add_op2(0x0F, 0x4F, R64_RM64, sup, ops);
        binary_opcode_tables[CMovG] = (BinaryOpTable) {
            .supported = sup,
            .entries = ops,
        };
    }
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

BinOpType retry_type(BinOpType type) {
    switch (type) {
    case R8_Imm8:
        return RM8_Imm8;
    case R16_Imm16:
        return RM16_Imm16;
    case R32_Imm32:
        return RM32_Imm32;
    case R64_Imm8:
        return RM64_Imm8;
    default:
        return 0;
    }
}

BinOpBytes lookup_binop_bytes(BinaryOp op, Location dest, Location src, Allocator* err_allocator, ErrorPoint* point) {
    BinOpType type = 0;

    // Step 1: determine memory type
    switch (dest.type) {
    case Dest_Register:
      // XMM register?
      if (dest.reg & XMM0) {
          if (src.type == Dest_Register && (src.reg & XMM0) && src.sz == dest.sz) {
              if (src.sz == sz_32) type = XMM32_XMM32;
              else if (src.sz == sz_64) type = XMM64_XMM64;
          } else if (src.type == Dest_Deref && src.sz == dest.sz) {
            if (src.sz == sz_32) {
                type = XMM32_M32;
            } else if (src.sz == sz_64) {
                type = XMM64_M64;
            } else {
              goto report_error;
            }
          } else {
              goto report_error;
          }
      } else {
          if (src.type == Dest_Immediate) {
              if (src.sz == sz_8) {
                  if (dest.sz == sz_8) {
                      type = R8_Imm8;
                  } else if (dest.sz == sz_16) {
                      type = RM16_Imm8;
                  } else if (dest.sz == sz_32) {
                      type = RM32_Imm8;
                  } else if (dest.sz == sz_64) {
                      type = R64_Imm8;
                  } else {
                      goto report_error;
                  }
              } else if (src.sz == sz_16 && dest.sz == sz_16) {
                  type = R16_Imm16;
              } else if (src.sz == sz_32) {
                if (dest.sz == sz_32) {
                      type = R32_Imm32;
                } else if (dest.sz == sz_64) {
                    // TODO : two-type this
                    type = RM64_Imm32;
                } else {
                    goto report_error;
                }
              } else if ((src.sz == sz_64) & (dest.sz == sz_64)) {
                  type = R64_Imm64;
              }
              else {
                  goto report_error;
              }
          } else {
              if (src.sz == dest.sz) {
                if (src.sz == sz_8) {
                    type = R8_RM8;
                } else if (src.sz == sz_16) {
                    type = R16_RM16;
                } else if (src.sz == sz_32) {
                    type = R32_RM32;
                } else if (src.sz == sz_64) {
                    type = R64_RM64;
                } else {
                    goto report_error;
                }
              } else {
                  goto report_error;
              }
          }
      }
      break;
    case Dest_Deref:
        if (src.type == Dest_Immediate) {
            switch (dest.sz) {
            case sz_8:
                goto report_error;
            case sz_16:
                if (src.sz == sz_16) type = RM16_Imm16;
                else goto report_error;
                break;
            case sz_32:
                if (src.sz == sz_32) type = RM32_Imm32;
                else goto report_error;
                break;
            case sz_64:
                if (src.sz == sz_8) type = RM64_Imm8;
                else if (src.sz == sz_32) type = RM64_Imm32;
                else goto report_error;
                break;
            }
        } else if (src.type == Dest_Register) {
            if (src.sz == dest.sz) {
                if (src.reg & XMM0) {
                    if (src.sz == dest.sz) {
                        if (src.sz == sz_32) {
                            type = M32_XMM32;
                        } else if (src.sz == sz_64) {
                            type = M64_XMM64;
                        } else {
                            goto report_error;
                        }
                    } else {
                        goto report_error;
                    }
                } else {
                    if (src.sz == sz_8)
                        type = RM8_R8;
                    else if (src.sz == sz_16)
                        type = RM16_R16;
                    else if  (src.sz == sz_32)
                        type = RM32_R32;
                    else if (src.sz == sz_64)
                        type = RM64_R64;
                }
            } else {
                goto report_error;
            }

        } else {
            goto report_error;
        }
        break;
    case Dest_Immediate:
        goto report_error;
    }

    if (type != 0) {
        BinaryOpTable table = binary_opcode_tables[op];
        if (!(table.supported & type)) {
            type = retry_type(type);
            if (!(table.supported & type)) goto report_error;
        }
        size_t idx = __builtin_popcount(table.supported & (type  - 1));
        return table.entries[idx];
    }

 report_error: {
        PtrArray nodes = mk_ptr_array(8, err_allocator);
        push_ptr(mk_str_doc(mv_string("Unsupported operand pair for: "), err_allocator), &nodes);
        push_ptr(pretty_binary_instruction(op, dest, src, err_allocator), &nodes);
        throw_error(point, doc_to_str(mv_cat_doc(nodes, err_allocator), 80, err_allocator));
    }
}

AsmResult build_binary_op(Assembler* assembler, BinaryOp op, Location dest, Location src, Allocator* err_allocator, ErrorPoint* point) {
    BinaryTableEntry be = binary_table[bindex(dest.type, dest.sz, src.type, src.sz)];
    if (!be.valid) {
        PtrArray nodes = mk_ptr_array(8, err_allocator);
        push_ptr(mk_str_doc(mv_string("Invalid binary table entry for: "), err_allocator), &nodes);
        push_ptr(pretty_binary_instruction(op, dest, src, err_allocator), &nodes);
        throw_error(point, doc_to_str(mv_cat_doc(nodes, err_allocator), 80, err_allocator));
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
    BinOpBytes opcode_bytes = lookup_binop_bytes(op, dest, src, err_allocator, point);
    opcode_byte = opcode_bytes.b1;
    if (opcode_byte == 0x90) {
        PtrArray nodes = mk_ptr_array(8, err_allocator);
        push_ptr(mk_str_doc(mv_string("Invalid binary opcode table entry for: "), err_allocator), &nodes);
        push_ptr(pretty_binary_instruction(op, dest, src, err_allocator), &nodes);
        throw_error(point, doc_to_str(mv_cat_doc(nodes, err_allocator), 80, err_allocator));
    }
    if (be.has_opcode_ext) {
        uint8_t ext_byte = opcode_bytes.reg_ext; 
        modrm_byte |= modrm_reg(ext_byte);
        if (ext_byte == 0x09) {
            PtrArray nodes = mk_ptr_array(8, err_allocator);
            push_ptr(mk_str_doc(mv_string("Invalid binary opcode extension entry for: "), err_allocator), &nodes);
            push_ptr(pretty_binary_instruction(op, dest, src, err_allocator), &nodes);
            throw_error(point, doc_to_str(mv_cat_doc(nodes, err_allocator), 80, err_allocator));
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
        case Dest_Register:
            if (rm_loc.reg == RIP) {
                throw_error(point, mv_string("Using RIP as a register is invalid"));
            }
            // Simplest : mod = 11, rm = register 
            modrm_byte |= modrm_mod(0b11);
            modrm_byte |= modrm_rm(rm_loc.reg);
            rex_byte |= rex_rm_ext((rm_loc.reg & 0b1000) >> 3); 
            break;
            
        case Dest_Deref:
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
        case Dest_Immediate:
            throw_error(point, mv_string("Internal error in build_binary_op: rm_loc is immediate."));
            break;
        }

        // Step 4: Reg encoding
        // Store the Reg location
        // TODO (INVESTIGATE): what if reg_loc is not register? (seems inserting
        //    throw results in spurious errors)
        if (reg_loc.type == Dest_Register) {
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
    if (be.use_size_override_prefix)
        push_u8(0x66, instructions);

    if (be.use_rex_byte)
        push_u8(rex_byte, instructions);

    // opcode
    push_u8(opcode_byte, instructions);

    // 2nd & 3rd opcode bytes (optional)
    if (0x90 != opcode_bytes.b2) {
        push_u8(opcode_bytes.b2, instructions);
    }
    if (0x90 != opcode_bytes.b3) {
        push_u8(opcode_bytes.b3, instructions);
    }

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
    bool use_size_prefix;
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
    unary_table[uindex(Dest_Register, sz_64)] = (UnaryTableEntry){
        .valid = true,
        .use_size_prefix = false,
        .use_modrm_byte = true,
        .num_immediate_bytes = 0,
        .order = M,
    };
    unary_table[uindex(Dest_Deref, sz_64)] = (UnaryTableEntry){
        .valid = true,
        .use_size_prefix = false,
        .use_modrm_byte = true,
        .num_immediate_bytes = 0,
        .order = M,
    };
    unary_table[uindex(Dest_Register, sz_32)] = (UnaryTableEntry){
        .valid = true,
        .use_size_prefix = false,
        .use_modrm_byte = true,
        .num_immediate_bytes = 0,
        .order = M,
    };
    unary_table[uindex(Dest_Deref, sz_32)] = (UnaryTableEntry){
        .valid = true,
        .use_size_prefix = false,
        .use_modrm_byte = true,
        .num_immediate_bytes = 0,
        .order = M,
    };
    unary_table[uindex(Dest_Register, sz_16)] = (UnaryTableEntry){
        .valid = true,
        .use_size_prefix = true,
        .use_modrm_byte = true,
        .num_immediate_bytes = 0,
        .order = M,
    };
    unary_table[uindex(Dest_Deref, sz_16)] = (UnaryTableEntry){
        .valid = true,
        .use_size_prefix = true,
        .use_modrm_byte = true,
        .num_immediate_bytes = 0,
        .order = M,
    };
    unary_table[uindex(Dest_Register, sz_8)] = (UnaryTableEntry){
        .valid = true,
        .use_size_prefix = false,
        .use_modrm_byte = true,
        .num_immediate_bytes = 0,
        .order = M,
    };
    unary_table[uindex(Dest_Deref, sz_8)] = (UnaryTableEntry){
        .valid = true,
        .use_size_prefix = false,
        .use_modrm_byte = true,
        .num_immediate_bytes = 0,
        .order = M,
    };

    unary_table[uindex(Dest_Immediate, sz_8)] = (UnaryTableEntry){
        .valid = true,
        .use_size_prefix = false,
        .use_modrm_byte = false,
        .num_immediate_bytes = 1,
        .order = I,
    };
    unary_table[uindex(Dest_Immediate, sz_16)] = (UnaryTableEntry){
        .valid = true,
        .use_size_prefix = true,
        .use_modrm_byte = false,
        .num_immediate_bytes = 2,
        .order = I,
    };
    unary_table[uindex(Dest_Immediate, sz_32)] = (UnaryTableEntry){
        .valid = true,
        .use_size_prefix = false,
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
    unary_opcode_table[Call][uindex(Dest_Register, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xFF, .opcode_modrm = 0x2};
    unary_opcode_table[Call][uindex(Dest_Deref, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xFF, .opcode_modrm = 0x2};

    // Push
    // r/m 64, imm8,32
    unary_opcode_table[Push][uindex(Dest_Register, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xFF, .opcode_modrm = 0x6};
    unary_opcode_table[Push][uindex(Dest_Deref, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xFF, .opcode_modrm = 0x6};

    unary_opcode_table[Push][uindex(Dest_Immediate, sz_8)] =
        (UnaryOpEntry) {.opcode = 0x6A,};
    // 16-bit pushes get extended to 32??
    /* unary_opcode_table[Push][uindex(Immediate, sz_16)] = */
    /*     (UnaryOpEntry) {.opcode = 0x68, .opcode_modrm = 0x6, .init_rex_byte = 0x0}; */
    unary_opcode_table[Push][uindex(Dest_Immediate, sz_32)] =
        (UnaryOpEntry) {.opcode = 0x68,};

    unary_opcode_table[Pop][uindex(Dest_Register, sz_64)] =
        (UnaryOpEntry) {.opcode = 0x8F,};
    unary_opcode_table[Pop][uindex(Dest_Deref, sz_64)] =
        (UnaryOpEntry) {.opcode = 0x8F,};

    // ------------------
    //  Jumps
    // ------------------
    // jumps are imm/8, and imm/32 (64-bit mode doesn't support 16-bit jumps)
    unary_opcode_table[JE][uindex(Dest_Immediate, sz_8)] =
        (UnaryOpEntry) {.opcode = 0x74,};
    unary_opcode_table[JE][uindex(Dest_Immediate, sz_32)] =
        (UnaryOpEntry) {.opcode_prefix = 0x0F, .opcode = 0x84,};

    unary_opcode_table[JNE][uindex(Dest_Immediate, sz_8)] =
        (UnaryOpEntry) {.opcode = 0x75,};
    unary_opcode_table[JNE][uindex(Dest_Immediate, sz_32)] =
        (UnaryOpEntry) {.opcode_prefix = 0x0F, .opcode = 0x85,};

    unary_opcode_table[JMP][uindex(Dest_Immediate, sz_8)] =
        (UnaryOpEntry) {.opcode = 0xEB,};
    unary_opcode_table[JMP][uindex(Dest_Immediate, sz_32)] =
        (UnaryOpEntry) {.opcode = 0xE9, };

    unary_opcode_table[JMP][uindex(Dest_Register, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xFF, .opcode_modrm = 0x4,};
    unary_opcode_table[JMP][uindex(Dest_Deref, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xFF, .opcode_modrm = 0x4,};

    // --------------------------------
    //  Set Byte based on flag, logic
    // --------------------------------
    unary_opcode_table[Not][uindex(Dest_Register, sz_8)] =
        (UnaryOpEntry) {.opcode = 0xF6, .opcode_modrm = 0x2};
    unary_opcode_table[Not][uindex(Dest_Register, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x2};

    // TODO (BUG): in the future, change this to sz_8, as only sets r/m 8!
    unary_opcode_table[SetE][uindex(Dest_Register, sz_64)] =
        (UnaryOpEntry) {.opcode_prefix = 0x0F, .opcode = 0x94,};
    unary_opcode_table[SetNE][uindex(Dest_Register, sz_64)] =
        (UnaryOpEntry) {.opcode_prefix = 0x0F, .opcode = 0x95,};
    // TODO (FEAT): when sz/8 becomes available for Deref, enable this!
    /* unary_opcode_table[SetE][uindex(Deref, sz_64)] = */
    /*     (UnaryOpEntry) {.opcode = 0xFF, .opcode_modrm = 0x4,}; */

    unary_opcode_table[SetA][uindex(Dest_Register, sz_64)] =
        (UnaryOpEntry) {.opcode_prefix = 0x0F, .opcode = 0x97,};
    unary_opcode_table[SetAE][uindex(Dest_Register, sz_64)] =
        (UnaryOpEntry) {.opcode_prefix = 0x0F, .opcode = 0x93,};
    unary_opcode_table[SetB][uindex(Dest_Register, sz_64)] =
        (UnaryOpEntry) {.opcode_prefix = 0x0F, .opcode = 0x92,};
    unary_opcode_table[SetBE][uindex(Dest_Register, sz_64)] =
        (UnaryOpEntry) {.opcode_prefix = 0x0F, .opcode = 0x96,};
    unary_opcode_table[SetL][uindex(Dest_Register, sz_64)] =
        (UnaryOpEntry) {.opcode_prefix = 0x0F, .opcode = 0x9C,};
    unary_opcode_table[SetLE][uindex(Dest_Register, sz_64)] =
        (UnaryOpEntry) {.opcode_prefix = 0x0F, .opcode = 0x9E,};
    unary_opcode_table[SetG][uindex(Dest_Register, sz_64)] =
        (UnaryOpEntry) {.opcode_prefix = 0x0F, .opcode = 0x9F,};
    unary_opcode_table[SetGE][uindex(Dest_Register, sz_64)] =
        (UnaryOpEntry) {.opcode_prefix = 0x0F, .opcode = 0x9D,};

    // ------------------
    //  Arithmetic
    // ------------------

    unary_opcode_table[Neg][uindex(Dest_Register, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x03, .init_rex_byte = 0b01001000, /*REX.W*/};
    unary_opcode_table[Neg][uindex(Dest_Deref, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x03, .init_rex_byte = 0b01001000, /*REX.W*/};
    unary_opcode_table[Neg][uindex(Dest_Register, sz_32)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x03,};
    unary_opcode_table[Neg][uindex(Dest_Deref, sz_32)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x03, };
    unary_opcode_table[Neg][uindex(Dest_Register, sz_16)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x03,};
    unary_opcode_table[Neg][uindex(Dest_Deref, sz_16)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x03,};
    unary_opcode_table[Neg][uindex(Dest_Register, sz_8)] =
        (UnaryOpEntry) {.opcode = 0xF6, .opcode_modrm = 0x03, .init_rex_byte = 0b01000000, /*REX*/};
    unary_opcode_table[Neg][uindex(Dest_Deref, sz_8)] =
        (UnaryOpEntry) {.opcode = 0xF6, .opcode_modrm = 0x03, .init_rex_byte = 0b01000000, /*REX*/};

    unary_opcode_table[Mul][uindex(Dest_Register, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x04, .init_rex_byte = 0b01001000, /*REX.W*/};
    unary_opcode_table[Mul][uindex(Dest_Deref, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x04, .init_rex_byte = 0b01001000, /*REX.W*/};
    unary_opcode_table[Mul][uindex(Dest_Register, sz_32)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x04,};
    unary_opcode_table[Mul][uindex(Dest_Deref, sz_32)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x04, };
    unary_opcode_table[Mul][uindex(Dest_Register, sz_16)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x04,};
    unary_opcode_table[Mul][uindex(Dest_Deref, sz_16)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x04,};
    unary_opcode_table[Mul][uindex(Dest_Register, sz_8)] =
        (UnaryOpEntry) {.opcode = 0xF6, .opcode_modrm = 0x04, .init_rex_byte = 0b01000000, /*REX*/};
    unary_opcode_table[Mul][uindex(Dest_Deref, sz_8)] =
        (UnaryOpEntry) {.opcode = 0xF6, .opcode_modrm = 0x04, .init_rex_byte = 0b01000000, /*REX*/};

    unary_opcode_table[Div][uindex(Dest_Register, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x06, .init_rex_byte = 0b01001000, /*REX.W*/};
    unary_opcode_table[Div][uindex(Dest_Deref, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x06, .init_rex_byte = 0b01001000, /*REX.W*/};
    unary_opcode_table[Div][uindex(Dest_Register, sz_32)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x06,};
    unary_opcode_table[Div][uindex(Dest_Deref, sz_32)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x06,};
    unary_opcode_table[Div][uindex(Dest_Register, sz_16)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x06,};
    unary_opcode_table[Div][uindex(Dest_Deref, sz_16)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x06,};
    unary_opcode_table[Div][uindex(Dest_Register, sz_8)] =
        (UnaryOpEntry) {.opcode = 0xF6, .opcode_modrm = 0x06, .init_rex_byte = 0b01000000, /*REX*/};
    unary_opcode_table[Div][uindex(Dest_Deref, sz_8)] =
        (UnaryOpEntry) {.opcode = 0xF6, .opcode_modrm = 0x06, .init_rex_byte = 0b01000000, /*REX*/};

    unary_opcode_table[IMul][uindex(Dest_Register, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x05, .init_rex_byte = 0b01001000, /*REX.W*/};
    unary_opcode_table[IMul][uindex(Dest_Deref, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x05, .init_rex_byte = 0b01001000, /*REX.W*/};
    unary_opcode_table[IMul][uindex(Dest_Register, sz_32)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x05,};
    unary_opcode_table[IMul][uindex(Dest_Deref, sz_32)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x05,};
    unary_opcode_table[IMul][uindex(Dest_Register, sz_16)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x05,};
    unary_opcode_table[IMul][uindex(Dest_Deref, sz_16)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x05,};
    unary_opcode_table[IMul][uindex(Dest_Register, sz_8)] =
        (UnaryOpEntry) {.opcode = 0xF6, .opcode_modrm = 0x05, .init_rex_byte = 0b01000000, /*REX*/};
    unary_opcode_table[IMul][uindex(Dest_Deref, sz_8)] =
        (UnaryOpEntry) {.opcode = 0xF6, .opcode_modrm = 0x05, .init_rex_byte = 0b01000000, /*REX*/};

    unary_opcode_table[IDiv][uindex(Dest_Register, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x07, .init_rex_byte = 0b01001000, /*REX.W*/};
    unary_opcode_table[IDiv][uindex(Dest_Deref, sz_64)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x07, .init_rex_byte = 0b01001000, /*REX.W*/};
    unary_opcode_table[IDiv][uindex(Dest_Register, sz_32)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x07,};
    unary_opcode_table[IDiv][uindex(Dest_Deref, sz_32)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x07,};
    unary_opcode_table[IDiv][uindex(Dest_Register, sz_16)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x07,};
    unary_opcode_table[IDiv][uindex(Dest_Deref, sz_16)] =
        (UnaryOpEntry) {.opcode = 0xF7, .opcode_modrm = 0x07,};
    unary_opcode_table[IDiv][uindex(Dest_Register, sz_8)] =
        (UnaryOpEntry) {.opcode = 0xF6, .opcode_modrm = 0x07, .init_rex_byte = 0b01000000, /*REX*/};
    unary_opcode_table[IDiv][uindex(Dest_Deref, sz_8)] =
        (UnaryOpEntry) {.opcode = 0xF6, .opcode_modrm = 0x07, .init_rex_byte = 0b01000000, /*REX*/};
}

AsmResult build_unary_op(Assembler* assembler, UnaryOp op, Location loc, Allocator* err_allocator, ErrorPoint* point) {
    if (loc.type == Dest_Register && loc.reg == RIP) {
        throw_error(point, mv_string("RIP-relative addressing not supported for unary operations!"));
    }

    UnaryTableEntry ue = unary_table[uindex(loc.type, loc.sz)];
    if (!ue.valid) {
        Allocator* a = err_allocator;
        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(mk_str_doc(mv_string("Invalid unary table entry for op: "), a), &nodes);
        push_ptr(pretty_unary_instruction(op, loc, err_allocator), &nodes);
        throw_error(point, doc_to_str(mv_cat_doc(nodes, a), 80, a));
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
        Allocator* a = err_allocator;
        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(mk_str_doc(mv_string("Invalid unary opcode table entry for op: "), a), &nodes);
        push_ptr(pretty_unary_instruction(op, loc, err_allocator), &nodes);
        throw_error(point, doc_to_str(mv_cat_doc(nodes, a), 80, a));
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
        case Dest_Register:
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
            
        case Dest_Deref:
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
        case Dest_Immediate:
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
    if (ue.use_size_prefix)
        push_u8(0x66, instructions);

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

AsmResult build_nullary_op(Assembler* assembler, NullaryOp op, Allocator* err_allocator, ErrorPoint* point) {
    uint8_t opcode;
    bool use_rex_byte = false;
    uint8_t rex_byte = 0x0;
    switch (op) {
    case Ret:
        opcode = 0xC3;
        break;
    case CQO:
        opcode = 0x99;
        use_rex_byte = true;
        rex_byte = 0b01001000;
        break;
    default: {
        PtrArray nodes = mk_ptr_array(4, err_allocator);
        push_ptr(mk_str_doc(mv_string("Unrecognized unary instruction:"), err_allocator), &nodes);
        push_ptr(pretty_nullary_op(op, err_allocator), &nodes);
        push_ptr(mk_str_doc(mv_string("/"), err_allocator), &nodes);
        push_ptr(pretty_i32(op, err_allocator), &nodes);
        throw_error(point, doc_to_str(mv_sep_doc(nodes, err_allocator), 80, err_allocator));
    }
    }
    U8Array* instructions = &assembler->instructions;
    if (use_rex_byte)
        push_u8(rex_byte, instructions);
    push_u8(opcode, instructions);
    AsmResult out = {.backlink = 0};
    return out;
}

void init_asm() {
    build_unary_table();
    build_unary_opcode_table();

    build_binary_table();
    build_binary_opcode_tables();
}

// Utility & Pretty
Document* pretty_register(Regname reg, LocationSize sz, Allocator* a) {
    char* names[33][4] = {
        {"AL", "AX", "EAX", "RAX"},
        {"CL", "CX", "ECX", "RCX"},
        {"DL", "DX", "EDX", "RDX"},
        {"BL", "BX", "EBX", "RBX"},
        {"AH", "SP", "ESP", "RSP"},
        {"CH", "BP", "EBP", "RBP"},
        {"DH", "SI", "ESI", "RSI"},
        {"BH", "DI", "EDI", "RDI"},

        {"R8b", "R8w", "R8d", "R8"},
        {"R9b", "R9w", "R9d", "R9"},
        {"R10b", "R10w", "R10d", "R10"},
        {"R11b", "R11w", "R11d", "R11"},
        {"R12b", "R12w", "R12d", "R12"},
        {"R13b", "R13w", "R13d", "R13"},
        {"R14b", "R14w", "R14d", "R14"},
        {"R15b", "R15w", "R15d", "R15"},

        {"XMM0b", "XMM0w", "XMM0d", "XMM0"},
        {"XMM1b", "XMM1w", "XMM1d", "XMM1"},
        {"XMM2b", "XMM2w", "XMM2d", "XMM2"},
        {"XMM3b", "XMM3w", "XMM3d", "XMM3"},
        {"XMM4b", "XMM4w", "XMM4d", "XMM4"},
        {"XMM5b", "XMM5w", "XMM5d", "XMM5"},
        {"XMM6b", "XMM6w", "XMM6d", "XMM6"},
        {"XMM7b", "XMM7w", "XMM7d", "XMM7"},
        {"XMM8b", "XMM8w", "XMM8d", "XMM8"},
        {"XMM9b", "XMM9w", "XMM9d", "XMM9"},
        {"XMM10b", "XMM10w", "XMM10d", "XMM10"},
        {"XMM11b", "XMM11w", "XMM11d", "XMM11"},
        {"XMM12b", "XMM12w", "XMM12d", "XMM12"},
        {"XMM13b", "XMM13w", "XMM13d", "XMM13"},
        {"XMM14b", "XMM14w", "XMM14d", "XMM14"},
        {"XMM15b", "XMM15w", "XMM15d", "XMM15"},

        /* // Special! See RIP-Relative addressing, p50 of the Intel Manual Vol. 2 */
        // Note that I believe the non-64 bit 'IP' are invalid
        {"RIP", "EIP", "IP", "IPL"},
    };

    // TODO BUG bounds check here.
    return mk_str_doc(mv_string(names[reg][sz]), a);
};

Document* pretty_location(Location loc, Allocator* a) {
    switch(loc.type) {

    case Dest_Register:
        return pretty_register(loc.reg, loc.sz, a);

    case Dest_Deref: {
        PtrArray nodes = mk_ptr_array(6, a);
        push_ptr(pretty_register(loc.reg, loc.sz, a), &nodes);
        
        if (loc.is_scale) {
            push_ptr(mk_str_doc(mv_string(" + "), a), &nodes);
            push_ptr(pretty_i8(loc.scale, a), &nodes);
            push_ptr(pretty_register(loc.index, loc.sz, a), &nodes);
        }

        push_ptr(mk_str_doc(mv_string(" + "), a), &nodes);

        switch (loc.disp_sz) {
        case 0:
            break;
        case 1:
            push_ptr(pretty_i8(loc.disp_8, a), &nodes);
            break;
        case 4:
            push_ptr(pretty_i8(loc.disp_32, a), &nodes);
            break;
        default: 
            panic(mv_string("Invalid displacement size to pretty_location."));
        }

        return mk_paren_doc("[", "]", mv_cat_doc(nodes, a), a);
    }

    case Dest_Immediate: {
        PtrArray nodes = mk_ptr_array(2, a);
        switch (loc.sz) {
        case sz_8:
            push_ptr(pretty_i8(loc.immediate_8, a), &nodes);
            push_ptr(mk_str_doc(mv_string("i8"), a), &nodes);
            break;
        case sz_16:
            push_ptr(pretty_i64(loc.immediate_16, a), &nodes);
            push_ptr(mk_str_doc(mv_string("i16"), a), &nodes);
            break;
        case sz_32:
            push_ptr(pretty_i32(loc.immediate_32, a), &nodes);
            push_ptr(mk_str_doc(mv_string("i32"), a), &nodes);
            break;
        case sz_64:
            push_ptr(pretty_i64(loc.immediate_64, a), &nodes);
            push_ptr(mk_str_doc(mv_string("i64"), a), &nodes);
            break;
            // TODO: handle default
        default:
            panic(mv_string("Invalid immediate size size to pretty_location."));
        }
        return mv_cat_doc(nodes, a);
    }

    default:
        panic(mv_string("pretty_location: Invalid location type"));
    }
}

Document* pretty_binary_op(BinaryOp op, Allocator* a) {
    char* names[Binary_Op_Count] = {
        "Add", "Sub", "Cmp", "And", "Or", "Xor", "SHL", "SHR",
        "Mov", "MovSS", "LEA", "CMovE", "CMovL", "CMovG",
    };
    // TODO BUG bounds check here.
    return mk_str_doc(mv_string(names[op]), a);
}

Document* pretty_unary_op(UnaryOp op, Allocator* a) {
    char* names[Unary_Op_Count] = {
        "Call", "Push", "Pop", "JE", "JNE", "JMP",
        "Not", "SetE", "SetB", "SetA", "SetL", "SetG",
        "Mul", "Div", "IMul", "IDiv",
    };
    // TODO BUG bounds check here.
    return mk_str_doc(mv_string(names[op]), a);
}

Document* pretty_nullary_op(NullaryOp op, Allocator* a) {
    char* names[Nullary_Op_Count] = {
        "Ret", 
    };
    // TODO BUG bounds check here.
    return mk_str_doc(mv_string(names[op]), a);
}

Document* pretty_binary_instruction(BinaryOp op, Location dest, Location src, Allocator* a) {
    PtrArray nodes = mk_ptr_array(5, a);
    push_ptr(pretty_binary_op(op, a), &nodes);
    push_ptr(mk_str_doc(mv_string(" "), a), &nodes);
    push_ptr(pretty_location(dest, a), &nodes);
    push_ptr(mk_str_doc(mv_string(", "), a), &nodes);
    push_ptr(pretty_location(src, a), &nodes);
    return mv_cat_doc(nodes, a);
}

Document* pretty_unary_instruction(UnaryOp op, Location loc, Allocator* a) {
    PtrArray nodes = mk_ptr_array(3, a);
    push_ptr(pretty_unary_op(op, a), &nodes);
    push_ptr(mk_str_doc(mv_string(" "), a), &nodes);
    push_ptr(pretty_location(loc, a), &nodes);
    return mv_cat_doc(nodes, a);
}
