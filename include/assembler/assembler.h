#ifndef __ASSEMBLER_ASSEMBLER_H
#define __ASSEMBLER_ASSEMBLER_H


#include "data/result.h"
#include "data/array.h"
#include "pretty/document.h"

/* The assembler writes encoded instructions directly to a byte-array. */

typedef struct assembler assembler; 
assembler* mk_assembler(allocator a);
void delete_assembler(assembler* assembler);
void clear_assembler(assembler* assembler);

u8_array get_instructions(assembler* assembler);
size_t get_pos(assembler* assembler);

document* pretty_assembler(assembler* assembler, allocator a);

// Integral operations
typedef enum binary_op {
    // ------------------
    //  Arithmetic
    // ------------------
    Add,
    Sub,
    And,

    Cmp, // p289. Compare two operands

    // ------------------
    //  Logic
    // ------------------
    Or,

    // ------------------
    //  Bit Manipulation
    // ------------------
    LShift,
    RShift,

    // ------------------
    //  Memory
    // ------------------
    Mov,   // p 769.
} binary_op;

typedef enum unary_op {
    // ------------------
    //  Functions
    // ------------------
    Call,
    Push, // p 1250.
    Pop,  // 

    // ------------------
    //  Jumps
    // ------------------
    JE,  // p658, jump if equal


    JNE, // p658, jump if equal
    JMP, // p663, unconditional jump

    // ------------------------
    //  Set Byte based on flag 
    // ------------------------
    SetE,
    SetL,
    SetG,

} unary_op;

typedef enum nullary_op {
    Ret,
} nullary_op;

typedef enum regname {
    RAX = 0b0000,
    RBX = 0b0011,
    RCX = 0b0001,
    RDX = 0b0010,
    RSP = 0b0100,
    RBP = 0b0101,
    RSI = 0b0110,
    RDI = 0b0111,
    R8  = 0b1000,
    R9  = 0b1001,
    R10 = 0b1010,
    R11 = 0b1011,
    R12 = 0b1100,
    R13 = 0b1101,
    R14 = 0b1110,
    R15 = 0b1111
} regname;

typedef enum dest_t {
    Register,
    Deref,
    Immediate8,
    Immediate16,
    Immediate32,
    Immediate64,
} dest_t;

// Location: Can be one of
// + Register
// + [Register + Offset]
// + Immediate
typedef struct location {
    dest_t type;
    regname reg;
    int8_t immediate_8;
    int16_t immediate_16;
    int32_t immediate_32;
    int64_t immediate_64;
} location;

// Location Constructors 
location reg(regname name);
location rref(regname name, int8_t offset);
location imm8(int8_t immediate);
location imm16(int16_t immediate);
location imm32(int32_t immediate);
location imm64(int64_t immediate);

// Result 
typedef struct asm_result {
    Result_t type;
    union {
        size_t backlink; // backlink to immediate (if it exists)
        string error_message;
    }; 
} asm_result;

asm_result build_binary_op(assembler* ass, binary_op op, location dest, location src, allocator err_allocator);
asm_result build_unary_op(assembler* assembler, unary_op op, location loc, allocator err_allocator);
asm_result build_nullary_op(assembler* assembler, nullary_op op, allocator err_allocator);


#endif
