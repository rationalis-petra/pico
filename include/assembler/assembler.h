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
document* pretty_assembler(assembler* assembler, allocator a);
u8_array get_instructions(assembler* assembler);

void make_executable(assembler* assembler);
void make_writable(assembler* assembler);

// Integral operations
typedef enum binary_op {
    // ------------------
    //  Arithmetic
    // ------------------
    Add,
    Sub,
    And,

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
    Mov,   // p769.
} binary_op;

typedef enum unary_op {
    Call,
    Push,
    Pop,
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
    Immediate,
    Immediate64,
} dest_t;

// Location: Can be one of
// + Register
// + [Register + Offset]
// + Immediate
typedef struct location {
    dest_t type;
    regname reg;
    uint32_t immediate;
    uint64_t immediate_64;
} location;

// Location Constructors 
location reg(regname name);
location rref(regname name, uint8_t offset);
location imm32(uint32_t immediate);
location imm64(uint64_t immediate);

result build_binary_op(assembler* ass, binary_op op, location dest, location src);
result build_unary_op(assembler* assembler, unary_op op, location loc);
result build_nullary_op(assembler* assembler, nullary_op op);


#endif
