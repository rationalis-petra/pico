#ifndef __ASSEMBLER_ASSEMBLER_H
#define __ASSEMBLER_ASSEMBLER_H

#include <stddef.h>

#include "data/array.h"
#include "data/string.h"

/* The assembler writes encoded instructions directly to a byte-array. 
 * 
 */

typedef u8_array assembler; 
assembler* mk_assembler(allocator a);

// Integral operations
typedef enum binary_op {
    Add,
    Sub,
    And,
    Or,
} binary_op;

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
    Immediate
} dest_t;

// Location: Can be one of
// + Register
// + [Register + Offset]
// + Immediate
typedef struct location {
    dest_t type;
    regname reg;
    uint32_t immediate;
} location;

// result of assembling (might error due to inferior type system :()
typedef struct asm_result {
    bool succ;
    string msg;
} asm_result;

// Location Constructors 
location reg(regname name);
location rref(regname name, uint8_t offset);
location imm32(uint32_t immediate);

// Build a Binary (+,-,etc.) operation. May error
asm_result build_binary_op(assembler* ass, binary_op op, location dest, location src, allocator a);


#endif
