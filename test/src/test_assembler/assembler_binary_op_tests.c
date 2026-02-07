#include "platform/memory/arena.h"

#include "components/assembler/assembler.h"
#include "components/pretty/string_printer.h"

#include "test_assembler/test_assembler.h"
#include "test_assembler/helper.h"

#define ASM_TEST() ErrorPoint point; if (catch_error(point)) { test_log_error(log, doc_to_str(point.error_message, 120, &gpa)); test_fail(log); clear_assembler(ass); } else 

void run_binary_op_assembler_tests(TestLog *log, Allocator *a) {
    Assembler* ass = mk_assembler(current_cpu_feature_flags(), a);
    ArenaAllocator* arena = make_arena_allocator(16384, a);
    Allocator gpa = aa_to_gpa(arena);

    if (test_start(log, mv_string("asm-add-r64-imm8"))) { // Add RAX, 12
        ASM_TEST() {
            uint8_t expected[] = { 0x48, 0x83, 0xC0, 0x0C, 0x90 };
            build_binary_op(Add, reg(RAX, sz_64), imm8(12), ass, &gpa, &point);

            check_asm_eq(expected, ass, &gpa, log);
            clear_assembler(ass);
        }
    }

    if (test_start(log, mv_string("asm-add-r64-r64"))) { // Add RBX, RCX
        ASM_TEST() {
            uint8_t expected[] = { 0x48, 0x03, 0xD9, 0x90 };
            build_binary_op(Add, reg(RBX, sz_64), reg(RCX, sz_64), ass, &gpa, &point);

            check_asm_eq(expected, ass, &gpa, log);
            clear_assembler(ass);
        }
    }

    if (test_start(log, mv_string("asm-add-r64-m64"))) { // Add RDX, [R9 + 8]
        ASM_TEST() {
            uint8_t expected[] = { 0x49, 0x03, 0x51, 0x08, 0x90 };
            build_binary_op(Add, reg(RDX, sz_64), rref8(R9, 8, sz_64), ass, &gpa, &point);

            check_asm_eq(expected, ass, &gpa, log);
            clear_assembler(ass);
        }
    }

    if (test_start(log, mv_string("asm-add-r8-m8"))) { // Add DL, [R9 + 8]b
        ASM_TEST() {
            // 41 02 51 08
            uint8_t expected[] =  { 0x41, 0x02, 0x51, 0x08, 0x90 } ;
            build_binary_op(Add, reg(RDX, sz_8), rref8(R9, 8, sz_8), ass, &gpa, &point);

            check_asm_eq(expected, ass, &gpa, log);
            clear_assembler(ass);
        }
    }

    if (test_start(log, mv_string("asm-mov-r32-m32"))) { // Mov [RSI + 8]b, DIL, 
        ASM_TEST() {
            uint8_t expected[] =  { 0x41, 0x8B, 0x43, 0x08, 0x90 } ;
            build_binary_op(Mov, reg(RAX, sz_32), rref8(R11, 8, sz_32), ass, &gpa, &point);

            check_asm_eq(expected, ass, &gpa, log);
            clear_assembler(ass);
        }
    }

    if (test_start(log, mv_string("asm-mov-r32-m32"))) { // Mov [RSI + 8]b, DIL, 
        ASM_TEST() {
            uint8_t expected[] =  { 0x41, 0x8B, 0x43, 0x08, 0x90 } ;
            build_binary_op(Mov, reg(RAX, sz_32), rref8(R11, 8, sz_32), ass, &gpa, &point);

            check_asm_eq(expected, ass, &gpa, log);
            clear_assembler(ass);
        }
    }

    if (test_start(log, mv_string("asm-mov-m8-r8"))) { // Mov [RSI + 8]b, DIL, 
        ASM_TEST() {
            uint8_t expected[] =  { 0x40, 0x88, 0x7E, 0x08, 0x90 } ;
            build_binary_op(Mov, rref8(RSI, 8, sz_8), reg(RDI, sz_8), ass, &gpa, &point);

            check_asm_eq(expected, ass, &gpa, log);
            clear_assembler(ass);
        }
    }


    if (test_start(log, mv_string("asm-mov-dl-ah"))) { // Mov RSP, AH 
        ASM_TEST() {
            uint8_t expected[] =  { 0x8a, 0xD4, 0x90 };
            build_binary_op(Mov, reg(RDX, sz_8), reg(AH, sz_8), ass, &gpa, &point);

            check_asm_eq(expected, ass, &gpa, log);
            clear_assembler(ass);
        }
    }

    if (test_start(log, mv_string("asm-mov-r64-m64-big-offset"))) { // Mov [RSI + 8]b, DIL, 
        ASM_TEST() {
            uint8_t expected[] =  { 0x48, 0x8B, 0x85, 0xA8, 0xFE, 0xFF, 0xFF, 0x90 } ;
            build_binary_op(Mov, reg(RAX, sz_64), rref32(RBP, -344, sz_64), ass, &gpa, &point);

            check_asm_eq(expected, ass, &gpa, log);
            clear_assembler(ass);
        }
    }

    if (test_start(log, mv_string("asm-movzx-r64-r8"))) { // MovZx RSP, AL 
        ASM_TEST() {
            uint8_t expected[] =  {  0x48, 0x0F, 0xB6, 0xE0 , 0x90 } ;
            build_binary_op(MovZx, reg(RSP, sz_64), reg(RAX, sz_8), ass, &gpa, &point);

            check_asm_eq(expected, ass, &gpa, log);
            clear_assembler(ass);
        }
    }

    if (test_start(log, mv_string("asm-movzx-r64-r16"))) { // MovZx R9, R8w
        ASM_TEST() {
            uint8_t expected[] =  { 0x4D, 0x0F, 0xB7, 0xC8, 0x90 } ;
            build_binary_op(MovZx, reg(R9, sz_64), reg(R8, sz_16), ass, &gpa, &point);

            check_asm_eq(expected, ass, &gpa, log);
            clear_assembler(ass);
        }
    }

    delete_assembler(ass);
    delete_arena_allocator(arena);
}
