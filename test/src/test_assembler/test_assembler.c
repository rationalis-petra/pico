#include "platform/memory/arena.h"
#include "components/assembler/assembler.h"

#include "test_assembler/test_assembler.h"
#include "test_assembler/helper.h"

#define ASM_TEST() ErrorPoint point; if (catch_error(point)) { test_log_error(log, point.error_message); test_fail(log); clear_assembler(ass); } else 

void run_assembler_tests(TestLog *log, Allocator *a) {
    Assembler* ass = mk_assembler(current_cpu_feature_flags(), a);
    ArenaAllocator* arena = mk_arena_allocator(4096, a);
    Allocator gpa = aa_to_gpa(arena);

    if (test_start(log, mv_string("asm-add-r64-imm8"))) { // Add RAX, 12
        ASM_TEST() {
            uint8_t expected[] = { 0x48, 0x83, 0xC0, 0x0C, 0x90 };
            build_binary_op(Add, reg(RAX, sz_64), imm8(12), ass, &gpa, &point);

            check_asm_eq(expected, ass, a, log);
            clear_assembler(ass);
        }
    }

    if (test_start(log, mv_string("asm-add-r64-r64"))) { // Add RBX, RCX
        ASM_TEST() {
            uint8_t expected[] = { 0x48, 0x03, 0xD9, 0x90 };
            build_binary_op(Add, reg(RBX, sz_64), reg(RCX, sz_64), ass, &gpa, &point);

            check_asm_eq(expected, ass, a, log);
            clear_assembler(ass);
        }
    }

    if (test_start(log, mv_string("asm-add-r64-m64"))) { // Add RDX, [R9 + 8]
        ASM_TEST() {
            uint8_t expected[] = { 0x49, 0x03, 0x51, 0x08, 0x90 };
            build_binary_op(Add, reg(RDX, sz_64), rref8(R9, 8, sz_64), ass, &gpa, &point);

            check_asm_eq(expected, ass, a, log);
            clear_assembler(ass);
        }
    }

    if (test_start(log, mv_string("asm-add-r8-m8"))) { // Add DL, [R9 + 8]b
        ASM_TEST() {
            // 41 02 51 08
            uint8_t expected[] =  { 0x41, 0x02, 0x51, 0x08, 0x90 } ;
            build_binary_op(Add, reg(RDX, sz_8), rref8(R9, 8, sz_8), ass, &gpa, &point);

            check_asm_eq(expected, ass, a, log);
            clear_assembler(ass);
        }
    }

    if (test_start(log, mv_string("asm-mov-m8-r8"))) { // Mov [RSI + 8]b, DIL, 
        ASM_TEST() {
            uint8_t expected[] =  { 0x40, 0x88, 0x7E, 0x08, 0x90 } ;
            build_binary_op(Mov, rref8(RSI, 8, sz_8), reg(RDI, sz_8), ass, &gpa, &point);

            check_asm_eq(expected, ass, a, log);
            clear_assembler(ass);
        }
    }

    if (test_start(log, mv_string("movss-xmm32-xmm32"))) { // MovSS XMM0, XMM1
        ASM_TEST() {
            uint8_t expected[] = { 0xF3, 0x0F, 0x10, 0xC1, 0x90 };
            build_binary_op(MovSS, reg(XMM0, sz_32), reg(XMM1, sz_32), ass, &gpa, &point);

            check_asm_eq(expected, ass, a, log);
            clear_assembler(ass);
        }
    }

    if (test_start(log, mv_string("movss-xmm32-m32"))) { // MovSS XMM0, XMM1
        ASM_TEST() {
            uint8_t expected[] = { 0xF3, 0x0F, 0x10, 0x54, 0x24, 0x08, 0x90 };
            build_binary_op(MovSS, reg(XMM2, sz_32), rref8(RSP, 8, sz_32), ass, &gpa, &point);

            check_asm_eq(expected, ass, a, log);
            clear_assembler(ass);
        }
    }

    if (test_start(log, mv_string("movss-m32-xmm32"))) { // MovSS XMM0, XMM1
        ASM_TEST() {
            uint8_t expected[] = { 0xF3, 0x0F, 0x11, 0x6F, 0x08, 0x90 };
            build_binary_op(MovSS, rref8(RDI, 8, sz_32), reg(XMM5, sz_32), ass, &gpa, &point);

            check_asm_eq(expected, ass, a, log);
            clear_assembler(ass);
        }
    }

    if (test_start(log, mv_string("movsd-xmm64-m64"))) { // MovSD XMM0, XMM1
        ASM_TEST() {
            uint8_t expected[] =  { 0x48, 0xF2, 0x0F, 0x10, 0x44, 0x24, 0x08, 0x90 } ;
            build_binary_op(MovSD, reg(XMM0, sz_64), rref8(RSP, 8, sz_64), ass, &gpa, &point);

            check_asm_eq(expected, ass, a, log);
            clear_assembler(ass);
        }
    }

    if (test_start(log, mv_string("movsd-m64-xmm64"))) { // MovSD XMM0, XMM1
        ASM_TEST() {
            uint8_t expected[] = { 0x48, 0xF2, 0x0F, 0x11, 0x6F, 0x08, 0x90 };
            build_binary_op(MovSD, rref8(RDI, 8, sz_64), reg(XMM5, sz_64), ass, &gpa, &point);

            check_asm_eq(expected, ass, a, log);
            clear_assembler(ass);
        }
    }

    // TODO: add test for XMM{n} where n >= 8

    delete_assembler(ass);
    delete_arena_allocator(arena);
}
