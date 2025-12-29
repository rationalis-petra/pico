#include "platform/memory/arena.h"

#include "components/assembler/assembler.h"
#include "components/pretty/string_printer.h"

#include "test_assembler/test_assembler.h"
#include "test_assembler/helper.h"

#define ASM_TEST() ErrorPoint point; if (catch_error(point)) { test_log_error(log, doc_to_str(point.error_message, 120, a)); test_fail(log); clear_assembler(ass); } else 

void run_sse_assembler_tests(TestLog *log, Allocator *a) {
    Assembler* ass = mk_assembler(current_cpu_feature_flags(), a);
    ArenaAllocator* arena = make_arena_allocator(16384, a);
    Allocator gpa = aa_to_gpa(arena);

    if (test_start(log, mv_string("movss-xmm32-xmm32"))) { // MovSS XMM0, XMM1
        ASM_TEST() {
            uint8_t expected[] = { 0xF3, 0x0F, 0x10, 0xC1, 0x90 };
            build_binary_op(MovSS, reg(XMM0, sz_32), reg(XMM1, sz_32), ass, &gpa, &point);

            check_asm_eq(expected, ass, &gpa, log);
            clear_assembler(ass);
        }
    }

    if (test_start(log, mv_string("movss-xmm32-m32"))) { // MovSS XMM0, XMM1
        ASM_TEST() {
            uint8_t expected[] = { 0xF3, 0x0F, 0x10, 0x54, 0x24, 0x08, 0x90 };
            build_binary_op(MovSS, reg(XMM2, sz_32), rref8(RSP, 8, sz_32), ass, &gpa, &point);

            check_asm_eq(expected, ass, &gpa, log);
            clear_assembler(ass);
        }
    }

    if (test_start(log, mv_string("movss-m32-xmm32"))) { // MovSS XMM0, XMM1
        ASM_TEST() {
            uint8_t expected[] = { 0xF3, 0x0F, 0x11, 0x6F, 0x08, 0x90 };
            build_binary_op(MovSS, rref8(RDI, 8, sz_32), reg(XMM5, sz_32), ass, &gpa, &point);

            check_asm_eq(expected, ass, &gpa, log);
            clear_assembler(ass);
        }
    }

    if (test_start(log, mv_string("movsd-xmm64-m64"))) { // MovSD XMM0, XMM1
        ASM_TEST() {
            uint8_t expected[] =  { 0x48, 0xF2, 0x0F, 0x10, 0x44, 0x24, 0x08, 0x90 } ;
            build_binary_op(MovSD, reg(XMM0, sz_64), rref8(RSP, 8, sz_64), ass, &gpa, &point);

            check_asm_eq(expected, ass, &gpa, log);
            clear_assembler(ass);
        }
    }

    if (test_start(log, mv_string("movsd-m64-xmm64"))) { // MovSD XMM0, XMM1
        ASM_TEST() {
            uint8_t expected[] = { 0x48, 0xF2, 0x0F, 0x11, 0x6F, 0x08, 0x90 };
            build_binary_op(MovSD, rref8(RDI, 8, sz_64), reg(XMM5, sz_64), ass, &gpa, &point);

            check_asm_eq(expected, ass, &gpa, log);
            clear_assembler(ass);
        }
    }

    if (test_start(log, mv_string("cvtst2ss-xmm32-xmm64"))) { // MovSD XMM0, XMM1
        ASM_TEST() {
            uint8_t expected[] = { 0xF2, 0x0F, 0x5A, 0xDF , 0x90 };
            build_binary_op(CvtSD2SS, reg(XMM3, sz_32), reg(XMM7, sz_64), ass, &gpa, &point);

            check_asm_eq(expected, ass, &gpa, log);
            clear_assembler(ass);
        }
    }

    if (test_start(log, mv_string("cvtst2ss-xmm32-m64"))) { // MovSD XMM0, XMM1
        ASM_TEST() {
            uint8_t expected[] = { 0xF2, 0x0F, 0x5A, 0x62, 0x08, 0x90 };
            build_binary_op(CvtSD2SS, reg(XMM4, sz_32), rref8(RDX, 8, sz_64), ass, &gpa, &point);

            check_asm_eq(expected, ass, &gpa, log);
            clear_assembler(ass);
        }
    }

    // TODO: add test for XMM{n} where n >= 8

    delete_assembler(ass);
    delete_arena_allocator(arena);
}
