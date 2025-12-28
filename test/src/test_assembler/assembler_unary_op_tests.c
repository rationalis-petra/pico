#include "platform/memory/arena.h"

#include "components/assembler/assembler.h"
#include "components/pretty/string_printer.h"

#include "test_assembler/test_assembler.h"
#include "test_assembler/helper.h"

#define ASM_TEST() ErrorPoint point; if (catch_error(point)) { test_log_error(log, doc_to_str(point.error_message, 120, a)); test_fail(log); clear_assembler(ass); } else 

void run_unary_op_assembler_tests(TestLog *log, Allocator *a) {
    Assembler* ass = mk_assembler(current_cpu_feature_flags(), a);
    ArenaAllocator* arena = make_arena_allocator(16384, a);
    Allocator gpa = aa_to_gpa(arena);

    if (test_start(log, mv_string("bswap-r64"))) { // MovSD XMM0, XMM1
        ASM_TEST() {
            uint8_t expected[] = { 0x48, 0x0F, 0xCE, 0x90 };
            build_unary_op(BSwap, reg(RSI, sz_64), ass, &gpa, &point);

            check_asm_eq(expected, ass, &gpa, log);
            clear_assembler(ass);
        }
    }

    if (test_start(log, mv_string("bswap-r32"))) { // MovSD XMM0, XMM1
        ASM_TEST() {
            uint8_t expected[] = { 0x0F, 0xC9,  0x90 };
            build_unary_op(BSwap, reg(RCX, sz_32), ass, &gpa, &point);

            check_asm_eq(expected, ass, &gpa, log);
            clear_assembler(ass);
        }
    }

    // TODO: add test for XMM{n} where n >= 8

    delete_assembler(ass);
    delete_arena_allocator(arena);
}
