#ifndef __TEST_ASSEMBLER_HELPER_H
#define __TEST_ASSEMBLER_HELPER_H

#include "components/assembler/assembler.h"
#include "test/test_log.h"

void check_asm_eq(uint8_t* expected, Assembler* ass, Allocator* a, TestLog* log);

#endif
