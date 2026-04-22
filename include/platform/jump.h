#ifndef __PLATFORM_JUMP_H
#define __PLATFORM_JUMP_H
#include "machine_info.h"

// We use our own setjmp/longjmp because the Windows implementation crashes the
// program if it has to jump over a pico stackframe.

#if ABI == SYSTEM_V_AARCH64
#include <setjmp.h>
typedef jmp_buf jump_buf;
#else
typedef void *jump_buf[17];
#endif

int set_jump(jump_buf buf);
_Noreturn void long_jump(jump_buf buf, int val);

#endif
