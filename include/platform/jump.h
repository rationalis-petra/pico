#ifndef __PLATFORM_JUMP_H
#define __PLATFORM_JUMP_H

// We use our own setjmp/longjmp because the Windows implementation crashes the
// program if it has to jump over a pico stackframe.

typedef void *jump_buf[17];
int set_jump(jump_buf buf);
_Noreturn void long_jump(jump_buf buf, int val);

#endif
