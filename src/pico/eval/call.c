#include <stdio.h>
#include "pico/eval/call.h"

int64_t pico_run_expr(void* assembled_expr) {
    int64_t out;
    __asm__ __volatile__(
        "call %1       \n\t"
        "movq %%RAX, %0  \n\t"
        : "=r" (out)
        : "r"(assembled_expr));

    return out;
}
