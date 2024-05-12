#include <stdio.h>
#include "pico/eval/call.h"

int64_t pico_call(void* entry) {
    int64_t out;
    __asm__ __volatile__(
        "call %1       \n\t"
        "movq %%RAX, %0  \n\t"
        : "=r" (out)
        : "r"(entry));

    return out;
}
