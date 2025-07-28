#include "platform/profiling/profiling.h"
#include "platform/machine_info.h"

__attribute__((naked))
uint64_t query_performance_counter() {
    __asm__(
        "rdtsc\n"
        "shl $32, %rdx\n"
        "or %rdx, %rax\n"
        "ret\n"
    );
}