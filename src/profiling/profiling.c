#include "platform/profiling/profiling.h"
#include "platform/machine_info.h"

typedef struct {
    uint64_t time_sec;
    uint64_t time_ns;
} perf_time;

#if ABI == SYSTEM_V_64
#include <time.h>

perf_time query_performance_timer() {
    struct timespec time;
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time);
    return (perf_time) {
        .time_sec = time.tv_sec,
        .time_ns = time.tv_nsec,
    };
}

#elif ABI == WIN_64

#include <windows.h>

uint64_t query_performance_timer() {
    uint64_t perf;
    QueryPerformanceCounter(&perf);
    return perf;
}

#endif
