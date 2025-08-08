#include "platform/profiling/profiling.h"
#include "platform/machine_info.h"

double time_to_double(PerfTime time, TimeUnit unit) {
    double s_mul = 1.0;
    double ns_mul = 0.000000001;
    switch (unit) {
    case Seconds:
        s_mul = 1.0;
        ns_mul = 0.000000001;
        break;
    case MilliSeconds:
        s_mul = 1000.0;
        ns_mul = 0.000001;
        break;
    case MicroSeconds:
        s_mul = 1000000.0;
        ns_mul = 0.001;
        break;
    case NanoSeconds:
        s_mul = 1000000000.0;
        ns_mul = 1.0;
        break;
    }
    return time.time_ns * ns_mul + time.time_sec * s_mul;
}

PerfTime time_diff(PerfTime start, PerfTime end) {
    return (PerfTime) {
        .time_sec = end.time_sec - start.time_sec,
        .time_ns = end.time_ns - start.time_ns,
    };
}

#if ABI == SYSTEM_V_64
#include <time.h>

PerfTime query_performance_timer() {
    struct timespec time;
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time);
    return (PerfTime) {
        .time_sec = time.tv_sec,
        .time_ns = time.tv_nsec,
    };
}

#elif ABI == WIN_64

#include <windows.h>

PerfTime query_performance_timer() {
    LARGE_INTEGER perf;
    LARGE_INTEGER freq;
    QueryPerformanceCounter(&perf);
    QueryPerformanceFrequency(&freq);

    // Then, nanoseconds per cycle = 10^9/freq 
    // Thus, time_ns = (perf % freq) * 10^9 / freq
    return (PerfTime) {
        .time_ns = (perf.QuadPart % freq.QuadPart) * 1E9UL / freq.QuadPart,
        .time_sec = perf.QuadPart / freq.QuadPart,
    };
}

#endif
