#ifndef __PLATFORM_PROFILING_PROFILING_H
#define __PLATFORM_PROFILING_PROFILING_H

#include <stdint.h>

typedef enum {
    NanoSeconds,
    MicroSeconds,
    MilliSeconds,
    Seconds,
} TimeUnit;

typedef struct {
    uint64_t time_sec;
    uint64_t time_ns;
} PerfTime;

double time_to_double(PerfTime time, TimeUnit unit);

PerfTime time_diff(PerfTime start, PerfTime end);

PerfTime query_performance_timer();

#endif
