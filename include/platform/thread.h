#ifndef __PLATFORM_THREAD_H
#define __PLATFORM_THREAD_H

#include <stdint.h>

#include "platform/machine_info.h"

// TODO: (improvement) -  
//   allow conversions between different times.
typedef struct {
    uint64_t us;
} Microseconds;

#if OS_FAMILY == UNIX
#include <pthread.h>
typedef pthread_mutex_t Mutex;

#elif OS_FAMILY == WINDOWS
#include "windows.h"
typedef HANDLE Mutex;

#else
#error "unsupportedOS"
#endif

void mutex_init(Mutex* mutex);
void mutex_destroy(Mutex* mutex);

void mutex_lock(Mutex* mutex);
void mutex_unlock(Mutex* mutex);

// Sleep *current thread* for *at least* time.
void sleep_for(Microseconds time);


#endif
