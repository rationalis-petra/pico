#ifndef __PLATFORM_THREADS_H
#define __PLATFORM_THREADS_H

#include "platform/machine_info.h"

#if OS_FAMILY == UNIX
#include "pthread.h"
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

#endif
