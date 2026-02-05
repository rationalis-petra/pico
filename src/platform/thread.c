#include "platform/thread.h"

#if OS_FAMILY == UNIX
#include <unistd.h>
#include <pthread.h>

void mutex_init(Mutex* mutex) {
    pthread_mutex_init(mutex, NULL);
}

void mutex_destroy(Mutex* mutex) {
    pthread_mutex_destroy(mutex);
}

void mutex_lock(Mutex* mutex) {
    pthread_mutex_lock(mutex);
}

void mutex_unlock(Mutex* mutex) {
    pthread_mutex_unlock(mutex);
}

void sleep_for(Microseconds time) {
    usleep(time.us);
}

#elif OS_FAMILY == WINDOWS
#include <stdbool.h>
#include "windows.h"

void mutex_init(Mutex* mutex) {
    *mutex = CreateMutex(NULL, false, NULL);
}

void mutex_destroy(Mutex* mutex) {
    CloseHandle(*mutex);
}

void mutex_lock(Mutex* mutex) {
    WaitForSingleObject(*mutex, INFINITE);
}

void mutex_unlock(Mutex* mutex) {
    ReleaseMutex(*mutex);
}

#else 
#error "Do not support threads for this operating system."
#endif
