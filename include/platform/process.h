#ifndef __PLATFORM_PROCESS_H
#define __PLATFORM_PROCESS_H

#include <stdbool.h>
#include "data/string.h"
#include "data/result.h"

#include "pico/data/string_array.h"

typedef struct {union {uint64_t id; void* handle;};} Process;

// TODO: see https://www.man7.org/linux/man-pages/man3/posix_spawn.3.html
//       for errors... what about if cannot find the file?
typedef enum {
    SPExecNotExists,
    SPTooManyProcesses,
    SpNoMemory,
    SpNotSupported,
} SpawnError;

typedef struct {
    Result_t result;
    union { 
        Process process;
        SpawnError error;
    };
} ProcessResult;

ProcessResult create_process(String command, StringArray args);

typedef struct {
  Result_t result;
  int status;
} WaitResult;
WaitResult wait_on_process(Process process);
Result_t kill_process(Process process);

#endif
