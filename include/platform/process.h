#ifndef __PLATFORM_PROCESS_H
#define __PLATFORM_PROCESS_H

#include <stdbool.h>
#include "data/string.h"
#include "data/slice.h"
#include "data/result.h"

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
    Result_t type;
    union { 
        Process process;
        SpawnError error;
    };
} ProcessResult;

ProcessResult create_process(String command, StringSlice args);
ProcessResult create_process_in(String command, StringSlice args, String directory);

typedef struct {
  Result_t type;
  int status;
} WaitResult;
WaitResult wait_on_process(Process process);
Result_t kill_process(Process process);

#endif
