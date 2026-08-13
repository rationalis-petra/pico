#include "platform/machine_info.h"
#include "platform/memory/std_allocator.h"
#include "platform/process.h"
#include "platform/signals.h"

#if OS_FAMILY == UNIX

#include <signal.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <spawn.h>

ProcessResult create_process(String command, StringArray args) {
    Allocator* a = get_std_allocator();
    pid_t pid;
    char* c_command = to_c_string(command, a);
    char** c_args = mem_alloc(sizeof(char*) * (args.len + 1), a);
    for (size_t i = 0; i < args.len; i++) {
        c_args[i] = to_c_string(args.data[i], a);
    }
    c_args[args.len] = NULL;
    // TODO (DESIGN): The 'p' means that spawn will search 'path' (as opposed to
    //                being) absolute only.
    int status = posix_spawnp(&pid, c_command, NULL, NULL, c_args, environ);
    for (size_t i = 0; i < args.len; i++) {
        mem_free(c_args[i], a);
    }
    mem_free(c_args, a);
    mem_free(c_command, a);

    if (status == 0) {
        return (ProcessResult) {.result = Ok, .process = {pid}};
    } else  {
        SpawnError err;
        switch (status) {
        case ENOENT:
            err = SPExecNotExists;
            break;
        case EAGAIN:
            err = SpNoMemory;
            break;
        case ENOMEM:
            err = SpNoMemory;
            break;
        case ENOSYS:
            err = SpNotSupported;
            break;
        default:
            panic(mv_string("unrecognized errno in create_process"));
        }
        return (ProcessResult) {.result = Err, .error = err};
    }

}

WaitResult wait_on_process(Process process) {
    int status;
    int result = waitpid(process.id, &status, 0);
    if (result == -1) {
        return (WaitResult) {.result = Err, .status = 0};
    }
    // 2. Inspect the status AFTER waitpid populates 'status'
    if (WIFEXITED(status)) {
        int exit_code = WEXITSTATUS(status);
        // Process exited normally with exit_code
        return (WaitResult) {.result = Ok, .status = exit_code};
    }
    // See https://www.man7.org/linux/man-pages/man2/wait.2.html
    panic(mv_string("TODO: finish wait_on_process"));
}

Result_t kill_process(Process process) {
    pid_t pid = process.id;
    if (kill(pid, SIGKILL)) {
        // TODO: possible error codes?
        return Err;
    } else {
        return Ok;
    }
}

#elif OS_FAMILY == WINDOWS

Process* create_process(String command, String args, IStream* stdin, OStream* stdout) {
    panic(mv_string("Not implemented: create_process on windows"));
}

Result_t kill_process(Process* process) {
    panic(mv_string("Not implemented: kill_process on windows"));
}

#endif
