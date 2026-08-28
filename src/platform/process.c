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
        return (ProcessResult) {.result = Ok, .process = {.id = pid}};
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
#include <windows.h>

ProcessResult create_process(String command, StringArray args) {
    STARTUPINFO si;
    PROCESS_INFORMATION pi;
    Allocator* a = get_std_allocator();
    size_t total_memsize = command.memsize + 1;
    for (size_t i = 0; i < args.len; i++) {
        total_memsize += args.data[i].memsize + 1;
    }
    char* c_command = mem_alloc(total_memsize, a);
    memcpy(c_command, command.bytes, command.memsize);
    c_command[command.memsize] = ' ';
    size_t offset = command.memsize + 1;
    // TODO (BUG): do proper sanitization of inputs (adding quotes to arugments etc.)
    for (size_t i = 0; i < args.len; i++) {
        memcpy(c_command + offset, args.data[i].bytes, args.data[i].memsize);
        c_command[offset + args.data[i].memsize] = ' ';
        offset += args.data[i].memsize + 1;
    }
    c_command[offset - 1] = '\0';

    ZeroMemory( &si, sizeof(si) );
    si.cb = sizeof(si);
    ZeroMemory( &pi, sizeof(pi) );

    // Start the child process. 
    if( !CreateProcess( NULL,   // No module name (use command line)
        c_command,      // Command line
        NULL,           // Process handle not inheritable
        NULL,           // Thread handle not inheritable
        FALSE,          // Set handle inheritance to FALSE
        0,              // No creation flags
        NULL,           // Use parent's environment block
        NULL,           // Use parent's starting directory 
        &si,            // Pointer to STARTUPINFO structure
        &pi )           // Pointer to PROCESS_INFORMATION structure
    ) 
    {
        panic(mv_string("TODO: add proper error checks to process creation on windows"));
    }

    // Close thread handle
    CloseHandle( pi.hThread );

    return (ProcessResult) {.result = Ok, .process = {.handle = pi.hProcess}};
}

WaitResult wait_on_process(Process process) {
    WaitForSingleObject( process.handle, INFINITE );
    CloseHandle( process.handle ); // TODO: where to insert handle closing?? make a part of the API?
    DWORD exit_code;
    if (GetExitCodeProcess(process.handle, &exit_code)) {
        return (WaitResult) {.result = Err, .status = 0};
    } else {
        return (WaitResult) {.result = Ok, .status = exit_code};
    }

}

Result_t kill_process(Process process) {
    // TODO: this has different semantics to the unix equivalent...
    if (TerminateProcess(process.handle, 0)) {
        // TODO: possible error codes?
        return Ok;
    } else {
        return Err;
    }
}

#endif
