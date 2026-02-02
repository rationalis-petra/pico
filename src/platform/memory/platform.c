#include <string.h>

#include "platform/signals.h"
#include "platform/machine_info.h"

// Relevant OS includes 
#if OS_FAMILY == WINDOWS
  #include <windows.h>
  #include <memoryapi.h>
#elif OS_FAMILY == UNIX
  #include <sys/mman.h>
  #include <unistd.h>
#else 
  #error "platform/memory/executable.c: Only support unix/windows families"
#endif

#include "platform/memory/platform.h"

size_t platform_pagesize () {
#if OS_FAMILY == UNIX
    size_t pagesize = sysconf(_SC_PAGESIZE);
#elif OS_FAMILY == WINDOWS
    size_t pagesize = 1024;
#endif
    return pagesize;
}

MemoryBlock platform_allocate(size_t min_size, AllocateFlags flags) {
    MemoryBlock out;

    // calculate out.size as smallest multiple of pagesize satisfying > min_size
    // platform_pagesize is provided as long int, so we
    size_t pagesize = platform_pagesize();
    out.size = pagesize * ((min_size + pagesize - 1) / pagesize);

#if OS_FAMILY == UNIX
    int prot = 0;
    prot |= (flags & ARead)    ? PROT_READ  : 0;
    prot |= (flags & AWrite)   ? PROT_WRITE : 0;
    prot |= (flags & AExecute) ? PROT_EXEC  : 0;
    
    void* memory = mmap(NULL, out.size, prot, MAP_ANONYMOUS | MAP_FILE | MAP_PRIVATE, -1, 0);
#elif OS_FAMILY == WINDOWS
    int prot = 0;
    if (flags == (ARead | AWrite | AExecute))
        prot = PAGE_EXECUTE_READWRITE;
    else if (flags == (ARead | AExecute))
        prot = PAGE_EXECUTE_READ;
    else if (flags == (ARead | AWrite))
        prot = PAGE_READWRITE;
    else if (flags == ARead)
        prot = PAGE_READONLY;
    else if (flags == AExecute)
        prot = PAGE_EXECUTE;
    else if (flags == 0)
        prot = PAGE_NOACCESS;
    else 
        panic(mv_string("This set of allocation flags is not supported on windows."));

    void* memory = VirtualAlloc(NULL, out.size, MEM_COMMIT | MEM_RESERVE, prot);
#endif
    out.data = memory;

    return out;
}

void platform_free(MemoryBlock block) {
#if OS_FAMILY == UNIX
    munmap(block.data, block.size);
#elif OS_FAMILY == WINDOWS
    VirtualFree(block.data, block.size, MEM_DECOMMIT);
#endif
}
