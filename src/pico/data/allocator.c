#include "pico/data/allocator.h"

void* call_alloc(size_t size, PiAllocator* pa) {
    void* alloc_fn = pa->vtable->pi_alloc;
    void* ctx = pa->ctx_ptr;
    void* out;
    __asm__ __volatile__("push %2 \n"
                         "call *%1 \n"
                         "pop  %0  \n"
                         : "=r" (out)

                         : "r" (alloc_fn)
                         , "r" (size)
                         , "r" (ctx));
    return out;
}

void* call_realloc(void* data, size_t size, PiAllocator* pa) {
    void* realloc_fn = pa->vtable->pi_realloc;
    void* ctx = pa->ctx_ptr;
    void* out = NULL;
    __asm__ __volatile__("push %2 \n"
                         "push %3 \n"
                         "call *%1 \n"
                         "pop %0  \n"
                         : "=r" (out)
                         : "r" (realloc_fn)
                         , "r" (data)
                         , "r" (ctx));

    return out;
}

void call_free(void* data, PiAllocator* pa) {
    void* free_fn = pa->vtable->pi_free;
    void* ctx = pa->ctx_ptr;
    __asm__ __volatile__("push %1 \n"
                         "push %2 \n"
                         "call *%0 \n"
                         :  // No output...
                         : "r" (free_fn)
                         , "r" (data)
                         , "r" (ctx));
}

Allocator convert_allocator(PiAllocator* pa) {
    typedef void* (*malloc_type)(size_t, void*);
    typedef void* (*realloc_type)(void*, size_t, void*);
    typedef void (*free_type)(void*, void*);
    return (Allocator) {
        .malloc = (malloc_type)call_alloc,
        .realloc = (realloc_type)call_realloc,
        .free = (free_type)call_free,
        .ctx = pa
    };
}
