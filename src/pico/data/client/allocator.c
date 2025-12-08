#include "platform/machine_info.h"
#include "pico/data/client/allocator.h"

void* call_alloc(size_t size, PiAllocator* pa) {
    void* alloc_fn = pa->vtable->pi_alloc;
    void* ctx = pa->ctx_ptr;
    void* out;
    __asm__ __volatile__("push %3 \n"
                         "push %2 \n"
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
    __asm__ __volatile__("push %4 \n"
                         "push %3 \n"
                         "push %2 \n"
                         "call *%1 \n"
                         "pop %0  \n"
                         : "=r" (out)
                         : "r" (realloc_fn)
                         , "r" (size)
                         , "r" (data)
                         , "r" (ctx));

    return out;
}

void call_free(void* data, PiAllocator* pa) {
    void* free_fn = pa->vtable->pi_free;
    void* ctx = pa->ctx_ptr;
    __asm__ __volatile__("push %2 \n"
                         "push %1 \n"
                         "call *%0 \n"
                         : // No output...
                         : "r" (free_fn)
                         , "r" (data)
                         , "r" (ctx));
}

Allocator convert_to_callocator(PiAllocator* pa) {
    typedef void* (*malloc_type)(size_t, void*);
    typedef void* (*realloc_type)(void*, size_t, void*);
    typedef void (*free_type)(void*, void*);

    static AllocatorVTable converted_vtable = {
        .malloc = (malloc_type)call_alloc,
        .realloc = (realloc_type)call_realloc,
        .free = (free_type)call_free,
    };

    return (Allocator) {
        .vtable = &converted_vtable,
        .ctx = pa,
    };
}


__attribute__((naked))
void* pi_alloc_adapter() {
    // Pico alloc call - signature = Proc [(Ptr A) U64].
    // Therefore, stack looks like
    //        Allocator*
    //        size
    // RSP -- return address
    // 1. Pop Allocator* into RAX
    // 3. Move the allocator vtable into R9
    // 4. Move the context pointer R10
    // 2. Pop the size into 1st argument register.

#if ABI == SYSTEM_V_64
    __asm("mov 8(%rsp), %rdi\n"
          "mov 16(%rsp), %rax\n" // allocator
          "mov (%rax), %rcx\n"   // vtable
          "mov (%rcx), %r9\n"    // alloc
          "mov 8(%rax), %rsi\n"  // context

          // ALIGN STACK
          // Get the bottom byte of RSP (store in RAX), which is the info we
          // need to perform (16-byte) alignment.
          // current rsp should have rsp % 16 == 8
          "mov %rsp, %rdx\n"
          "and $0x8, %rdx\n"
          "add $0x8, %rdx\n"
          "and $0x8, %rdx\n"
          "sub %rdx, %rsp\n"
          "push %rdx\n"

          "call *%r9\n"
          "pop %r10\n"
          "add %r10, %rsp\n"

          // return - get return address
          "mov (%rsp), %rcx\n"  // ret addr
          "add $0x18, %rsp\n"    // pop values
          "push %rax\n"
          "push %rcx\n"
          "ret\n"
          );
#elif ABI == WIN_64
    __asm("mov 8(%rsp),  %rcx\n"
          "mov 16(%rsp), %rax\n"  // allocator
          "mov (%rax),   %r11\n"  // vtable
          "mov (%r11),   %r9\n"   // alloc
          "mov 8(%rax),  %rdx\n"  // context

          // ALIGN STACK
          // Get the bottom byte of RSP (store in RAX), which is the info we
          // need to perform (16-byte) alignment.
          // current rsp should have rsp % 16 == 8
          "mov %rsp, %r10\n"
          "and $0x8, %r10\n"
          "add $0x8, %r10\n"
          "and $0x8, %r10\n"
          "sub %r10, %rsp\n"
          "push %r10\n"

          "sub $0x20, %rsp\n"
          "call *%r9\n"
          "add $0x20, %rsp\n"
          "pop %r10\n"
          "add %r10, %rsp\n"

          // return - get return address
          "mov (%rsp), %rcx\n"  // ret addr
          "add $0x18, %rsp\n"    // pop values
          "push %rax\n"
          "push %rcx\n"
          "ret\n"
          );
#else
#error "Unknown calling convention"
#endif
}

__attribute__((naked))
void* pi_realloc_adapter() {
#if ABI == SYSTEM_V_64
  __asm(
      "mov 0x8(%rsp),  %rsi\n" // size
      "mov 0x10(%rsp), %rdi\n" // data
      "mov 0x18(%rsp), %rax\n" // allocator (context)
      "mov (%rax), %rcx\n"     // vtable
      "mov 0x8(%rcx), %r9\n"   // realloc
      "mov 0x8(%rax), %rdx\n"  // context

      // ALIGN THE STACK
      // Step 1. Ensure there is at least 8 bytes of space (to store the offset)
      "mov %rsp, %rcx\n"
      "and $0x8, %rcx\n"
      "add $0x8, %rcx\n"
      "and $0x8, %rcx\n"
      "sub %rcx, %rsp\n"
      "push %rcx\n"

      "call *%r9\n"
      "pop %r10\n"
      "add %r10, %rsp\n"

      // return - get return address
      "mov (%rsp), %rcx\n"    // ret addr
      "add $0x20, %rsp\n"     // pop values
      "push %rax\n"
      "push %rcx\n"
      "ret\n"
        );
#elif ABI == WIN_64
  __asm(
      "mov 0x8(%rsp),  %rdx  \n" // size
      "mov 0x10(%rsp), %rcx  \n" // data
      "mov 0x18(%rsp), %rax  \n" // allocator (context)
      "mov (%rax), %r11      \n" // vtable
      "mov 0x8(%r11), %r9    \n" // realloc
      "mov 0x8(%rax), %r8   \n" // context

      // ALIGN THE STACK
      // Step 1. Ensure there is at least 8 bytes of space (to store the offset)
      "mov %rsp, %r10 \n"
      "and $0x8, %r10  \n"
      "add $0x8, %r10  \n"
      "and $0x8, %r10  \n"
      "sub %r10, %rsp \n"
      "push %r10       \n"

      "sub $0x20, %rsp \n"
      "call *%r9\n"
      "add $0x20, %rsp \n"
      "pop %r10\n"
      "add %r10, %rsp\n"

      // return - get return address
      "mov (%rsp), %rcx\n"    // ret addr
      "add $0x20, %rsp\n"     // pop values
      "push %rax\n"
      "push %rcx\n"
      "ret\n"
        );
#else
#error "Unknown calling convention"
#endif
}

__attribute__((naked))
void pi_free_adapter() {
#if ABI == SYSTEM_V_64
  __asm("mov 8(%rsp), %rdi\n"  // address
        "mov 16(%rsp), %rax\n" // allocator
        "mov (%rax), %rcx\n"   // vtable
        "mov 16(%rcx), %r9\n"  // alloc
        "mov 8(%rax), %rsi\n"  // context

        // ALIGN THE STACK
        // Step 1. Ensure there is at least 8 bytes of space (to store the offset)
        "mov %rsp, %rdx\n"
        "and $0x8, %rdx\n"
        "add $0x8, %rdx\n"
        "and $0x8, %rdx\n"
        "sub %rdx, %rsp\n"
        "push %rdx\n"

        "call *%r9\n"
        "pop %r10\n"
        "add %r10, %rsp\n"

        // return - get return address
        "mov (%rsp), %rcx\n"  // ret addr
        "add $0x18, %rsp\n"   // pop values
        "push %rcx\n"
        "ret\n"
        );
#elif ABI == WIN_64
  __asm("mov 8(%rsp),  %rcx\n"   // address
        "mov 16(%rsp), %rax\n"   // allocator
        "mov (%rax),   %r11\n"   // vtable
        "mov 16(%r11), %r9\n"   // alloc
        "mov 8(%rax),  %rdx\n"  // context

        // ALIGN THE STACK
        // Step 1. Ensure there is at least 8 bytes of space (to store the offset)
        "mov %rsp, %r10 \n"
        "and $0x8, %r10 \n"
        "add $0x8, %r10 \n"
        "and $0x8, %r10 \n"
        "sub %r10, %rsp \n"
        "push %r10      \n"

        "sub $0x20, %rsp\n"
        "call *%r9\n"
        "add $0x20, %rsp\n"
        "pop %r10\n"
        "add %r10, %rsp\n"

        // return - get return address
        "mov (%rsp), %rcx\n"  // ret addr
        "add $0x18, %rsp\n"   // pop values
        "push %rcx\n"
        "ret\n"
        );
#else
#error "Unknown calling convention"
#endif
}

PiAllocator convert_to_pallocator(Allocator *ca) {
    static PiAllocTable converted_vtable = {
        .pi_alloc =  (void*)pi_alloc_adapter,
        .pi_realloc = (void*)pi_realloc_adapter,
        .pi_free =    (void*)pi_free_adapter,
    };

    return (PiAllocator) {
        .type_data = 0, // TODO x00800180018
        .vtable = &converted_vtable,
        .ctx_ptr = ca,
    };
}
