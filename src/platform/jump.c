#include "platform/jump.h"
#include "platform/machine_info.h"

// We use the naked attribute to instruct gcc to avoid geneating a prolog/epilog
// We store all registers (even ones that are caller saved) to avoid generating
// different code for different ABIs,

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
__attribute__((naked))
int set_jump(jump_buf buf) {
#if ABI == SYSTEM_V_64
    __asm(
          // Store the return RIP address (Currently @ top of stack) first
          // element of BUF, which is located in RCX (as per SYSTEM_V_64)
          "mov (%rsp), %rax\n"
          "mov %rax,  0(%rdi)\n"

          // Now, store the return stack pointer in the second element of buf
          "lea 8(%rsp), %rax\n"
          "mov %rax,  8(%rdi)\n"

          // For the other registers, we can directly write each into their
          // respective indices in the buffer
          "mov %rbp, 16(%rdi)\n"
          "mov %rbx, 24(%rdi)\n"
          /* "mov %rdi, 32(%rdi)\n" */
          /* "mov %rsi, 40(%rdi)\n" */
          "mov %r12, 48(%rdi)\n"
          "mov %r13, 56(%rdi)\n"
          "mov %r14, 64(%rdi)\n"
          "mov %r15, 72(%rdi)\n"

          // Set the return value to 0
          // Note: we use eax as int is 32-bit!
          "xor %eax, %eax\n"
          "ret\n"
          );
#elif ABI == WIN_64
    __asm(
          // Store the return RIP address (Currently @ top of stack) first
          // element of BUF, which is located in RCX (as per SYSTEM_V_64)
          "mov (%rsp), %rax\n"
          "mov %rax,  0(%rcx)\n"

          // Now, store the return stack pointer in the second element of buf
          "lea 8(%rsp), %rax\n"
          "mov %rax,  8(%rcx)\n"

          // For the other registers, we can directly write each into their
          // respective indices in the buffer
          "mov %rbp, 16(%rcx)\n"
          "mov %rbx, 24(%rcx)\n"
          "mov %rdi, 32(%rcx)\n"
          "mov %rsi, 40(%rcx)\n"
          "mov %r12, 48(%rcx)\n"
          "mov %r13, 56(%rcx)\n"
          "mov %r14, 64(%rcx)\n"
          "mov %r15, 72(%rcx)\n"

          // Set the return value to 0
          // Note: we use eax as int is 32-bit!
          "xor %eax, %eax\n"
          "ret\n"
          );
#else
#error "Unknown calling convention"
#endif
}

__attribute__((naked,noreturn))
void long_jump(jump_buf buf, int val) {
    // avoid unused parameter warnings
#if ABI == SYSTEM_V_64
    __asm(
          // Restore the registers in inverse order of how they were pushed
          // This is completely arbitrary and therefore for aesthetic reasons only!
          "mov 72(%rdi), %r15\n"
          "mov 64(%rdi), %r14\n"
          "mov 56(%rdi), %r13\n"
          "mov 48(%rdi), %r12\n"
          /* "mov 40(%rdi), %rsi\n" */
          /* "mov 32(%rdi), %rdi\n" */
          "mov 24(%rdi), %rbx\n"
          "mov 16(%rdi), %rbp\n"
          "mov  8(%rdi), %rsp\n"

          // longjmp will 'return' (in eax) it's second argument (passed in esi)
          "mov %esi, %eax\n"

          // jmp to the cached RIP, making it look as if setjmp just returned!
          "jmp *0(%rdi)\n"
    );
#elif ABI == WIN_64
    __asm(
          // Restore the registers in inverse order of how they were pushed
          // This is completely arbitrary and therefore for aesthetic reasons only!
          "mov 72(%rcx), %r15\n"
          "mov 64(%rcx), %r14\n"
          "mov 56(%rcx), %r13\n"
          "mov 48(%rcx), %r12\n"
          "mov 40(%rcx), %rsi\n"
          "mov 32(%rcx), %rdi\n"
          "mov 24(%rcx), %rbx\n"
          "mov 16(%rcx), %rbp\n"
          "mov  8(%rcx), %rsp\n"

          // longjmp will 'return' (in eax) it's second argument (passed in edx)
          "mov %edx, %eax\n"

          // jmp to the cached RIP, making it look as if setjmp just returned!
          "jmp *0(%rcx)\n"
    );
#else
#error "Unknown calling convention"
#endif
}
#pragma GCC diagnostic pop

