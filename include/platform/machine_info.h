#ifndef __PLATFORM_MACHINE_INFO_H
#define __PLATFORM_MACHINE_INFO_H

#define UNIX 10
#define SYSTEM_V_64 11

#define WINDOWS 20
#define WIN_64  21

// For now, we assume that the instruction set is x86_64
// This gives the following sizes
#define REGISTER_SIZE 8
#define ADDRESS_SIZE 8
#define ADDRESS_ALIGN 8
#define ADDRESSABLE_BITS 1

#if defined(_WIN32) || defined(WIN32) || defined(__CYGWIN__)
  #define OS_FAMILY WINDOWS
#elif defined(__unix__)
  #define OS_FAMILY UNIX
#else 
  #error "Only recognized families are Unix/Windows"
#endif

#if OS_FAMILY == WINDOWS
  /* Widnows AMD64 ABI
   * -----------------
   * Argument Registers: RCX, RDX, R8, R9
   *
   * Nonvolatile: RBX, RBP, RDI, RSI, RSP, R12, R13, R14, R15
   * Volatile: RAX, RCX, R8, R9, R10, R11
   */
  #define ABI WIN_64
#elif OS_FAMILY == UNIX
  /* System V AMD64 ABI
   * ------------------
   * Argument Registers: RDI, RSI, RDX, RCX, R8, R9
   * 
   * Nonvolatile: RBX, RSP, RBP, R12, R13, R14, R15
   * Volatile: RAX, RCX, RDX, RSI, RDI, R8, R9, R10, R11
   */
#define ABI SYSTEM_V_64
#else 
  #error "Callign convention: only recongized families are Windows and Unix"
#endif


#endif
