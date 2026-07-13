#ifndef __PICO_BUILD_BUILD_H
#define __PICO_BUILD_BUILD_H

#include "pico/values/modular.h"

/**
 * The build interface for relic (currently) is very simple:
 *   Provide a module and an entry-point (start function), and the build process
 *   will:
 *   • Walk through all dependencies of the entry-point
 *   • Build up a representation of an executable file by copying and
 *     linking all *relic* functions. This representation
 *   • Write the program to disk.
 *   • Call the system linker to link this file to a plain 'library' file,
 *     containing all symbols needed by the base package.
 */


typedef struct RelicProgram RelicProgram;

RelicProgram* build_program(Module* module, Symbol entry_point, Allocator* a);
void write_program(RelicProgram* program, String filename);
void link_program(String program, String lib, String out_name);

#endif
