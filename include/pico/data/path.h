#ifndef __PICO_DATA_PATH_H
#define __PICO_DATA_PATH_H

/**
 * The symbol table is a global data-structure used by pico to map 
 * strings (slow to compare) to symbols (fast to compare). 
 * 
 * The table is string-specfic, so takes advantage of foreknowledge of string layout 
 */

#include "platform/memory/allocator.h"
#include "pico/values/values.h"

typedef NameArray Path;

bool path_eq(Path lhs, Path rhs);
uint64_t hash_path(Path path);

#endif
