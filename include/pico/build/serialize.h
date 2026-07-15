#ifndef __PICO_BUILD_SERIALIZE_H
#define __PICO_BUILD_SERIALIZE_H

#include "data/meta/array_header.h"
#include "pico/build/gather.h"
#include "pico/values/modular.h"

/**
 * Generally, the output of a build is highly platform dependent,
 * with unix systems producing ELF files and windows systems producing COFF/PE
 * files.
 *
 * However, the process of 'flattening' the existing module structure into flat
 * arrays of both data and code fragments with linking information is necessary
 * for both. As such, the relevant types + functions are stored in a shared impl
 * unit.
 */

U8Array serialize_fragments(ProgramFragments fragments, Allocator* allocator);

#endif
