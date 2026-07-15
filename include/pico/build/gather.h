#ifndef __PICO_BUILD_GATHER_H
#define __PICO_BUILD_GATHER_H

#include "data/meta/array_header.h"
#include "pico/values/modular.h"
#include "pico/data/build_error.h"

/**
 * Generally, the output of a build is highly platform dependent,
 * with unix systems producing ELF files and windows systems producing COFF/PE
 * files.
 *
 * However, the process of 'flattening' gathered fragments about  the existing
 * module structure into flat arrays of both data and code fragments with
 * linking information is necessary for both. As such, the relevant types +
 * functions are stored in a shared impl unit.
 */

typedef enum {
    DFString,
} DataFragmentType;

typedef struct {
    size_t source_offest;

    size_t fragment_id;
    size_t offest;
} GlobalLink;

typedef struct {
    size_t code_size;
    void* binary;
} CodeFragment;

typedef struct {
    DataFragmentType type;
    size_t data_size;
    void* data;
} DataFragment;

ARRAY_HEADER(CodeFragment, cf, CodeFragment);
ARRAY_HEADER(DataFragment, df, DataFragment);

typedef struct {
    CodeFragmentArray code_frags;
    DataFragmentArray data_frags;
    size_t code_size;
    size_t data_size;
} ProgramFragments;

ProgramFragments gather_fragments(Symbol entry_point, Module* module, BuildErrorPoint* point, Allocator* allocator);

#endif
