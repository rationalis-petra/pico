#ifndef __ATLAS_DATA_ERROR_H
#define __ATLAS_DATA_ERROR_H

#include "platform/jump.h"
#include "components/pretty/document.h"

#include "pico/data/range.h"

typedef struct {
    Document* message;
    Range range;
    String captured_file;
    String filename;
} AtlasError;

typedef struct {
    volatile AtlasError error;
    jump_buf buf; 
} AtErrorPoint;

_Noreturn void throw_at_error(AtErrorPoint* point, AtlasError err); 

#endif
