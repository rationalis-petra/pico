#ifndef __ATLAS_DATA_ERROR_H
#define __ATLAS_DATA_ERROR_H

#include "platform/jump.h"
#include "components/pretty/document.h"

#include "pico/data/error.h"

typedef struct {
    Document* message;
    Range range;
    String captured_file;
    String filename;
} AtlasError;

typedef struct {
    MultiError error;
    String captured_file;
    String filename;
} AtlasMultiError;

typedef struct {
    volatile AtlasMultiError error;
    jump_buf buf; 
} AtErrorPoint;

_Noreturn void throw_at_error(AtErrorPoint* point, AtlasError err); 
_Noreturn void throw_at_multi_error(AtErrorPoint* point, AtlasMultiError err); 

#endif
