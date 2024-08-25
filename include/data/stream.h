#ifndef __DATA_STREAM_H
#define __DATA_STREAM_H

#include <stdint.h>
#include <stdbool.h>

#include "data/string.h"

// The stream type is a generic interface allowing character input or output.

typedef enum stream_result {
    StreamSuccess,
    StreamEnd,
    StreamLostSource,
    StreamEncodingFailue,
    StreamImplError,
} stream_result;

typedef struct istream istream;
typedef struct ostream ostream;
typedef struct iostream {
    istream* istream;
    ostream* ostream;
} iostream;

    
// Constructors
istream* get_stdin_stream();
ostream* get_stdout_stream();

// Destructors
void delete_istream(istream* stream, allocator a);
void delete_ostream(ostream* stream, allocator a);


// istream methods
stream_result peek(istream* stream, uint32_t* out);
stream_result next(istream* stream, uint32_t* out);

// ostream methods
void write_impl(int char_literal, ostream* stream);
void write_codepoint(uint32_t codepoint, ostream* stream);
void write_string(string str, ostream* stream);

#endif
