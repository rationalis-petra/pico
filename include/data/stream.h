#ifndef __DATA_STREAM_H
#define __DATA_STREAM_H

#include <stdint.h>
#include <stdbool.h>

#include "data/string.h"

// The stream type is a generic interface allowing character input or output.

typedef enum StreamResult {
    StreamSuccess,
    StreamEnd,
    StreamLostSource,
    StreamEncodingFailue,
    StreamImplError,
} StreamResult;

typedef struct IStream IStream;
typedef struct OStream OStream;
typedef struct IOStream {
    IStream* istream;
    OStream* ostream;
} IOStream;

// Constructors
IStream* get_stdin_stream();
OStream* get_stdout_stream();

// Destructors
void delete_istream(IStream* stream, Allocator* a);
void delete_ostream(OStream* stream, Allocator* a);


// istream methods
StreamResult peek(IStream* stream, uint32_t* out);
StreamResult next(IStream* stream, uint32_t* out);

// ostream methods
void write_impl(int char_literal, OStream* stream);
void write_codepoint(uint32_t codepoint, OStream* stream);
void write_string(String str, OStream* stream);

#endif
