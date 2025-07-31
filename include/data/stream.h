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
} StreamResult;

typedef struct IStream IStream;
typedef struct OStream OStream;
typedef struct IOStream {
    IStream* istream;
    OStream* ostream;
} IOStream;

// Constructors
IStream* get_stdin_stream();
IStream* open_file_istream(String filename, Allocator* a);

// mv/mk string istream - creates an istream which reads the contents of a string.
// The end of the string produces an EOF, and the mv_ constructor will take ownership 
// of the string.
IStream* mk_string_istream(String contents, Allocator* a);
IStream* mv_string_istream(String contents, Allocator* a);

// A capturing istream - an istream which stores process bytes into a buffer.
// This buffer can be retrieved with get_captured_bufffer. Calling get_captured_buffer
// on an istream that is non-capturing will return NULL.
// the capturing istream is NOT responsible for the cleanup of its inner stream
IStream* mk_capturing_istream(IStream* stream, Allocator* a);
String* get_captured_buffer(IStream* contents);
void uncapture_istream(IStream* stream);

OStream* get_stdout_stream();
OStream* open_file_ostream(String filename, Allocator* a);
OStream* mk_string_ostream(Allocator* a);

String* current_string(OStream* os, Allocator* a);

// Destructors
void delete_istream(IStream* stream, Allocator* a);
void delete_ostream(OStream* stream, Allocator* a);

// istream methods
StreamResult peek(IStream* stream, uint32_t* out);
StreamResult next(IStream* stream, uint32_t* out);
size_t bytecount(IStream* stream);
void reset_bytecount(IStream* stream);

// ostream methods
void write_impl(int char_literal, OStream* stream);
void write_codepoint(uint32_t codepoint, OStream* stream);
void write_string(String str, OStream* stream);

#endif
