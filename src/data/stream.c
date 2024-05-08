#include <stdio.h>

#include "data/stream.h"
#include "encodings/types.h"

//--------------------------- istream definitions ---------------------------//

typedef enum IStreamType {
    IStreamFile
} IStreamType;

typedef struct FileIStream {
    FILE* file_ptr;
    bool owns;
    bool peeked;
    uint32_t peek_codepoint;
    encoding encoding;
} FileIStream;

struct istream {
    IStreamType type;
    union {
        FileIStream file_istream;
    } impl;
};

// Constructors
istream* get_stdin_stream(void) {
    static bool initialized = false;
    static istream cin;

    if (!initialized) {
        cin.type = IStreamFile;
        cin.impl.file_istream.peeked = false;
        cin.impl.file_istream.file_ptr = stdin;
        cin.impl.file_istream.owns = false;
        cin.impl.file_istream.encoding = UTF_8;
        initialized = false;
    }

    return &cin;
}

void delete_istream(istream* stream, allocator a) {
    switch (stream->type) {
    case IStreamFile: {
        if (stream->impl.file_istream.owns) {
            fclose(stream->impl.file_istream.file_ptr);
        }
        mem_free(stream, a);
    } break;
    }
}

// istream methods
stream_result peek(istream* stream, uint32_t* out) {
    switch (stream->type) {
    case IStreamFile: {
        // save current position
        if (stream->impl.file_istream.peeked) {
            *out = stream->impl.file_istream.peek_codepoint;
            return StreamSuccess;
        }
        else {
            stream_result result = next(stream, &(stream->impl.file_istream.peek_codepoint));
            stream->impl.file_istream.peeked = true;
            *out = stream->impl.file_istream.peek_codepoint;
            return result;
        }
    } break;
    default:
        return StreamImplError;
    }
}

stream_result next(istream* stream, uint32_t* out) {
    switch (stream->type) {
    case IStreamFile: {
        if (stream->impl.file_istream.peeked == false) {
            int next_int = fgetc(stream->impl.file_istream.file_ptr);

            if (next_int == EOF) {
                *out = 0;
                return StreamEnd;
            };
            // Assume utf-8 encoding (may be wrong!!)
            uint8_t head = (uint8_t)next_int;
            uint8_t num_bytes = num_bytes_utf8(head);
            uint8_t in_bytes[4];
            in_bytes[0] = head;
            for (uint8_t i = 1; i < num_bytes; i++) {
                int next_byte = fgetc(stream->impl.file_istream.file_ptr);
                if (next_byte == EOF) {
                    *out = 0;
                    return StreamEnd;
                };
                in_bytes[i] = (uint8_t)next_byte;
            }
            if (decode_point_utf8(&num_bytes, in_bytes, out)) {
                return StreamSuccess;
            }
            else {
                return StreamEncodingFailue;
            }
        }
        else {
            stream->impl.file_istream.peeked = false;
            *out = stream->impl.file_istream.peek_codepoint;
        }
    } break;
    default:
        return StreamImplError;
    }
}

//string get_n(istream* stream, size_t nchars);
//string get_all(istream* stream);
//string get_until(istream* stream, uint32_t codepoint);



//--------------------------- ostream definitions ---------------------------//

typedef enum OStreamType {
    OStreamFile
} OStreamType;

typedef struct FileOStream {
    FILE* file_ptr;
    bool owns;
    encoding etype;
} FileOStream;

struct ostream {
    OStreamType type;
    union {
        FileOStream file_ostream;
    } impl;
};

ostream* get_stdout_stream() {
    static bool initialized = false;
    static ostream cout;

    if (!initialized) {
        initialized = true;
        cout.type = OStreamFile;
        cout.impl.file_ostream.file_ptr = stdout;
        cout.impl.file_ostream.owns = false;
        cout.impl.file_ostream.etype = UTF_8;
    }
    return &cout;
}

void delete_ostream(ostream* stream, allocator a) {
    switch (stream->type) {
    case OStreamFile: {
        if (stream->impl.file_ostream.owns) {
            fclose(stream->impl.file_ostream.file_ptr);
        }
        mem_free(stream, a);
    } break;
    }
}

void write_impl(int char_literal, ostream* stream) {
    switch (stream->type) {
    case OStreamFile: {
        fputc(char_literal, stream->impl.file_ostream.file_ptr);
        break;
    }
    }
}

void write_codepoint(uint32_t codepoint, ostream* stream) {
    switch (stream->type) {
    case OStreamFile: {
        // for now, we just use UTF-8 encoding
        uint8_t nchar;
        uint8_t data[4];
        encode_point_utf8(data, &nchar, codepoint);
        data[nchar] = 0;

        // Note: This assumes that the output stream is utf-8 encoded
        //       it also assumes that the char = uint8_t
        fputs((char*)data, stream->impl.file_ostream.file_ptr);
        break;
    }
    }
}

void write_string(string str, ostream* stream) {
    switch (stream->type) {
    case OStreamFile:
        // TODO: this is only right for now (while strings are utf-8 internally...)
        fputs((char*)str.bytes, stream->impl.file_ostream.file_ptr);
        break;
    }
}


