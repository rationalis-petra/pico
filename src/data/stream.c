#include <stdio.h>
#include <string.h>

#include "platform/filesystem/filesystem.h"
#include "platform/signals.h"
#include "data/stream.h"

#include "components/encodings/types.h"
#include "components/encodings/utf8.h"

//--------------------------- istream definitions ---------------------------//

typedef enum {
    IStreamFile,
    IStreamString,
    IStreamCapturing,
} IStreamType;

typedef struct {
    File* file_ptr;
    bool owns;
    bool peeked;
    uint8_t peeked_bytes;
    Encoding encoding;

    uint32_t peek_codepoint;
    StreamResult peek_result;
} FileIStream;

typedef struct {
    String string;
    bool owns;
    bool peeked;
    uint8_t peeked_bytes;
    Encoding encoding;
    size_t index;

    uint32_t peek_codepoint;
    StreamResult peek_result;
} StringIStream;

typedef struct  {
    IStream* inner;
    String buffer;
    size_t total_bufsize;
    Allocator* gpa;
} CapturingIStream;

struct IStream {
    IStreamType type;
    size_t bytecount;
    union {
        FileIStream file_istream;
        StringIStream string_istream;
        CapturingIStream capturing_istream;
    } impl;
};

IStream* mk_capturing_istream(IStream *stream, Allocator *a) {
    IStream* outer_stream = mem_alloc(sizeof(IStream), a);
    const size_t initial_memsize =  1024 * sizeof(uint8_t);
    String buffer = (String) {
        .memsize = 0,
        .bytes = mem_alloc(initial_memsize, a),
    };

    *outer_stream = (IStream) {
        .type = IStreamCapturing,
        .bytecount = stream->bytecount,
        .impl.capturing_istream.inner = stream,
        .impl.capturing_istream.buffer = buffer,
        .impl.capturing_istream.total_bufsize = initial_memsize,
        .impl.capturing_istream.gpa = a,
    };
    return outer_stream;
}

String* get_captured_buffer(IStream* stream) {
    if (stream->type != IStreamCapturing) return NULL;
    return &stream->impl.capturing_istream.buffer;
}

void uncapture_istream(IStream *stream) {
    Allocator* gpa = stream->impl.capturing_istream.gpa;
    mem_free(stream->impl.capturing_istream.buffer.bytes, gpa);
    mem_free(stream, gpa);
}

// Constructors
IStream* get_stdin_stream(void) {
    static bool initialized = false;
    static IStream cin;

    if (!initialized) {
        cin = (IStream) {
            .type = IStreamFile,
            .bytecount = 0,
            .impl.file_istream.peeked = false,
            .impl.file_istream.file_ptr = std_in(),
            .impl.file_istream.owns = false,
            .impl.file_istream.encoding = UTF_8,
        };
        initialized = false;
    }

    return &cin;
}

IStream* open_file_istream(String filename, Allocator* a) {
    FileResult res = open_file(filename, Read, a);
    if (res.type != Ok) return NULL;

    IStream* ifile = mem_alloc(sizeof(IStream), a);
    *ifile = (IStream) {
     .type = IStreamFile,
     .impl.file_istream.peeked = false,
     .impl.file_istream.file_ptr = res.file,
     .impl.file_istream.owns = true,
     .impl.file_istream.encoding = UTF_8,
    };
    return ifile;
}

IStream* mv_string_istream(String string, Allocator* a) {
    IStream* istring = mem_alloc(sizeof(IStream), a);

    *istring = (IStream) {
     .type = IStreamString,
     .bytecount = 0,
     .impl.string_istream.peeked = false,
     .impl.string_istream.string = string,
     .impl.string_istream.index = 0,
     .impl.string_istream.owns = false,
     .impl.string_istream.encoding = UTF_8,
    };
    return istring;
}

IStream* mk_string_istream(String string, Allocator* a) {
    IStream* istring = mem_alloc(sizeof(IStream), a);

    *istring = (IStream) {
     .type = IStreamString,
     .bytecount = 0,
     .impl.string_istream.peeked = false,
     .impl.string_istream.string = copy_string(string, a),
     .impl.string_istream.index = 0,
     .impl.string_istream.owns = true,
     .impl.string_istream.encoding = UTF_8,
    };
    return istring;
}

void delete_istream(IStream* stream, Allocator* a) {
    switch (stream->type) {
    case IStreamFile: {
        if (stream->impl.file_istream.owns) {
            close_file(stream->impl.file_istream.file_ptr);
        }
        mem_free(stream, a);
        break;
    } 
    case IStreamString: {
        if (stream->impl.string_istream.owns) {
            delete_string(stream->impl.string_istream.string, a);
        }
        mem_free(stream, a);
        break;
    }
    case IStreamCapturing: {
        mem_free(stream->impl.capturing_istream.buffer.bytes, a);
        mem_free(stream, a);
        break;
    }
    }
}

// istream methods
StreamResult peek(IStream* stream, uint32_t* out) {
    switch (stream->type) {
    case IStreamFile: {
        // save current position
        if (stream->impl.file_istream.peeked) {
            *out = stream->impl.file_istream.peek_codepoint;
            return stream->impl.file_istream.peek_result;
        }
        else {
            // This is a peek: don't adjust bytecount!
            size_t old_bytecount = stream->bytecount;
            StreamResult result = next(stream, &(stream->impl.file_istream.peek_codepoint));
            stream->impl.file_istream.peeked_bytes = stream->bytecount - old_bytecount;
            stream->bytecount = old_bytecount;

            stream->impl.file_istream.peeked = true;
            *out = stream->impl.file_istream.peek_codepoint;
            stream->impl.file_istream.peek_result = result;
            return result;
        }
    } break;
    case IStreamString: {
        // save current position
        if (stream->impl.string_istream.peeked) {
            *out = stream->impl.string_istream.peek_codepoint;
            return stream->impl.string_istream.peek_result;
        }
        else {
            // This is a peek: don't adjust bytecount!
            size_t old_bytecount = stream->bytecount;
            StreamResult result = next(stream, &(stream->impl.string_istream.peek_codepoint));
            stream->impl.string_istream.peeked_bytes = stream->bytecount - old_bytecount;
            stream->bytecount = old_bytecount;

            stream->impl.string_istream.peeked = true;
            *out = stream->impl.string_istream.peek_codepoint;
            stream->impl.string_istream.peek_result = result;
            return result;
        }
    } break;
    case IStreamCapturing: {
        return peek(stream->impl.capturing_istream.inner, out);
    }
    default:
        panic(mv_string("Invalid istream provided to next!"));
    }
}

StreamResult next(IStream* stream, uint32_t* out) {
    switch (stream->type) {
    case IStreamFile: {
        if (stream->impl.file_istream.peeked == false) {
            uint8_t head;
            bool fail = read_byte(stream->impl.file_istream.file_ptr, &head);

            // TODO: improve next_byte function with more return values/encoding
            if (fail) {
                *out = 0;
                return StreamEnd;
            };
            // Assume utf-8 encoding (may be wrong!!)
            uint8_t num_bytes = num_bytes_utf8(head);
            uint8_t in_bytes[4];
            in_bytes[0] = head;
            for (uint8_t i = 1; i < num_bytes; i++) {
                uint8_t next_byte;
                fail = read_byte(stream->impl.file_istream.file_ptr, &next_byte);
                if (fail) {
                    *out = 0;
                    return StreamEnd;
                };
                in_bytes[i] = next_byte;
            }
            if (decode_point_utf8(&num_bytes, in_bytes, out)) {
                stream->bytecount += num_bytes;
                return StreamSuccess;
            }
            else {
                return StreamEncodingFailue;
            }
        }
        else {
            stream->impl.file_istream.peeked = false;
            stream->bytecount += stream->impl.file_istream.peeked_bytes;
            *out = stream->impl.file_istream.peek_codepoint;
            return stream->impl.file_istream.peek_result;
        }
        break;
    }
    case IStreamString: {
        if (stream->impl.string_istream.peeked == false) {
            size_t* index = &stream->impl.string_istream.index;
            String* string = &stream->impl.string_istream.string;
            if (*index == string->memsize) {
                *out = 0;
                return StreamEnd;
            };

            // Assume utf-8 encoding (may be wrong!!)
            uint8_t num_bytes;
            if (decode_point_utf8(&num_bytes, string->bytes + *index, out)) {
                *index += num_bytes;
                stream->bytecount += num_bytes;
                return StreamSuccess;
            }
            else {
                return StreamEncodingFailue;
            }
        }
        else {
            stream->impl.string_istream.peeked = false;
            stream->bytecount += stream->impl.string_istream.peeked_bytes;
            *out = stream->impl.string_istream.peek_codepoint;
            return stream->impl.string_istream.peek_result;
        }
        break;
    }
    case IStreamCapturing: {
        StreamResult res = next(stream->impl.capturing_istream.inner, out);
        if (res == StreamSuccess) {
            stream->bytecount = stream->impl.capturing_istream.inner->bytecount;

            uint8_t bytes[4];
            uint8_t size;
            encode_point_utf8(bytes, &size, *out);
            CapturingIStream* cis = &stream->impl.capturing_istream;
            if (cis->buffer.memsize + size >= cis->total_bufsize) {
                size_t new_total_size = cis->total_bufsize * 2;
                String new_buffer = (String) {.memsize = cis->buffer.memsize};
                new_buffer.bytes = mem_alloc(new_total_size, cis->gpa);
                memcpy(new_buffer.bytes, cis->buffer.bytes, cis->buffer.memsize);
                mem_free(cis->buffer.bytes, cis->gpa);
                cis->buffer = new_buffer;
                cis->total_bufsize = new_total_size;
            }
            uint8_t *base = cis->buffer.bytes + cis->buffer.memsize;
            for (size_t i = 0; i < size; i++) {
                base[i] = bytes[i];
            }
            cis->buffer.memsize += size;

#ifdef VALIDATE_INPUTS
            if (cis->buffer.memsize != stream->bytecount + 1)
                panic(mv_string("Capturing istream internal state invalid!"));
#endif
        }
        return res;
        break;
    }
    default:
        panic(mv_string("Invalid istream provided to next!"));
    }
}

StreamResult read_line(IStream* stream, String* out) {
    panic(mv_string("read_line not implemented"));
}

size_t bytecount(IStream *stream) {
    return stream->bytecount;
}
void reset_bytecount(IStream *stream) {
    if (stream->type == IStreamCapturing) {
        reset_bytecount(stream->impl.capturing_istream.inner);
    }
    stream->bytecount = 0;
}

//string get_n(istream* stream, size_t nchars);
//string get_all(istream* stream);
//string get_until(istream* stream, uint32_t codepoint);



//--------------------------- ostream definitions ---------------------------//

typedef enum OStreamType {
    OStreamFile,
    OStreamString,
} OStreamType;

typedef struct FileOStream {
    File* file_ptr;
    bool owns;
    Encoding etype;
} FileOStream;

typedef struct StringOStream {
    U8Array buffer;
} StringOStream;

struct OStream {
    OStreamType type;
    union {
        FileOStream file_ostream;
        StringOStream string_ostream;
    } impl;
};

OStream* get_stdout_stream() {
    static bool initialized = false;
    static OStream cout;

    if (!initialized) {
        initialized = true;
        cout.type = OStreamFile;
        cout.impl.file_ostream.file_ptr = std_out();
        cout.impl.file_ostream.owns = false;
        cout.impl.file_ostream.etype = UTF_8;
    }
    return &cout;
}

OStream* open_file_ostream(String filename, Allocator* a) {
    FileResult res = open_file(filename, Write, a);
    if (res.type != Ok) return NULL;

    OStream* ofile = mem_alloc(sizeof(OStream), a);
    *ofile = (OStream) {
        .type = OStreamFile,
        .impl.file_ostream.file_ptr = res.file,
        .impl.file_ostream.owns = false,
        .impl.file_ostream.etype = UTF_8,
    };

    return ofile;
}

OStream* mk_string_ostream(Allocator *a) {
    OStream* ostring = mem_alloc(sizeof(OStream), a);
    *ostring = (OStream) {
        .type = OStreamString,
        .impl.string_ostream.buffer = mk_u8_array(1024, a),
    };
    return ostring;
}

String* current_string(OStream *os, Allocator *a) {
    if (os->type != OStreamString) {
        return NULL;
    }
    String* out = mem_alloc(sizeof(String), a);
    out->memsize = os->impl.string_ostream.buffer.len;
    out->bytes = mem_alloc(out->memsize, a);
    memcpy(out->bytes, os->impl.string_ostream.buffer.data, out->memsize);
    return out;
}

void delete_ostream(OStream* stream, Allocator* a) {
    switch (stream->type) {
    case OStreamFile: {
        if (stream->impl.file_ostream.owns) {
            close_file(stream->impl.file_ostream.file_ptr);
        }
        mem_free(stream, a);
        break;
    } 
    case OStreamString: {
        sdelete_u8_array(stream->impl.string_ostream.buffer);
        mem_free(stream, a);
        break;
    }
    }
}

void st_write_byte(uint8_t byte, OStream* stream) {
    switch (stream->type) {
    case OStreamFile: {
        write_byte(stream->impl.file_ostream.file_ptr, byte);
        break;
    }
    case OStreamString: {
        // TODO (BUG): check char_literal < MAX_UINT8
        push_u8(byte, &stream->impl.string_ostream.buffer);
        break;
    }
    }
}

void st_write_codepoint(uint32_t codepoint, OStream* stream) {
    switch (stream->type) {
    case OStreamFile: {
        // for now, we just use UTF-8 encoding
        uint8_t nchar;
        uint8_t data[4];
        encode_point_utf8(data, &nchar, codepoint);
        data[nchar] = 0;

        // Note: This assumes that the output stream is utf-8 encoded
        U8Array chunk = {
            .size = nchar,
            .len = nchar,
            .data = data,
        };
        write_chunk(stream->impl.file_ostream.file_ptr, chunk);
        break;
    }
    case OStreamString: {
        // for now, we just use UTF-8 encoding
        uint8_t nchar;
        uint8_t data[4];
        encode_point_utf8(data, &nchar, codepoint);
        data[nchar] = 0;

        add_u8_chunk(data, nchar, &stream->impl.string_ostream.buffer);
        break;
    }
    }
}

void st_write_string(String str, OStream* stream) {
    switch (stream->type) {
    case OStreamFile: {
        // TODO (FEATURE): Support outputting to non utf-8 ostreams
        // Note: This assumes that the output stream is utf-8 encoded
        U8Array chunk = {
            .size = str.memsize,
            .len = str.memsize,
            .data = str.bytes,
        };
        write_chunk(stream->impl.file_ostream.file_ptr, chunk);
        break;
    }
    case OStreamString:
        add_u8_chunk(str.bytes, str.memsize, &stream->impl.string_ostream.buffer);
        break;
    }
}

void st_write_line(String str, OStream* stream) {
    switch (stream->type) {
    case OStreamFile:
        // TODO (FEATURE): Support outputting to non utf-8 ostreams
        st_write_string(str, stream);
        st_write_string(mv_string("\n"), stream);
        break;
    case OStreamString:
        // TODO: (windows) - \r\n??, part of stream?
        add_u8_chunk(str.bytes, str.memsize, &stream->impl.string_ostream.buffer);
        add_u8_chunk((uint8_t*)"\n", 1, &stream->impl.string_ostream.buffer);
        break;
    }
}


