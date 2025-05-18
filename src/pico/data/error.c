#include "data/stream.h"
#include "platform/io/terminal.h"

#include "pico/data/error.h"

void throw_pi_error(PiErrorPoint* point, PicoError err) {
    point->error = err;
    long_jump(point->buf, 1);
}

void display_error(PicoError error, IStream *is, OStream* cout, Allocator* a) {
    String* buffer = get_captured_buffer(is);
    if (buffer) {
        String s1 = substring(0, error.range.start, *buffer, a);
        // TODO (BUG) '+1' won't work with UTF-8!
        String err = substring(error.range.start, error.range.end, *buffer, a);
        String s2 = substring(error.range.end, buffer->memsize, *buffer, a);;

        write_string(s1, cout);

        start_coloured_text(colour(200, 0, 0));
        write_string(err, cout);
        end_coloured_text();

        write_string(s2, cout);

        delete_string(s1, a);
        delete_string(err, a);
        delete_string(s2, a);

        write_string(mv_string("\n"), cout);
        write_string(error.message, cout);
    } else {
        write_string(error.message, cout);
    }
    write_string(mv_string("\n"), cout);
}
