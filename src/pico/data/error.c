#include "data/stream.h"
#include "platform/io/terminal.h"

#include "pretty/document.h"
#include "pretty/stream_printer.h"
#include "pretty/standard_types.h"
#include "pico/data/error.h"

void throw_pi_error(PiErrorPoint* point, PicoError err) {
    point->error = err;
    long_jump(point->buf, 1);
}

const size_t max_prev_line_numbers = 5;

void display_error(PicoError error, IStream *is, OStream* cout, Allocator* a) {
    // TODO (FEAT): As user code can produce source positions, we should ensure
    // that we the (start, end) range in the error to avoid segfaults and provide
    // friendlier errors.
    String* buffer = get_captured_buffer(is);
    if (buffer) {
        // Get the line number of the affected area, and the start position of at most 
        // the previous max_prev_line_numbers lines
        size_t current_start = 0;
        U64Array prev_line_starts = mk_u64_array(max_prev_line_numbers, a);
        prev_line_starts.len = max_prev_line_numbers;
        size_t line_number = 0;
        size_t prev_line = 0;
        for (size_t i = 0; i < error.range.start; i++) {
          if (buffer->bytes[i] == '\n') {
              prev_line_starts.data[prev_line] = current_start;
              line_number++;
              prev_line = (prev_line + 1) % 5;
              current_start = i + 1;
          }
        }
        size_t affected_line_end = buffer->memsize;
        for (size_t i = error.range.end; i < buffer->memsize; i++) {
          if (buffer->bytes[i] == '\n') {
              affected_line_end = i;
              break;
          }
        }
        
        // Colour for "irrelevant" code
        start_coloured_text(colour(150, 150, 150));
        // Now, gather the past n lines
        if (line_number < max_prev_line_numbers) {
            for (size_t i = 0; i < line_number; i++) {
                size_t line_start = prev_line_starts.data[i];
                size_t next_line_start = (i + 1 == line_number)
                    ? current_start
                    : prev_line_starts.data[1 + i];

                Document* doc = pretty_u64(i + 1, a);
                write_doc(doc, cout);
                delete_doc(doc, a);
                write_string(mv_string(" | "), cout);
                String str = substring(line_start, next_line_start, *buffer, a);
                write_string(str, cout);
                delete_string(str, a);
            }
        }
        else {
            for (size_t i = 0; i < max_prev_line_numbers; i++) {
                size_t line_start = prev_line_starts.data[(prev_line + i) % 5];
                size_t next_line_start = (i + 1 == max_prev_line_numbers)
                    ? current_start
                    : prev_line_starts.data[(prev_line + 1 + i) % 5];

                Document* doc = pretty_u64(line_number + 1 - (5 - i), a);
                write_doc(doc, cout);
                delete_doc(doc, a);
                write_string(mv_string(" | "), cout);
                String str = substring(line_start, next_line_start, *buffer, a);
                write_string(str, cout);
                delete_string(str, a);
            }
        }

        Document* doc = pretty_u64(line_number + 1, a);
        write_doc(doc, cout);
        delete_doc(doc, a);
        write_string(mv_string(" | "), cout);

        String s1 = substring(current_start, error.range.start, *buffer, a);
        // TODO (BUG) '+1' won't work with UTF-8!
        String bad_code = substring(error.range.start, error.range.end, *buffer, a);
        String s2 = substring(error.range.end, affected_line_end, *buffer, a);;

        write_string(s1, cout);

        start_coloured_text(colour(208, 105, 30));
        write_string(bad_code, cout);
        end_coloured_text();

        write_string(s2, cout);

        delete_string(s1, a);
        delete_string(bad_code, a);
        delete_string(s2, a);

        // End of surrounding code
        end_coloured_text();

        write_string(mv_string("\n"), cout);
        start_coloured_text(colour(200, 20, 20));
        write_string(error.message, cout);
        end_coloured_text();
    } else {
        write_string(error.message, cout);
    }
    write_string(mv_string("\n"), cout);
}
