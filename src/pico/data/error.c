#include "data/stream.h"
#include "data/stringify.h"
#include "platform/io/terminal.h"

#include "pretty/document.h"
#include "pretty/stream_printer.h"
#include "pretty/standard_types.h"
#include "pico/data/error.h"

_Noreturn void throw_pi_error(PiErrorPoint* point, PicoError err) {
    point->multi.has_many = false;
    point->multi.error = err;
    long_jump(point->buf, 1);
}

_Noreturn void throw_pi_errors(PiErrorPoint *point, PtrArray errors) {
    point->multi.has_many = true;
    point->multi.errors = errors;
    long_jump(point->buf, 1);
}

void display_error(MultiError multi, IStream *is, FormattedOStream* fos, Allocator* a) {
    // TODO (FEAT): As user code can produce source positions, we should ensure
    // that we the (start, end) range in the error to avoid segfaults and provide
    // friendlier errors.
    String* buffer = get_captured_buffer(is);

    if (multi.has_many) {
        for (size_t i = 0; i < multi.errors.len; i++) {
            PicoError error = *(PicoError*)multi.errors.data[i];
            if (buffer) {
                size_t prev_lines = i == 0 ? 5 : 2;
                display_code_region(*buffer, error.range, prev_lines, fos, a);
            }

            write_fstring(mv_string("\n"), fos);
            start_coloured_text(colour(200, 20, 20), fos);
            write_doc_formatted(error.message, 120, fos);
            end_coloured_text(fos);
            write_fstring(mv_string("\n"), fos);
        }
    } else {
        if (buffer) {
            display_code_region(*buffer, multi.error.range, 5, fos, a);
        }
        write_fstring(mv_string("\n"), fos);
        start_coloured_text(colour(200, 20, 20), fos);
        write_doc_formatted(multi.error.message, 120, fos);
        end_coloured_text(fos);
        write_fstring(mv_string("\n"), fos);
    }

}

void display_code_region(String buffer, Range range, const size_t lines_prior, FormattedOStream* fos, Allocator* a) {
    // Get the line number of the affected area, and the start position of at most 
    // the previous max_prev_line_numbers lines
    size_t current_start = 0;
    U64Array prev_line_starts = mk_u64_array(lines_prior, a);
    prev_line_starts.len = lines_prior;
    size_t line_number = 0;
    size_t prev_line = 0;
    for (size_t i = 0; i < range.start; i++) {
        if (buffer.bytes[i] == '\n') {
            prev_line_starts.data[prev_line] = current_start;
            line_number++;
            prev_line = (prev_line + 1) % lines_prior;
            current_start = i + 1;
        }
    }
    size_t affected_line_end = buffer.memsize;
    for (size_t i = range.end; i < buffer.memsize; i++) {
        if (buffer.bytes[i] == '\n') {
            affected_line_end = i;
            break;
        }
    }
        
    // Colour for "irrelevant" code
    start_coloured_text(colour(150, 150, 150), fos);
    // Now, gather the past n lines

    // 
    String strnum = string_u64(line_number + 1, a);
    size_t line_width = strnum.memsize;
    delete_string(strnum, a);
    if (line_number < lines_prior) {
        for (size_t i = 0; i < line_number; i++) {
            size_t line_start = prev_line_starts.data[i];
            size_t next_line_start = (i + 1 == line_number)
                ? current_start
                : prev_line_starts.data[1 + i];

            String strnum = string_u64(i + 1, a);
            for (size_t i = 0; i < line_width - strnum.memsize; i++) {
                write_fstring(mv_string(" "), fos);
            }
            write_fstring(strnum, fos);
            delete_string(strnum, a);
            write_fstring(mv_string(" | "), fos);
            String str = substring(line_start, next_line_start, buffer, a);
            write_fstring(str, fos);
            delete_string(str, a);
        }
    }
    else {
        for (size_t i = 0; i < lines_prior; i++) {
            size_t line_start = prev_line_starts.data[(prev_line + i) % lines_prior];
            size_t next_line_start = (i + 1 == lines_prior)
                ? current_start
                : prev_line_starts.data[(prev_line + 1 + i) % lines_prior];

            String strnum = string_u64(line_number + 1 - (lines_prior - i), a);
            for (size_t i = 0; i < line_width - strnum.memsize; i++) {
                write_fstring(mv_string(" "), fos);
            }
            write_fstring(strnum, fos);
            write_fstring(mv_string(" | "), fos);
            delete_string(strnum, a);

            String str = substring(line_start, next_line_start, buffer, a);
            write_fstring(str, fos);
            delete_string(str, a);
        }
    }

    Document* doc = pretty_u64(++line_number, a);
    write_doc_formatted(doc, 80, fos);
    delete_doc(doc, a);
    write_fstring(mv_string(" | "), fos);

    String s1 = substring(current_start, range.start, buffer, a);
    // TODO (BUG) '+1' won't work with UTF-8!
    String bad_code = substring(range.start, range.end, buffer, a);
    String s2 = substring(range.end, affected_line_end, buffer, a);;

    write_fstring(s1, fos);

    start_coloured_text(colour(208, 105, 30), fos);
    {
        // We need to add line-numbers if bad code spans multiple lines!  
        size_t start_idx = 0;
        size_t i = 0;
        for (; i < bad_code.memsize; i++) {
            if (bad_code.bytes[i] == '\n') {
                String bad_line = substring(start_idx, i + 1, bad_code, a);
                if (start_idx != 0) {
                    Document* doc = pretty_u64(++line_number, a);
                    end_coloured_text(fos);
                    write_doc_formatted(doc, 80, fos);
                    delete_doc(doc, a);
                    write_fstring(mv_string(" | "), fos);
                    start_coloured_text(colour(208, 105, 30), fos);
                }
                write_fstring(bad_line, fos);
                start_idx = i + 1;
            }
        }
        String bad_line = substring(start_idx, i + 1, bad_code, a);
        if (start_idx != 0) {
            line_number++;
            Document* doc = pretty_u64(line_number, a);
            end_coloured_text(fos);
            write_doc_formatted(doc, 80, fos);
            delete_doc(doc, a);
            write_fstring(mv_string(" | "), fos);
            start_coloured_text(colour(208, 105, 30), fos);
        } 
        write_fstring(bad_line, fos);
        start_idx = i + 1;
    }
    end_coloured_text(fos);

    write_fstring(s2, fos);

    delete_string(s1, a);
    delete_string(bad_code, a);
    delete_string(s2, a);

    // End of surrounding code
    end_coloured_text(fos);
    write_fstring(mv_string("\n"), fos);
}
