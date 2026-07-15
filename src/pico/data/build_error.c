#include "platform/terminal/terminal.h"

#include "components/pretty/document.h"
#include "components/pretty/stream_printer.h"

#include "pico/data/build_error.h"

const Colour build_message_colour = (Colour){.r = 200, .g = 20, .b = 20};

void display_build_error(MultiBuildError multi, FormattedOStream* fos, Allocator* a) {
    if (multi.has_many) {
        write_fstring(mv_string("Errors encountered during the build process:\n"), fos);
        for (size_t i = 0; i < multi.errors.len; i++) {
            BuildError error = *(BuildError*)multi.errors.data[i];

            write_fstring(mv_string("\n"), fos);
            start_coloured_text(build_message_colour, fos);
            Document* iderr = mv_nest_doc(2, error.message, a);
            write_doc_formatted(iderr, 120, fos);
            mem_free(iderr, a); // TODO: fix me! (fix mk doc & replace the mv with mk)
            end_coloured_text(fos);
            write_fstring(mv_string("\n\n"), fos);
        }
    } else {
        write_fstring(mv_string("Error encountered during the build process:\n"), fos);
        start_coloured_text(build_message_colour, fos);
        Document* iderr = mv_nest_doc(2, multi.error.message, a);
        write_doc_formatted(iderr, 120, fos);
        mem_free(iderr, a); // TODO: fix me! (fix mk doc & replace the mv with mk)

        end_coloured_text(fos);
        write_fstring(mv_string("\n\n"), fos);
    }
}

_Noreturn void throw_build_error(BuildErrorPoint* point, BuildError err) {
    point->multi.has_many = false;
    point->multi.error = err;
    long_jump(point->buf, 1);
}

_Noreturn void throw_build_errors(BuildErrorPoint* point, PtrArray errors) {
    point->multi.has_many = true;
    point->multi.errors = errors;
    long_jump(point->buf, 1);
}

