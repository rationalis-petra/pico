#include "pretty/stream_printer.h"
#include "pretty/document.h"
#include "data/stream.h"

// See: https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.34.2200
// See: http://cambium.inria.fr/~fpottier/pprint/doc/pprint/

typedef struct {
    bool flatten;
    uint16_t indent;
    uint16_t width;
    uint16_t* current_column;
} RenderState;

void render_doc(Document* doc, RenderState state, OStream* os) {
    switch (doc->type) {
    case LineDocument:
        write_string(mv_string("\n"), os);
        for (size_t i = 0; i < state.indent; i++) {
            write_string(mv_string(" "), os);
            *state.current_column = state.indent;
        }
        break;
    case StringDocument:
        write_string(doc->string, os);
        *state.current_column += doc->string.memsize;
        break;
    case CatDocument:
        for (size_t i = 0; i < doc->docs.len; i++) {
            render_doc(doc->docs.data[i], state, os);
        }
        break;
    case NestDocument: {
        state.indent += doc->nest.indent;
        if (*state.current_column < state.indent) {
            for (uint16_t i = 0; i < state.indent - *state.current_column; i++) {
                write_string(mv_string(" "), os);
            }
            *state.current_column = state.indent;
        }
        render_doc(doc->nest.inner, state, os);
        break;
    }
    case GroupDocument: {
        if (!state.flatten
            && doc->requirement.fin == Finite
            && doc->requirement.cols + *state.current_column < state.width) {
            state.flatten = true;
        }
        render_doc(doc->group, state, os);
        break;
    }
    case SepDocument:
        for (size_t i = 0; i < doc->docs.len; i++) {
            render_doc(doc->docs.data[i], state, os);
            if (state.flatten) {
                if (i + 1 < doc->docs.len) write_impl(' ', os);
                *state.current_column += 1;
            } else {
                if (i + 1 < doc->docs.len) {
                    write_impl('\n', os);
                    for (size_t i = 0; i < state.indent; i++) {
                        write_string(mv_string(" "), os);
                    }
                    *state.current_column = state.indent;
                }
            }
        }
        break;
    case VSepDocument:
        for (size_t i = 0; i < doc->docs.len; i++) {
            render_doc(doc->docs.data[i], state, os);
            if (i + 1 < doc->docs.len) {
                write_impl('\n', os) ;
                for (size_t i = 0; i < state.indent; i++) {
                    write_string(mv_string(" "), os);
                }
                *state.current_column = state.indent;
            }
        }
        break;
    }
}

void write_doc(Document* doc, uint16_t width, OStream* os) {
    uint16_t column = 0;
    RenderState initial_state = (RenderState) {
        .width = width,
        .indent = 0,
        .flatten = (doc->requirement.fin == Finite && doc->requirement.cols < width),
        .current_column = &column,
    };
     
    render_doc(doc, initial_state, os);
}
