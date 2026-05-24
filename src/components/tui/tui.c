#include "data/meta/array_impl.h"
#include "components/tui/tui.h"

ARRAY_COMMON_IMPL(BufferCell, buf_cell, BufCell)

void layout(Element root, TUIContext ctx) {
}

void render(Element root, Buffer target) {
}

void write_updates(Buffer old, Buffer new) {
}

Buffer make_buffer(uint16_t rows, uint16_t cols, Allocator* a) {
    return (Buffer) {
        .rows = rows,
        .cols = cols,
        .cells = mk_buf_cell_array(rows * cols, a),
    };
}

void free_buffer(Buffer buffer) {
    sdelete_buf_cell_array(buffer.cells);
}
