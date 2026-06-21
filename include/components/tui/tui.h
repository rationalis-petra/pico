#ifndef __COMPONENTS_TUI
#define __COMPONENTS_TUI

#include <stdint.h>
#include "data/meta/array_header.h"
#include "platform/terminal/terminal.h"

/**
 * TUI Module
 * ---------------------------
 * The TUI Module works somewhat like immediate mode GUI libraries: the user
 * produces an abstract description of a layout (in this case with the 'Element'
 * type), and this layout is then fed to the library, which calculates positions
 * and sizes of components that can then be drawn to the terminal.
 * 
 * Rendering
 * ---------
 * - The user must create a pair of buffers, each of which represent a complete
 *   screen-state for a terminal 
 * - The user may then create a discription of a UI layout via the 'Element' 
 *   struct. This struct contains descriptions of elements that can grow and
 *   shrink, both to fit their contents, and to fit their parent/container.
 * - The user will then use the `layout` function, which will calculate final
 *   positions and sizes of various elements.
 * - After laying out, an element can be `rendered` to a buffer, which will
 *   update its contents with the desired final output.
 * - Finally, `write_update` functino is used to send a minimal set of commands
 *   to the terminal to represent the new (desired) output state.
 * 
 * Events
 * ---------
 * The user is responsible for handling input events from the terminal. Some
 * event data should be forwarded to the TUI component by keeping the
 * `TUIContext` up to date. This will  ensure layout works correctly. For
 * example, the layout needs the width and height of the terminal when rendering. 
 * 
 * Instead of using callbacks for events (like hover, mouse click), the `layout`
 * method will provide a description of which element has mouse focus, etc.  
 * 
 * Advanced Usage
 * --------------
 * Technically, the layout stage is completely optional: as long as all elements
 * have a final size and position, the renderer can draw them to a buffer. It is
 * thus entirely possible for a user to either modify an element post-layout
 * (for example, layout to determine where elements are, then perform actions
 *  based on where the mouse is relative to an element).
 */

/**
 * Element
 * ---------------------------
 * An element is a rectangular 
 */

typedef enum {
    EText,
    EContainer,
    EScissor,
    EPicture,
} ElementType;

typedef enum : uint8_t {
    Fit,
    Grow,
    Fixed,
} SizeType;

typedef struct {
} TextUnit;

typedef struct {
    String text;
} TextElement;

typedef struct {

} PictureElement;

typedef enum {
    None, 
    Regular, // │┌ 
    Round,   // │╭ 
    Thick,   // ┃┏ 
    Double,  // ║╔   
} BorderType;

typedef struct {
    BorderType border; 
    U64Array children;
} ContainerElement;

typedef struct {
    ElementType type;

    SizeType size_x;
    SizeType size_y;

    uint16_t width_min;
    uint16_t width_max;

    uint16_t height_min;
    uint16_t height_max;

    uint16_t width;
    uint16_t height;

    // Use larger ints because components in scrolling containers may have
    //  larger 'off-screen' limits.
    int32_t pos_x;
    int32_t pos_y;
    union {
        TextElement text;
        ContainerElement container;
        PictureElement picture;
    };
} Element;

/**
 * Context 
 * ----------------------------
 * Used to store context of the TUI that is needed for layout. Currently, this
 * is just the width and height of the terminal. 
 * 
 */

typedef struct {
    uint16_t width;
    uint16_t height;
} TUIContext;

/**
 * Buffer
 * ----------------------------
 * A representation of the expected state of the terminal buffer, not compressed
 * or compacted. 
 */

typedef struct {
    Colour bg_col;
    Colour fg_col;
    FontProperties props;
    uint32_t codepoint;
} BufferCell;

ARRAY_HEADER(BufferCell, buf_cell, BufCell)

typedef struct {
    uint16_t rows;
    uint16_t cols;
    BufCellArray cells;
} Buffer;


/**
 * Functions
 * ----------------------------
 * The set of functions for interacting with the TUI component. Relativel small
 * set (most information lives in the data).
 * 
 *  - layout: Given a root element and a context, calculate the positions and
 *        sizes of all children of the context. 
 *  - render: Given an element that has gone through the 'layout' phase, render it
 *        to a buffer, i.e. update the buffer contents so that it reflects the layout.
 *  - write_updates: Given two buffers, with one representing the desired (new)
 *        screen state, and one representing the current (old) screen state,
 *        calculate and send a sequence of characters to the terminal  
 * 
 *  - make_buffer, free_buffer: create and destroy buffers.
 */

void layout(Element root, TUIContext ctx);

void render(Element root, Buffer target);

void write_updates(Buffer old, Buffer new);

Buffer make_buffer(uint16_t rows, uint16_t cols, Allocator* a);
void free_buffer(Buffer buffer);

#endif
