#ifndef __PLATFORM_WINDOW_WINDOW_H
#define __PLATFORM_WINDOW_WINDOW_H

#include "data/meta/array_header.h"
#include "data/string.h"

typedef struct PlWindow PlWindow;

// Initialize the window system
// returns zero on success, exit code on failure
int pl_init_window_system(Allocator* a);
void pl_teardown_window_system();

PlWindow* pl_create_window(String name, int width, int height);
void pl_destroy_window(PlWindow* window);

bool pl_window_should_close(PlWindow* window);

typedef enum : uint64_t {
    WKEY_A,
    WKEY_B,
    WKEY_C,
    WKEY_D,
    WKEY_E,
    WKEY_F,
    WKEY_G,
    WKEY_H,
    WKEY_I,
    WKEY_J,
    WKEY_K,
    WKEY_L,
    WKEY_M,
    WKEY_N,
    WKEY_O,
    WKEY_P,
    WKEY_Q,
    WKEY_R,
    WKEY_S,
    WKEY_T,
    WKEY_U,
    WKEY_V,
    WKEY_W,
    WKEY_X,
    WKEY_Y,
    WKEY_Z,

    WKEY_1,
    WKEY_2,
    WKEY_3,
    WKEY_4,
    WKEY_5,
    WKEY_6,
    WKEY_7,
    WKEY_8,
    WKEY_9,
    WKEY_0,

    WKEY_EXCLAMATION,
    WKEY_AT,
    WKEY_HASH,
    WKEY_DOLLAR,
    WKEY_PERCENT,
    WKEY_CARET,
    WKEY_AMPERSAND,
    WKEY_ASTERISK,
    WKEY_LPAREN,
    WKEY_RPAREN,
    WKEY_MINUS,
    WKEY_PLUS,

    WKEY_LBRACE,
    WKEY_RBRACE,
    WKEY_COLON,
    WKEY_SEMICOLON,
    WKEY_COMMA,
    WKEY_DOT,
    WKEY_QUERY,

    WKEY_SPACE,

    WKEY_ENTER,
    WKEY_BACKSPACE,
} Key;

typedef enum : uint32_t {
    MOD_SHIFT,
    MOD_CTRL,
    MOD_META,
} ModifierKey;

typedef enum : uint64_t {
    WindowResized,
    KeyEvent,

    MouseMoved,
    MouseLButtonDown,
    MouseRButtonDown,
    MouseMButtonDown,
    MouseLButtonUp,
    MouseRButtonUp,
    MouseMButtonUp,
} MessageType;

typedef struct {
    int64_t xpos;
    int64_t ypos;
} MousePos;

typedef struct {
    uint64_t key_id;
    uint16_t modifier_key_mask;
    bool key_pressed;
} KeyEventData;

typedef struct {
    uint32_t width;
    uint32_t height;
} WindowDimensions;

typedef struct {
    MessageType type;
    union {
        WindowDimensions dims;
        KeyEventData key_event;
    };
} WinMessage;

ARRAY_HEADER(WinMessage, wm, WinMessage);

WinMessageArray pl_poll_events(PlWindow* window, Allocator* a);

bool is_key_pressed();

#endif
