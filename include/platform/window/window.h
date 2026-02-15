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
} RawKey;

typedef enum : uint64_t {
    PKEY_A_Lower,
    PKEY_B_Lower,
    PKEY_C_Lower,
    PKEY_D_Lower,
    PKEY_E_Lower,
    PKEY_F_Lower,
    PKEY_G_Lower,
    PKEY_H_Lower,
    PKEY_I_Lower,
    PKEY_J_Lower,
    PKEY_K_Lower,
    PKEY_L_Lower,
    PKEY_M_Lower,
    PKEY_N_Lower,
    PKEY_O_Lower,
    PKEY_P_Lower,
    PKEY_Q_Lower,
    PKEY_R_Lower,
    PKEY_S_Lower,
    PKEY_T_Lower,
    PKEY_U_Lower,
    PKEY_V_Lower,
    PKEY_W_Lower,
    PKEY_X_Lower,
    PKEY_Y_Lower,
    PKEY_Z_Lower,

    PKEY_A,
    PKEY_B,
    PKEY_C,
    PKEY_D,
    PKEY_E,
    PKEY_F,
    PKEY_G,
    PKEY_H,
    PKEY_I,
    PKEY_J,
    PKEY_K,
    PKEY_L,
    PKEY_M,
    PKEY_N,
    PKEY_O,
    PKEY_P,
    PKEY_Q,
    PKEY_R,
    PKEY_S,
    PKEY_T,
    PKEY_U,
    PKEY_V,
    PKEY_W,
    PKEY_X,
    PKEY_Y,
    PKEY_Z,

    PKEY_1,
    PKEY_2,
    PKEY_3,
    PKEY_4,
    PKEY_5,
    PKEY_6,
    PKEY_7,
    PKEY_8,
    PKEY_9,
    PKEY_0,

    PKEY_EXCLAMATION,
    PKEY_AT,
    PKEY_HASH,
    PKEY_DOLLAR,
    PKEY_PERCENT,
    PKEY_CARET,
    PKEY_AMPERSAND,
    PKEY_ASTERISK,
    PKEY_LPAREN,
    PKEY_RPAREN,
    PKEY_MINUS,
    PKEY_PLUS,

    PKEY_LBRACE,
    PKEY_RBRACE,
    PKEY_COLON,
    PKEY_SEMICOLON,
    PKEY_COMMA,
    PKEY_DOT,
    PKEY_QUERY,

    PKEY_SPACE,

    PKEY_ENTER,
    PKEY_BACKSPACE,
} Key;


typedef enum : uint32_t {
    MOD_SHIFT,
    MOD_CTRL,
    MOD_META,
} ModifierKey;

typedef enum : uint64_t {
    WindowResized,
    KeyEvent,    // This event is processed, so, e.g. a shift + a will send 'A' rather 
    RawKeyEvent, // 'Raw' here meaning that an unprocessed keycode is used 

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
    RawKey key_id;
    uint16_t modifier_key_mask;
    bool key_pressed;
} RawKeyEventData;

// TOOD (refactor): we may want to move the raw->processed
//   key API outside of the 'core' ?
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
        RawKeyEventData raw_key_event;
        KeyEventData key_event;
    };
} WinMessage;

ARRAY_HEADER(WinMessage, wm, WinMessage);

WinMessageArray pl_poll_events(PlWindow* window, Allocator* a);

bool is_key_pressed(RawKey raw);

typedef enum : uint64_t { KeyRawOnly, KeyProcessedOnly, KeyBoth } KeyProcessing;

void set_key_processing(KeyProcessing proc);

#endif
