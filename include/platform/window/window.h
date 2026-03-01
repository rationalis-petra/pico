#ifndef __PLATFORM_WINDOW_WINDOW_H
#define __PLATFORM_WINDOW_WINDOW_H

#include "data/meta/array_header.h"
#include "data/string.h"

#include "platform/window/keycodes.h"

typedef struct PlWindow PlWindow;

// Initialize the window system
// returns zero on success, exit code on failure
int pl_init_window_system(Allocator* a);
void pl_teardown_window_system();

PlWindow* pl_create_window(String name, int width, int height);
void pl_destroy_window(PlWindow* window);

bool pl_window_should_close(PlWindow* window);

typedef enum : uint64_t {
    WindowResized,
    KeyEvent,
    ModifierKeyEvent,
    KeymapChanged,
} MessageType;

typedef struct {
    int64_t xpos;
    int64_t ypos;
} MousePos;

typedef struct {
    RawKey key_id;
    uint32_t modifier_key_mask;
    bool key_pressed;
} KeyEventData;

typedef struct {
    uint32_t depressed;
    uint32_t latched;
    uint32_t locked;
    uint32_t group;
} ModifierKeyEventData;

typedef struct {
    uint32_t width;
    uint32_t height;
} WindowDimensions;


typedef struct KeyMap KeyMap;
typedef struct KeyboardState KeyboardState;

typedef struct {
    MessageType type;
    union {
        WindowDimensions dims;
        KeyEventData key_event;
        ModifierKeyEventData mod_event;
        KeyMap* keymap;
    };
} WinMessage;

ARRAY_HEADER(WinMessage, wm, WinMessage);

// TODO (IMPROVEMENT):
//  - separate window events from regular events?
//  - add window ID to window events?
WinMessageArray pl_poll_events(PlWindow* window, Allocator* a);

KeyboardState* create_keyboard_state(KeyMap* keymap);
void destroy_keyboard_state(KeyboardState*);

void update_keystate_key(RawKey raw, uint32_t modifier_mask, bool is_pressed, KeyboardState* state);
void update_keystate_modifiers(uint32_t depressed, uint32_t latched, uint32_t locked, uint32_t group, KeyboardState* state);

// TODO: what about multiple unicocde characters?
Key get_key(RawKey raw, KeyboardState* state);



#endif
