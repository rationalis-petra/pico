#ifndef __PLATFORM_WINDOW_XKB_TRANSLATE_H
#define __PLATFORM_WINDOW_XKB_TRANSLATE_H

#include <xkbcommon/xkbcommon.h>
#include "platform/window/keycodes.h"

RawKey scancode_to_rawkey(uint32_t scancode);

uint32_t rawkey_to_scancode(RawKey rawkey);

Key translate_xkb_keycode(xkb_keysym_t key);


#endif
