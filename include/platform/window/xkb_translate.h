#ifndef __PLATFORM_WINDOW_XKB_TRANSLATE_H
#define __PLATFORM_WINDOW_XKB_TRANSLATE_H

#include <xkbcommon/xkbcommon.h>
#include "platform/window/window.h"

Key translate_xkb_keycode(xkb_keysym_t key);


#endif
