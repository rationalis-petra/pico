#ifndef __PLATFORM_WINDOW_WINKEY_TRANSLATE_H
#define __PLATFORM_WINDOW_WINKEY_TRANSLATE_H

#include "platform/machine_info.h"
#ifdef WINDOW_SYSTEM
#if (OS_FAMILY == WINDOWS)

#include <stdbool.h>
#include <windows.h>

RawKey keycode_to_rawkey(WPARAM windows_keycode);

WPARAM rawkey_to_keycode(RawKey rawkey);

Key translate_win_keycode(RawKey key, uint16_t unicode, bool use_unicode);

#endif
#endif
#endif
