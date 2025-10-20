#include "data/meta/array_impl.h"
#include "platform/window/window.h"
#include "platform/window/internal.h"
#include "platform/machine_info.h"

// For platform-specific implementations, please see:
// impl_win32.c
// impl_wayland.c
// impl_x11.c

ARRAY_COMMON_IMPL(WinMessage, wm, WinMessage)
