#include "platform/window/window.h"
#include "platform/machine_info.h"

#if OS_FAMILY == UNIX

struct Window {
};

#elif OS_FAILY == WINDOWS

#include <windows.h>

struct Window {
    HINSTNACE window_handle;
};

#endif
