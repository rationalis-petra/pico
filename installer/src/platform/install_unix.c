#include "platform/machine_info.h"
#if OS_FAMILY == UNIX

#include "data/string.h"
#include "data/stream.h"

#include "install.h"

int install_unix(int argc, char **argv) {
    OStream* cout = get_stdout_stream();
    write_string(mv_string("Unix Version of the installer is not yet implemented...\n"), cout);
    
    return 0;
}

#endif
