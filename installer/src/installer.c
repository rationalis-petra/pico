#include "data/string.h"
#include "data/stream.h"

#include "platform/machine_info.h"
#include "platform/terminal/terminal.h"
#include "platform/memory/std_allocator.h"

#include "install.h"


int main(int argc, char **argv) {
    Allocator* a = get_std_allocator();
    init_terminal(a);

    OStream* cout = get_stdout_stream();
    write_string(mv_string("Welcome to the Pico Installer.\n"), cout);

    // Installer should be set-up in the following way:
    // installer-dir
    //   <installer binary>
    //   assets
    //    ┣ pico 
    //    ┣ keeper
    //    ┗ archive
    //       ┣ documentation
    //       ┣ examples
    //       ┗ builtin libraries?
    // The end goal of the installer is for the following to happen:
    // • binaries end up in /usr/local (unix) or %LOCALAPPATA%\Programs (windows)
    // • archives end up in ~/.local/relic (unix) or %APPDATA%\relic (Windows)

#if OS_FAMILY == WINDOWS
    return  install_windows(argc, argv);
#elif OS_FAMILY == UNIX
    return install_unix(argc, argv);
#else
    #error "Unrecognized OS Family"
#endif
}
