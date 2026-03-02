#include "data/string.h"
#include "data/stream.h"

int main(int argc, char **argv) {
    OStream* cout = get_stdout_stream();

    // Keeper is expecting documentation to be located in 
    // ~/.local/relic on Unix System; and
    // %APPDATA%/ROAMING on Windows
    // TOOD: add directories if stored system-wide?

    write_string(mv_string("Hello. I am the Relic Keeper Documentation Archivist and Assistant.\n"), cout);

    return 0;
}
