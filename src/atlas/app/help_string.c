#include "platform/terminal/terminal.h"

#include "atlas/app/help_string.h"

void write_atlas_help_string(FormattedOStream *os) {
    start_boldness(Bold, os);
    start_coloured_text(colour(80, 250, 250), os);
    write_fstring(mv_string("\n                             The Atlas Build System                             "), os);
    write_fstring(mv_string("\n────────────────────────────────────────────────────────────────────────────────"), os);
    end_coloured_text(os);
    end_boldness(os);

    write_fstring(mv_string("\n\n"), os);
    write_fstring(mv_string("  Usage: "), os);
    start_boldness(Dim, os);
    write_fstring(mv_string("pico atlas [init | run | test] <target>\n"), os);
    end_boldness(os);

    write_fstring(mv_string("\n"), os);
    write_fstring(mv_string(" The Atlas build system is provided as part of the pico relic compiler. It works \n"), os);
    write_fstring(mv_string(" based an 'atlas_project' file in the root directory of your project, and with \n"), os);
    write_fstring(mv_string(" 'atlas' files describing differrent build targets, scripts and so on."), os);
    write_fstring(mv_string("\n"), os);

    start_boldness(Bold, os);
    write_fstring(mv_string("\n                                      Init                                      "), os);
    write_fstring(mv_string("\n           ──────────────────────────────────────────────────────────           "), os);
    end_boldness(os);

    write_fstring(mv_string("\n"), os);
    write_fstring(mv_string(" Usage: "), os);
    start_boldness(Dim, os);
    write_fstring(mv_string("pico atlas init <project-name>\n"), os);
    end_boldness(os);

    write_fstring(mv_string("\n\n"), os);
    write_fstring(mv_string(" The init command will create a new Relic project using Atlas, with a basic\n"), os);
    write_fstring(mv_string(" project structure including an executable and test-suite.\n"), os);
    write_fstring(mv_string("\n\n"), os);
    

    start_boldness(Bold, os);
    write_fstring(mv_string("\n                                       Run                                      "), os);
    write_fstring(mv_string("\n           ──────────────────────────────────────────────────────────           "), os);
    end_boldness(os);

    write_fstring(mv_string("\n"), os);
    write_fstring(mv_string("\n"), os);

    write_fstring(mv_string(" Usage: "), os);
    start_boldness(Dim, os);
    write_fstring(mv_string("pico atlas run <target-name>\n"), os);
    end_boldness(os);

    write_fstring(mv_string("\n"), os);
  
    write_fstring(mv_string(" If there is either an 'executable' or script target defined somewhere in the\n"), os);
    write_fstring(mv_string(" atlas project, then this will build (if needed) and run the target. If the\n"), os);
    write_fstring(mv_string(" target is a script, then no executable will be produced.\n"), os);
    write_fstring(mv_string("\n"), os);
    write_fstring(mv_string(" Any additional arguments are passed directly to the test suite, and are not \n"), os);
    write_fstring(mv_string(" processed by Atlas.\n"), os);

    write_fstring(mv_string("\n"), os);

    start_boldness(Bold, os);
    write_fstring(mv_string("\n                                      Test                                      "), os);
    write_fstring(mv_string("\n           ──────────────────────────────────────────────────────────           "), os);
    end_boldness(os);

    write_fstring(mv_string("\n"), os);
    write_fstring(mv_string("\n"), os);
    
    write_fstring(mv_string(" Usage: "), os);
    start_boldness(Dim, os);
    write_fstring(mv_string("pico atlas test <test-suite-name>\n"), os);
    end_boldness(os);

    write_fstring(mv_string(" If there is a test target defined somewhere in the atlas project, then this\n"), os);
    write_fstring(mv_string(" command will run the matching test suite. Any additional arguments are passed\n"), os);
    write_fstring(mv_string(" directly to the test suite, and are not processed by Atlas.\n"), os);

    write_fstring(mv_string("\n"), os);
}
