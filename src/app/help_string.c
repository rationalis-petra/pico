#include "platform/io/terminal.h"

#include "app/help_string.h"


void write_help_string(OStream *os) {
    set_boldness(Bold);
    start_coloured_text(colour(80, 250, 250));
    write_string(mv_string("\n                             The Pico Relic Compiler                            "), os);
    write_string(mv_string("\n────────────────────────────────────────────────────────────────────────────────"), os);
    end_coloured_text();
    set_boldness(Normal);

    write_string(mv_string("\n\n"), os);
    write_string(mv_string("  Usage: "), os);
    set_boldness(Dim);
    write_string(mv_string("pico [repl | script | eval] [options]\n"), os);
    set_boldness(Normal);
    write_string(mv_string("\n"), os);

    set_boldness(Bold);
    write_string(mv_string("\n                                      Repl                                      "), os);
    write_string(mv_string("\n           ──────────────────────────────────────────────────────────           "), os);
    set_boldness(Normal);

    write_string(mv_string("\n\n"), os);
    write_string(mv_string(" The repl, or Read Eval Print Loop is the default mode that pico will start in\n"), os);
    write_string(mv_string(" when provided with no arguments. The repl is used as one would a regular shell,\n"), os);
    write_string(mv_string(" where you type in relic expressions which are "), os);
    start_italics();
    write_string(mv_string("read"), os);
    end_italics();
    write_string(mv_string(" in, "), os);
    start_italics();
    write_string(mv_string("evaluated"), os);
    end_italics();
    write_string(mv_string(" and then the \n"), os);
    write_string(mv_string(" the result is "), os);
    start_italics();
    write_string(mv_string("printed"), os);
    end_italics();
    write_string(mv_string(" back to the console, before "), os);
    start_italics();
    write_string(mv_string("looping"), os);
    end_italics();
    write_string(mv_string(" to the beginning\n"), os);
    write_string(mv_string(" allowing you to type in another expression, and so on.\n"), os);

    write_string(mv_string("\n"), os);

    write_string(mv_string(" If one wishes to make use of the various options, then the repl subcommand must\n"), os);
    write_string(mv_string(" be explicitly provided, instead of relying upon this as the default mode, i.e.\n"), os);
    write_string(mv_string(" type "), os);
    set_boldness(Dim);
    write_string(mv_string("pico repl [options]"), os);
    set_boldness(Normal);
    write_string(mv_string("\n"), os);

    set_boldness(Bold);
    write_string(mv_string("\nOptions:"), os);
    set_boldness(Normal);

    set_boldness(Bold);
    write_string(mv_string("\n  -d :: "), os);
    set_boldness(Normal);
    write_string(mv_string("Use this option to debug print intermediate results:\n"), os);

    write_string(mv_string("\n"), os);
    

    set_boldness(Bold);
    write_string(mv_string("\n                                     Script                                     "), os);
    write_string(mv_string("\n           ──────────────────────────────────────────────────────────           "), os);
    set_boldness(Normal);

    write_string(mv_string("\n"), os);
    write_string(mv_string("\n"), os);

    write_string(mv_string(" Usage: "), os);
    set_boldness(Dim);
    write_string(mv_string("pico script filename\n"), os);
    set_boldness(Normal);

    write_string(mv_string("\n"), os);
  
    write_string(mv_string(" The script subcommand takes a filename as input. It will try and evaluate the\n"), os);
    write_string(mv_string(" contents of filename as a sequence of relic terms, evaluating top-to-bottom.\n"), os);

    write_string(mv_string(" The script will caese evaluation if it encounters any errors during\n"), os);
    write_string(mv_string(" parsing, typechecking or compilation, and will provide a printout of the error\n"), os);
    write_string(mv_string(" encountered.\n"), os);

    write_string(mv_string("\n"), os);

    set_boldness(Bold);
    write_string(mv_string("\n                                      Eval                                      "), os);
    write_string(mv_string("\n           ──────────────────────────────────────────────────────────           "), os);
    set_boldness(Normal);

    write_string(mv_string("\n"), os);
    write_string(mv_string("\n"), os);
    
    write_string(mv_string(" Usage: "), os);
    set_boldness(Dim);
    write_string(mv_string("pico eval expression\n"), os);
    set_boldness(Normal);


    write_string(mv_string(" The script subcommand takes a single string as input. It will try and evaluate\n"), os);
    write_string(mv_string(" the string as a single relic term. If the expression evaluates to a value, e.g.\n"), os);
    start_coloured_text(colour(40, 160, 40));
    write_string(mv_string(" \"(+ 2 3)\""), os);
    end_coloured_text();
    write_string(mv_string(", then the final value, in this case 5, will not be printed to the\n"), os);
    write_string(mv_string(" output console.\n"), os);

    write_string(mv_string("\n"), os);

    write_string(mv_string(" Like script, this will report to the terminal any errors it encounters as part\n"), os);
    write_string(mv_string(" of compilation.\n"), os);

    write_string(mv_string("\n"), os);
}
