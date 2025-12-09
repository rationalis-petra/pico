#include "platform/terminal/terminal.h"

#include "app/help_string.h"

void write_help_string(FormattedOStream *os) {
    start_boldness(Bold, os);
    start_coloured_text(colour(80, 250, 250), os);
    write_fstring(mv_string("\n                             The Pico Relic Compiler                            "), os);
    write_fstring(mv_string("\n────────────────────────────────────────────────────────────────────────────────"), os);
    end_coloured_text(os);
    end_boldness(os);

    write_fstring(mv_string("\n\n"), os);
    write_fstring(mv_string("  Usage: "), os);
    start_boldness(Dim, os);
    write_fstring(mv_string("pico [repl | script | eval | atlas] <options>\n"), os);
    end_boldness(os);
    write_fstring(mv_string("\n"), os);

    start_boldness(Bold, os);
    write_fstring(mv_string("\n                                      Repl                                      "), os);
    write_fstring(mv_string("\n           ──────────────────────────────────────────────────────────           "), os);
    end_boldness(os);

    write_fstring(mv_string("\n\n"), os);
    write_fstring(mv_string(" The repl, or Read Eval Print Loop is the default mode that pico will start in\n"), os);
    write_fstring(mv_string(" when provided with no arguments. The repl is used as one would a regular shell,\n"), os);
    write_fstring(mv_string(" where you type in relic expressions which are "), os);
    start_italics(os);
    write_fstring(mv_string("read"), os);
    end_italics(os);
    write_fstring(mv_string(" in, "), os);
    start_italics(os);
    write_fstring(mv_string("evaluated"), os);
    end_italics(os);
    write_fstring(mv_string(" and then the \n"), os);
    write_fstring(mv_string(" the result is "), os);
    start_italics(os);
    write_fstring(mv_string("printed"), os);
    end_italics(os);
    write_fstring(mv_string(" back to the console, before "), os);
    start_italics(os);
    write_fstring(mv_string("looping"), os);
    end_italics(os);
    write_fstring(mv_string(" to the beginning\n"), os);
    write_fstring(mv_string(" allowing you to type in another expression, and so on.\n"), os);

    write_fstring(mv_string("\n"), os);

    write_fstring(mv_string(" If one wishes to make use of the various options, then the repl subcommand must\n"), os);
    write_fstring(mv_string(" be explicitly provided, instead of relying upon this as the default mode, i.e.\n"), os);
    write_fstring(mv_string(" type "), os);
    start_boldness(Dim, os);
    write_fstring(mv_string("pico repl <options>"), os);
    end_boldness(os);
    write_fstring(mv_string("\n"), os);

    start_boldness(Bold, os);
    write_fstring(mv_string("\nOptions:"), os);
    end_boldness(os);

    start_boldness(Bold, os);
    write_fstring(mv_string("\n  -d :: "), os);
    end_boldness(os);
    write_fstring(mv_string("Use this option to debug print intermediate results:\n"), os);

    write_fstring(mv_string("\n"), os);
    

    start_boldness(Bold, os);
    write_fstring(mv_string("\n                                     Script                                     "), os);
    write_fstring(mv_string("\n           ──────────────────────────────────────────────────────────           "), os);
    end_boldness(os);

    write_fstring(mv_string("\n"), os);
    write_fstring(mv_string("\n"), os);

    write_fstring(mv_string(" Usage: "), os);
    start_boldness(Dim, os);
    write_fstring(mv_string("pico script <filename>\n"), os);
    end_boldness(os);

    write_fstring(mv_string("\n"), os);
  
    write_fstring(mv_string(" The script subcommand takes a filename as input. It will try and evaluate the\n"), os);
    write_fstring(mv_string(" contents of filename as a sequence of relic terms, evaluating top-to-bottom.\n"), os);

    write_fstring(mv_string(" The script will caese evaluation if it encounters any errors during\n"), os);
    write_fstring(mv_string(" parsing, typechecking or compilation, and will provide a printout of the error\n"), os);
    write_fstring(mv_string(" encountered.\n"), os);

    write_fstring(mv_string("\n"), os);

    start_boldness(Bold, os);
    write_fstring(mv_string("\n                                      Eval                                      "), os);
    write_fstring(mv_string("\n           ──────────────────────────────────────────────────────────           "), os);
    end_boldness(os);

    write_fstring(mv_string("\n"), os);
    write_fstring(mv_string("\n"), os);
    
    write_fstring(mv_string(" Usage: "), os);
    start_boldness(Dim, os);
    write_fstring(mv_string("pico eval expression\n"), os);
    end_boldness(os);

    write_fstring(mv_string(" The script subcommand takes a single string as input. It will try and evaluate\n"), os);
    write_fstring(mv_string(" the string as a single relic term. If the expression evaluates to a value, e.g.\n"), os);
    start_coloured_text(colour(40, 160, 40), os);
    write_fstring(mv_string(" \"(+ 2 3)\""), os);
    end_coloured_text(os);
    write_fstring(mv_string(", then the final value, in this case 5, will not be printed to the\n"), os);
    write_fstring(mv_string(" output console.\n"), os);

    write_fstring(mv_string("\n"), os);

    write_fstring(mv_string(" Like script, this will report to the terminal any errors it encounters as part\n"), os);
    write_fstring(mv_string(" of compilation.\n"), os);

    write_fstring(mv_string("\n"), os);
}
