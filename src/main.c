#include "data/string.h"
#include "data/stream.h"

#include "platform/memory/std_allocator.h"
#include "platform/memory/executable.h"
#include "platform/terminal/terminal.h"
#include "platform/window/window.h"

#include "components/assembler/assembler.h"
#include "pico/codegen/codegen.h"
#include "pico/stdlib/stdlib.h"
#include "pico/stdlib/user.h"
#include "pico/stdlib/platform/submodules.h"
#include "pico/stdlib/meta/meta.h"

#include "atlas/atlas.h"

#include "app/dev/module_load.h"
#include "app/command_line_opts.h"
#include "app/help_string.h"
#include "app/repl.h"

static const char* version = "0.2.8";

int main(int argc, char** argv) {
  // Setup
  Allocator* stdalloc = get_std_allocator();
  IStream* cin = get_stdin_stream();
  OStream* cout = get_stdout_stream();
  Allocator exalloc = mk_executable_allocator(stdalloc);

  // Init terminal first, as other initializers may panic, therefore writing
  //   to stdout, which uses the terminal module.
  init_terminal(stdalloc);

  // Initialization order here is not important
  init_ctypes();
  init_asm();
  init_symbols(stdalloc);
  init_dynamic_vars(stdalloc);

#ifdef WINDOW_SYSTEM
  if (pl_init_window_system(stdalloc)) {
    st_write_string(mv_string("Warning: failed to init window system!\n"), cout);
  }
#endif

  thread_init_dynamic_vars();

  RegionAllocator* region = make_region_allocator(8096, true, stdalloc);
  RegionAllocator* subregion = make_subregion(region);

  PiAllocator module_allocator = convert_to_pallocator(stdalloc);

  Assembler* ass = mk_assembler(current_cpu_feature_flags(), &exalloc);
  Assembler* ass_base = mk_assembler(current_cpu_feature_flags(), &exalloc);
  Package* base = base_package(ass_base, stdalloc, &module_allocator, subregion);
  reset_subregion(subregion);
  delete_assembler(ass_base);

  set_current_package(base);
  set_std_istream(cin);
  set_std_ostream(cout);

  // Argument parsing
  StringArray args = mk_string_array(argc - 1, stdalloc);
  for (int i = 1; i < argc; i++) {
    push_string(mv_string(argv[i]), &args);
  }
  Command command = parse_command(args);
  sdelete_string_array(args);

  switch (command.type) {
  case CRepl: {
    /** TODO (FUTURE BUG): move the codegen init to BEFORE the base package
        is created! */
    init_codegen(command.repl.backend, stdalloc);

    Package* user_pkg = mk_user_package(base, &module_allocator, subregion);
    Module* user = get_module(string_to_name(mv_string("user")), user_pkg);
    set_std_current_module(user);
    set_current_package(user_pkg);

    IterOpts opts = (IterOpts) {
      .debug_print = command.repl.debug_print,
      .is_eval = false,
    };

    st_write_string(mv_string("Pico Relic Compiler\n  version: "), cout);
    st_write_line(mv_string(version), cout);
    st_write_string(mv_string("\n"), cout);

    if (command.repl.interactive) { 
      st_write_line(mv_string("Press 'Ctrl + q' to quit"), cout);
      terminal_set_raw_mode(true);
      while (repl_iter(stdalloc, subregion, &exalloc, user, opts)) {
        reset_subregion(subregion);
      }
      terminal_set_raw_mode(false);
    } else {
      IStream* in = get_std_istream();
      while (noninteractive_repl_iter(in, stdalloc, subregion, &exalloc, user, opts)) {
        reset_subregion(subregion);
      }
    }
    delete_package(user_pkg);
    break;
  }
  case CScript: {
    /** TODO (FUTURE BUG): move the codegen init to BEFORE the base package
        is created! */
    init_codegen(command.script.backend, stdalloc);

    Package* user_pkg = mk_user_package(base, &module_allocator, subregion);
    Module* user = get_module(string_to_name(mv_string("user")), user_pkg);
    set_std_current_module(user);
    set_current_package(user_pkg);

    IStream* fin = open_file_istream(command.script.filename, stdalloc);
    if (fin) {
      RegionAllocator* region = make_region_allocator(16384, true, stdalloc);
      run_script_from_istream(fin, get_formatted_stdout(), command.script.filename, user, region);
      delete_region_allocator(region);
            
      delete_istream(fin, stdalloc);
    } else {
      st_write_string(mv_string("Failed to open file: "), cout);
      st_write_string(command.script.filename, cout);
      st_write_string(mv_string("\n"), cout);
    }
    delete_package(user_pkg);
    break;
  }
  case CEval: {
    /** TODO (FUTURE BUG): move the codegen init to BEFORE the base package
        is created! */
    init_codegen(command.eval.backend, stdalloc);

    Package* user_pkg = mk_user_package(base, &module_allocator, subregion);
    Module* user = get_module(string_to_name(mv_string("user")), user_pkg);
    set_std_current_module(user);
    set_current_package(user_pkg);

    IterOpts opts = (IterOpts) {
      .debug_print = false,
      .is_eval = true,
    };

    IStream* sin = mv_string_istream(command.eval.expr, stdalloc);
    while (noninteractive_repl_iter(sin, stdalloc, subregion, &exalloc, user, opts)) {
      reset_subregion(subregion);
    }
    delete_istream(sin, stdalloc);
    delete_package(user_pkg);
    break;
  }
  case CHelp:
    write_help_string(get_formatted_stdout());
    break;
  case CVersion:
    st_write_string(mv_string("Pico Relic Compiler - Version "), cout);
    st_write_string(mv_string(version), cout);
    st_write_string(mv_string("\n"), cout);
    st_write_string(mv_string("target: x86_64\n"), cout);
    break;
  case CAtlas:
    run_atlas(base, command.for_atlas, get_formatted_stdout());
    sdelete_string_array(command.for_atlas);
    break;
  case CInvalid:
    st_write_string(command.error_message, cout);
    st_write_string(mv_string("\n"), cout);
    break;
  default:
    st_write_string(mv_string("Invalid Command Produced by parse_command!"), cout);
    st_write_string(mv_string("\n"), cout);
    break;
  }

  // Cleanup
  delete_region_allocator(region);
  delete_package(base);
  delete_assembler(ass);
  release_executable_allocator(exalloc);

  teardown_codegen();
  clear_symbols();
  thread_clear_dynamic_vars();
  clear_dynamic_vars();

#ifdef WINDOW_SYSYTEM
  pl_teardown_window_system();
#endif

  return 0;
}
