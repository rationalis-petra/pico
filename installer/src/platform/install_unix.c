#include "platform/machine_info.h"
#if OS_FAMILY == UNIX

#include "data/string.h"
#include "data/stream.h"
#include "data/result.h"

#include "platform/memory/std_allocator.h"
#include "platform/memory/arena.h"
#include "platform/environment.h"
#include "platform/filesystem/filesystem.h"

#include "install.h"

#define CHECK_RESULT(result) {if (result.type == Err) { write_string(res.error_message, cout); write_string(mv_string("\n"), cout); return 1; }}

int install_unix(int argc, char **argv) {
    Allocator* stdalloc = get_std_allocator();
    ArenaAllocator* arena = make_arena_allocator(16384, stdalloc);
    Allocator a = aa_to_gpa(arena);

    OStream* cout = get_stdout_stream();

    // TODO (FEATURE): check that ~/.local/bin is in $PATH, report an error if
    // it isn't
    StringOption home_dir = get_env_var(mv_string("HOME"));
    if (home_dir.type == None) {
        write_string(mv_string("Couldn't find env var 'HOME'\n"), cout);
        return 1;
    }
    String bin_dir = string_cat(home_dir.val, mv_string("/.local/bin/"), &a);

    // First: copy binaries (assests/{pico, keeper}) to ~/.local/bin
    String pico_dest = string_cat(bin_dir, mv_string("pico"), &a);
    Result res = copy_file(mv_string("assets/pico"), pico_dest);
    CHECK_RESULT(res);

    FilePermissions exec_perms = (FilePermissions) {
      .user = FRead | FWrite | FExecute,
      .group = FRead | FExecute,
      .other = FRead | FExecute
    };
    res = set_permissions(pico_dest, exec_perms);
    CHECK_RESULT(res);

    String keeper_dest = string_cat(bin_dir, mv_string("keeper"), &a);
    res = copy_file(mv_string("assets/keeper"), keeper_dest);
    CHECK_RESULT(res);

    res = set_permissions(keeper_dest, exec_perms);
    CHECK_RESULT(res);
    
    write_string(mv_string("Done!\n"), cout);
    return 0;
}

#endif
