#include "platform/machine_info.h"
#if OS_FAMILY == UNIX

#include "data/string.h"
#include "data/stream.h"
#include "data/result.h"
#include "data/stringify.h"

#include "platform/memory/std_allocator.h"
#include "platform/memory/arena.h"
#include "platform/environment.h"
#include "platform/filesystem/filesystem.h"

#include "install.h"

#define CHECK_RESULT(result, action, target)               \
    if (result.type == Err) {                               \
        write_string(mv_string("failure: while "), cout);   \
        write_string(mv_string(action), cout);              \
        write_string(mv_string(" "), cout);                 \
        write_string(target, cout);                         \
        write_string(mv_string("\n  error code: "), cout);  \
        write_string(string_u64(res.error, &a), cout);      \
        write_string(mv_string("\n"), cout);                \
        delete_arena_allocator(arena);                      \
        return 1;                                           \
    }                                                       \

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
    String bin_dir = path_cat(home_dir.val, mv_string(".local/bin"), &a);

    // First: copy binaries (assests/{pico, keeper}) to ~/.local/bin
    String pico_dest = path_cat(bin_dir, mv_string("pico"), &a);
    RecordResult res = copy_file(mv_string("assets/pico"), pico_dest);
    CHECK_RESULT(res, "copying", mv_string("assets/pico"));

    FilePermissions exec_perms = (FilePermissions) {
      .user = FRead | FWrite | FExecute,
      .group = FRead | FExecute,
      .other = FRead | FExecute
    };
    res = set_permissions(pico_dest, exec_perms);
    CHECK_RESULT(res, "setting permissions of", pico_dest);

    String keeper_dest = path_cat(bin_dir, mv_string("keeper"), &a);
    res = copy_file(mv_string("assets/pico_keeper"), keeper_dest);
    CHECK_RESULT(res, "copying", mv_string("assets/pico_keeper"));

    res = set_permissions(keeper_dest, exec_perms);
    CHECK_RESULT(res, "setting permissions of", keeper_dest);

    String pico_share_dir = path_cat(home_dir.val, mv_string(".local/share/pico"), &a);
    if (!record_exists(pico_share_dir)) {
        res = create_directory(pico_share_dir);
        CHECK_RESULT(res, "creating", pico_share_dir);
    }


    // Copy Archive to archive_dir
    // ------------------------------ 
    String archive_dir = path_cat(pico_share_dir, mv_string("archive"), &a);
    res = copy_directory(mv_string("assets/archive"), archive_dir);
    CHECK_RESULT(res, "copying", mv_string("assets/archive"));
    
    write_string(mv_string("Done!\n"), cout);
    return 0;
}

#endif
