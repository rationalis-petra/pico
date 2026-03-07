#include "platform/machine_info.h"
#if OS_FAMILY == WINDOWS

#include <windows.h>

#include "data/stream.h"
#include "data/result.h"
#include "data/string.h"
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

Result add_to_path(String to_add, Allocator* a) {
    HKEY hKey;
    DWORD path_type;
    DWORD old_path_size = 0;

    // Try to access the environment registry key
    if (RegOpenKeyEx(HKEY_CURRENT_USER,
                     "Environment",
                     0,
                     KEY_ALL_ACCESS,
                     &hKey) != ERROR_SUCCESS) {
        return (Result){.type = Err, .error_message = mv_string("Failed to access the environment registery key.")};
    }

    RegQueryValueEx(hKey, "Path", NULL, &path_type, NULL, &old_path_size);

    uint8_t* old_path_data = mem_alloc(old_path_size, a);
    RegQueryValueEx(hKey, "Path", NULL, &path_type, old_path_data, &old_path_size);

    // 3. Construct the new PATH string
    // Check if newPath is already in pszOldPath to avoid duplicates

    String new_path = string_ncat(a, 3,
        (String){.bytes=old_path_data, .memsize=old_path_size},
        mv_string(";"),
        to_add);

    // 4. Set the new PATH value in the registry
    if (RegSetValueEx(hKey, "Path", 0, REG_EXPAND_SZ, new_path.bytes, new_path.memsize)
        != ERROR_SUCCESS) {
        RegCloseKey(hKey);
        mem_free(new_path.bytes, a);
        mem_free(old_path_data, a);
        return (Result){.type = Err, .error_message = mv_string("Failed to access the environment registery key.")};
    }

    RegCloseKey(hKey);
    mem_free(new_path.bytes, a);
    mem_free(old_path_data, a);

    // 5. Broadcast the change to other windows/processes
    SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, 0, (LPARAM)L"Environment", SMTO_ABORTIFHUNG, 5000, NULL); return (Result){.type = Ok}; 
}

#include <stdio.h>
int install_windows(int argc, char **argv) {
    Allocator* stdalloc = get_std_allocator();
    ArenaAllocator* arena = make_arena_allocator(16384, stdalloc);
    Allocator a = aa_to_gpa(arena);
    OStream* cout = get_stdout_stream();

    RecordResult res = {.type = Ok};


    // TODO (FEATURE): check that ~/.local/bin is in $PATH, report an error if
    // it isn't
    StringOption app_data_dir = get_env_var(mv_string("LOCALAPPDATA"));
    if (app_data_dir.type == None) {
        write_string(mv_string("Couldn't find env var 'LOCALAPDATA'\n"), cout);
        return 1;
    }

    String bin_dir = path_cat(app_data_dir.val, mv_string("Programs\\pico"), &a);
    if (!record_exists(bin_dir)) {
         res = create_directory(bin_dir);
         CHECK_RESULT(res, "creating", bin_dir);
    }

    // TODO (BUG): check $PATH for our directory first...
    // TODO (FEATURE): Ask user for if they want to update $PATH
    StringOption path = get_env_var(mv_string("PATH"));
    if (path.type == Some && !is_substring(path.val, bin_dir)) {
        Result res = add_to_path(bin_dir, &a);
        if (res.type == Err) {
            write_string(mv_string("Failed to add bin directory to path\n"), cout);
            delete_arena_allocator(arena);
        }
    }

    // First: copy binaries (assests/{pico, keeper}) to ~/.local/bin
    String pico_dest = path_cat(bin_dir, mv_string("pico.exe"), &a);
    res = copy_file(mv_string("assets\\pico.exe"), pico_dest);
    CHECK_RESULT(res, "copying", mv_string("assets\\pico.exe"));

    FilePermissions exec_perms = (FilePermissions) {
      .user = FRead | FWrite | FExecute,
      .group = FRead | FExecute,
      .other = FRead | FExecute
    };
    res = set_permissions(pico_dest, exec_perms);
    CHECK_RESULT(res, "setting permissions of", pico_dest);

    String keeper_dest = path_cat(bin_dir, mv_string("keeper.exe"), &a);
    res = copy_file(mv_string("assets\\keeper.exe"), keeper_dest);
    CHECK_RESULT(res, "copying", mv_string("assets\\keeper.exe"));

    String relic_dest = path_cat(bin_dir, mv_string("relic.bat"), &a);
    res = copy_file(mv_string("assets\\scripts\\windows\\relic.bat"), relic_dest);
    CHECK_RESULT(res, "copying", mv_string("assets\\scripts\\windows\\relic.bat"));

    String atlas_dest = path_cat(bin_dir, mv_string("atlas.bat"), &a);
    res = copy_file(mv_string("assets\\scripts\\windows\\atlas.bat"), atlas_dest);
    CHECK_RESULT(res, "copying", mv_string("assets\\scripts\\windows\\atlas.bat"));

    res = set_permissions(keeper_dest, exec_perms);
    CHECK_RESULT(res, "settings permissions of", keeper_dest);

    String pico_data_dir = path_cat(app_data_dir.val, mv_string("pico"), &a);
    if (!record_exists(pico_data_dir)) {
         res = create_directory(pico_data_dir);
         CHECK_RESULT(res, "creating", pico_data_dir);
    }

    // Copy Archive to archive_dir
    // ------------------------------ 
    String archive_dir = path_cat(pico_data_dir, mv_string("archive"), &a);
    if (!record_exists(archive_dir)) {
         res = create_directory(archive_dir);
         CHECK_RESULT(res, "creating", archive_dir);
    }

    String archive_base_out_dir = path_cat(archive_dir, mv_string("base"), &a);
    if (record_exists(archive_base_out_dir)) {
        res = delete_directory(archive_base_out_dir, true);
        CHECK_RESULT(res, "deleting", archive_base_out_dir);
    }

    String archive_base_dir = mv_string("assets\\archive\\base");
    res = copy_directory(archive_base_dir, archive_base_out_dir);
    CHECK_RESULT(res, "copying", archive_base_dir);
    
    write_string(mv_string("Done!\n"), cout);
    return 0;
}

#endif
