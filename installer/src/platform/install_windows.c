#include "platform/machine_info.h"
#if OS_FAMILY == WINDOWS

#include <windows.h>

#include "data/string.h"
#include "data/stream.h"
#include "data/result.h"

#include "platform/memory/std_allocator.h"
#include "platform/memory/arena.h"
#include "platform/environment.h"
#include "platform/filesystem/filesystem.h"

#include "install.h"

#define CHECK_RESULT(result) {if (result.type == Err) { \
      write_string(res.error_message, cout); \
      write_string(mv_string("\n"), cout);  \
      delete_arena_allocator(arena); \
      printf("%lu\n", GetLastError()); \
      return 1; }}

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

    Result res = (Result){.type = Ok};


    // TODO (FEATURE): check that ~/.local/bin is in $PATH, report an error if
    // it isn't
    StringOption app_data_dir = get_env_var(mv_string("LOCALAPPDATA"));
    if (app_data_dir.type == None) {
        write_string(mv_string("Couldn't find env var 'LOCALAPDATA'\n"), cout);
        return 1;
    }
    String bin_dir = string_cat(app_data_dir.val, mv_string("/Programs/pico/"), &a);

    if (!file_exists(bin_dir)) {
         res = create_directory(bin_dir);
         CHECK_RESULT(res);
    }

    // TODO (BUG): check $PATH for our directory first...
    // TODO (FEATURE): Ask user for if they want to update $PATH
    StringOption path = get_env_var(mv_string("PATH"));
    if (path.type == Some && !is_substring(path.val, bin_dir)) {
        res = add_to_path(bin_dir, &a);
        CHECK_RESULT(res)
    }

    // First: copy binaries (assests/{pico, keeper}) to ~/.local/bin
    String pico_dest = string_cat(bin_dir, mv_string("pico.exe"), &a);
    res = copy_file(mv_string("assets/pico.exe"), pico_dest);
    CHECK_RESULT(res);

    FilePermissions exec_perms = (FilePermissions) {
      .user = FRead | FWrite | FExecute,
      .group = FRead | FExecute,
      .other = FRead | FExecute
    };
    res = set_permissions(pico_dest, exec_perms);
    CHECK_RESULT(res);

    String keeper_dest = string_cat(bin_dir, mv_string("keeper.exe"), &a);
    res = copy_file(mv_string("assets/keeper.exe"), keeper_dest);
    CHECK_RESULT(res);

    res = set_permissions(keeper_dest, exec_perms);
    CHECK_RESULT(res);
    
    write_string(mv_string("Done!\n"), cout);
    return 0;
}

#endif
