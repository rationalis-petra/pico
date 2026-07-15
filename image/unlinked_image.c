#include "data/string.h"
#include "data/stream.h"

#include "platform/memory/std_allocator.h"
#include "platform/terminal/terminal.h"
#include "platform/window/window.h"
#include "platform/hedron/hedron.h"

#include "components/assembler/assembler.h"
#include "pico/stdlib/platform/submodules.h"
#include "pico/eval/call.h"

void* pico_entry_point;

int main(int argc, char** argv) {
    // Setup
    Allocator* stdalloc = get_std_allocator();
    IStream* cin = get_stdin_stream();
    OStream* cout = get_stdout_stream();

    // Init terminal first, as other initializers may panic, therefore writing
    //   to stdout, which uses the terminal module.
    init_terminal(stdalloc);

    // Initialization order here is not important
    init_ctypes();
    init_asm();
    init_dynamic_vars(stdalloc);

#ifdef WINDOW_SYSTEM
    if (pl_init_window_system(stdalloc)) {
        st_write_string(mv_string("Warning: failed to init window system!\n"), cout);
    }
#endif

#ifdef USE_VULKAN
    if (init_hedron(stdalloc)) {
        st_write_string(mv_string("Warning: failed to init hedron!\n"), cout);
    }
#endif

    thread_init_dynamic_vars();

    /** TODO: current package/module?? */
    //set_std_current_module(module);
    //set_current_package(base);
    set_std_istream(cin);
    set_std_ostream(cout);

    /**
     * Run the main symbol
     */
    call_unit_fn(pico_entry_point, stdalloc);


    // Cleanup
    thread_clear_dynamic_vars();
    clear_dynamic_vars();

#ifdef USE_VULKAN
    teardown_hedron();
#endif

#ifdef WINDOW_SYSYTEM
    pl_teardown_window_system();
#endif

    return 0;
}
