#include "platform/memory/arena.h"

#include "pico/stdlib/extra.h"

#include "test_pico/stdlib/components.h"
#include "test_pico/helper.h"

void run_pico_stdlib_extra_tests(TestLog *log, Module* module, Allocator *a) {
    Allocator arena = mk_arena_allocator(4096, a);
    if (test_start(log, mv_string("print"))) {
        test_toplevel_stdout("(print \"test\")", "test", module, log, a) ;
    }

    if (test_start(log, mv_string("single-for-upto"))) {
        Allocator current_old = get_std_current_allocator();
        set_std_current_allocator(arena);
        test_toplevel_stdout("(loop [for i from 1 upto 10] (print (u64.to-string i)))", "12345678910", module, log, a) ;
        set_std_current_allocator(current_old);
    }

    if (test_start(log, mv_string("single-for-below"))) {
        Allocator current_old = get_std_current_allocator();
        set_std_current_allocator(arena);
        test_toplevel_stdout("(loop [for i from 1 below 10] (print (u64.to-string i)))", "123456789", module, log, a) ;
        set_std_current_allocator(current_old);
    }

    if (test_start(log, mv_string("single-for-downto"))) {
        Allocator current_old = get_std_current_allocator();
        set_std_current_allocator(arena);
        test_toplevel_stdout("(loop [for i from 10 downto 1] (print (u64.to-string i)))", "10987654321", module, log, a) ;
        set_std_current_allocator(current_old);
    }

    if (test_start(log, mv_string("single-for-below"))) {
        Allocator current_old = get_std_current_allocator();
        set_std_current_allocator(arena);
        test_toplevel_stdout("(loop [for i from 10 above 1] (print (u64.to-string i)))", "1098765432", module, log, a) ;
        set_std_current_allocator(current_old);
    }

    if (test_start(log, mv_string("for-then-=-loop"))) {
        Allocator current_old = get_std_current_allocator();
        set_std_current_allocator(arena);
        test_toplevel_stdout("(loop [for i from 1 upto 10] [for j = 0 then (u64.mod (u64.+ 1 j) 2)] (print (u64.to-string j)))", "0101010101", module, log, a) ;
        set_std_current_allocator(current_old);
    }
    release_arena_allocator(arena);
}
