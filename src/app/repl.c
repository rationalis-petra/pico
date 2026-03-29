#include "platform/jump.h"
#include "platform/terminal/terminal.h"

#include "data/stream.h"
#include "data/stringbuffer.h"

#include "components/pretty/stream_printer.h"
#include "pico/data/error.h"
#include "pico/stdlib/extra.h"
#include "pico/codegen/codegen.h"
#include "pico/eval/call.h"

#include "app/repl.h"
#include "app/dev/process_term.h"

// REPL Function
// The REPL uses the RAW mode of the terminal, meaning it can process
// input, resizing and mouse events. It can also offer more interactive
// features, such as syntax highlighting and live errors.
// -----------------------------------------------------------------------------
// The stages of the REPL work as follows:
//
// In the 'input' stage, the repl will loop on user input, processing events as
// follows:
//  - If the event is a character input, then it will add the input to an
//    internal string-buffer and write it to the screen. From here, the
//    stringbuffer is parsed.
//    - If the parse is unsuccessful, then a parse error is written below the
//      input.
//    - If the parse is successful, the term will be typecheked and the output
//      type (or error) is printed below the input.
// 
//  - If the event is an up/down arrow, then the repl will cycle through a
//    history of past input phrases
//  - If the event is a newline/return, then the repl will attempt to parse the
//    input and proceed to the PROCESSING. This will not happen ONLY IF the parser indicates
//    that the expression is incomplete. In this case, the newline will
//    introduce an actual newline character and the user may continue typing.
//    
//

#define CTRL_KEY(k) ((k) & 0x1f)

bool repl_iter(Allocator* stdalloc, RegionAllocator* region, Allocator* exec, Module* module, IterOpts opts) {
    IStream *cin = get_stdin_stream();
    // Note: we need to be aware of the arena and error point, as both are used
    // by code in the 'true' branches of the nonlocal exits, and may be stored
    // in registers, so they cannotbe changed (unless marked volatile).
    Allocator ra = ra_to_gpa(region);

    IStream* volatile cp_in = mk_capturing_istream(cin, &ra);
    reset_bytecount(cp_in);

    jump_buf exit_point;
    if (set_jump(exit_point)) return false;
    set_exit_callback(&exit_point);

    StringBuffer buffer = mk_string_buffer(256, &ra);
    bool running = true;

    PosCursorData start_pos = get_cursor_pos();

    String name = view_symbol_string(module_name(module));
    terminal_write_string_unbuffered(name);
    terminal_write_string_unbuffered(mv_string(" > "));
    
    while (running) {
      // Process input event:
      InTermEvent in = get_in_terminal_event();
      bool pressed_enter = false;
      switch (in.type) {
      case ITNone:
        continue;
        break;
      case ITChar: {
        if (in.codepoint == CTRL_KEY('q')) {
          return false;
        } else if (in.codepoint == '\b' || in.codepoint == 127) {
          delete_nchars(1, &buffer);
        } else if (in.codepoint == '\r') {
          pressed_enter = true;
        } else {
          OutTermEvent out = (OutTermEvent) {
            .type = OTChar,
            .codepoint = in.codepoint,
          };
          send_output_terminal_event(out);
          insert_char(in.codepoint, &buffer);
        }
      
        break;
      }
      default:  
        break;
      }


      // ------------------------------------------------------------
      // Print current state
      // ------------------------------------------------------------
    
      // TODO: add buffering/batch sending
      send_output_terminal_event((OutTermEvent) {
          .type = OTPosCursor,
          .cursor_pos = start_pos,
        });
      send_output_terminal_event((OutTermEvent) {
          .type = OTClear,
          .clear = ClearToScreenEnd,
        });
      send_output_terminal_event((OutTermEvent) {
          .type = OTPosCursor,
          .cursor_pos = start_pos,
        });

      String name = view_symbol_string(module_name(module));
      terminal_write_string_unbuffered(name);
      terminal_write_string_unbuffered(mv_string(" > "));
      String buf_contents = get_contents(buffer, &ra);
      terminal_write_string_unbuffered(buf_contents);

      // ------------------------------------------------------------
      //   Generate Status
      // ------------------------------------------------------------
      ProcessInput input = {
        .current = module,
        .input = buf_contents,
        .cursor_pos = buffer.cursor_pos,
        .options = POErrors,
      };
      ProcessResult result = process_term(input, region);

      // ------------------------------------------------------------
      //   Print Status
      // ------------------------------------------------------------
      PosCursorData edit_pos = get_cursor_pos();
      if (result.errors) {
        terminal_write_string_unbuffered(mv_string("\r\nErrors:\r\n--------\r\n"));
        for (size_t i = 0; i < result.errors->len; i++) {
          PicoError* error = result.errors->data[i];
          // TODO: use min (terminal width, 120)
          write_doc_formatted(error->message, 120, get_formatted_stdout());
        }
      } else if (result.has_term == Some) {
        // Only proceed if the key justpressed was a newline.
        if (pressed_enter) {
          terminal_write_string_unbuffered(mv_string("\r\n"));
          Target gen_target = {
            .target = mk_assembler(current_cpu_feature_flags(), exec),
            .code_aux = mk_assembler(current_cpu_feature_flags(), exec),
            .data_aux = mem_alloc(sizeof(U8Array), &ra)
          };
          *gen_target.data_aux = mk_u8_array(128, &ra);

          clear_target(gen_target);
          ErrorPoint point;
          if (catch_error(point)) goto on_error;
          Environment* env = env_from_module(module, &point, &ra);

          CodegenContext cg_ctx = {
            .a = &ra, .point = &point, .target = gen_target,
          };
          LinkData links = generate_toplevel(result.checked_term, env, cg_ctx);
          EvalResult call_res = pico_run_toplevel(result.checked_term, gen_target, links, module, &ra, &point);
          Document* doc = pretty_res(call_res, &ra);
          // TODO: use terminal width
          write_doc_formatted(doc, 120, get_formatted_stdout());
          terminal_write_string_unbuffered(mv_string("\r\n"));

          return true;

        on_error:
          write_doc_formatted(point.error_message, 120, get_formatted_stdout());
          terminal_write_string_unbuffered(mv_string("\r\n"));
        }

      } else {
        if (pressed_enter) {
          terminal_write_string_unbuffered(mv_string("\r\ninternal error: failed to eval\r\n"));
          return true;
        }
      }

      send_output_terminal_event((OutTermEvent) {
          .type = OTPosCursor,
          .cursor_pos = edit_pos,
        });
    }

    return true;
}
