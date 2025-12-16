#include "platform/signals.h"
#include "platform/memory/std_allocator.h"
#include "platform/memory/region.h"
#include "platform/filesystem/filesystem.h"

#include "components/pretty/stream_printer.h"

#include "atlas/atlas.h"

#include "atlas/data/error.h"
#include "atlas/app/command_line_opts.h"
#include "atlas/app/help_string.h"
#include "atlas/parse.h"
#include "atlas/analysis/abstraction.h"
#include "atlas/eval/instance.h"

static const char* version = "0.0.1";

bool process_atlas(AtlasInstance* instance, IStream* in, FormattedOStream* out, String path, String filename, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) goto on_pi_error;

    bool running = true;
    while (running) {
        AtParseResult parse_res = parse_atlas_defs(in, region);
        if (parse_res.type == ParseNone) {
            running = false;
        } else if (parse_res.type == ParseFail) {
            MultiError multi = (MultiError) {
                .has_many = false,
                .error = parse_res.error,
            };
            display_error(multi, *get_captured_buffer(in), get_formatted_stdout(), filename, &ra);
            goto on_error_generic;
        } else {
            Stanza stanza = abstract_atlas(parse_res.result, region, &pi_point);
            switch (stanza.type) {
            case StExecutable:
                add_executable(stanza.executable, path, instance);
                break;
            case StLibrary:
                add_library(stanza.library, path, instance);
                break;
            }
        }
    }

    return false;

 on_pi_error:
    display_error(pi_point.multi, *get_captured_buffer(in), out, filename, &ra);
 on_error_generic:
    return true;
}

bool process_atlas_project(AtlasInstance* instance, IStream* in, FormattedOStream* out, String filename, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);

    Project project;
    ProjectRecord record;

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) goto on_pi_error;

    bool running = true;
    while (running) {
        AtParseResult parse_res = parse_atlas_defs(in, region);
        if (parse_res.type == ParseNone) {
            running = false;
        } else if (parse_res.type == ParseFail) {
            MultiError multi = (MultiError) {
                .has_many = false,
                .error = parse_res.error,
            };
            display_error(multi, *get_captured_buffer(in), get_formatted_stdout(), filename, &ra);
            goto on_error_generic;
              
        } else if (parse_res.type != ParseSuccess) {
            panic(mv_string("Atlas parse returned invalid result"));
        } else {
            abstract_atlas_project(&project, &record, parse_res.result, region, &pi_point);
        }
    }

    // TODO: check all fields were filled out!
    set_instance_project(instance, project);
    return 0;

 on_pi_error:
    display_error(pi_point.multi, *get_captured_buffer(in), out, filename, &ra);
 on_error_generic:
    return true;
}

bool load_atlas_files(String path, FormattedOStream* out, AtlasInstance* instance, RegionAllocator* region) {
    Allocator* stda = get_std_allocator();
    Directory* dir = open_directory(path, stda);
    DirEntArray entries = list_children(dir, stda);

    bool fail = false;
    for (size_t i = 0; i < entries.len; i++) {
      DirectoryEntry entry = entries.data[i];
      // If is a non-hidden directory
      if (entry.is_directory && (entry.name.memsize > 0 && entry.name.bytes[0] != '.')) {
          String newpath = string_ncat(stda, 3, path, mv_string("/"), entry.name);
          fail = load_atlas_files(newpath, out, instance, region);
          mem_free(newpath.bytes, stda);
      } else if (string_cmp(entry.name, mv_string("atlas")) == 0) {
          String newpath = string_ncat(stda, 3, path, mv_string("/"), entry.name);
          IStream* fstream = open_file_istream(newpath, stda);
          IStream* captured_fstream = mk_capturing_istream(fstream, stda);
          RegionAllocator* subregion = make_subregion(region);

          fail = process_atlas(instance, captured_fstream, out, path, newpath, region);

          release_subregion(subregion);
          delete_istream(captured_fstream, stda);
          delete_istream(fstream, stda);
          mem_free(newpath.bytes, stda);
      } else if (string_cmp(entry.name, mv_string("atlas-project")) == 0) {
          String newpath = string_ncat(stda, 3, path, mv_string("/"), entry.name);
          IStream* fstream = open_file_istream(newpath, stda);
          IStream* captured_fstream = mk_capturing_istream(fstream, stda);
          RegionAllocator* subregion = make_subregion(region);

          fail = process_atlas_project(instance, captured_fstream, out, newpath, region);

          release_subregion(subregion);
          delete_istream(captured_fstream, stda);
          delete_istream(fstream, stda);
          mem_free(newpath.bytes, stda);
      }

      if (fail) break;
    }

    sdelete_dirent_array(entries);
    close_directory(dir);
    return fail;
}

void run_atlas(Package* package, StringArray args, FormattedOStream* out) {
    AtlasCommand command = atlas_parse_command(args);

    switch (command.type) {
    case CInit:
        write_fstring(mv_string("TODO: Implement atlas init "), out);
        write_fstring(command.init.name, out);
        write_fstring(mv_string("\n"), out);
        break;
    case CRun: {
        Allocator* stda = get_std_allocator();
        AtlasInstance* instance = make_atlas_instance(stda);
        register_package(instance, package);

        RegionAllocator* region = make_region_allocator(4096, true, stda);
        load_atlas_files(mv_string("."), out, instance, region);

        AtErrorPoint point;
        if (catch_error(point)) {
            Allocator ra = ra_to_gpa(region);
            MultiError error = (MultiError) {
              .has_many = false,
              .error = (PicoError) {.range = point.error.range, .message = point.error.message},
            };
            display_error(error, point.error.captured_file, out, point.error.filename, &ra);
        } else {
            atlas_run(instance, command.run.target, region, &point);
        }
        delete_region_allocator(region);
        delete_atlas_instance(instance);

        break;
    }
    case CHelp:
        write_atlas_help_string(get_formatted_stdout());
        break;
    case CVersion:
        write_fstring(mv_string("Atlas Relic Build System - Version "), out);
        write_fstring(mv_string(version), out);
        write_fstring(mv_string("\n"), out);
        break;
    case CInvalid:
        write_fstring(command.error_message, out);
        write_fstring(mv_string("\n"), out);
        break;
    default:
        write_fstring(mv_string("Error in atlas command line parser: invalid result returned"), out);
        break;
    }
}
