#include "platform/memory/std_allocator.h"
#include "platform/memory/region.h"
#include "platform/filesystem/filesystem.h"

#include "components/pretty/stream_printer.h"

#include "atlas/atlas.h"

#include "atlas/app/command_line_opts.h"
#include "atlas/app/help_string.h"
#include "atlas/parse.h"

static const char* version = "0.0.1";

bool load_atlas_files(String path, OStream* out, RegionAllocator* region) {
    Allocator* stda = get_std_allocator();
    Directory* dir = open_directory(path, stda);
    DirEntArray entries = list_children(dir, stda);

    for (size_t i = 0; i < entries.len; i++) {
      DirectoryEntry entry = entries.data[i];
      bool fail = false;
      // If is a non-hidden directory
      if (entry.is_directory && (entry.name.memsize > 0 && entry.name.bytes[0] != '.')) {
          String newpath = string_ncat(stda, 3, path, mv_string("/"), entry.name);
          fail = load_atlas_files(newpath, out, region);
          mem_free(newpath.bytes, stda);
      } else if (string_cmp(entry.name, mv_string("atlas")) == 0) {
          String newpath = string_ncat(stda, 3, path, mv_string("/"), entry.name);
          IStream* fstream = open_file_istream(newpath, stda);
          IStream* captured_fstream = mk_capturing_istream(fstream, stda);

          RegionAllocator* subregion = make_subregion(region);
          AtParseResult parse_res = parse_atlas_defs(captured_fstream, subregion);
          if (parse_res.type == ParseNone) {
              write_string(mv_string("parse returned nothing...\n"), out);
          } else if (parse_res.type == ParseFail) {
              MultiError multi = (MultiError) {
                  .has_many = false,
                  .error = parse_res.error,
              };
              display_error(multi, captured_fstream, get_formatted_stdout(), NULL, stda);
              
              fail = true;
          } else if (parse_res.type == ParseSuccess) {
              write_string(mv_string("parse returned value:\n"), out);
              Document* prndoc = pretty_rawatlas(parse_res.result, stda);
              write_doc(prndoc, 120, out);
              write_string(mv_string("\n"), out);
              delete_doc(prndoc, stda);
          }

          release_subregion(subregion);

          delete_istream(captured_fstream, stda);
          delete_istream(fstream, stda);

          mem_free(newpath.bytes, stda);
      } else if (string_cmp(entry.name, mv_string("atlas-project")) == 0) {
          write_string(mv_string("atlas project at: "), out);

          write_string(path, out);
          write_string(mv_string("/"), out);
          write_string(entry.name, out);
          write_string(mv_string("\n"), out);
      }

      if (fail) break;
    }

    sdelete_dirent_array(entries);
    close_directory(dir);

    return 0;
}

void run_atlas(StringArray args, OStream* out) {
    AtlasCommand command = atlas_parse_command(args);

    switch (command.type) {
    case CInit:
        write_string(mv_string("TODO: Implement atlas init "), out);
        write_string(command.init.name, out);
        write_string(mv_string("\n"), out);
        break;
    case CRun: {
        write_string(mv_string("IMPLEMENTING: atlas run "), out);
        write_string(command.run.target, out);
        write_string(mv_string("\n"), out);

        Allocator* stda = get_std_allocator();
        RegionAllocator* region = make_region_allocator(4096, true, stda);
        load_atlas_files(mv_string("."), out, region);
        delete_region_allocator(region);

        break;
    }
    case CHelp:
        write_atlas_help_string(get_formatted_stdout());
        break;
    case CVersion:
        write_string(mv_string("Atlas Relic Build System - Version "), out);
        write_string(mv_string(version), out);
        write_string(mv_string("\n"), out);
        break;
    case CInvalid:
        write_string(command.error_message, out);
        write_string(mv_string("\n"), out);
        break;
    default:
        write_string(mv_string("Error in atlas command line parser: invalid result returned"), out);
        break;
    }
}
