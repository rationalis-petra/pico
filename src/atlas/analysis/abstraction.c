#include "platform/signals.h"

#include "atlas/analysis/abstraction.h"
#include "atlas/analysis/propchecker.h"

static Symbol get_symbol(RawAtlas raw, PiErrorPoint* point, Allocator* a);

typedef enum {
    LName,
    LFile,
    LSubmodules,
} LibraryChecks;

typedef enum {
    EName,
    EFile,
    EEntryPoint,
    EDependencies,
} ExecutableChecks;


typedef struct {
} Properties;


Stanza abstract_atlas(RawAtlas raw, RegionAllocator* region, PiErrorPoint* point) {
    Allocator ra = ra_to_gpa(region);

    switch (raw.type) {
    case AtlBranch: {
        if (raw.branch.len == 0) {
            PicoError err = {
                .range = raw.range,
                .message = mv_cstr_doc("Empty stanza.", &ra),
            };
            throw_pi_error(point, err);
        }
        RawAtlas rawhead = raw.branch.data[0];
        Symbol head = get_symbol(rawhead, point, &ra);
        if (string_cmp(mv_string("library"), symbol_to_string(head, &ra)) == 0) {
            Library library_stanza = {};

            bool library_checks[] = {false, false, false};

            //property(, );
            // Library stanza components:
            //   name :: symbol
            //   file :: string option
            //   submodules :: string list
            PropSet* props = make_prop_set(4, &ra);
            add_symbol_prop(mv_string("name"), &library_stanza.name, props);
            add_string_option_prop(mv_string("file"), &library_stanza.filename, props);
            add_string_array_prop(mv_string("submodules"), &library_stanza.submodules, props);

            for (size_t i = 1; i < raw.branch.len; i++) {
                parse_prop(raw.branch.data[i], props, library_checks, point, &ra);
            }

            check_props(props, library_checks, raw.range, point, &ra);

            return (Stanza) {
                .type = StLibrary,
                .library = library_stanza,
            };
        } else if (string_cmp(mv_string("executable"), symbol_to_string(head, &ra)) == 0) {
            Executable executable_stanza = {};
            bool executable_checks[] = {false, false, false, false};

            // Executable 
            //   name :: symbol
            //   file :: string 
            //   entry-point :: symbol
            //   dependencies :: symbol list
            PropSet* props = make_prop_set(4, &ra);
            add_symbol_prop(mv_string("name"), &executable_stanza.name, props);
            add_string_prop(mv_string("file"), &executable_stanza.filename, props);
            add_string_prop(mv_string("entry-point"), &executable_stanza.entry_point, props);
            add_symbol_array_prop(mv_string("dependencies"), &executable_stanza.dependencies, props);

            for (size_t i = 1; i < raw.branch.len; i++) {
                parse_prop(raw.branch.data[i], props, executable_checks, point, &ra);
            }

            check_props(props, executable_checks, raw.range, point, &ra);

            return (Stanza) {
                .type = StExecutable,
                .executable = executable_stanza,
            };
        }


        break;
    }
    case AtlAtom: {
        PicoError err = {
            .range = raw.range,
            .message = mv_cstr_doc("Expecting a stanza, but got an atom instead.", &ra),
        };
        throw_pi_error(point, err);
        break;
    }
    }

    panic(mv_string("Invalid raw stanza received from parsing stage."));
}

Symbol get_symbol(RawAtlas raw, PiErrorPoint* point, Allocator* a) {
    if (raw.type != AtlAtom) {
        PicoError err = {
            .range = raw.range,
            .message = mk_str_doc(mv_string("Expected symbol here, got compound term instead."), a),
        };
        throw_pi_error(point, err);
    }

    if (raw.atom.type != AtSymbol) {
        PicoError err = {
            .range = raw.range,
            .message = mk_str_doc(mv_string("Expected symbol here."), a),
        };
        throw_pi_error(point, err);
    }

    return raw.atom.symbol;
}
