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

            bool library_checks[] = {false, false, false, false};

            //property(, );
            // Library stanza components:
            //   name :: symbol
            //   file :: string option
            //   submodules :: string list
            PropSet* props = make_prop_set(4, &ra);
            add_name_prop(mv_string("name"), &library_stanza.name, props);
            add_string_option_prop(mv_string("file"), &library_stanza.filename, props);
            add_string_array_prop(mv_string("submodules"), &library_stanza.submodules, props);
            add_name_array_prop(mv_string("dependencies"), &library_stanza.dependencies, props);

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
            add_name_prop(mv_string("name"), &executable_stanza.name, props);
            add_string_prop(mv_string("file"), &executable_stanza.filename, props);
            add_name_prop(mv_string("entry-point"), &executable_stanza.entry_point, props);
            add_name_array_prop(mv_string("dependencies"), &executable_stanza.dependencies, props);

            for (size_t i = 1; i < raw.branch.len; i++) {
                parse_prop(raw.branch.data[i], props, executable_checks, point, &ra);
            }

            check_props(props, executable_checks, raw.range, point, &ra);

            return (Stanza) {
                .type = StExecutable,
                .executable = executable_stanza,
            };
        } else {
            PicoError err = {
                .range = raw.range,
                .message = mv_cstr_doc("Unrecognized stanza header.", &ra),
            };
            throw_pi_error(point, err);
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

void parse_default(RawAtlas raw, PiErrorPoint *point, RegionAllocator* region, AtlPackage* out) {
    Allocator ra = ra_to_gpa(region);
    if (raw.type != AtlBranch && raw.branch.len != 3) {
        PicoError err = {
            .range = raw.range,
            .message = mv_cstr_doc("The 'default' clause expects two terms, as it has form: (default <type> <target>).", &ra),
        };
        throw_pi_error(point, err);
    }
    RawAtlas raw_type = raw.branch.data[1];
    if (raw_type.type != AtlAtom && raw_type.atom.type != AtKeyword) {
        PicoError err = {
            .range = raw.range,
            .message =
            mv_cstr_doc("The 'type' argument to the 'default' clause is "
                        "exepected to be a keyword - \n either :run :build or"
                        " :test. Recall that the default clause has form (default <type> <target>).", &ra), 
        };
        throw_pi_error(point, err);
    }

    RawAtlas raw_target = raw.branch.data[2];
    if (raw_target.type != AtlAtom && raw_target.atom.type != AtSymbol) {
        PicoError err = {
            .range = raw.range,
            .message =
            mv_cstr_doc("The 'target' argument to the 'default' clause is "
                        "exepected to be a symbol \n whose value is the name"
                        " of the target to use. Recall that the default clause has form (default <type> <target>).", &ra), 
        };
        throw_pi_error(point, err);
    }

    // TODO: report error if duplicated!
    if (symbol_eq(raw_type.atom.keyword, string_to_symbol(mv_string("run")))) {
        out->default_run = (NameOption) {.type = Some, .val = raw_target.atom.symbol.name};
    } else if (symbol_eq(raw_type.atom.keyword, string_to_symbol(mv_string("build")))) {
        out->default_build = (NameOption) {.type = Some, .val = raw_target.atom.symbol.name};
    } else if (symbol_eq(raw_type.atom.keyword, string_to_symbol(mv_string("test")))) {
        out->default_test = (NameOption) {.type = Some, .val = raw_target.atom.symbol.name};
    } else {
        PicoError err = {
            .range = raw.range,
            .message =
            mv_cstr_doc("The 'type' keyword to the 'default' clause is "
                        "exepected have a value that is one of \n :run, :build or "
                        ":test. Recall that the default clause has form (default <type> <target>).", &ra), 
        };
        throw_pi_error(point, err);
    }
}

void abstract_atlas_project(Project *project, ProjectRecord *record, RawAtlas raw, RegionAllocator *region, PiErrorPoint *point) {
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
        if (string_cmp(mv_string("lang"), symbol_to_string(head, &ra)) == 0) {
            return;
        } else if (string_cmp(mv_string("package"), symbol_to_string(head, &ra)) == 0) {
            bool package_checks[] = {false, false , false};

            // Package 
            // - package-name :: symbol
            // - package-dependencies :: symbol list
            PropSet* props = make_prop_set(4, &ra);
            add_name_prop(mv_string("name"), &project->package.name, props);
            add_name_array_prop(mv_string("dependencies"), &project->package.dependencies, props);
            add_callback_prop(mv_string("default"), (PropCb)parse_default, &ra, &project->package, props);

            for (size_t i = 1; i < raw.branch.len; i++) {
                parse_prop(raw.branch.data[i], props, package_checks, point, &ra);
            }

            check_props(props, package_checks, raw.range, point, &ra);
            record->package = true;
            return;
        } else {
            PicoError err = {
                .range = raw.range,
                .message = mv_cstr_doc("Unrecognized atlas project stanza header.", &ra),
            };
            throw_pi_error(point, err);
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
