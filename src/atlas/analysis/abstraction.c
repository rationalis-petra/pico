#include "platform/signals.h"

#include "pico/abstraction/helpers.h"
#include "atlas/analysis/abstraction.h"
#include "atlas/analysis/propchecker.h"

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

HostRef (*host_abstract)(RawTree raw, HostCallbackData host_data, RegionAllocator* region, PiErrorPoint* point);   
HostRef abstract_atlas(RawTree raw, HostCallbackData host_data, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point) {
    Allocator ra = ra_to_gpa(region);

    switch (raw.type) {
    case RawBranch: {
        if (raw.branch.nodes.len == 0) {
            PicoError err = {
                .range = raw.range,
                .message = mv_cstr_doc("Empty stanza.", &ra),
            };
            throw_pi_error(point, err);
        }
        RawTree rawhead = raw.branch.nodes.data[0];
        Symbol head = get_symbol_err(rawhead, point, &ra);
        if (string_cmp(mv_string("library"), symbol_to_string(head, &ra)) == 0) {
            HostRef host = new_host(pool);
            TargetExpr texpr = {.type = Library};

            bool library_checks[] = {false, false, false};

            //property(, );
            // Library stanza components:
            //   name :: symbol
            //   file :: string option
            //   submodules :: string list
            PropSet* props = make_prop_set(3, &ra);
            add_expr_option_prop(mv_string("file"), &texpr.library.filename, props);
            add_expr_array_prop(mv_string("submodules"), &texpr.library.submodules, props);
            add_expr_array_prop(mv_string("depends-on"), &texpr.dependencies, props);

            for (size_t i = 1; i < raw.branch.nodes.len; i++) {
                parse_prop(raw.branch.nodes.data[i], props, library_checks, host_data, pool, point, region);
            }

            check_props(props, library_checks, raw.range, point, region);

            set_host(host, &texpr, pool);
            return host;
        } else if (string_cmp(mv_string("executable"), symbol_to_string(head, &ra)) == 0) {
            HostRef host = new_host(pool);
            TargetExpr texpr = {.type = Executable};
            bool executable_checks[] = {false, false, false};

            // Executable 
            //   name :: symbol
            //   file :: string 
            //   entry-point :: symbol
            //   dependencies :: symbol list
            PropSet* props = make_prop_set(3, &ra);
            add_expr_prop(mv_string("file"), &texpr.executable.filename, props);
            add_expr_prop(mv_string("entry-point"), &texpr.executable.entry_point, props);
            add_expr_array_prop(mv_string("dependencies"), &texpr.dependencies, props);

            for (size_t i = 1; i < raw.branch.nodes.len; i++) {
                parse_prop(raw.branch.nodes.data[i], props, executable_checks, host_data, pool, point, region);
            }

            check_props(props, executable_checks, raw.range, point, region);

            set_host(host, &texpr, pool);
            return host;
        } else {
            PicoError err = {
                .range = raw.range,
                .message = mv_cstr_doc("Unrecognized target header.", &ra),
            };
            throw_pi_error(point, err);
        }
        break;
    }
    case RawAtom: {
        PicoError err = {
            .range = raw.range,
            .message = mv_cstr_doc("Expecting a target, but got an atom instead.", &ra),
        };
        throw_pi_error(point, err);
        break;
    }
    }

    panic(mv_string("Invalid raw stanza received from parsing stage."));
}

Def abstract_atlas_def(RawTree raw, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point) {
    Name raw_names[] = {
        string_to_name(mv_string("library")),
        string_to_name(mv_string("executable")),
        string_to_name(mv_string("target")),
    }; 
    NameArray names = {
        .data = raw_names,
        .len = sizeof(raw_names) / sizeof(Name),
        .size = sizeof(raw_names) / sizeof(Name),
    };
    HostCallbackData hdata = {
        .abstract = abstract_atlas,
        .names = names,
    };

    return abstract_rune_def(raw, hdata, pool, region, point);
}

void parse_default(RawTree raw, PiErrorPoint *point, RegionAllocator* region, AtlPackage* out) {
    Allocator ra = ra_to_gpa(region);
    if (raw.type != RawBranch && raw.branch.nodes.len != 3) {
        PicoError err = {
            .range = raw.range,
            .message = mv_cstr_doc("The 'default' clause expects two terms, as it has form: (default <type> <target>).", &ra),
        };
        throw_pi_error(point, err);
    }
    RawTree raw_type = raw.branch.nodes.data[1];
    Symbol type;
    if (!get_fieldname(&raw_type, FColon, &type)) {
        PicoError err = {
            .range = raw.range,
            .message =
            mv_cstr_doc("The 'type' argument to the 'default' clause is "
                        "exepected to be a keyword - \n either :run :build or"
                        " :test. Recall that the default clause has form (default <type> <target>).", &ra), 
        };
        throw_pi_error(point, err);
    }

    RawTree raw_target = raw.branch.nodes.data[2];
    Symbol target;
    if (!get_symbol(raw_target, &target)) {
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
    if (symbol_eq(type, string_to_symbol(mv_string("run")))) {
        out->default_run = (NameOption) {.type = Some, .val = raw_target.atom.symbol.name};
    } else if (symbol_eq(type, string_to_symbol(mv_string("build")))) {
        out->default_build = (NameOption) {.type = Some, .val = raw_target.atom.symbol.name};
    } else if (symbol_eq(type, string_to_symbol(mv_string("test")))) {
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

void abstract_atlas_project(Project *project, ProjectRecord *record, RawTree raw, RegionAllocator *region, PiErrorPoint *point) {
    Allocator ra = ra_to_gpa(region);

    switch (raw.type) {
    case RawBranch: {
        if (raw.branch.nodes.len == 0) {
            PicoError err = {
                .range = raw.range,
                .message = mv_cstr_doc("Empty stanza.", &ra),
            };
            throw_pi_error(point, err);
        }
        RawTree rawhead = raw.branch.nodes.data[0];
        Symbol head = get_symbol_err(rawhead, point, &ra);
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

            HostCallbackData hdata = {};
            for (size_t i = 1; i < raw.branch.nodes.len; i++) {
                parse_prop(raw.branch.nodes.data[i], props, package_checks, hdata, NULL, point, region);
            }

            check_props(props, package_checks, raw.range, point, region);
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
    case RawAtom: {
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
