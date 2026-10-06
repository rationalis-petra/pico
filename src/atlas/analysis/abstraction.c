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

// Will give us a type error if the callback signature changes
HostCallback abstract_atlas;
void abstract_atlas(RawTree raw, HostCallbackData host_data, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point, void* cb_data, Expr* out) {
    Allocator ra = ra_to_gpa(region);
    AtAbsCallbackData types = {}; //*(AtAbsCallbackData*)cb_data;

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
            // Extract from the target expression into a syntax tree!
            ExprRef ctor_ref = new_expr(pool);
            ExprRef ttype_ref = new_expr(pool);
            ExprSlice members = new_expr_slice(1, pool);
            *out = (Expr) {
                .type = EApp,
                .app = {
                    .fn = ctor_ref,
                    .args = members,
                }
            };

            Expr ctor = {
                .type = ECtor,
                .ctor = {
                    .name = string_to_name(mv_string("library")),
                    .type = {.type = Some, .val = ttype_ref},
                },
            };
            set_expr(ctor_ref, ctor, pool);

            Expr ttype = {
                .type = EVal,
                .value = types.target_type,
            };
            set_expr(ttype_ref, ttype, pool);

            NameExprMap fields = new_expr_map(3, pool);
            Expr lib_record = {
                .type = ERecord,
                .record.fields = fields,
            };
            set_expr_elt(members, 0, lib_record, pool);
            NameExprCell dependencies = {
                .name = string_to_name(mv_string("depends-on")), 
            };
            NameExprCell filename = {
                .name = string_to_name(mv_string("file")), 
            };
            NameExprCell submodules = {
                .name = string_to_name(mv_string("submodules")), 
            };

            /**
             * Begin parsing: defer to propchecker.
             */
            bool library_checks[] = {false, false, false};

            //property(, );
            // Library stanza components:
            //   file :: string option
            //   submodules :: string list
            //   dependencies :: dependency list
            PropSet* props = make_prop_set(3, &ra);
            add_expr_array_prop(mv_string("depends-on"), &dependencies.val, props);
            add_expr_option_prop(mv_string("file"), &filename.val, props);
            add_expr_array_prop(mv_string("submodules"), &submodules.val, props);

            for (size_t i = 1; i < raw.branch.nodes.len; i++) {
                parse_prop(raw.branch.nodes.data[i], props, library_checks, host_data, pool, point, region);
            }

            check_props(props, library_checks, raw.range, point, region);

            /**
             * End parsing, set expressions
             */
            set_expr_map_elt(fields, 0, dependencies, pool);
            set_expr_map_elt(fields, 1, filename, pool);
            set_expr_map_elt(fields, 2, submodules, pool);
        } else if (string_cmp(mv_string("executable"), symbol_to_string(head, &ra)) == 0) {
            bool executable_checks[] = {false, false, false};

            ExprRef ctor_ref = new_expr(pool);
            ExprRef ttype_ref = new_expr(pool);
            ExprSlice members = new_expr_slice(1, pool);
            *out = (Expr) {
                .type = EApp,
                .app = {
                    .fn = ctor_ref,
                    .args = members,
                }
            };
            Expr ctor = {
                .type = ECtor,
                .ctor = {
                    .name = string_to_name(mv_string("executable")),
                    .type = {.type = Some, .val = ttype_ref},
                },
            };
            set_expr(ctor_ref, ctor, pool);

            Expr ttype = {
                .type = EVal,
                .value = types.target_type,
            };
            set_expr(ttype_ref, ttype, pool);

            NameExprMap fields = new_expr_map(3, pool);
            Expr lib_record = {
                .type = ERecord,
                .record.fields = fields,
            };
            set_expr_elt(members, 0, lib_record, pool);
            NameExprCell dependencies = {
                .name = string_to_name(mv_string("depends-on")), 
            };
            NameExprCell filename = {
                .name = string_to_name(mv_string("file")), 
            };
            NameExprCell entry_point = {
                .name = string_to_name(mv_string("entry-point")), 
            };

            // Executable 
            //   file :: string 
            //   entry-point :: symbol
            //   dependencies :: dependency list
            PropSet* props = make_prop_set(3, &ra);
            add_expr_prop(mv_string("file"), &filename.val, props);
            add_expr_prop(mv_string("entry-point"), &entry_point.val, props);
            add_expr_array_prop(mv_string("depends-on"), &dependencies.val, props);

            for (size_t i = 1; i < raw.branch.nodes.len; i++) {
                parse_prop(raw.branch.nodes.data[i], props, executable_checks, host_data, pool, point, region);
            }

            check_props(props, executable_checks, raw.range, point, region);

            /**
             * End parsing, set expressions
             */
            set_expr_map_elt(fields, 0, dependencies, pool);
            set_expr_map_elt(fields, 1, filename, pool);
            set_expr_map_elt(fields, 2, entry_point, pool);
        } else if (string_cmp(mv_string("target"), symbol_to_string(head, &ra)) == 0) {
            bool executable_checks[] = {false, false, false};
            ExprRef ctor_ref = new_expr(pool);
            ExprRef ttype_ref = new_expr(pool);
            ExprSlice members = new_expr_slice(1, pool);

            *out = (Expr) {
                .type = EApp,
                .app = {
                    .fn = ctor_ref,
                    .args = members,
                }
            };

            Expr ctor = {
                .type = ECtor,
                .ctor = {
                    .type = {.type = Some, .val = ttype_ref},
                    .name = string_to_name(mv_string("general")),
                },
            };
            set_expr(ctor_ref, ctor, pool);

            Expr ttype = {
                .type = EVal,
                .value = types.target_type,
            };
            set_expr(ttype_ref, ttype, pool);

            NameExprMap fields = new_expr_map(3, pool);
            Expr lib_record = {
                .type = ERecord,
                .record.fields = fields,
            };
            set_expr_elt(members, 0, lib_record, pool);
            NameExprCell dependencies = {
                .name = string_to_name(mv_string("depends-on")), 
            };
            NameExprCell provides = {
                .name = string_to_name(mv_string("provides")), 
            };
            NameExprCell run = {
                .name = string_to_name(mv_string("run")), 
            };
            // provides :: string list 
            // entry-point :: symbol
            // dependencies :: symbol list

            PropSet* props = make_prop_set(3, &ra);
            add_expr_array_prop(mv_string("depends-on"), &provides.val, props);
            add_expr_array_prop(mv_string("provides"), &dependencies.val, props);
            add_expr_prop(mv_string("run"), &run.val, props);

            for (size_t i = 1; i < raw.branch.nodes.len; i++) {
                parse_prop(raw.branch.nodes.data[i], props, executable_checks, host_data, pool, point, region);
            }

            check_props(props, executable_checks, raw.range, point, region);

            /**
             * End parsing, set expressions
             */
            set_expr_map_elt(fields, 0, dependencies, pool);
            set_expr_map_elt(fields, 1, provides, pool);
            set_expr_map_elt(fields, 2, run, pool);
        } else {
            PicoError err = {
                .range = raw.range,
                .message = mv_cstr_doc("Unrecognized target header.", &ra),
            };
            throw_pi_error(point, err);
        }
        return;
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
            bool package_checks[] = {false, false , false, false};

            // Package 
            // - package-name :: symbol
            // - package-dependencies :: symbol list
            PropSet* props = make_prop_set(4, &ra);
            add_name_prop(mv_string("name"), &project->package.name, props);
            add_string_prop(mv_string("build-file"), &project->package.build_file, props);
            add_name_array_prop(mv_string("depends-on"), &project->package.dependencies, props);
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
