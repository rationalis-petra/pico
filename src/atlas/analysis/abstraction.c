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
    AtAbsCallbackData cb = *(AtAbsCallbackData*)cb_data;

    ExprRef path_expr = new_expr(pool);
    set_expr(path_expr, (Expr){.type = EString, .string = cb.path}, pool);
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
                .value = cb.target_type,
            };
            set_expr(ttype_ref, ttype, pool);

            NameExprMap fields = new_expr_map(4, pool);
            Expr lib_record = {
                .type = ERecord,
                .record.fields = fields,
            };
            set_expr_elt(members, 0, lib_record, pool);
            NameExprCell path = {
                .name = string_to_name(mv_string("path")), 
                .val = path_expr,
            };
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
            add_expr_prop(mv_string("depends-on"), &dependencies.val, props);
            add_expr_option_prop(mv_string("file"), &filename.val, props);
            add_expr_array_prop(mv_string("submodules"), &submodules.val, props);

            for (size_t i = 1; i < raw.branch.nodes.len; i++) {
                parse_prop(raw.branch.nodes.data[i], props, library_checks, host_data, pool, point, region);
            }

            check_props(props, library_checks, raw.range, point, region);

            /**
             * End parsing, set expressions
             */
            set_expr_map_elt(fields, 0, path, pool);
            set_expr_map_elt(fields, 1, dependencies, pool);
            set_expr_map_elt(fields, 2, filename, pool);
            set_expr_map_elt(fields, 3, submodules, pool);
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
                .value = cb.target_type,
            };
            set_expr(ttype_ref, ttype, pool);

            NameExprMap fields = new_expr_map(4, pool);
            Expr lib_record = {
                .type = ERecord,
                .record.fields = fields,
            };
            set_expr_elt(members, 0, lib_record, pool);
            NameExprCell path = {
                .name = string_to_name(mv_string("path")), 
                .val = path_expr,
            };
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
            add_expr_prop(mv_string("depends-on"), &dependencies.val, props);

            for (size_t i = 1; i < raw.branch.nodes.len; i++) {
                parse_prop(raw.branch.nodes.data[i], props, executable_checks, host_data, pool, point, region);
            }

            check_props(props, executable_checks, raw.range, point, region);

            /**
             * End parsing, set expressions
             */
            set_expr_map_elt(fields, 0, path, pool);
            set_expr_map_elt(fields, 1, dependencies, pool);
            set_expr_map_elt(fields, 2, filename, pool);
            set_expr_map_elt(fields, 3, entry_point, pool);
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
                    .name = string_to_name(mv_string("generic")),
                },
            };
            set_expr(ctor_ref, ctor, pool);

            Expr ttype = {
                .type = EVal,
                .value = cb.target_type,
            };
            set_expr(ttype_ref, ttype, pool);

            NameExprMap fields = new_expr_map(4, pool);
            Expr lib_record = {
                .type = ERecord,
                .record.fields = fields,
            };
            set_expr_elt(members, 0, lib_record, pool);
            NameExprCell path = {
                .name = string_to_name(mv_string("path")), 
                .val = path_expr,
            };
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
            add_expr_prop(mv_string("depends-on"), &dependencies.val, props);
            add_expr_array_prop(mv_string("provides"), &provides.val, props);
            add_expr_prop(mv_string("run"), &run.val, props);

            for (size_t i = 1; i < raw.branch.nodes.len; i++) {
                parse_prop(raw.branch.nodes.data[i], props, executable_checks, host_data, pool, point, region);
            }

            check_props(props, executable_checks, raw.range, point, region);

            /**
             * End parsing, set expressions
             */
            set_expr_map_elt(fields, 0, path, pool);
            set_expr_map_elt(fields, 1, dependencies, pool);
            set_expr_map_elt(fields, 2, provides, pool);
            set_expr_map_elt(fields, 3, run, pool);
        } else if (string_cmp(mv_string("file"), symbol_to_string(head, &ra)) == 0) {
            // Convert (file str) to 
            // (:file path str)
            if (raw.branch.nodes.len != 2) {
                PicoError err = {
                    .range = raw.range,
                    .message = mv_cstr_doc("File targets require precisely 1 argument.", &ra),
                };
                throw_pi_error(point, err);
            }
            ExprRef ctor = new_expr(pool);
            ExprSlice args = new_expr_slice(1, pool);
            *out = (Expr) {
                .type = EApp,
                .app.fn = ctor,
                .app.args = args,
            };
            set_expr(ctor,
                     (Expr){
                         .type = ECtor,
                         .ctor.name = string_to_name(mv_string("file")),
                     },
                     pool);
            NameExprMap fields = new_expr_map(2, pool);
            Expr file_record = {
                .type = ERecord,
                .record.fields = fields,
            };
            set_expr_elt(args, 0, file_record, pool);

            ExprRef path_expr = new_expr(pool);
            set_expr(path_expr, (Expr){.type = EString, .string = cb.path}, pool);

            NameExprCell path = {
                .name = string_to_name(mv_string("path")), 
                .val = path_expr,
            };
            set_expr_map_elt(fields, 0, path, pool);
            NameExprCell dependencies = {
                .name = string_to_name(mv_string("name")), 
                .val = abstract_rune_expr(raw.branch.nodes.data[1], host_data, pool, region, point),
            };
            set_expr_map_elt(fields, 1, dependencies, pool);
        } else if (string_cmp(mv_string("shell"), symbol_to_string(head, &ra)) == 0) {
            // Convert (shell str str str ...) to 
            // :shell (list str str str)
            ExprRef ctor = new_expr(pool);
            ExprSlice args = new_expr_slice(1, pool);
            *out = (Expr) {
                .type = EApp,
                .app.fn = ctor,
                .app.args = args,
            };
            set_expr(ctor,
                     (Expr){
                         .type = ECtor,
                         .ctor.name = string_to_name(mv_string("shell")),
                     },
                     pool);
            ExprSlice slice = new_expr_slice(raw.branch.nodes.len - 1, pool);
            for (size_t i = 1; i < raw.branch.nodes.len; i++) {
                Expr expr;
                abstract_rune_to(raw.branch.nodes.data[i], host_data, pool, region, point, &expr);
                set_expr_elt(slice, i - 1, expr, pool);
            }

            set_expr_elt(args,
                         0,
                         (Expr){.type = EList, .list = slice},
                         pool);

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

Def abstract_atlas_def(RawTree raw, AtAbsCallbackData cb_data, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point) {
    Name raw_names[] = {
        string_to_name(mv_string("library")),
        string_to_name(mv_string("executable")),
        string_to_name(mv_string("target")),
        string_to_name(mv_string("file")),
        string_to_name(mv_string("shell")),
    }; 
    NameArray names = {
        .data = raw_names,
        .len = sizeof(raw_names) / sizeof(Name),
        .size = sizeof(raw_names) / sizeof(Name),
    };

    HostCallbackData hdata = {
        .abstract = abstract_atlas,
        .names = names,
        .closure_data = &cb_data,
    };

    return abstract_rune_def(raw, hdata, pool, region, point);
}

bool is_require(RawTree raw) {
    if (raw.type != RawBranch) return false;
    if (raw.branch.hint != HExpression) return false;
    if (raw.branch.nodes.len < 1) return false;
    RawTree head = raw.branch.nodes.data[0];
    return eq_symbol(&head, string_to_symbol(mv_string("require")));
}

Toplevel abstract_atlas_toplevel(RawTree raw, AtAbsCallbackData cb_data, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point) {
    if (is_require(raw)) { 
        Allocator ra = ra_to_gpa(region);
        StringArray out = mk_string_array(raw.branch.nodes.len - 1, &ra);
        for (size_t i = 1; i < raw.branch.nodes.len; i++) {
            RawTree rawname = raw.branch.nodes.data[i];
            if (rawname.type != RawAtom || rawname.atom.type != AString) {
                PicoError err = {
                    .range = raw.range,
                    .message = mv_cstr_doc("The 'require' expects only strings as arguments.", &ra),
                };
                throw_pi_error(point, err);
            }
            push_string(rawname.atom.string, &out);
        }
        return (Toplevel) {
            .type = TLRequire,
            .requires = out,
        };
    } else {
        return (Toplevel) {
            .type = TLDef,
            .def = abstract_atlas_def(raw, cb_data, pool, region, point),
        };
    }
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
