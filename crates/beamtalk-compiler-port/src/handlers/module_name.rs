// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `module_name_for_path`: the parse-free path-to-module-name command.

use beamtalk_core::semantic_analysis::{ModuleRootKind, module_name_for_path};
use beamtalk_etf::{atom, binary_from_str as binary, map_get, term_to_string};
use eetf::{Map, Term};

use crate::respond::error_response;

/// Handle a `module_name_for_path` request (BT-4868).
///
/// The one place the REPL loader asks "what is this file's module name?".
/// It is a thin wrapper over `beamtalk_core`'s `module_name_for_path`, the
/// same function `beamtalk test` calls in-process, so the loader, the build
/// and the test runner cannot disagree. No source is read or parsed.
///
/// Request fields:
/// - `path` (binary): the file path. For `root_kind` `src` / `test` it is
///   relative to that root (extension included, `/`-joined); for `none` only
///   its file name is used.
/// - `package_name` (binary): the project's package name (ignored for `none`)
/// - `root_kind` (binary): `src` | `test` | `none`
///
/// Response: `#{status => ok, module_name => <<"bt@web@test@foo">>}`, or
/// `#{status => error, reason => invalid_path_segment, message => ...}` for a
/// segment outside `[A-Za-z0-9_]`.
pub(crate) fn handle_module_name_for_path(request: &Map) -> Term {
    let Some(path) = map_get(request, "path").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'path' field".to_string()]);
    };
    let Some(package_name) = map_get(request, "package_name").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'package_name' field".to_string()]);
    };
    let Some(root_kind) = map_get(request, "root_kind")
        .and_then(term_to_string)
        .and_then(|s| ModuleRootKind::parse(&s))
    else {
        return error_response(&[
            "Missing or invalid 'root_kind' field (expected src, test or none)".to_string(),
        ]);
    };

    match module_name_for_path(camino::Utf8Path::new(&path), &package_name, root_kind) {
        Ok(module_name) => Term::from(Map::from([
            (atom("status"), atom("ok")),
            (atom("module_name"), binary(&module_name)),
        ])),
        Err(err) => Term::from(Map::from([
            (atom("status"), atom("error")),
            (atom("reason"), atom("invalid_path_segment")),
            (atom("message"), binary(&err.to_string())),
        ])),
    }
}
