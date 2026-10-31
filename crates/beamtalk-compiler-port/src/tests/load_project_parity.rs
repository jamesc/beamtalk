// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4322: `compile` must typecheck a file with its own project's context.
//!
//! The workspace `load_project`/`load_file` path compiles one file per
//! `compile` request, while `beamtalk build` typechecks each file with the
//! whole package in scope: cross-file `type` aliases, the native Erlang FFI
//! registry (`_build/type_cache/`), and the project's own `[diagnostics]`
//! table. A `compile` that sees none of that reports diagnostics the build
//! never does — a zero-arg `Erlang erlang monotonic_time` with a declared
//! `-> Integer` infers `Dynamic (untyped FFI)`, and a `field: x :: Scheme =
//! #http` default mismatches when `Scheme` lives in another file — and under
//! the project's own `type = "error"` those warnings fail the load of a
//! package the build accepts.
//!
//! These tests build a miniature buccaneer-shaped project on disk (manifest
//! with `[diagnostics] type = "error"`, a cross-file alias, a dependency
//! alias, and a seeded `_build/type_cache/` Erlang entry) and drive
//! `handle_compile` with the file's real `source_path`, exactly as
//! `beamtalk_repl_compiler:compile_file/5` does on the load path. They fail
//! while `compile` derives its context from the port process's cwd instead
//! of the compiled file's project.

use super::*;
use std::fs;

/// Lays out the fixture project and returns the temp dir (deleted on drop)
/// plus the absolute path of the file under test:
///
/// ```text
/// <root>/beamtalk.toml            ([diagnostics] type/type-annotation = "error")
/// <root>/src/parity_config.bt     (type Scheme = #http | #https, plus a class)
/// <root>/src/parity_state.bt      (typed class: Scheme default + zero-arg FFI)
/// <root>/src/parity_bad.bt        (typed class: genuine Integer/"oops" mismatch)
/// <root>/_build/deps/fakedep/beamtalk.toml
/// <root>/_build/deps/fakedep/src/dep_types.bt   (type DepAddr = #local | #remote)
/// <root>/_build/type_cache/erlang_<hash>.json    (seeded monotonic_time/0 spec)
/// ```
fn write_parity_project() -> (tempfile::TempDir, std::path::PathBuf) {
    let dir = tempfile::tempdir().expect("tempdir for parity project");
    let root = dir.path();
    fs::create_dir_all(root.join("src")).expect("fixture src/");
    fs::create_dir_all(root.join("_build/deps/fakedep/src")).expect("fixture dep src/");
    fs::create_dir_all(root.join("_build/type_cache")).expect("fixture type_cache/");

    fs::write(
        root.join("beamtalk.toml"),
        "[package]\nname = \"parityproj\"\nversion = \"0.1.0\"\n\n[diagnostics]\ntype = \"error\"\ntype-annotation = \"error\"\n",
    )
    .expect("fixture beamtalk.toml");
    fs::write(
        root.join("src/parity_config.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         type Scheme = #http | #https\n\
         \n\
         Value subclass: ParityConfig\n\
         \x20 defaultScheme -> Symbol => #http\n",
    )
    .expect("fixture parity_config.bt");
    let state_path = root.join("src/parity_state.bt");
    fs::write(
        &state_path,
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         typed Value subclass: ParityState\n\
         \x20 field: scheme :: Scheme = #http\n\
         \x20 field: addr :: DepAddr = #local\n\
         \n\
         \x20 monotonicTime -> Integer => Erlang erlang monotonic_time\n",
    )
    .expect("fixture parity_state.bt");
    fs::write(
        root.join("src/parity_bad.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         typed Value subclass: ParityBad\n\
         \x20 field: n :: Integer = \"oops\"\n",
    )
    .expect("fixture parity_bad.bt");

    fs::write(
        root.join("_build/deps/fakedep/beamtalk.toml"),
        "[package]\nname = \"fakedep\"\nversion = \"0.1.0\"\n",
    )
    .expect("fixture dep beamtalk.toml");
    fs::write(
        root.join("_build/deps/fakedep/src/dep_types.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         type DepAddr = #local | #remote\n\
         \n\
         Value subclass: DepTypes\n\
         \x20 defaultAddr -> Symbol => #local\n",
    )
    .expect("fixture dep_types.bt");

    // Seeded OTP spec entry for `erlang:monotonic_time/0 -> integer`, in the
    // exact shape `beamtalk lint`'s fixture seeding uses: replay
    // (`load_type_cache_registry`) accepts entries with no `beam_path` as
    // fresh, and groups them by the module stem, so the filename hash only
    // needs to be 16 hex digits. The stamp must match the running compiler
    // (see `current_spec_mapping_stamp`'s doc).
    fs::write(
        root.join("_build/type_cache/erlang_0123456789abcdef.json"),
        format!(
            r#"{{"beam_mtime_secs":0,"beam_mtime_nanos":0,"mapping_stamp":"{}","specs_line":"beamtalk-specs-module:erlang:[#{{name => <<\"monotonic_time\">>,arity => 0,params => [],return_type => <<\"Integer\">>}}]"}}"#,
            beamtalk_core::ffi_type_specs::current_spec_mapping_stamp()
        ),
    )
    .expect("fixture type_cache entry");

    (dir, state_path)
}

/// `compile` request shaped like the workspace load path sends it: the file's
/// source plus its real on-disk `source_path` (which is what lets the port
/// locate the file's project).
fn load_like_compile_request(source: &str, source_path: &std::path::Path) -> Map {
    Map::from([
        (atom("command"), atom("compile")),
        (atom("source"), binary(source)),
        (
            atom("source_path"),
            binary(source_path.to_string_lossy().as_ref()),
        ),
    ])
}

fn compile_warnings(response: &Term) -> Vec<String> {
    let Term::Map(m) = response else {
        panic!("Expected map response: {response:?}");
    };
    let Some(Term::List(warnings)) = map_get(m, "warnings") else {
        panic!("Expected warnings list: {response:?}");
    };
    warnings
        .elements
        .iter()
        .filter_map(term_to_string)
        .collect()
}

/// BT-4322 AC 1-3: a project file using a cross-file alias (`Scheme`), a
/// dependency alias (`DepAddr`), and a zero-arg FFI call with a declared
/// return type compiles with no diagnostics — the same result `beamtalk
/// build` gives for the same package.
#[test]
fn compile_file_sees_project_aliases_and_ffi_specs() {
    let (_dir, state_path) = write_parity_project();
    let source = fs::read_to_string(&state_path).expect("read fixture state file");

    let response = handle_compile(&load_like_compile_request(&source, &state_path));
    let Term::Map(ref m) = response else {
        panic!("Expected map response: {response:?}");
    };
    assert_eq!(
        map_get(m, "status"),
        Some(&atom("ok")),
        "project file must compile: {response:?}"
    );
    let warnings = compile_warnings(&response);
    assert!(
        !warnings.iter().any(|w| w.contains("untyped FFI")),
        "zero-arg FFI with a registry spec must not warn: {warnings:?}"
    );
    assert!(
        !warnings.iter().any(|w| w.contains("declared as Scheme")),
        "cross-file alias default must not mismatch: {warnings:?}"
    );
    assert!(
        !warnings.iter().any(|w| w.contains("declared as DepAddr")),
        "dependency alias default must not mismatch: {warnings:?}"
    );
}

/// BT-4322 severity parity: the project's own `[diagnostics]` table applies
/// to its files, so a genuine type error fails the `compile` the way it
/// fails `beamtalk build` — even when the port process's cwd has no table.
#[test]
fn compile_file_applies_project_diagnostics_table() {
    let (dir, _state_path) = write_parity_project();
    let bad_path = dir.path().join("src/parity_bad.bt");
    let source = fs::read_to_string(&bad_path).expect("read fixture bad file");

    let response = handle_compile(&load_like_compile_request(&source, &bad_path));
    let Term::Map(ref m) = response else {
        panic!("Expected map response: {response:?}");
    };
    assert_eq!(
        map_get(m, "status"),
        Some(&atom("error")),
        "a genuine type mismatch must fail under the project's type = \"error\": {response:?}"
    );
}
