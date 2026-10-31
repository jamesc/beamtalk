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
    let messages = error_diagnostic_messages(&response);
    assert!(
        messages
            .iter()
            .any(|msg| msg.contains("declared as Integer")),
        "the failure must be the intended Integer mismatch, not a spurious alias/FFI diagnostic: {messages:?}"
    );
}

/// BT-4322: a method patched into a project class typechecks with the same
/// project scope as a whole-file `compile` — the merged class sees the
/// project's aliases and FFI specs.
#[test]
fn compile_method_patch_sees_project_scope() {
    let (_dir, state_path) = write_parity_project();
    let class_source = "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         typed Value subclass: ParityState\n\
         \x20 field: scheme :: Scheme = #http\n\
         \x20 field: addr :: DepAddr = #local\n";
    let request = Map::from([
        (atom("command"), atom("compile_method")),
        (atom("class_source"), binary(class_source)),
        (
            atom("method_source"),
            binary("monotonicTime -> Integer => Erlang erlang monotonic_time"),
        ),
        (atom("is_class_method"), atom("false")),
        (
            atom("source_path"),
            binary(state_path.to_string_lossy().as_ref()),
        ),
    ]);
    let response = handle_compile_method(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response: {response:?}");
    };
    assert_eq!(
        map_get(m, "status"),
        Some(&atom("ok")),
        "patched method must compile: {response:?}"
    );
    let warnings = compile_warnings(&response);
    assert!(
        !warnings.iter().any(|w| w.contains("untyped FFI")),
        "patched zero-arg FFI with a registry spec must not warn: {warnings:?}"
    );
    assert!(
        !warnings.iter().any(|w| w.contains("declared as Scheme")),
        "merged class must see the cross-file alias: {warnings:?}"
    );
    assert!(
        !warnings.iter().any(|w| w.contains("declared as DepAddr")),
        "merged class must see the dependency alias: {warnings:?}"
    );
}

fn error_diagnostic_messages(response: &Term) -> Vec<String> {
    let Term::Map(m) = response else {
        panic!("Expected map response: {response:?}");
    };
    let Some(Term::List(diagnostics)) = map_get(m, "diagnostics") else {
        panic!("Expected diagnostics list: {response:?}");
    };
    diagnostics
        .elements
        .iter()
        .map(|diag| {
            let Term::Map(dm) = diag else {
                panic!("Expected diagnostic map, got {diag:?}");
            };
            map_get(dm, "message")
                .and_then(term_to_string)
                .unwrap_or_default()
        })
        .collect()
}

/// BT-4322 session-wins merge: a session alias keeps its declaration over a
/// same-named project alias (REPL "current turn wins") instead of colliding
/// as a hard error the moment the project reloads. The session's narrower
/// `Scheme` still rejects the `#http` default — the project's `Scheme`
/// would accept it, so the error proves the session declaration (not the
/// project's) was used.
#[test]
fn session_alias_wins_over_project_alias() {
    let (_dir, state_path) = write_parity_project();
    let source = fs::read_to_string(&state_path).expect("read fixture state file");
    let mut request = load_like_compile_request(&source, &state_path);
    request.map.insert(
        atom("known_type_aliases"),
        Term::from(List::from(vec![binary("type Scheme = Integer")])),
    );

    let response = handle_compile(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response: {response:?}");
    };
    assert_eq!(
        map_get(m, "status"),
        Some(&atom("error")),
        "session-narrowed Scheme must still mismatch #http: {response:?}"
    );
    let messages = error_diagnostic_messages(&response);
    assert!(
        messages
            .iter()
            .any(|msg| msg.contains("declared as Scheme")),
        "the session declaration must win (the project's Scheme would accept #http): {messages:?}"
    );
    assert!(
        !messages.iter().any(|msg| msg.contains("collides")),
        "no alias/alias collision may fire: {messages:?}"
    );
}

/// Files with no enclosing manifest — and stdlib files even under a
/// manifest — keep the legacy behaviour: no project scope. An unknown alias
/// still surfaces (nothing is swallowed), but with no project table it stays
/// a warning and the compile succeeds.
#[test]
fn manifest_less_and_stdlib_files_keep_legacy_scope() {
    let stray_source = "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         typed Value subclass: Stray\n\
         \x20 field: s :: Scheme = #http\n";

    // No beamtalk.toml above a fresh tempdir: legacy scope.
    let bare = tempfile::tempdir().expect("bare tempdir");
    let stray_path = bare.path().join("stray.bt");
    fs::write(&stray_path, stray_source).expect("write stray file");
    let response = handle_compile(&load_like_compile_request(stray_source, &stray_path));
    let Term::Map(ref m) = response else {
        panic!("Expected map response: {response:?}");
    };
    assert_eq!(
        map_get(m, "status"),
        Some(&atom("ok")),
        "manifest-less file must compile: {response:?}"
    );
    let warnings = compile_warnings(&response);
    assert!(
        warnings.iter().any(|w| w.contains("declared as Scheme")),
        "unknown alias must still surface without project scope: {warnings:?}"
    );

    // Stdlib mode under a real manifest: legacy scope too.
    let (_dir, state_path) = write_parity_project();
    let source = fs::read_to_string(&state_path).expect("read fixture state file");
    let mut request = load_like_compile_request(&source, &state_path);
    request.map.insert(atom("stdlib_mode"), atom("true"));
    let response = handle_compile(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response: {response:?}");
    };
    assert_eq!(
        map_get(m, "status"),
        Some(&atom("ok")),
        "stdlib file must compile: {response:?}"
    );
    let warnings = compile_warnings(&response);
    assert!(
        warnings.iter().any(|w| w.contains("declared as Scheme")),
        "stdlib file must not see project aliases: {warnings:?}"
    );
}

/// Editing a project alias file re-resolves on the next compile: the memo
/// is keyed by an exact path→mtime fingerprint, so the second compile sees
/// the new expansion rather than a stale cached one.
#[test]
fn edited_alias_file_resolves_on_next_compile() {
    let (dir, state_path) = write_parity_project();
    let source = fs::read_to_string(&state_path).expect("read fixture state file");
    let first = handle_compile(&load_like_compile_request(&source, &state_path));
    assert_eq!(
        response_status(&first).as_deref(),
        Some("ok"),
        "baseline must compile: {first:?}"
    );
    assert!(
        !compile_warnings(&first)
            .iter()
            .any(|w| w.contains("declared as Scheme")),
        "baseline must see Scheme: {:#?}",
        compile_warnings(&first)
    );

    // Narrow the alias so `#http` no longer matches.
    fs::write(
        dir.path().join("src/parity_config.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         type Scheme = #https\n\
         \n\
         Value subclass: ParityConfig\n\
         \x20 defaultScheme -> Symbol => #http\n",
    )
    .expect("rewrite fixture alias file");
    let second = handle_compile(&load_like_compile_request(&source, &state_path));
    let Term::Map(ref m) = second else {
        panic!("Expected map response: {second:?}");
    };
    // Narrowing `Scheme` to `#https` turns the `#http` default into a genuine
    // mismatch, which the project's `type = "error"` escalates.
    assert_eq!(
        map_get(m, "status"),
        Some(&atom("error")),
        "edited alias must reject the old default: {second:?}"
    );
    let messages = error_diagnostic_messages(&second);
    assert!(
        messages
            .iter()
            .any(|msg| msg.contains("declared as Scheme (#https)")),
        "edited alias expansion must be visible on recompile: {messages:?}"
    );
}
