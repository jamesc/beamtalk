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
/// <root>/beamtalk.toml            ([dependencies] fakedep; [diagnostics] type/type-annotation = "error")
/// <root>/src/parity_config.bt     (type Scheme = #http | #https, plus a class)
/// <root>/src/parity_state.bt      (typed class: Scheme default + zero-arg FFI)
/// <root>/src/parity_bad.bt        (typed class: genuine Integer/"oops" mismatch)
/// <root>/_build/deps/.dep-graph.json            (resolved graph: fakedep)
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
        "[package]\nname = \"parityproj\"\nversion = \"0.1.0\"\n\n[dependencies]\nfakedep = { git = \"https://example.test/fakedep\", tag = \"v0.1.0\" }\n\n[diagnostics]\ntype = \"error\"\ntype-annotation = \"error\"\n",
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
    // The build's resolved dependency graph (BT-4378): the port reads the
    // dependency set from here, not from a listing of `_build/deps/`.
    write_dep_graph_snapshot(root, &["fakedep"]);

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

/// Record `deps` (each with no dependencies of its own) as the project's
/// resolved dependency graph, in the schema `beamtalk build` writes after a
/// resolve.
fn write_dep_graph_snapshot(root: &std::path::Path, deps: &[&str]) {
    let graph: Vec<(&str, &[&str])> = deps.iter().map(|name| (*name, &[][..])).collect();
    write_dep_graph_snapshot_with_edges(root, &graph);
}

/// Record `graph` — each dependency with its own direct dependencies — as
/// the project's resolved dependency graph.
fn write_dep_graph_snapshot_with_edges(root: &std::path::Path, graph: &[(&str, &[&str])]) {
    use beamtalk_core::compilation::dep_graph::{
        DEP_GRAPH_SNAPSHOT_FILE, DepGraphEntry, DepGraphSnapshot,
    };
    let snapshot = DepGraphSnapshot::new(
        graph
            .iter()
            .map(|(name, dependencies)| DepGraphEntry {
                name: (*name).to_string(),
                via: Vec::new(),
                dependencies: dependencies.iter().map(|d| (*d).to_string()).collect(),
                source: format!("git: https://example.test/{name} (tag: v0.1.0)"),
            })
            .collect(),
    );
    fs::create_dir_all(root.join("_build/deps")).expect("fixture _build/deps/");
    fs::write(
        root.join("_build/deps").join(DEP_GRAPH_SNAPSHOT_FILE),
        serde_json::to_string_pretty(&snapshot).expect("serialise snapshot"),
    )
    .expect("fixture dependency-graph snapshot");
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
    let warnings = response_warning_messages(&response);
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
    let warnings = response_warning_messages(&response);
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
    let warnings = response_warning_messages(&response);
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
    let warnings = response_warning_messages(&response);
    assert!(
        warnings.iter().any(|w| w.contains("declared as Scheme")),
        "stdlib file must not see project aliases: {warnings:?}"
    );
}

/// Compile the fixture's state file and assert the baseline sees `Scheme`
/// (so a later compile's outcome reflects an edit, not a broken fixture).
/// Returns the state file's source for the recompile.
fn compile_baseline_state_file(state_path: &std::path::Path) -> String {
    let source = fs::read_to_string(state_path).expect("read fixture state file");
    let first = handle_compile(&load_like_compile_request(&source, state_path));
    assert_eq!(
        response_status(&first).as_deref(),
        Some("ok"),
        "baseline must compile: {first:?}"
    );
    assert!(
        !response_warning_messages(&first)
            .iter()
            .any(|w| w.contains("declared as Scheme")),
        "baseline must see Scheme: {:#?}",
        response_warning_messages(&first)
    );
    source
}

/// Rewrite the fixture's alias file, narrowing `Scheme` to `#https` so the
/// state file's `#http` default no longer matches. The new content is
/// shorter than the original, so the file's length changes. Returns the
/// alias file's path.
fn narrow_scheme_alias(root: &std::path::Path) -> std::path::PathBuf {
    let alias_path = root.join("src/parity_config.bt");
    fs::write(
        &alias_path,
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         type Scheme = #https\n\
         \n\
         Value subclass: ParityConfig\n\
         \x20 defaultScheme -> Symbol => #http\n",
    )
    .expect("rewrite fixture alias file");
    alias_path
}

/// Set `path`'s mtime to `time`.
fn set_mtime(path: &std::path::Path, time: std::time::SystemTime) {
    fs::OpenOptions::new()
        .write(true)
        .open(path)
        .expect("open file to set mtime")
        .set_modified(time)
        .expect("set mtime");
}

/// Assert a recompile after [`narrow_scheme_alias`] saw the narrowed alias:
/// the `#http` default is now a genuine mismatch, which the project's
/// `type = "error"` escalates.
fn assert_sees_narrowed_alias(second: &Term) {
    let Term::Map(m) = second else {
        panic!("Expected map response: {second:?}");
    };
    assert_eq!(
        map_get(m, "status"),
        Some(&atom("error")),
        "edited alias must reject the old default: {second:?}"
    );
    let messages = error_diagnostic_messages(second);
    assert!(
        messages
            .iter()
            .any(|msg| msg.contains("declared as Scheme (#https)")),
        "edited alias expansion must be visible on recompile: {messages:?}"
    );
}

/// Editing a project alias file re-resolves on the next compile: the memo
/// is keyed by an exact path→(mtime, length) fingerprint, so the second
/// compile sees the new expansion rather than a stale cached one.
#[test]
fn edited_alias_file_resolves_on_next_compile() {
    let (dir, state_path) = write_parity_project();
    let source = compile_baseline_state_file(&state_path);

    // Bump the mtime forward explicitly rather than relying on wall-clock
    // time: on NTFS both writes can land in one ~15.6 ms tick, leaving the
    // mtime unchanged (BT-4365). This test covers the ordinary mtime-change
    // path; `same_mtime_alias_edit_resolves_on_next_compile` covers the
    // same-mtime path deterministically.
    let alias_path = narrow_scheme_alias(dir.path());
    let bumped = fs::metadata(&alias_path)
        .and_then(|m| m.modified())
        .expect("read rewritten alias mtime")
        + std::time::Duration::from_secs(5);
    set_mtime(&alias_path, bumped);

    let second = handle_compile(&load_like_compile_request(&source, &state_path));
    assert_sees_narrowed_alias(&second);
}

/// BT-4365: an edit that keeps the alias file's mtime (two writes within one
/// coarse filesystem tick, or `cp -p`/`rsync -t`) still re-resolves, because
/// the fingerprint also records the file length and the edit changes it.
#[test]
fn same_mtime_alias_edit_resolves_on_next_compile() {
    let (dir, state_path) = write_parity_project();
    let alias_path = dir.path().join("src/parity_config.bt");
    let original = fs::metadata(&alias_path).expect("stat original alias");
    let original_mtime = original.modified().expect("read original alias mtime");
    let source = compile_baseline_state_file(&state_path);

    narrow_scheme_alias(dir.path());
    set_mtime(&alias_path, original_mtime);
    let edited = fs::metadata(&alias_path).expect("stat edited alias");
    assert_eq!(
        edited.modified().expect("read edited alias mtime"),
        original_mtime,
        "fixture must restore the original mtime"
    );
    assert_ne!(
        edited.len(),
        original.len(),
        "fixture edit must change the file length"
    );

    let second = handle_compile(&load_like_compile_request(&source, &state_path));
    assert_sees_narrowed_alias(&second);
}

/// Copy the shared `tests/parity/projects/stubbed_ffi/` fixture — the same
/// package `cli_build`'s BT-4378 test builds — into a fresh tempdir, so a
/// test can edit it (and the port's per-root memo sees a new root).
fn stubbed_ffi_project() -> tempfile::TempDir {
    let dir = tempfile::tempdir().expect("tempdir for stubbed_ffi project");
    beamtalk_workspace::copy_tree(
        &beamtalk_workspace::cargo_workspace_root!().join("tests/parity/projects/stubbed_ffi"),
        dir.path(),
    )
    .expect("copy stubbed_ffi fixture");
    dir
}

/// BT-4378 AC: a package that types its FFI through `stubs/*.bt`, under
/// `[diagnostics] type = "error"`, gets the verdict `beamtalk build` gives
/// it (`cli_build::build_types_ffi_through_project_stubs_like_load_project`
/// builds the same fixture): accepted with the stub, rejected for the
/// untyped FFI call once the stub is gone. The fixture was never built, so
/// the project-local stub tier is the only registry input.
#[test]
fn compile_file_types_ffi_through_project_stubs_like_build() {
    let project = stubbed_ffi_project();
    let path = project.path().join("src/stubbed_answer.bt");
    let source = fs::read_to_string(&path).expect("read fixture source");

    let response = handle_compile(&load_like_compile_request(&source, &path));
    assert_eq!(
        response_status(&response).as_deref(),
        Some("ok"),
        "the stub types the FFI call, as in `beamtalk build`: {response:?}"
    );
    assert!(
        !response_warning_messages(&response)
            .iter()
            .any(|w| w.contains("untyped FFI")),
        "{response:?}"
    );

    // Without the stub the call is untyped, which `type = "error"` rejects
    // — and the memo must notice the stub tier changed.
    fs::remove_dir_all(project.path().join("stubs")).expect("remove stubs/");
    let response = handle_compile(&load_like_compile_request(&source, &path));
    assert_eq!(
        response_status(&response).as_deref(),
        Some("error"),
        "an untyped FFI call fails under type = \"error\": {response:?}"
    );
    assert!(
        error_diagnostic_messages(&response)
            .iter()
            .any(|msg| msg.contains("untyped FFI")),
        "{response:?}"
    );
}

/// The parity project's manifest with its `fakedep` dependency removed.
const MANIFEST_WITHOUT_DEPS: &str = "[package]\nname = \"parityproj\"\nversion = \"0.1.0\"\n\n[diagnostics]\ntype = \"error\"\ntype-annotation = \"error\"\n";

/// BT-4378 AC: a dependency removed from `beamtalk.toml` stops contributing
/// aliases, even though its checkout still sits in `_build/deps/` and the
/// last resolve's graph snapshot still lists it.
#[test]
fn removed_dependency_aliases_do_not_resolve() {
    let (dir, state_path) = write_parity_project();
    let source = fs::read_to_string(&state_path).expect("read fixture state file");
    let first = handle_compile(&load_like_compile_request(&source, &state_path));
    assert!(
        !response_warning_messages(&first)
            .iter()
            .any(|w| w.contains("declared as DepAddr")),
        "baseline must see the dependency alias: {first:?}"
    );

    fs::write(dir.path().join("beamtalk.toml"), MANIFEST_WITHOUT_DEPS)
        .expect("drop fakedep from the manifest");
    let second = handle_compile(&load_like_compile_request(&source, &state_path));
    assert_eq!(
        response_status(&second).as_deref(),
        Some("error"),
        "an undeclared dependency's alias must not resolve: {second:?}"
    );
    assert!(
        error_diagnostic_messages(&second)
            .iter()
            .any(|msg| msg.contains("DepAddr")),
        "{second:?}"
    );
}

/// A leftover checkout under `_build/deps/` — neither declared nor in the
/// resolved graph, as after a rebuild that dropped the dependency —
/// contributes no aliases: the dependency set is never a directory listing.
#[test]
fn leftover_checkout_aliases_do_not_resolve() {
    let (dir, state_path) = write_parity_project();
    fs::write(dir.path().join("beamtalk.toml"), MANIFEST_WITHOUT_DEPS)
        .expect("drop fakedep from the manifest");
    write_dep_graph_snapshot(dir.path(), &[]);
    let source = fs::read_to_string(&state_path).expect("read fixture state file");
    let response = handle_compile(&load_like_compile_request(&source, &state_path));
    assert_eq!(
        response_status(&response).as_deref(),
        Some("error"),
        "a checkout outside the resolved graph must not leak aliases: {response:?}"
    );
    assert!(
        error_diagnostic_messages(&response)
            .iter()
            .any(|msg| msg.contains("DepAddr")),
        "{response:?}"
    );
}

/// A missing dep-graph snapshot (never written, older toolchain, or a
/// best-effort write that failed) must not drop the directly declared
/// dependency: its `_build/deps/` checkout still supplies its aliases, as
/// `beamtalk build` sees them.
#[test]
fn missing_snapshot_still_resolves_declared_dependencies() {
    let (dir, state_path) = write_parity_project();
    fs::remove_file(dir.path().join("_build/deps/.dep-graph.json"))
        .expect("remove the dep-graph snapshot");
    let source = fs::read_to_string(&state_path).expect("read fixture state file");
    let response = handle_compile(&load_like_compile_request(&source, &state_path));
    assert_eq!(
        response_status(&response).as_deref(),
        Some("ok"),
        "a declared dependency must resolve without a snapshot: {response:?}"
    );
}

/// A corrupt snapshot behaves like a missing one.
#[test]
fn corrupt_snapshot_still_resolves_declared_dependencies() {
    let (dir, state_path) = write_parity_project();
    fs::write(dir.path().join("_build/deps/.dep-graph.json"), "{ not json")
        .expect("corrupt the dep-graph snapshot");
    let source = fs::read_to_string(&state_path).expect("read fixture state file");
    let response = handle_compile(&load_like_compile_request(&source, &state_path));
    assert_eq!(
        response_status(&response).as_deref(),
        Some("ok"),
        "a declared dependency must resolve with a corrupt snapshot: {response:?}"
    );
}

/// An unknown-schema snapshot (a newer or older toolchain's) behaves like a
/// missing one.
#[test]
fn unknown_schema_snapshot_still_resolves_declared_dependencies() {
    let (dir, state_path) = write_parity_project();
    fs::write(
        dir.path().join("_build/deps/.dep-graph.json"),
        r#"{"schema": 99, "deps": []}"#,
    )
    .expect("write an unknown-schema dep-graph snapshot");
    let source = fs::read_to_string(&state_path).expect("read fixture state file");
    let response = handle_compile(&load_like_compile_request(&source, &state_path));
    assert_eq!(
        response_status(&response).as_deref(),
        Some("ok"),
        "a declared dependency must resolve with an unknown-schema snapshot: {response:?}"
    );
}

/// Adds a transitive dependency to the parity project — `fakedep` declares
/// `transdep`, whose checkout defines `type TransAddr` — plus a project file
/// using that alias under `type = "error"`. Returns the new file's path.
fn add_transitive_dependency(root: &std::path::Path) -> std::path::PathBuf {
    fs::create_dir_all(root.join("_build/deps/transdep/src")).expect("fixture transdep src/");
    fs::write(
        root.join("_build/deps/fakedep/beamtalk.toml"),
        "[package]\nname = \"fakedep\"\nversion = \"0.1.0\"\n\n[dependencies]\ntransdep = { git = \"https://example.test/transdep\", tag = \"v0.1.0\" }\n",
    )
    .expect("fixture dep beamtalk.toml");
    fs::write(
        root.join("_build/deps/transdep/beamtalk.toml"),
        "[package]\nname = \"transdep\"\nversion = \"0.1.0\"\n",
    )
    .expect("fixture transdep beamtalk.toml");
    fs::write(
        root.join("_build/deps/transdep/src/trans_types.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         type TransAddr = #near | #far\n\
         \n\
         Value subclass: TransTypes\n\
         \x20 defaultAddr -> Symbol => #near\n",
    )
    .expect("fixture trans_types.bt");
    let path = root.join("src/parity_trans.bt");
    fs::write(
        &path,
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         typed Value subclass: ParityTrans\n\
         \x20 field: addr :: TransAddr = #near\n",
    )
    .expect("fixture parity_trans.bt");
    path
}

/// BT-4394: with a usable snapshot, a transitive dependency resolves through
/// the edge its parent's entry records.
#[test]
fn transitive_dependency_aliases_resolve_through_recorded_edges() {
    let (dir, _) = write_parity_project();
    let trans_path = add_transitive_dependency(dir.path());
    let source = fs::read_to_string(&trans_path).expect("read fixture trans file");

    // The fixture's snapshot records `fakedep` with no edges, so the
    // `transdep` checkout sits outside the resolved graph.
    let response = handle_compile(&load_like_compile_request(&source, &trans_path));
    assert_eq!(
        response_status(&response).as_deref(),
        Some("error"),
        "a checkout outside the recorded graph must not resolve: {response:?}"
    );

    write_dep_graph_snapshot_with_edges(
        dir.path(),
        &[("fakedep", &["transdep"]), ("transdep", &[])],
    );
    let response = handle_compile(&load_like_compile_request(&source, &trans_path));
    assert_eq!(
        response_status(&response).as_deref(),
        Some("ok"),
        "a transitive dependency's alias must resolve: {response:?}"
    );
}

/// BT-4394: an unusable snapshot (here, missing) still resolves transitive
/// dependencies — the fallback scans `_build/deps/` checkouts with a
/// manifest, not only the directly declared names.
#[test]
fn missing_snapshot_still_resolves_transitive_dependencies() {
    let (dir, _) = write_parity_project();
    let trans_path = add_transitive_dependency(dir.path());
    fs::remove_file(dir.path().join("_build/deps/.dep-graph.json"))
        .expect("remove the dep-graph snapshot");
    let source = fs::read_to_string(&trans_path).expect("read fixture trans file");
    let response = handle_compile(&load_like_compile_request(&source, &trans_path));
    assert_eq!(
        response_status(&response).as_deref(),
        Some("ok"),
        "a transitive dependency's alias must resolve without a snapshot: {response:?}"
    );
}

/// BT-4378: requests sharing a `project_batch` token verify the project
/// scope once — an edit made mid-batch is not seen until the next batch (or
/// an untagged request), which re-verifies.
#[test]
fn project_batch_verifies_scope_once_per_batch() {
    let (dir, state_path) = write_parity_project();
    let source = fs::read_to_string(&state_path).expect("read fixture state file");
    let batched = |batch: &str| {
        let mut request = load_like_compile_request(&source, &state_path);
        request.map.insert(atom("project_batch"), binary(batch));
        handle_compile(&request)
    };
    assert_eq!(response_status(&batched("1")).as_deref(), Some("ok"));

    // Narrow `Scheme` so `#http` no longer matches.
    fs::write(
        dir.path().join("src/parity_config.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         type Scheme = #https\n",
    )
    .expect("rewrite fixture alias file");
    assert_eq!(
        response_status(&batched("1")).as_deref(),
        Some("ok"),
        "the rest of batch 1 reuses its verified scope"
    );
    assert_eq!(
        response_status(&batched("2")).as_deref(),
        Some("error"),
        "batch 2 re-verifies and sees the narrowed alias"
    );
}

/// `diagnostics` request shaped like an editor buffer for `source_path`
/// (BT-4325): the buffer text plus the file it edits, no `project_batch`.
fn editor_diagnostics_request(source: &str, source_path: Option<&std::path::Path>) -> Map {
    let mut request = Map::from([
        (atom("command"), atom("diagnostics")),
        (atom("source"), binary(source)),
        (atom("mode"), binary("expression")),
    ]);
    if let Some(path) = source_path {
        request
            .map
            .insert(atom("source_path"), binary(path.to_string_lossy().as_ref()));
    }
    request
}

/// Every `(severity, message)` a `diagnostics` response carries, sorted.
fn editor_diagnostics(response: &Term) -> Vec<(String, String)> {
    let Some(list) = response_diagnostics(response) else {
        panic!("Expected diagnostics list: {response:?}");
    };
    let mut entries: Vec<(String, String)> = list
        .elements
        .iter()
        .map(|diag| {
            let Term::Map(dm) = diag else {
                panic!("Expected diagnostic map, got {diag:?}");
            };
            let field = |key| {
                map_get(dm, key)
                    .and_then(term_to_string)
                    .unwrap_or_default()
            };
            (field("severity"), field("message"))
        })
        .collect();
    entries.sort();
    entries
}

/// The editor's view of a file, split the way `compile` splits it: error
/// messages (which fail a load) and every other message (which `compile`
/// returns as warnings).
fn editor_errors_and_warnings(response: &Term) -> (Vec<String>, Vec<String>) {
    let (errors, warnings): (Vec<_>, Vec<_>) = editor_diagnostics(response)
        .into_iter()
        .partition(|(severity, _)| severity == "error");
    let messages = |entries: Vec<(String, String)>| {
        let mut messages: Vec<String> = entries.into_iter().map(|(_, m)| m).collect();
        messages.sort();
        messages
    };
    (messages(errors), messages(warnings))
}

/// BT-4325 AC 1 + 4: the editor's `diagnostics/3` for a project file reports
/// exactly what a `load_project` `compile` of the same file reports — same
/// errors (under the project's own `[diagnostics]` table), same warnings
/// (none of the spurious alias/FFI ones a scope-less check produces) — for
/// every file of the shared BT-4322 fixture.
#[test]
fn editor_diagnostics_agree_with_load_compile() {
    let (dir, _state_path) = write_parity_project();
    for name in ["parity_config.bt", "parity_state.bt", "parity_bad.bt"] {
        let path = dir.path().join("src").join(name);
        let source = fs::read_to_string(&path).expect("read fixture file");

        let load = handle_compile(&load_like_compile_request(&source, &path));
        let (load_errors, load_warnings) = if response_status(&load).as_deref() == Some("ok") {
            let mut warnings = response_warning_messages(&load);
            warnings.sort();
            (Vec::new(), warnings)
        } else {
            let mut errors = error_diagnostic_messages(&load);
            errors.sort();
            (errors, Vec::new())
        };

        let editor = handle_diagnostics(&editor_diagnostics_request(&source, Some(&path)));
        assert_eq!(
            response_status(&editor).as_deref(),
            Some("ok"),
            "{editor:?}"
        );
        let (editor_errors, editor_warnings) = editor_errors_and_warnings(&editor);

        assert_eq!(
            editor_errors, load_errors,
            "{name}: editor errors must match what load_project fails on"
        );
        if load_errors.is_empty() {
            assert_eq!(
                editor_warnings, load_warnings,
                "{name}: editor warnings must match load_project's"
            );
        }
    }

    // The fixture exercises both outcomes: one file the project's table
    // fails, one whose aliases/FFI resolve cleanly only with project scope.
    let bad_path = dir.path().join("src/parity_bad.bt");
    let bad = fs::read_to_string(&bad_path).expect("read fixture bad file");
    let (errors, _) = editor_errors_and_warnings(&handle_diagnostics(&editor_diagnostics_request(
        &bad,
        Some(&bad_path),
    )));
    assert!(
        errors.iter().any(|m| m.contains("declared as Integer")),
        "the project's type = \"error\" must apply in the editor: {errors:?}"
    );
}

/// BT-4325: a `diagnostics/3` request with no `source_path` (a scratch
/// buffer) keeps the legacy scope — the cross-file alias and the FFI spec
/// are not in scope, and the port cwd's table (none here) leaves the
/// resulting type findings as non-errors.
#[test]
fn editor_diagnostics_without_path_keep_legacy_scope() {
    let (_dir, state_path) = write_parity_project();
    let source = fs::read_to_string(&state_path).expect("read fixture state file");

    let (errors, warnings) = editor_errors_and_warnings(&handle_diagnostics(
        &editor_diagnostics_request(&source, None),
    ));
    assert!(errors.is_empty(), "no project table, no errors: {errors:?}");
    assert!(
        warnings.iter().any(|w| w.contains("declared as Scheme")),
        "a scope-less check must not see the project alias: {warnings:?}"
    );
}

/// BT-4325 AC 3: an inline class typed at the REPL keeps the legacy scope
/// even when the request names a project file — REPL input belongs to no
/// file's project, so the project's `type = "error"` table must not fail
/// it, and the project's aliases are not in scope.
#[test]
fn repl_inline_class_ignores_source_path_project() {
    let (_dir, state_path) = write_parity_project();
    let source = fs::read_to_string(&state_path).expect("read fixture state file");
    let mut request = compile_expression_request(&source);
    request.map.insert(
        atom("source_path"),
        binary(state_path.to_string_lossy().as_ref()),
    );

    let response = handle_compile_expression(&request);
    assert_eq!(
        response_status(&response).as_deref(),
        Some("ok"),
        "REPL inline class must not take the project's error table: {response:?}"
    );
    let warnings = response_warning_messages(&response);
    assert!(
        warnings.iter().any(|w| w.contains("declared as Scheme")),
        "REPL inline class must not see project aliases: {warnings:?}"
    );
}

/// BT-4325: an unsaved editor buffer declares its file's aliases itself —
/// the saved copy's must not stay in scope. Here the buffer for
/// `parity_config.bt` deletes `type Scheme` but still uses it: the editor
/// must already report what `load_project` reports once that buffer is
/// saved, not stay clean on the strength of the stale on-disk declaration.
#[test]
fn unsaved_buffer_alias_deletion_matches_load_after_save() {
    let (dir, _state_path) = write_parity_project();
    let config_path = dir.path().join("src/parity_config.bt");
    let buffer = "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         typed Value subclass: ParityConfig\n\
         \x20 field: s :: Scheme = #http\n";

    let (editor_errors, _) = editor_errors_and_warnings(&handle_diagnostics(
        &editor_diagnostics_request(buffer, Some(&config_path)),
    ));
    assert!(
        editor_errors.iter().any(|m| m.contains("Scheme")),
        "the saved copy's deleted alias must not stay in scope: {editor_errors:?}"
    );

    fs::write(&config_path, buffer).expect("save the buffer");
    let load = handle_compile(&load_like_compile_request(buffer, &config_path));
    assert_eq!(response_status(&load).as_deref(), Some("error"), "{load:?}");
    let mut load_errors = error_diagnostic_messages(&load);
    load_errors.sort();
    assert_eq!(
        editor_errors, load_errors,
        "editor (before save) and load (after save) must agree"
    );
}

/// BT-4326: lays out a project with a *path* dependency `pathdep` (sources
/// outside `_build/`) declaring `type PathAddr`, plus a project file using
/// it under `type = "error"`. Returns the project dir and that file.
fn write_path_dep_project(extra_deps: &str) -> (tempfile::TempDir, std::path::PathBuf) {
    let dir = tempfile::tempdir().expect("tempdir for path-dep project");
    let root = dir.path().join("app");
    let dep = dir.path().join("pathdep");
    fs::create_dir_all(root.join("src")).expect("app src/");
    fs::create_dir_all(dep.join("src")).expect("dep src/");
    fs::write(
        root.join("beamtalk.toml"),
        format!("[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n[dependencies]\npathdep = {{ path = \"../pathdep\" }}\n{extra_deps}\n[diagnostics]\ntype = \"error\"\ntype-annotation = \"error\"\n"),
    )
    .expect("app manifest");
    fs::write(
        dep.join("beamtalk.toml"),
        "[package]\nname = \"pathdep\"\nversion = \"0.1.0\"\n",
    )
    .expect("dep manifest");
    fs::write(
        dep.join("src/path_types.bt"),
        "// Copyright 2026 James Casey\n// SPDX-License-Identifier: Apache-2.0\n\ntype PathAddr = #a | #b\n\nValue subclass: PathTypes\n  one -> Integer => 1\n",
    )
    .expect("dep source");
    let file = root.join("src/uses_path.bt");
    fs::write(
        &file,
        "// Copyright 2026 James Casey\n// SPDX-License-Identifier: Apache-2.0\n\ntyped Value subclass: UsesPath\n  field: addr :: PathAddr = #a\n",
    )
    .expect("app source");
    (dir, file)
}

/// BT-4326: a path dependency's `type` aliases are in scope for the port,
/// with or without a dependency-graph snapshot.
#[test]
fn path_dependency_aliases_resolve() {
    let (dir, file) = write_path_dep_project("");
    let source = fs::read_to_string(&file).expect("read app file");
    let response = handle_compile(&load_like_compile_request(&source, &file));
    assert_eq!(
        response_status(&response).as_deref(),
        Some("ok"),
        "path dependency alias must resolve without a snapshot: {response:?}"
    );

    write_dep_graph_snapshot(&dir.path().join("app"), &["pathdep"]);
    let response = handle_compile(&load_like_compile_request(&source, &file));
    assert_eq!(
        response_status(&response).as_deref(),
        Some("ok"),
        "path dependency alias must resolve with a snapshot: {response:?}"
    );
}

/// BT-4326: a path dependency's own path dependency (relative to *its*
/// manifest) resolves through the manifest walk.
#[test]
fn transitive_path_dependency_aliases_resolve() {
    let (dir, file) = write_path_dep_project("");
    let leaf = dir.path().join("leafdep");
    fs::create_dir_all(leaf.join("src")).expect("leaf src/");
    fs::write(
        leaf.join("beamtalk.toml"),
        "[package]\nname = \"leafdep\"\nversion = \"0.1.0\"\n",
    )
    .expect("leaf manifest");
    fs::write(
        leaf.join("src/leaf_types.bt"),
        "// Copyright 2026 James Casey\n// SPDX-License-Identifier: Apache-2.0\n\ntype LeafAddr = #x | #y\n",
    )
    .expect("leaf source");
    fs::write(
        dir.path().join("pathdep/beamtalk.toml"),
        "[package]\nname = \"pathdep\"\nversion = \"0.1.0\"\n\n[dependencies]\nleafdep = { path = \"../leafdep\" }\n",
    )
    .expect("dep manifest with leaf");
    fs::write(
        &file,
        "// Copyright 2026 James Casey\n// SPDX-License-Identifier: Apache-2.0\n\ntyped Value subclass: UsesLeaf\n  field: addr :: LeafAddr = #x\n",
    )
    .expect("app source");
    let source = fs::read_to_string(&file).expect("read app file");
    let response = handle_compile(&load_like_compile_request(&source, &file));
    assert_eq!(
        response_status(&response).as_deref(),
        Some("ok"),
        "transitive path dependency alias must resolve: {response:?}"
    );
}

/// BT-4326 ordering: two dependencies declaring the same alias name collide
/// first-wins in the build's compilation order (leaves first, ties by name
/// largest-first), so the port must pick the same winner. `pathdep` and
/// `zzdep` are independent leaves: the build compiles `zzdep` first, so its
/// `Clash = Integer` beats `pathdep`'s `Clash = String`, whatever the
/// sorted-dirname order says.
#[test]
fn dependency_merge_order_follows_build_compilation_order() {
    let (dir, file) = write_path_dep_project("zzdep = { path = \"../zzdep\" }\n");
    let dep_a = dir.path().join("pathdep");
    let dep_z = dir.path().join("zzdep");
    fs::create_dir_all(dep_z.join("src")).expect("zz src/");
    fs::write(
        dep_z.join("beamtalk.toml"),
        "[package]\nname = \"zzdep\"\nversion = \"0.1.0\"\n",
    )
    .expect("zz manifest");
    let hdr = "// Copyright 2026 James Casey\n// SPDX-License-Identifier: Apache-2.0\n\n";
    fs::write(
        dep_z.join("src/z.bt"),
        format!("{hdr}type Clash = Integer\n"),
    )
    .expect("zz src");
    fs::write(
        dep_a.join("src/clash.bt"),
        format!("{hdr}type Clash = String\n"),
    )
    .expect("a src");
    fs::write(
        &file,
        format!("{hdr}typed Value subclass: UsesClash\n  field: n :: Clash = 1\n"),
    )
    .expect("app src");
    let source = fs::read_to_string(&file).expect("read app file");
    let response = handle_compile(&load_like_compile_request(&source, &file));
    let messages = error_diagnostic_messages(&response);
    assert!(
        !messages.iter().any(|m| m.contains("declared as Clash")),
        "zzdep's Clash = Integer must win (build order), got: {response:?}"
    );
}
