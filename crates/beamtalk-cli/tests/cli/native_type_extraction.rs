// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Regression test: the spec-extraction worker's `-pa` list must
//! include the current project's own native ebin directory.
//!
//! A package's native Erlang code can be split across sibling modules (ADR
//! 0072 "package-bundled native code") where one module's `-spec` references
//! another module's exported `-type t()` (tagged with `'$beamtalk_class'`,
//! ADR 0075). Without the project's own ebin directory on the extraction
//! worker's code path, `code:which/1` cannot find the sibling module and the
//! reference collapses to `Dynamic` — degrading `{ok, T} | {error, E}` to a
//! bare `Result` instead of the parameterized `Result(T, E)`.

use crate::cli_common;
use predicates::prelude::PredicateBooleanExt;

#[test]
fn native_sibling_type_reference_resolves_after_build() {
    let project = cli_common::fixture_project();

    std::fs::create_dir_all(project.path().join("native")).unwrap();

    // Writing `.erl` source (not a precompiled `.beam`) relies on `beamtalk
    // build` compiling it itself: with no `[native.dependencies]` in
    // beamtalk.toml, native/*.erl files are compiled directly via
    // `compile:file/2` in the build worker (ADR 0072 Path A, no rebar3).

    // One native module exports a type tagged `'$beamtalk_class'` so the spec
    // reader recognises it as the Beamtalk class `NativeThingResponse`.
    std::fs::write(
        project.path().join("native/native_thing_response.erl"),
        "%% Copyright 2026 James Casey\n\
         %% SPDX-License-Identifier: Apache-2.0\n\
         -module(native_thing_response).\n\
         -export([make/0]).\n\
         \n\
         -type t() :: #{\n\
         \x20\x20\x20\x20'$beamtalk_class' := 'NativeThingResponse',\n\
         \x20\x20\x20\x20'value' := integer()\n\
         }.\n\
         -export_type([t/0]).\n\
         \n\
         -spec make() -> t().\n\
         make() -> #{'$beamtalk_class' => 'NativeThingResponse', value => 42}.\n",
    )
    .unwrap();

    // A sibling native module whose `-spec` references the other module's
    // type in an `{ok, T} | {error, E}` return — the pattern that should
    // resolve to a parameterized `Result(NativeThingResponse, Exception)`.
    std::fs::write(
        project.path().join("native/native_thing.erl"),
        "%% Copyright 2026 James Casey\n\
         %% SPDX-License-Identifier: Apache-2.0\n\
         -module(native_thing).\n\
         -export([get/0]).\n\
         \n\
         -spec get() -> {ok, native_thing_response:t()} | {error, term()}.\n\
         get() -> {ok, native_thing_response:make()}.\n",
    )
    .unwrap();

    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("build")
        .assert()
        .success();

    // The project-local type cache should have resolved `native_thing:get/0`
    // to `Result(NativeThingResponse, Exception)`, not a bare `Result` or
    // `Dynamic` (the error side is always an Exception class, BT-4023).
    let cache_dir = project.path().join("_build/type_cache");
    let entries = std::fs::read_dir(&cache_dir)
        .unwrap_or_else(|e| panic!("expected {cache_dir:?} to exist: {e}"));

    let mut combined = String::new();
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_file() {
            combined.push_str(&std::fs::read_to_string(path).unwrap());
        }
    }

    assert!(
        combined.contains("Result(NativeThingResponse, Exception)"),
        "expected native_thing:get/0 to resolve to Result(NativeThingResponse, Exception); \
         type cache contents:\n{combined}"
    );
}

/// BT-5342: `check` on a clean tree must see the `-spec`s of the package's
/// own `native/*.erl`, exactly as it does after a build. Before the fix the
/// FFI call was inferred as Dynamic and a `typed` class warned "untyped FFI",
/// so `check --warnings-as-errors` depended on whether `_build` existed.
#[test]
fn check_on_clean_tree_sees_own_native_specs() {
    let project = cli_common::fixture_project();
    std::fs::create_dir_all(project.path().join("native")).unwrap();
    std::fs::write(
        project.path().join("native/probe_native.erl"),
        "%% Copyright 2026 James Casey\n\
         %% SPDX-License-Identifier: Apache-2.0\n\
         -module(probe_native).\n\
         -export([reset/1]).\n\
         \n\
         -spec reset(term()) -> nil.\n\
         reset(_Self) -> nil.\n",
    )
    .unwrap();
    std::fs::write(
        project.path().join("src/Probe.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         typed Object subclass: Probe\n\
         \n\
         \x20\x20reset -> Nil =>\n\
         \x20\x20\x20\x20(Erlang probe_native) reset: self\n",
    )
    .unwrap();
    assert!(
        !project
            .path()
            .join("_build")
            .join("dev")
            .join("native")
            .exists(),
        "fixture must start without built native artifacts"
    );

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["check", "--warnings-as-errors"])
        .assert()
        .success()
        .stderr(predicates::str::contains("untyped FFI").not());
    // Positive signal, independent of the warning's wording.
    assert!(
        project
            .path()
            .join("_build/dev/native/ebin/probe_native.beam")
            .exists(),
        "check should have compiled the package's own native module"
    );
}

/// BT-5342: a broken `native/*.erl` must not hide `.bt` diagnostics. `check`
/// warns about the native compile failure and still reports the `.bt` error.
#[test]
fn check_reports_bt_errors_when_native_compile_fails() {
    let project = cli_common::fixture_project();
    std::fs::create_dir_all(project.path().join("native")).unwrap();
    std::fs::write(
        project.path().join("native/broken_native.erl"),
        "%% Copyright 2026 James Casey\n%% SPDX-License-Identifier: Apache-2.0\n\
         -module(broken_native).\nthis is not erlang\n",
    )
    .unwrap();
    std::fs::write(
        project.path().join("src/Probe.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         Value subclass: Probe\n\
         \n\
         \x20\x20go => 42 frobnicate\n",
    )
    .unwrap();

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["check", "--warnings-as-errors"])
        .assert()
        .failure()
        .stderr(
            predicates::str::contains("could not compile native Erlang sources").and(
                predicates::str::contains("does not understand 'frobnicate'"),
            ),
        );
}
