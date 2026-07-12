// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Regression test for BT-2861: the spec-extraction worker's `-pa` list must
//! include the current project's own native ebin directory.
//!
//! A package's native Erlang code can be split across sibling modules (ADR
//! 0072 "package-bundled native code") where one module's `-spec` references
//! another module's exported `-type t()` (tagged with `'$beamtalk_class'`,
//! ADR 0075). Before the fix, `spawn_build_worker_for_specs` only added the
//! Beamtalk runtime's own ebin directories to the extraction worker's code
//! path, so `code:which/1` could not find the sibling module and the
//! reference collapsed to `Dynamic` — degrading `{ok, T} | {error, E}` to a
//! bare `Result` instead of the parameterized `Result(T, E)`.

mod cli_common;

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
    // resolve to a parameterized `Result(NativeThingResponse)`.
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
    // to `Result(NativeThingResponse)`, not a bare `Result` or `Dynamic`.
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
        combined.contains("Result(NativeThingResponse)"),
        "expected native_thing:get/0 to resolve to Result(NativeThingResponse); \
         type cache contents:\n{combined}"
    );
}
