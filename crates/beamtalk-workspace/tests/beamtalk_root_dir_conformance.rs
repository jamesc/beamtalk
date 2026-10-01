// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Rust half of the `~/.beamtalk` root-dir conformance corpus (BT-3680).
//!
//! The Erlang runtime resolves `<home>/.beamtalk[/workspaces]` in
//! `beamtalk_platform:beamtalk_root_dir/0` / `workspaces_base_dir/0`; the Rust
//! side resolves it in `beamtalk_home::beamtalk_root_dir` and
//! `beamtalk_workspace::workspaces_base_dir`. Both must agree on where
//! workspace state lives (the node writes `port`/`cookie`/`workspace.log`; the
//! CLI/MCP/LSP read them), so both are pinned to the shared fixture
//! `runtime/apps/beamtalk_runtime/test/fixtures/beamtalk_root_dir_conformance.json`
//! (Erlang side: `beamtalk_platform_tests:root_dir_conformance_matches_shared_corpus_test/0`).
//!
//! Fixture `rust` column: `always` = run everywhere; `unix` = needs `HOME` to
//! drive `dirs::home_dir()` (Windows ignores it); `never` = Erlang-only (the
//! Rust resolver cannot be forced into "no home" portably because `dirs`
//! falls back to the passwd database — Rust surfaces that case as an error
//! from `workspaces_base_dir`, and Erlang as `undefined`; neither invents a
//! fallback directory).

use std::path::{Path, PathBuf};

fn corpus_path() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join(
        "../../runtime/apps/beamtalk_runtime/test/fixtures/beamtalk_root_dir_conformance.json",
    )
}

#[test]
fn rust_resolver_matches_shared_corpus() {
    let raw = std::fs::read_to_string(corpus_path()).expect("read corpus");
    let cases: Vec<serde_json::Value> = serde_json::from_str(&raw).expect("corpus is a JSON array");
    assert!(!cases.is_empty());

    let mut ran = 0;
    for case in &cases {
        let name = case["name"].as_str().expect("case.name");
        let applies = match case["rust"].as_str().expect("case.rust") {
            "always" => true,
            "unix" => cfg!(unix),
            "never" => false,
            other => panic!("unknown rust mode {other:?} in case {name}"),
        };
        if !applies {
            continue;
        }

        // SAFETY: this integration-test binary contains exactly one test, so
        // nothing else reads or writes the environment concurrently.
        unsafe {
            std::env::remove_var("BEAMTALK_HOME");
            std::env::remove_var("HOME");
            for (k, v) in case["env"].as_object().expect("case.env") {
                std::env::set_var(k, v.as_str().expect("env value"));
            }
        }

        let root = beamtalk_home::beamtalk_root_dir();
        let workspaces = beamtalk_workspace::workspaces_base_dir().ok();
        let expect = |key: &str| case[key].as_str().map(PathBuf::from);
        // `Path` equality is component-wise, so `/` in the fixture matches
        // Windows separators too.
        assert_eq!(root, expect("root"), "root dir: {name}");
        assert_eq!(workspaces, expect("workspaces"), "workspaces dir: {name}");
        ran += 1;
    }
    assert!(ran > 0, "no corpus rows applied on this platform");
}
