// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Cross-boundary conformance for the protocol registration shape (ADR 0127
//! §10 "Protocol module"; BT-3625; architecture-principles §7).
//!
//! `generate_protocol_registrations` (this crate) emits the map handed to
//! `beamtalk_protocol_registry:register_protocol/1` (Erlang). The shared
//! fixture `runtime/apps/beamtalk_runtime/test/fixtures/
//! protocol_registration_conformance.json` lists the keys of that map and of
//! each provided-method row. This test compiles the fixture's real `.bt`
//! source and asserts the generated registration carries every key; the
//! Erlang half (`beamtalk_protocol_registry_tests:
//! protocol_registration_shape_conformance_test/0`) asserts the registry
//! keeps every key it is handed.

use std::path::{Path, PathBuf};

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("crates/")
        .parent()
        .expect("repo root")
        .to_path_buf()
}

#[test]
fn compiled_protocol_registration_carries_every_key_in_the_shared_fixture() {
    let path = repo_root()
        .join("runtime/apps/beamtalk_runtime/test/fixtures/protocol_registration_conformance.json");
    let raw = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("read fixture {}: {e}", path.display()));
    let fixture: serde_json::Value = serde_json::from_str(&raw).expect("fixture is JSON");
    let strings = |key: &str| -> Vec<String> {
        fixture[key]
            .as_array()
            .unwrap_or_else(|| panic!("fixture key {key} is an array"))
            .iter()
            .map(|v| v.as_str().expect("string").to_string())
            .collect()
    };

    let source = fixture["source"].as_str().expect("source");
    let (module, diags) =
        beamtalk_core::source_analysis::parse(beamtalk_core::source_analysis::lex_with_eof(source));
    assert!(diags.is_empty(), "fixture source must parse: {diags:?}");
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("bt@bt3625_greetable").with_source(source),
    )
    .expect("codegen");

    // The registration call's argument map.
    let reg_start = code
        .find("'register_protocol'(")
        .expect("compiled module registers its protocol");
    let registration = &code[reg_start..];
    for key in strings("registration_keys") {
        assert!(
            registration.contains(&format!("'{key}' => ")),
            "registration is missing key '{key}':\n{registration}"
        );
    }

    // Each provided method's row: `~{'selector' => 'greet', ...}~`.
    let provided = &registration[registration
        .find("'provided_methods' => ")
        .expect("provided_methods key")..];
    for selector in strings("provided_selectors") {
        let row_start = provided
            .find(&format!("~{{'selector' => '{selector}'"))
            .unwrap_or_else(|| panic!("no provided row for {selector}"));
        let row = &provided[row_start..];
        let row = &row[..row.find("}~").expect("row terminator")];
        for key in strings("provided_method_keys") {
            assert!(
                row.contains(&format!("'{key}' => ")),
                "provided row for {selector} is missing key '{key}': {row}"
            );
        }
    }

    // A protocol module with provisions exports the source carrier.
    let source_fn = fixture["source_function"]
        .as_str()
        .expect("source_function");
    assert!(
        code.contains(&format!("'{source_fn}'/0 = fun () ->")),
        "expected the protocol module to define '{source_fn}'/0, got:\n{code}"
    );
}

/// Protocol modules *without* provisions do not get the source carrier
/// (ADR 0127 §10: "it exists only on protocol modules that have provisions").
#[test]
fn provisionless_protocol_module_has_no_protocol_source_function() {
    let source = "Protocol define: Bt3625Plain\n  greeting -> String\n";
    let (module, _) =
        beamtalk_core::source_analysis::parse(beamtalk_core::source_analysis::lex_with_eof(source));
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("bt@bt3625_plain").with_source(source),
    )
    .expect("codegen");
    assert!(!code.contains("__beamtalk_protocol_source"), "got:\n{code}");
}
