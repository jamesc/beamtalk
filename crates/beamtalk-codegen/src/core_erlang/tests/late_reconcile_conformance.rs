// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Cross-boundary conformance fixture for ADR 0124 §8/B9's `late`-slot
//! reconcile decision table (BT-3556).
//!
//! CLAUDE.md's duplication rule: "A rule crossing the Rust/Erlang boundary
//! needs a shared conformance fixture or code generation, not a comment."
//! `beamtalk_shape_migration:reconcile_declared/7` (Erlang) decides a
//! declared-but-absent field's reconcile outcome from three pieces of
//! **compiled `__beamtalk_meta/0` metadata** this crate's `class_meta.rs`
//! emits — `field_kinds` (BT-3547/B5a), `field_has_default`
//! (`beamtalk_behaviour_intrinsics:classAllFieldHasDefaultByName/1`'s
//! source), and `is_typed` — plus whether the migration's incoming
//! dictionary still has the key. Two independent languages, two
//! independent compile/review paths: exactly the "boundary you cannot
//! delete" `docs/development/architecture-principles.md` §6/§7 keeps as a
//! **permanent conformance test**, not a "keep in sync" comment.
//!
//! The shared fixture is
//! `runtime/apps/beamtalk_runtime/test/fixtures/late_reconcile_conformance.json`
//! — one row per `(class, field, kind, has_default, is_typed, key_present)`
//! -> `outcome` case, each naming a real compiled `.bt` fixture under
//! `runtime/apps/beamtalk_runtime/test_fixtures/` (never a hand-copied
//! snippet — the same "read the real fixture" precedent
//! `sendability.rs::tests::runtime_field_tier_kind_mapping_matches_compile_time_base_tier`
//! and `class_var_shadow_contract.rs` both already use). This test asserts
//! that each row's fixture actually compiles to the declared `kind`/
//! `has_default`/`is_typed` meta — the inputs `reconcile_declared/7`
//! reads. The Erlang side asserts the *outcome* half of each row (the
//! actual `migrate/3` result against these same real fixture classes) in
//! `beamtalk_shape_migration_tests:test_late_reconcile_conformance_matches_shared_corpus/0`.
//! Between the two, a drift in either side's meta emission, or in
//! `reconcile_declared/7`'s own decision logic, fails in CI rather than
//! silently reconciling a `late` slot wrong.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Returns the repository root (`CARGO_MANIFEST_DIR/../..`), mirroring the
/// `repo_root` helper in `source_analysis::method_span_corpus_tests` and
/// `class_var_shadow_contract.rs`.
fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("crates/")
        .parent()
        .expect("repo root")
        .to_path_buf()
}

/// Compile `source_fixture` (a path relative to the repo root) and return
/// its generated Core Erlang text.
fn compile_fixture(source_fixture: &str) -> String {
    let path = repo_root().join(source_fixture);
    let src = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("read fixture {}: {e}", path.display()));
    let tokens = beamtalk_core::source_analysis::lex_with_eof(&src);
    let (module, parse_diags) = beamtalk_core::source_analysis::parse(tokens);
    assert!(
        parse_diags.is_empty(),
        "parse failed for {source_fixture}: {parse_diags:?}"
    );
    crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("bt@late_reconcile_fixture"),
    )
    .unwrap_or_else(|e| panic!("codegen failed for {source_fixture}: {e:?}"))
}

#[test]
fn rust_meta_matches_shared_corpus() {
    let path = repo_root()
        .join("runtime/apps/beamtalk_runtime/test/fixtures/late_reconcile_conformance.json");
    let raw = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("read corpus {}: {e}", path.display()));
    let cases: Vec<serde_json::Value> = serde_json::from_str(&raw).expect("corpus is a JSON array");
    assert!(!cases.is_empty(), "corpus must have cases");

    // Several rows share the same source fixture (a `key_present` variant
    // of the same compiled class) — cache each fixture's compiled output
    // so it is only compiled once.
    let mut compiled: HashMap<String, String> = HashMap::new();

    for case in &cases {
        let source_fixture = case["source_fixture"]
            .as_str()
            .expect("case.source_fixture");
        let field_name = case["field_name"].as_str().expect("case.field_name");
        let expected_kind = case["kind"].as_str().expect("case.kind");
        let expected_has_default = case["has_default"]
            .as_bool()
            .expect("case.has_default is a bool");
        let expected_is_typed = case["is_typed"].as_bool().expect("case.is_typed is a bool");
        let why = case["why"].as_str().unwrap_or("");

        let code = compiled
            .entry(source_fixture.to_string())
            .or_insert_with(|| compile_fixture(source_fixture));

        let kind_fragment = format!("'{field_name}' => '{expected_kind}'");
        assert!(
            code.contains(&kind_fragment),
            "expected 'field_kinds' to contain {kind_fragment} for {source_fixture} \
             ({why}). Got:\n{code}"
        );

        let has_default_bin = if expected_has_default {
            "true"
        } else {
            "false"
        };
        let has_default_fragment = format!("'{field_name}' => '{has_default_bin}'");
        assert!(
            code.contains(&has_default_fragment),
            "expected 'field_has_default' to contain {has_default_fragment} for \
             {source_fixture} ({why}). Got:\n{code}"
        );

        let is_typed_bin = if expected_is_typed { "true" } else { "false" };
        let is_typed_fragment = format!("'is_typed' => '{is_typed_bin}'");
        assert!(
            code.contains(&is_typed_fragment),
            "expected {is_typed_fragment} for {source_fixture} ({why}). Got:\n{code}"
        );
    }
}
