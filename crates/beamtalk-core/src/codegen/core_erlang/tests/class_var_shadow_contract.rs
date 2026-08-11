// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Cross-boundary conformance fixture for the ADR 0110 class-var shadow
//! write-through contract (ADR 0111 Phase D / BT-3135).
//!
//! CLAUDE.md's duplication rule: "A rule crossing the Rust/Erlang boundary
//! needs a shared conformance fixture or code generation, not a comment."
//! The `'$bt_class_vars_shadow'` process-dictionary key atom is produced by
//! this crate's codegen (`expressions.rs::generate_field_assignment`, as
//! literal Core Erlang text emitted into every compiled class method that
//! mutates a class var) and consumed by
//! `runtime/apps/beamtalk_runtime/src/beamtalk_class_dispatch.erl`'s
//! `invoke_class_method/7`. Two independent languages, two independent
//! compile/review paths — exactly the case
//! `docs/development/architecture-principles.md` §6's table keeps as a
//! **permanent boundary test**, not something to consolidate away.
//!
//! The shared fixture is
//! `runtime/apps/beamtalk_runtime/include/beamtalk.hrl`'s
//! `?BT_CLASS_VARS_SHADOW_KEY_ATOM` macro — the Erlang side's single source
//! of truth (`beamtalk_class_dispatch.erl` and
//! `beamtalk_class_dispatch_tests.erl` both `-include` it, so its `EUnit`
//! suite is testing the same macro the production code uses, not a
//! hand-typed copy of the atom). This test reads that exact checked-in file
//! at test time and asserts the atom text it defines appears verbatim in
//! *actually compiled* codegen output — so a future change to either side's
//! atom spelling without updating the other fails here, in CI, rather than
//! corrupting a class's mutation on the first foreign-NLR relay that hits
//! production (ADR 0110's original, shipped bug).

use std::path::{Path, PathBuf};

/// Returns the repository root (`CARGO_MANIFEST_DIR/../..`), mirroring the
/// `repo_root` helper in `source_analysis::method_span_corpus_tests`.
fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("crates/")
        .parent()
        .expect("repo root")
        .to_path_buf()
}

/// Extracts the quoted Erlang atom literal from
/// `-define(BT_CLASS_VARS_SHADOW_KEY_ATOM, '<atom>').` in `beamtalk.hrl`'s
/// raw text — no Erlang parser needed, just the one macro this test cares
/// about. Panics with a descriptive message if the fixture ever changes
/// shape out from under this test, rather than silently passing on `None`.
fn shadow_key_atom_from_hrl(hrl_text: &str) -> String {
    let needle = "-define(BT_CLASS_VARS_SHADOW_KEY_ATOM, '";
    let start = hrl_text.find(needle).unwrap_or_else(|| {
        panic!(
            "expected beamtalk.hrl to define ?BT_CLASS_VARS_SHADOW_KEY_ATOM as a quoted atom; \
             fixture shape changed — update this test's parser alongside it"
        )
    }) + needle.len();
    let end = hrl_text[start..].find("').").unwrap_or_else(|| {
        panic!("expected the ?BT_CLASS_VARS_SHADOW_KEY_ATOM -define(...) to close with `').`")
    });
    // Re-quote as the atom literal codegen emits (leading `'`, no trailing
    // dollar — `$` is part of the atom name itself, not Rust/Erlang syntax).
    format!("'{}'", &hrl_text[start..start + end])
}

#[test]
fn shadow_key_atom_in_hrl_fixture_matches_compiled_codegen_output() {
    let hrl_path = repo_root().join("runtime/apps/beamtalk_runtime/include/beamtalk.hrl");
    let hrl_text = std::fs::read_to_string(&hrl_path)
        .unwrap_or_else(|e| panic!("failed to read shared fixture {}: {e}", hrl_path.display()));
    let hrl_atom = shadow_key_atom_from_hrl(&hrl_text);

    // A real compiled fixture — the same class-var-mutation shape ADR 0110
    // fixes and `test_class_var_mutation_emits_shadow_write` (gen_server.rs)
    // pins byte-for-byte — exercised here specifically to cross-check its
    // shadow-write atom against the shared Erlang-side fixture, not to
    // re-pin the whole emitted line (that's the other test's job).
    let src = "Object subclass: ShadowContractCounter\n  classState: runs = 0\n\n  class bump =>\n    self.runs := self.runs + 1\n    self.runs";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code = crate::codegen::core_erlang::generate_module(
        &module,
        crate::codegen::core_erlang::CodegenOptions::new("bt@shadowcontractcounter")
            .with_workspace_mode(true),
    )
    .expect("codegen should succeed");

    assert!(
        code.contains(&hrl_atom),
        "compiled codegen output does not contain the shadow-key atom \
         ({hrl_atom}) defined in {}; the Rust codegen emission site \
         (expressions.rs::generate_field_assignment) and the Erlang \
         ?BT_CLASS_VARS_SHADOW_KEY_ATOM macro have drifted apart. Got:\n{code}",
        hrl_path.display()
    );
}

#[test]
fn shadow_key_atom_parser_pins_expected_literal() {
    // Sanity-pins the parser above against the fixture's real, current
    // content — if this ever fails on its own (fixture unmodified), the
    // parser regressed, not the contract.
    let hrl_path = repo_root().join("runtime/apps/beamtalk_runtime/include/beamtalk.hrl");
    let hrl_text = std::fs::read_to_string(&hrl_path)
        .unwrap_or_else(|e| panic!("failed to read shared fixture {}: {e}", hrl_path.display()));
    assert_eq!(
        shadow_key_atom_from_hrl(&hrl_text),
        "'$bt_class_vars_shadow'"
    );
}
