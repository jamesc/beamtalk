// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Auto-generated compiler snapshot tests.
//!
//! Test cases are discovered from the `cases/` directory.
//! Each subdirectory with a `main.bt` file becomes a test.
//!
//! Each test case generates:
//! - parser check - a span-free parse -> unparse -> re-parse round trip for
//!   error-free cases; `{case}_parser_diagnostics` (compact) for cases with
//!   parse errors; `{case}_parser` (full AST dump) only for the cases in
//!   `PARSER_DUMP_CASES`, which exercise error recovery
//! - `{case}_semantic` - Semantic error diagnostics
//! - `{case}_codegen` - Generated Core Erlang code
//!
//! There is deliberately no lexer snapshot: the lexer is covered by unit and
//! property tests in `beamtalk-core`.
//!
//! Additionally, a compilation test verifies the generated Core Erlang
//! compiles successfully with `erlc +from_core` (skipped if erlc unavailable).

use beamtalk_codegen::core_erlang::{CodegenOptions, generate_module};
use beamtalk_core::semantic_analysis;
use beamtalk_core::source_analysis::{lex_with_eof, parse};
use camino::Utf8PathBuf;
use std::fs;

/// Helper function to read test case source files
fn read_test_case(case_name: &str) -> String {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let path = Utf8PathBuf::from(manifest_dir)
        .join("cases")
        .join(case_name)
        .join("main.bt");
    fs::read_to_string(&path).unwrap_or_else(|e| {
        panic!(
            "Failed to read test case '{}' at {}: {}",
            case_name, path, e
        )
    })
}

/// Sibling `.bt` files in a case directory (everything except `main.bt`),
/// as `(file name, source)` pairs sorted by name.
///
/// ADR 0127 requires a protocol and the class that `uses:` it to live in
/// *separate* files, so a trait case is a directory with `main.bt` (the user)
/// plus one sibling file per provision-bearing protocol. Those siblings are
/// the cross-file protocol carrier — exactly what the CLI build hands to
/// `analyse_full` as `pre_loaded_protocol_defs` — so the case exercises the
/// legal cross-file path end to end. Empty for every ordinary single-file
/// case.
fn read_sibling_sources(case_name: &str) -> Vec<(String, String)> {
    let dir = Utf8PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("cases")
        .join(case_name);
    let mut siblings: Vec<(String, String)> = fs::read_dir(&dir)
        .unwrap_or_else(|e| panic!("Failed to read case dir {dir}: {e}"))
        .filter_map(Result::ok)
        .filter_map(|entry| {
            let name = entry.file_name().to_string_lossy().into_owned();
            if name == "main.bt" || !name.ends_with(".bt") {
                return None;
            }
            let text = fs::read_to_string(entry.path())
                .unwrap_or_else(|e| panic!("Failed to read {name}: {e}"));
            Some((name, text))
        })
        .collect();
    siblings.sort();
    siblings
}

/// Parses each sibling file and returns the provision-bearing protocol ASTs
/// plus their source identity (file name + text), as the CLI's Pass 1 does.
fn sibling_protocols(
    siblings: &[(String, String)],
) -> (
    Vec<beamtalk_core::ast::ProtocolDefinition>,
    semantic_analysis::ProtocolSourceMap,
) {
    let mut defs = Vec::new();
    let mut sources = semantic_analysis::ProtocolSourceMap::new();
    for (name, text) in siblings {
        let (module, _) = parse(lex_with_eof(text));
        for protocol in module
            .protocols
            .into_iter()
            .filter(|p| !p.provided_methods.is_empty())
        {
            sources.insert(
                protocol.name.name.clone(),
                semantic_analysis::ProtocolSource {
                    path: Some(name.as_str().into()),
                    text: text.as_str().into(),
                    package: None,
                },
            );
            defs.push(protocol);
        }
    }
    (defs, sources)
}

/// Cases whose parser test pins the full `{:#?}` AST dump plus diagnostics.
///
/// Reserved for cases that exercise error recovery or parser diagnostics,
/// where the (partial) AST shape is the thing under test. Every other case
/// gets the span-free round-trip check in [`test_parser_check`], so adding an
/// AST field does not rewrite dozens of snapshots.
const PARSER_DUMP_CASES: &[&str] = &[
    "boundary_deeply_nested",
    "boundary_mixed_errors",
    "boundary_unicode_identifiers",
    "character_literals",
    "control_flow_mutations",
    "control_flow_mutations_errors",
    "error_recovery_invalid_syntax",
    "error_recovery_malformed_message",
    "future_pattern_matching",
    "set_theoretic_operators",
];

/// Parser check for a given test case.
///
/// * Allowlisted cases ([`PARSER_DUMP_CASES`]): full AST dump + diagnostics.
/// * Other cases with parse errors: a compact `severity span: message`
///   diagnostics snapshot (no AST).
/// * Error-free cases: parse, unparse, re-parse and unparse again, asserting
///   the re-parse is error-free and the unparse is a fixed point.
fn test_parser_check(case_name: &str) {
    let source = read_test_case(case_name);
    let (module, diagnostics) = parse(lex_with_eof(&source));

    if PARSER_DUMP_CASES.contains(&case_name) {
        let mut output = format!("AST:\n{module:#?}\n");
        if !diagnostics.is_empty() {
            output.push_str("\nDiagnostics:\n");
            for diag in &diagnostics {
                output.push_str(&format!("{diag:?}\n"));
            }
        }
        insta::assert_snapshot!(format!("{case_name}_parser"), output);
        return;
    }

    let has_errors = diagnostics
        .iter()
        .any(|d| d.severity == beamtalk_core::source_analysis::Severity::Error);
    if has_errors {
        let output = diagnostics
            .iter()
            .map(|d| {
                format!(
                    "{:?} {}..{}: {}",
                    d.severity,
                    d.span.start(),
                    d.span.end(),
                    d.message
                )
            })
            .collect::<Vec<_>>()
            .join("\n");
        insta::assert_snapshot!(format!("{case_name}_parser_diagnostics"), output);
        return;
    }

    let pass1 = beamtalk_core::unparse::unparse_module(&module);
    let (module2, diagnostics2) = parse(lex_with_eof(&pass1));
    let errors2: Vec<_> = diagnostics2
        .iter()
        .filter(|d| d.severity == beamtalk_core::source_analysis::Severity::Error)
        .collect();
    assert!(
        errors2.is_empty(),
        "unparsed output of '{case_name}' fails to re-parse: {errors2:?}\n\n{pass1}"
    );
    let pass2 = beamtalk_core::unparse::unparse_module(&module2);
    // Compared verbatim, comments included.
    assert_eq!(
        pass1, pass2,
        "unparse is not a fixed point for '{case_name}':\n--- pass 1\n{pass1}\n--- pass 2\n{pass2}"
    );
}

/// Test the Core Erlang codegen output for a given test case
fn test_codegen_snapshot(case_name: &str) {
    let (_module_name, core_erlang) = generate_core_erlang(case_name);
    insta::assert_snapshot!(format!("{}_codegen", case_name), core_erlang);
}

/// Helper function to generate Core Erlang from a test case.
///
/// Returns a tuple of (module_name, core_erlang_code).
///
/// Uses workspace mode for code generation.
fn generate_core_erlang(case_name: &str) -> (String, String) {
    generated_cases()
        .get(case_name)
        .unwrap_or_else(|| panic!("unknown test case '{case_name}'"))
        .clone()
        .unwrap_or_else(|e| panic!("{e}"))
}

/// Every case's `(module_name, core_erlang)`, generated once per test run and
/// shared by the codegen snapshot and `erlc` tests. A failed generation is
/// stored as its panic message so the one test for that case reports it.
fn generated_cases() -> &'static std::collections::HashMap<String, Result<(String, String), String>>
{
    static CACHE: std::sync::OnceLock<
        std::collections::HashMap<String, Result<(String, String), String>>,
    > = std::sync::OnceLock::new();
    CACHE.get_or_init(|| {
        ALL_CASES
            .iter()
            .map(|case| {
                let generated =
                    std::panic::catch_unwind(|| generate_core_erlang_uncached(case)).map_err(|p| {
                        p.downcast_ref::<String>()
                            .cloned()
                            .or_else(|| p.downcast_ref::<&str>().map(ToString::to_string))
                            .unwrap_or_else(|| format!("Codegen panicked for '{case}'"))
                    });
                ((*case).to_string(), generated)
            })
            .collect()
    })
}

fn generate_core_erlang_uncached(case_name: &str) -> (String, String) {
    let source = read_test_case(case_name);
    let tokens = lex_with_eof(&source);
    let (mut module, _diagnostics) = parse(tokens);

    // Generate Core Erlang with a module name derived from the test case
    let module_name = case_name.replace('-', "_");
    let options = CodegenOptions::new(&module_name);

    let siblings = read_sibling_sources(case_name);
    if siblings.is_empty() {
        let core_erlang = generate_module(&module, options)
            .unwrap_or_else(|e| panic!("Codegen failed for '{}': {}", case_name, e));
        return (module_name, core_erlang);
    }

    // Cross-file trait case: the CLI build pipeline —
    // `analyse_full` (with the sibling protocols) -> `lower_module_for_codegen`
    // -> `generate_module(..).with_analysis(..)`.
    let (protocol_defs, protocol_sources) = sibling_protocols(&siblings);
    let analysis = semantic_analysis::analyse_full(
        &module,
        semantic_analysis::AnalysisContext::default().with_pre_loaded_protocol_defs(protocol_defs),
    );
    semantic_analysis::lower_analysed_module_for_codegen(&mut module, &analysis);
    let core_erlang = generate_module(
        &module,
        options
            .with_source(&source)
            .with_source_path_opt(Some("main.bt"))
            .with_protocol_sources(protocol_sources)
            .with_analysis(analysis),
    )
    .unwrap_or_else(|e| panic!("Codegen failed for '{}': {}", case_name, e));

    (module_name, core_erlang)
}

/// Per-case `erlc` verdict. All cases' Core Erlang is compiled once per run
/// with `erlc_batched` (batches of 32) instead of one `erlc` spawn per case;
/// `Err` carries the combined output of every failing batch.
fn batched_erlc_verdict() -> &'static Result<(), String> {
    static VERDICT: std::sync::OnceLock<Result<(), String>> = std::sync::OnceLock::new();
    VERDICT.get_or_init(|| {
        let modules: Vec<(String, String)> = ALL_CASES
            .iter()
            .filter_map(|case| generated_cases().get(*case)?.as_ref().ok().cloned())
            .collect();
        let dir = tempfile::TempDir::new().expect("Failed to create temp dir");
        beamtalk_core::test_helpers::test_support::erlc_batched(dir.path(), &modules)
    })
}

/// Test that the generated Core Erlang compiles successfully with erlc.
///
/// This test is skipped if `erlc` is not available on the system PATH.
/// `erlc` runs once for the whole corpus (see [`batched_erlc_verdict`]); each
/// case's test then fails only if the output names that case's `<module>.core`.
fn test_codegen_compiles(case_name: &str) {
    if !beamtalk_core::test_helpers::test_support::erlc_available() {
        eprintln!(
            "Skipping {} compilation test - erlc not available",
            case_name
        );
        return;
    }

    let (module_name, core_erlang) = generate_core_erlang(case_name);

    if let Err(output) = batched_erlc_verdict() {
        let core_file = format!("{module_name}.core");
        let mine: Vec<&str> = output
            .lines()
            .filter(|line| line.contains(&core_file))
            .collect();
        // A spawn failure or output naming no file fails every case.
        let unattributed = !output.contains(".core");
        assert!(
            mine.is_empty() && !unattributed,
            "erlc compilation failed for '{}':\n{}\n\nGenerated Core Erlang:\n{}",
            case_name,
            if unattributed { output.as_str().to_string() } else { mine.join("\n") },
            core_erlang
        );
    }
}

/// Shared helper for semantic analysis diagnostic snapshots.
///
/// Runs the full semantic analysis pipeline, filters diagnostics by `severity`,
/// and captures the result in a snapshot named `{case_name}_{snapshot_suffix}`.
fn test_diagnostics_snapshot(
    case_name: &str,
    severity: beamtalk_core::source_analysis::Severity,
    heading: &str,
    none_text: &str,
    snapshot_suffix: &str,
) {
    let source = read_test_case(case_name);
    let tokens = lex_with_eof(&source);
    let (module, _parser_diagnostics) = parse(tokens);

    // Cross-file trait cases analyse `main.bt` against its sibling protocol
    // files (see `read_sibling_sources`); every other case is single-file.
    let siblings = read_sibling_sources(case_name);
    let result = if siblings.is_empty() {
        semantic_analysis::analyse(&module)
    } else {
        semantic_analysis::analyse_full(
            &module,
            semantic_analysis::AnalysisContext::default()
                .with_pre_loaded_protocol_defs(sibling_protocols(&siblings).0),
        )
    };

    let diagnostics: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.severity == severity)
        .collect();

    let snapshot_name = format!("{case_name}_{snapshot_suffix}");
    if diagnostics.is_empty() {
        // Clean cases assert inline instead of snapshotting `none_text`: a
        // snapshot exists only for cases that have diagnostics. A leftover
        // snapshot means the case used to report diagnostics and now silently
        // reports none, so fail loudly rather than let it go stale.
        let stale = Utf8PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("tests")
            .join("snapshots")
            .join(format!("compiler_tests__{snapshot_name}.snap"));
        assert!(
            !stale.exists(),
            "{none_text} for '{case_name}', but a stale snapshot exists at {stale}; \
             delete it (the clean result is asserted inline)"
        );
        return;
    }

    let mut output = format!("{} ({}):\n", heading, diagnostics.len());
    for diag in &diagnostics {
        output.push_str(&format!("  - {}\n", diag.message));
    }
    insta::assert_snapshot!(snapshot_name, output);
}

/// Test semantic analysis error diagnostics for a given test case.
///
/// Runs the full semantic analysis pipeline (class hierarchy, name resolution,
/// type checking, block analysis) and captures error diagnostics in a snapshot.
fn test_semantic_snapshot(case_name: &str) {
    test_diagnostics_snapshot(
        case_name,
        beamtalk_core::source_analysis::Severity::Error,
        "Semantic errors",
        "No semantic errors",
        "semantic",
    );
}

/// Test semantic analysis warnings for a given test case.
///
/// Runs the full semantic analysis pipeline and captures WARNING-level
/// diagnostics in a snapshot. Used to verify that typed class annotation
/// warnings are properly emitted.
fn test_warnings_snapshot(case_name: &str) {
    test_diagnostics_snapshot(
        case_name,
        beamtalk_core::source_analysis::Severity::Warning,
        "Warnings",
        "No warnings",
        "warnings",
    );
}

// Test cases will be generated here by build.rs
include!(concat!(env!("OUT_DIR"), "/generated_tests.rs"));

/// BT-490 / ADR 0019 Phase 3: Workspace bindings compile as normal class references.
/// Transcript, Beamtalk, Workspace are no longer special-cased — they go through
/// standard class dispatch (class_send) like any other class.
#[test]
fn test_workspace_binding_compiles_as_normal_class() {
    let source = "Actor subclass: Greeter\n  greet => Transcript show: \"Hello\"";
    let tokens = lex_with_eof(source);
    let (module, _) = parse(tokens);

    // Both batch and workspace mode should succeed — no special treatment
    let batch_result = generate_module(&module, CodegenOptions::new("test_batch"));
    assert!(
        batch_result.is_ok(),
        "Transcript should compile in batch mode: {:?}",
        batch_result.err()
    );

    let ws_result = generate_module(&module, CodegenOptions::new("test_ws"));
    assert!(
        ws_result.is_ok(),
        "Transcript should compile in workspace mode: {:?}",
        ws_result.err()
    );

    // `Transcript` is a sealed, stateless class-side facade (ADR 0129), so the
    // send is direct-called rather than routed through `class_send`.
    let batch_code = batch_result.unwrap();
    assert!(
        batch_code.contains("Transcript"),
        "Batch mode should reference the Transcript class, got:\n{batch_code}"
    );
    assert_eq!(
        non_static_call_key_persistent_term_reads(&batch_code),
        0,
        "Should NOT generate a persistent_term binding lookup, got:\n{batch_code}"
    );

    // ADR 0019 Phase 4: Actor methods in workspace mode use class_send only,
    // no persistent_term fallback (removed in BT-491).
    let ws_code = ws_result.unwrap();
    assert!(
        ws_code.contains("Transcript"),
        "Workspace mode should reference the Transcript class, got:\n{ws_code}"
    );
    assert_eq!(
        non_static_call_key_persistent_term_reads(&ws_code),
        0,
        "Workspace mode should NOT use a persistent_term binding (ADR 0019 Phase 4), got:\n{ws_code}"
    );
}

/// `persistent_term` reads in `code` other than guarded static class sends'
/// static call keys (BT-4316): each of those reads is paired with exactly one
/// miss-path `static_call_target` call, so any surplus read is something else
/// (such as the BT-491 workspace-binding lookup).
fn non_static_call_key_persistent_term_reads(code: &str) -> usize {
    let reads = code.matches("call 'persistent_term':").count();
    let static_keys = code
        .matches("call 'beamtalk_class_shadow_flags':'static_call_target'(")
        .count();
    reads.saturating_sub(static_keys)
}

/// BT-941: Smoke test — typed class annotation warnings are emitted correctly.
///
/// A `typed` class with missing annotations should surface type checker
/// warnings. This verifies the warning pipeline works end-to-end.
#[test]
fn test_typed_class_warnings_warnings() {
    test_warnings_snapshot("typed_class_warnings");
}

/// BT-2746 / ADR 0102 §4: the advisory, non-exhaustive `match:` warning over
/// a closed singleton union is pinned end-to-end through the compiler
/// pipeline here — BUnit cannot observe compiler warnings (BT-2745's
/// finding), so this is the one place the warning-*appearing* case (as
/// opposed to the silence cases, pinned in
/// `stdlib/test/singleton_match_exhaustiveness_test.bt` and
/// `stdlib/test/set_theoretic_operators_e2e_test.bt`) is verified.
#[test]
fn test_set_theoretic_operators_warnings() {
    test_warnings_snapshot("set_theoretic_operators");
}
