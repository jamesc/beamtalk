// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3724 (ADR 0111 amendment): `ThreadedIr` verifier findings reach the
//! driver as `internal:` warnings, and *only* those are forwarded by
//! [`generate_module_surfacing_verifier`]; generation still succeeds.

use crate::core_erlang::{
    CodegenOptions, GeneratedModule, generate_module_surfacing_verifier,
    generate_module_with_warnings,
};
use beamtalk_core::source_analysis::{Diagnostic, DiagnosticCategory, Severity, Span};

const SOURCE: &str = "Object subclass: Probe\n  go => 1 + 2\n";

fn parse() -> beamtalk_core::ast::Module {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(SOURCE);
    beamtalk_core::source_analysis::parse(tokens).0
}

#[test]
fn valid_program_surfaces_no_verifier_diagnostics() {
    let (code, diags) =
        generate_module_surfacing_verifier(&parse(), CodegenOptions::new("probe")).unwrap();
    assert!(code.contains("module 'probe'"));
    assert!(
        diags.is_empty(),
        "no new diagnostics for valid code: {diags:?}"
    );
}

#[test]
fn injected_violation_surfaces_as_warning_and_generation_succeeds() {
    let (code, diags) = generate_module_surfacing_verifier(
        &parse(),
        CodegenOptions::new("probe").with_injected_verifier_violation(),
    )
    .expect("a verifier finding must not fail generation");

    assert!(code.contains("module 'probe'"), "output is still produced");
    assert_eq!(diags.len(), 1, "{diags:?}");
    assert_eq!(diags[0].severity, Severity::Warning);
    assert_eq!(
        diags[0].category,
        Some(DiagnosticCategory::InternalVerifier)
    );
    assert!(diags[0].message.starts_with("internal: "));
}

#[test]
fn only_verifier_diagnostics_are_forwarded() {
    let module = generate_module_with_warnings(&parse(), CodegenOptions::new("probe")).unwrap();
    let other = Diagnostic::warning("some other codegen warning", Span::new(0, 1));
    let verifier = Diagnostic::warning("internal: x", Span::new(0, 1))
        .with_category(DiagnosticCategory::InternalVerifier);
    let mixed = GeneratedModule {
        code: module.code,
        warnings: vec![other, verifier.clone()],
    };
    let (_, forwarded) = mixed.into_code_and_verifier_diagnostics();
    assert_eq!(forwarded, vec![verifier]);
}
