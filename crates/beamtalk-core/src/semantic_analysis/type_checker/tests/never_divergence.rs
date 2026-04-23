// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Deep-descendant Never detection for block divergence (BT-2051).

use super::common::*;

// ── BT-2051: Deep-descendant `Never` detection in `block_diverges` ──
//
// BT-2049 only scanned top-level statement roots for `Never`. A diverging call
// buried in a method-send argument — e.g. `logger info: (self error: "…")` —
// was missed because the *argument* is `Never` but the outer `info:` send is
// typed by its own return type. BT-2051 walks descendants via
// `expr_contains_never`, symmetric with `expr_contains_return` from BT-2047.

/// BT-2051: A `Never` call buried inside a method-send argument should still
/// mark the enclosing guard block as diverging, narrowing the nullable local
/// on subsequent statements.
#[test]
fn bt2051_never_inside_message_argument_diverges() {
    let source = r#"
typed Object subclass: Sink
  class log: msg :: String => msg

typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: ms :: Integer | Nil =>
    ms isNil ifTrue: [Sink log: (self error: "missing")]
    Receiver process: ms
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse_with_options_and_classes(
        &module,
        &crate::CompilerOptions::default(),
        vec![],
    );
    let type_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "`ms` should be narrowed to Integer after `[Sink log: (self error: ...)]` guard \
         (the argument is `Never`, so the whole send diverges). Got: {type_warnings:?}"
    );
}

/// BT-2051: A `Never` call buried inside a parenthesized expression should
/// also diverge.
#[test]
fn bt2051_never_inside_parenthesized_diverges() {
    let source = r#"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: ms :: Integer | Nil =>
    ms isNil ifTrue: [(self error: "missing")]
    Receiver process: ms
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse_with_options_and_classes(
        &module,
        &crate::CompilerOptions::default(),
        vec![],
    );
    let type_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "`ms` should narrow after `[(self error: ...)]` — parenthesized `Never` still \
         diverges. Got: {type_warnings:?}"
    );
}

/// BT-2051: A `Never` call on the RHS of an assignment statement should
/// diverge — evaluating the RHS throws before the binding is ever updated.
#[test]
fn bt2051_never_in_assignment_rhs_diverges() {
    let source = r#"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: ms :: Integer | Nil =>
    sink :: Integer := 0
    ms isNil ifTrue: [sink := (self error: "missing")]
    Receiver process: ms
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse_with_options_and_classes(
        &module,
        &crate::CompilerOptions::default(),
        vec![],
    );
    let type_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "`ms` should narrow after `[sink := (self error: ...)]` — RHS diverges before \
         the assignment completes. Got: {type_warnings:?}"
    );
}

/// BT-2051: A `Never` call used as a cascade receiver should diverge even
/// though the cascade's own inferred type is not `Never`.
#[test]
fn bt2051_never_as_cascade_receiver_diverges() {
    let source = r#"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: ms :: Integer | Nil =>
    ms isNil ifTrue: [(self error: "missing") yourself; yourself]
    Receiver process: ms
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse_with_options_and_classes(
        &module,
        &crate::CompilerOptions::default(),
        vec![],
    );
    let type_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "`ms` should narrow after cascade on `Never` receiver — evaluating the receiver \
         diverges before any cascade message runs. Got: {type_warnings:?}"
    );
}

/// BT-2051 soundness: a `Never`-typed expression INSIDE a nested block literal
/// must NOT make the enclosing guard count as diverging. The inner block is
/// constructed, not executed, so the outer guard can still fall through and
/// `ms` must stay nullable at the downstream use site.
#[test]
fn bt2051_never_inside_nested_block_literal_does_not_diverge() {
    let source = r#"
typed Object subclass: Callbacks
  add: block => self

typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: ms :: Integer | Nil =>
    callbacks := Callbacks new
    ms isNil ifTrue: [callbacks add: [self error: "later"]]
    Receiver process: ms
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse_with_options_and_classes(
        &module,
        &crate::CompilerOptions::default(),
        vec![],
    );
    // The `ifTrue:` branch constructs a block that *would* diverge if run,
    // but the outer branch doesn't execute it — control falls through to
    // `Receiver process: ms` with `ms` still `Integer | Nil`, so the
    // argument-mismatch diagnostic must still fire.
    //
    // BT-2066: user-facing messages render the source-sympathetic `Nil`
    // spelling, not the canonical `UndefinedObject` hierarchy name.
    let mismatch_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| {
            d.category == Some(DiagnosticCategory::Type)
                && d.message.contains("'process:'")
                && d.message.contains("Nil")
        })
        .collect();
    assert!(
        !mismatch_warnings.is_empty(),
        "nested block literal containing `self error:` must NOT cause post-guard narrowing; \
         `process:` call should still warn. Got diagnostics: {:?}",
        result
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
    // BT-2066: canonical `UndefinedObject` must NOT leak into user-facing
    // diagnostic messages.
    for d in &mismatch_warnings {
        assert!(
            !d.message.contains("UndefinedObject"),
            "user-facing diagnostic leaked canonical `UndefinedObject`: {}",
            d.message
        );
    }
}
