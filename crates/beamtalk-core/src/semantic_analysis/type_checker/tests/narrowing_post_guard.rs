// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Post-guard narrowing of nullable locals/fields after divergent `isNil ifTrue: [^err]` (BT-2049).

use super::common::*;

// ── BT-2049: Post-guard narrowing for locals and self.field after diverging guards ──

/// BT-2049: `x isNil ifTrue: [self error: "..."]` should narrow the local
/// `x` to non-Nil for subsequent statements, even though the block has no
/// `^` return — the `error:` call returns `Never` and therefore diverges.
#[test]
fn bt2049_local_narrows_after_self_error_guard() {
    let source = r#"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: ms :: Integer | Nil =>
    ms isNil ifTrue: [self error: "missing"]
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
        "`ms` should be narrowed to Integer after `self error:` guard, got: {type_warnings:?}"
    );
}

/// BT-2049: `self.field isNil ifTrue: [^err]` should narrow `self.field`
/// to non-Nil for subsequent statements, so passing it to a typed parameter
/// does not warn.
#[test]
fn bt2049_self_field_narrows_after_return_guard() {
    let source = r"
typed Object subclass: Store
  class process: v :: Integer => v

typed Actor subclass: Engine
  state: eventStore :: Integer | Nil = nil
  run =>
    self.eventStore isNil ifTrue: [^nil]
    Store process: self.eventStore
";
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
        "`self.eventStore` should be narrowed to Integer after `^nil` guard, got: {type_warnings:?}"
    );
}

/// BT-2049: `self.field isNil ifTrue: [self error: "..."]` should narrow
/// `self.field` to non-Nil via the diverging-call path (no `^`).
#[test]
fn bt2049_self_field_narrows_after_self_error_guard() {
    let source = r#"
typed Object subclass: Store
  class process: v :: Integer => v

typed Actor subclass: Engine
  state: eventStore :: Integer | Nil = nil
  run =>
    self.eventStore isNil ifTrue: [self error: "no event store"]
    Store process: self.eventStore
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
        "`self.eventStore` should be narrowed to Integer after `self error:` guard, got: {type_warnings:?}"
    );
}

/// BT-2049: A non-diverging `ifTrue:` block (no `^`, last expression is not
/// `Never`) must NOT narrow the variable — the binding may still be Nil
/// when control falls through.
#[test]
fn bt2049_non_diverging_guard_does_not_narrow() {
    let source = r"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: ms :: Integer | Nil =>
    ms isNil ifTrue: [42]
    Receiver process: ms
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse_with_options_and_classes(
        &module,
        &crate::CompilerOptions::default(),
        vec![],
    );
    // The negative case must produce the specific argument-mismatch
    // diagnostic at the `process:` call site: `ms` should still be
    // `Integer | Nil` because the `ifTrue: [42]` block does not
    // diverge. A bare "any Type warning" check could hide regressions where
    // narrowing silently happens but some other Type warning appears.
    //
    // BT-2066: user-facing messages render the source-sympathetic `Nil`
    // spelling, not the canonical `UndefinedObject` hierarchy name. The
    // assertion below is deliberately strict — if the renderer regresses
    // and leaks `UndefinedObject` back into diagnostics, this test fails.
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
        "non-diverging guard must leave `ms` nullable and warn at `process:`; \
         got diagnostics: {:?}",
        result
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
    // BT-2066: the canonical `UndefinedObject` name must NOT leak into the
    // user-facing message. Guard against regressions explicitly.
    for d in &mismatch_warnings {
        assert!(
            !d.message.contains("UndefinedObject"),
            "user-facing diagnostic leaked canonical `UndefinedObject`: {}",
            d.message
        );
    }
}

/// BT-2049: Bare identifier `eventStore` (sugar for `self.eventStore`) should
/// also be narrowed after a `self.eventStore isNil ifTrue: [^nil]` guard,
/// because both spellings resolve through the same synthetic `self.field` key.
#[test]
fn bt2049_bare_field_narrows_after_guard() {
    let source = r"
typed Object subclass: Store
  class process: v :: Integer => v

typed Actor subclass: Engine
  state: eventStore :: Integer | Nil = nil
  run =>
    self.eventStore isNil ifTrue: [^nil]
    Store process: eventStore
";
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
        "Bare `eventStore` should be narrowed to Integer after guard, got: {type_warnings:?}"
    );
}

/// BT-2049: `block_diverges` must treat a block as diverging when a
/// `Never`-typed statement appears anywhere in the body, not only as the
/// trailing statement. `[self error: "...". 42]` — the trailing `42` looks
/// reachable but the block actually diverges at `self error:`.
#[test]
fn bt2049_guard_with_diverge_then_trailing_nondiverging() {
    let source = r#"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: ms :: Integer | Nil =>
    ms isNil ifTrue: [
      self error: "missing"
      42
    ]
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
        "ms should narrow: non-tail `self error:` still diverges. got: {type_warnings:?}"
    );
}

/// BT-2049: `ifTrue: [diverge] ifFalse: [reassign to nil]` must NOT narrow
/// after the statement — execution reaches the next statement through the
/// `ifFalse:` branch, and the reassignment makes the variable nil again.
#[test]
fn bt2049_if_true_if_false_reassigning_false_branch_does_not_narrow() {
    let source = r#"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: ms :: Integer | Nil =>
    ms isNil
      ifTrue: [self error: "missing"]
      ifFalse: [ms := nil]
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
    // Because the ifFalse: block reassigns `ms` to nil, we must still warn
    // on the `process:` call — otherwise we'd be unsound.
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
        "ifFalse: branch reassigns `ms` to nil — `process:` call must still warn; got: {:?}",
        result
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
    // BT-2066: the canonical `UndefinedObject` name must NOT leak into the
    // user-facing message.
    for d in &mismatch_warnings {
        assert!(
            !d.message.contains("UndefinedObject"),
            "user-facing diagnostic leaked canonical `UndefinedObject`: {}",
            d.message
        );
    }
}
