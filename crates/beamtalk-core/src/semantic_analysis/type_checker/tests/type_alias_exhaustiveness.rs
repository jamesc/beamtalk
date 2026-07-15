// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0108 / BT-2895: exhaustiveness regression — an alias-annotated
//! `match:`/`matchExhaustive:` scrutinee must behave *identically* to the
//! spelled-out union, because the exhaustiveness checker itself is
//! unchanged (`check_singleton_match_exhaustiveness` / `matchExhaustive:`
//! operate purely on the resolved `InferredType`, computed by the same
//! `resolve_type_annotation` every other annotation goes through — ADR 0108
//! Implementation: "no changes" to the exhaustiveness checker).
//!
//! This is verification, not new machinery: every test here has a spelled-
//! out-union sibling already pinned in `asserted_match_exhaustiveness.rs` /
//! `singleton_match_exhaustiveness.rs` — the assertions below mirror those
//! exact diagnostic shapes, but reached through a `type Direction = ...`
//! declaration and a `Direction`-typed local instead.

use super::common::*;

use crate::source_analysis::Severity;

/// Parses `source`, runs the full `analyse_with_options_and_classes`
/// pipeline (so `type` declarations get registered into an `AliasRegistry`
/// exactly as they would for a real compile — see `semantic_analysis::mod`'s
/// `analyse_full`), and returns every diagnostic.
fn analyse_diagnostics(source: &str) -> Vec<crate::source_analysis::Diagnostic> {
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse_with_options_and_classes(
        &module,
        &crate::CompilerOptions::default(),
        vec![],
    );
    result.diagnostics
}

// ── Advisory `match:` over an alias-typed local (BT-2745 sibling) ───────

#[test]
fn alias_typed_local_advisory_match_warns_on_missing_member() {
    // `type Direction = #north | #south | #east | #west` — same gap
    // (`#west` missing) that `asserted_match_exhaustiveness.rs`'s spelled-out
    // union sibling pins as a single `Warning` under plain `match:`.
    let source = r"
type Direction = #north | #south | #east | #west

heading :: Direction := #east
heading match: [
  #north -> 0;
  #south -> 180;
  #east -> 90
]
";
    let diags = analyse_diagnostics(source);
    let warnings: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("non-exhaustive"))
        .collect();
    assert_eq!(warnings.len(), 1, "expected one warning, got: {diags:?}");
    assert_eq!(warnings[0].severity, Severity::Warning);
    assert!(warnings[0].message.contains("#west"));
    assert!(!warnings[0].message.contains("matchExhaustive:"));
}

#[test]
fn alias_typed_local_advisory_match_silent_on_full_coverage() {
    let source = r"
type Direction = #north | #south | #east | #west

heading :: Direction := #east
heading match: [
  #north -> 0;
  #south -> 180;
  #east -> 90;
  #west -> 270
]
";
    let diags = analyse_diagnostics(source);
    let warnings: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("non-exhaustive"))
        .collect();
    assert!(
        warnings.is_empty(),
        "full coverage through an alias must stay silent, got: {diags:?}"
    );
}

// ── Asserted `matchExhaustive:` over an alias-typed local (BT-2763 sibling) ──

#[test]
fn alias_typed_local_matchexhaustive_errors_naming_missing_member() {
    // The asserted counterpart: same missing `#west`, `Error` not `Warning`,
    // reached through the alias instead of the spelled-out union — pins
    // that `matchExhaustive:` "sees" exactly the same residual type either
    // way.
    let source = r"
type Direction = #north | #south | #east | #west

heading :: Direction := #east
heading matchExhaustive: [
  #north -> 0;
  #south -> 180;
  #east -> 90
]
";
    let diags = analyse_diagnostics(source);
    let errors: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("matchExhaustive:"))
        .collect();
    assert_eq!(errors.len(), 1, "expected one error, got: {diags:?}");
    assert_eq!(errors[0].severity, Severity::Error);
    assert!(errors[0].message.contains("#west"));
    assert!(errors[0].message.contains("residual type"));
}

#[test]
fn alias_typed_local_matchexhaustive_silent_on_full_coverage() {
    let source = r"
type Direction = #north | #south | #east | #west

heading :: Direction := #east
heading matchExhaustive: [
  #north -> 0;
  #south -> 180;
  #east -> 90;
  #west -> 270
]
";
    let diags = analyse_diagnostics(source);
    let errors: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("matchExhaustive:"))
        .collect();
    assert!(
        errors.is_empty(),
        "full coverage through an alias must satisfy matchExhaustive: silently, got: {diags:?}"
    );
}

#[test]
fn alias_typed_local_matchexhaustive_unguarded_wildcard_satisfies() {
    let source = r"
type Direction = #north | #south | #east | #west

heading :: Direction := #west
heading matchExhaustive: [
  #north -> 0;
  _ -> -1
]
";
    let diags = analyse_diagnostics(source);
    let errors: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("matchExhaustive:"))
        .collect();
    assert!(
        errors.is_empty(),
        "unguarded wildcard must satisfy the assertion through an alias too, got: {diags:?}"
    );
}

// ── Method parameter / return type positions (ADR 0108's flagship examples) ──

#[test]
fn alias_typed_method_param_matchexhaustive_behaves_like_spelled_out_union() {
    // ADR 0108's motivating signature shape: `restart: policy :: Direction`.
    let source = r"
type Direction = #north | #south | #east | #west

Object subclass: Compass
  heading: h :: Direction =>
    h matchExhaustive: [
      #north -> 0;
      #south -> 180;
      #east -> 90
    ]
";
    let diags = analyse_diagnostics(source);
    let errors: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("matchExhaustive:"))
        .collect();
    assert_eq!(errors.len(), 1, "expected one error, got: {diags:?}");
    assert!(errors[0].message.contains("#west"));
}

#[test]
fn alias_typed_method_return_resolves_to_structural_union() {
    // ADR 0108's other motivating shape: `defaultHeading -> Direction`. A
    // mismatched return (returning a value outside the union) must be
    // caught exactly as it would be against the spelled-out union — proving
    // `check_return_type` (validation.rs) also expands the alias, not just
    // parameter positions.
    let source = r"
type Direction = #north | #south | #east | #west

Object subclass: Compass
  defaultHeading -> Direction => 42
";
    let diags = analyse_diagnostics(source);
    let type_mismatches: Vec<_> = diags
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect();
    assert!(
        !type_mismatches.is_empty(),
        "returning Integer from a Direction-declared method should warn, got: {diags:?}"
    );
}

// ── Namespace collision surfaces from the full pipeline ──────────────────

#[test]
fn alias_colliding_with_class_reports_diagnostic_end_to_end() {
    let source = r"
type Foo = String

Object subclass: Foo
";
    let diags = analyse_diagnostics(source);
    assert!(
        diags
            .iter()
            .any(|d| d.message.contains("already defined as a class")),
        "expected a namespace-collision diagnostic, got: {diags:?}"
    );
}

// ── Type parameter bounds see through an alias-named type argument ───────
//
// Not exhaustiveness, but the same underlying fix: `check_type_param_bounds_
// in_module` (protocol.rs) used to resolve a `Generic`'s type args through
// the thin, registry-less `Self::resolve_type_annotation` wrapper, so an
// alias-named type argument (e.g. `Logger(NotFrobnicatable)`) resolved to an
// opaque unknown class. `receiver_knowledge::classify_receiver` treats an
// unknown class as open-world and conforms by default (ADR 0100 Rule 1), so
// the bound violation was silently skipped — a false negative, not a false
// positive, which is why it needed its own end-to-end regression test rather
// than only a unit test on the resolver.

#[test]
fn alias_named_type_arg_is_checked_against_declared_bound() {
    let source = r"
Protocol define: Frobnicatable
  frobnicate -> Object

type NotFrobnicatable = Integer

Actor subclass: Logger(T :: Frobnicatable)
  log: item :: T => item

Object subclass: Holder
  state: thing :: Logger(NotFrobnicatable) = nil
";
    let diags = analyse_diagnostics(source);
    let bound_violations: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("does not conform to"))
        .collect();
    assert_eq!(
        bound_violations.len(),
        1,
        "NotFrobnicatable expands to Integer, which lacks `frobnicate` — expected exactly \
         one bound-violation diagnostic, got: {diags:?}"
    );
    assert!(bound_violations[0].message.contains("Integer"));
    assert!(bound_violations[0].message.contains("Frobnicatable"));
}
