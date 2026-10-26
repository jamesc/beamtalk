// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4039: member-compatible defaults must be accepted for alias-union
//! fields, whether the alias is package-local or a stdlib alias.
//!
//! `field: a :: Timeout = 5` warned "Type mismatch: state `a` declared as
//! Timeout, default is Integer" while the same union written inline — or a
//! package-local alias — resolved fine. Root cause: the compiler-baked
//! stdlib alias snapshot (`ClassHierarchy::generated_stdlib_aliases`) had
//! no consumers, so a stdlib alias referenced from application code stayed
//! an opaque name with nothing to resolve against. `analyse_full` now
//! seeds that snapshot as the last alias-resolution tier of every
//! non-stdlib compile (mirroring `with_builtins`' always-on stdlib
//! classes and `stdlib_protocols`' last-tier `uses:` fallback).

use crate::semantic_analysis::{AnalysisContext, analyse, analyse_full};

fn analyse_diagnostics(source: &str) -> Vec<crate::source_analysis::Diagnostic> {
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    analyse(&module).diagnostics
}

fn analyse_diagnostics_as_stdlib(source: &str) -> Vec<crate::source_analysis::Diagnostic> {
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let ctx = AnalysisContext {
        current_package: Some(crate::semantic_analysis::stdlib_protocols::STDLIB_PACKAGE),
        ..AnalysisContext::default()
    };
    analyse_full(&module, ctx).diagnostics
}

fn state_mismatches(
    diags: &[crate::source_analysis::Diagnostic],
) -> Vec<&crate::source_analysis::Diagnostic> {
    diags
        .iter()
        .filter(|d| d.message.contains("Type mismatch: state"))
        .collect()
}

/// The issue's repro: an Integer default on a stdlib alias-union field
/// (`Timeout = Integer | #infinity`) is accepted with no local
/// declaration or import of the alias.
#[test]
fn bt4039_integer_default_accepted_for_stdlib_alias_union_field() {
    let source = r"
Value subclass: W
  field: a :: Timeout = 5
  field: c :: ShutdownTimeout = 5
";
    let diags = analyse_diagnostics(source);
    let hits = state_mismatches(&diags);
    assert!(
        hits.is_empty(),
        "Integer defaults must be accepted for stdlib alias unions, got: {diags:?}"
    );
}

/// Same for `state:` slots: `state: timeoutMs :: Timeout = 5000`
/// (the `stdlib/src/timeout_proxy.bt` shape) draws no warning when the
/// alias resolves — here via the baked stdlib snapshot, no local `type`.
#[test]
fn bt4039_integer_default_accepted_for_stdlib_alias_union_state() {
    let source = r"
Actor subclass: W
  state: t :: Timeout = 5000
";
    let diags = analyse_diagnostics(source);
    let hits = state_mismatches(&diags);
    assert!(
        hits.is_empty(),
        "Integer default must be accepted for stdlib alias union state, got: {diags:?}"
    );
}

/// Symbol members stay accepted too — the fix must not trade one
/// member kind for another.
#[test]
fn bt4039_symbol_default_accepted_for_stdlib_alias_union() {
    let source = r"
Value subclass: W
  field: b :: Timeout = #infinity
  field: d :: ShutdownTimeout = #brutalKill
";
    let diags = analyse_diagnostics(source);
    let hits = state_mismatches(&diags);
    assert!(
        hits.is_empty(),
        "Symbol defaults must be accepted for stdlib alias unions, got: {diags:?}"
    );
}

/// Negative control: a genuinely incompatible default still warns, and
/// the diagnostic renders the stdlib alias with its expansion —
/// proving the positive cases above resolve the alias rather than
/// passing vacuously (e.g. via an unknown-class escape hatch).
#[test]
fn bt4039_mismatched_default_still_warns_with_stdlib_expansion() {
    let source = r#"
Value subclass: W
  field: t :: Timeout = "bad"
"#;
    let diags = analyse_diagnostics(source);
    let hits = state_mismatches(&diags);
    assert_eq!(
        hits.len(),
        1,
        "expected 1 mismatch for String default against Timeout, got: {diags:?}"
    );
    assert!(
        hits[0].message.contains("Timeout (Integer | #infinity)"),
        "diagnostic must render the stdlib alias expansion, got: {}",
        hits[0].message
    );
}

/// Package-local aliases keep working (the acceptance's second tier):
/// a same-module alias union accepts an Integer default.
#[test]
fn bt4039_integer_default_accepted_for_package_local_alias_union() {
    let source = r"
type LocalTimeout = Integer | #infinity

Value subclass: W
  field: t :: LocalTimeout = 5
";
    let diags = analyse_diagnostics(source);
    let hits = state_mismatches(&diags);
    assert!(
        hits.is_empty(),
        "Integer default must be accepted for package-local alias union, got: {diags:?}"
    );
}

/// A same-named local alias shadows the stdlib snapshot silently: the
/// module's own `type Timeout = String` wins, so a String default is
/// accepted and an Integer default is rejected.
#[test]
fn bt4039_local_alias_shadows_stdlib_snapshot() {
    let source = r#"
type Timeout = String

Value subclass: W
  field: s :: Timeout = "ok"
"#;
    let diags = analyse_diagnostics(source);
    let hits = state_mismatches(&diags);
    assert!(
        hits.is_empty(),
        "local alias must shadow the stdlib snapshot, got: {diags:?}"
    );

    let source = r"
type Timeout = String

Value subclass: W
  field: n :: Timeout = 5
";
    let diags = analyse_diagnostics(source);
    let hits = state_mismatches(&diags);
    assert_eq!(
        hits.len(),
        1,
        "Integer default must be rejected against shadowing `type Timeout = String`, got: {diags:?}"
    );
}

/// A same-named local class wins over the stdlib snapshot silently:
/// `:: Timeout` keeps meaning the class, so an Integer default is
/// still rejected (it is not a `Timeout` instance) instead of newly
/// accepted as a union member.
#[test]
fn bt4039_local_class_shadows_stdlib_snapshot() {
    let source = r#"
Value subclass: Timeout
  field: label :: String = "t"

Value subclass: W
  field: n :: Timeout = 5
"#;
    let diags = analyse_diagnostics(source);
    let hits = state_mismatches(&diags);
    assert_eq!(
        hits.len(),
        1,
        "local class must shadow the stdlib snapshot (Integer is not a Timeout), got: {diags:?}"
    );
    assert!(
        !hits[0].message.contains("(Integer | #infinity)"),
        "diagnostic must not render the stdlib expansion for the class, got: {}",
        hits[0].message
    );
}

/// Compiling the stdlib itself never seeds the baked snapshot (live
/// sources win): with no live `Timeout` in scope, the name stays
/// opaque and the check behaves exactly as before the fix.
#[test]
fn bt4039_stdlib_package_does_not_seed_baked_snapshot() {
    let source = r"
Value subclass: W
  field: a :: Timeout = 5
";
    let diags = analyse_diagnostics_as_stdlib(source);
    let hits = state_mismatches(&diags);
    assert_eq!(
        hits.len(),
        1,
        "stdlib package must not see the baked snapshot, got: {diags:?}"
    );
    assert!(
        hits[0].message.contains("declared as Timeout, default is"),
        "opaque name must render bare, got: {}",
        hits[0].message
    );
}
