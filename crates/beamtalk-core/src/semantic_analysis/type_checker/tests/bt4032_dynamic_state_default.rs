// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4032: `field: x :: Dynamic = <literal>` must not warn
//! "Type mismatch: declared as Dynamic, default is Integer".
//!
//! An explicit `Dynamic` annotation accepts any default — it is the
//! documented way to opt a slot out of checking in a `typed` class
//! (ADR 0077). `check_state_defaults` compared the default's inferred
//! type against the declared type with `is_assignable_to`, which has
//! no unknown-class escape hatch (unlike `is_type_compatible`), so a
//! declared `Dynamic` rejected every `Known` literal default while a
//! class-reference default (inferred `Meta`, skipped conservatively)
//! stayed silent.

use super::common::*;

fn check_source(source: &str) -> Vec<crate::source_analysis::Diagnostic> {
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    checker.take_diagnostics()
}

fn type_mismatches(
    diags: &[crate::source_analysis::Diagnostic],
) -> Vec<&crate::source_analysis::Diagnostic> {
    diags
        .iter()
        .filter(|d| d.message.contains("Type mismatch"))
        .collect()
}

/// The issue's repro shape: Integer, Nil, String and Symbol literal
/// defaults on `:: Dynamic` fields are all accepted.
#[test]
fn bt4032_dynamic_fields_accept_any_literal_default() {
    let diags = check_source(
        "typed Value subclass: Dv\n\
         \x20 field: t :: Dynamic = 3\n\
         \x20 field: h :: Dynamic = nil\n\
         \x20 field: s :: Dynamic = \"hi\"\n\
         \x20 field: y :: Dynamic = #sym\n",
    );
    let mismatches = type_mismatches(&diags);
    assert!(
        mismatches.is_empty(),
        "explicit Dynamic fields should accept any default, got: {mismatches:?} (all: {diags:?})"
    );
}

/// Same exemption for the `state:` keyword: a `:: Dynamic` state slot
/// in a typed Actor accepts literal defaults too.
#[test]
fn bt4032_dynamic_state_accepts_literal_default() {
    let diags = check_source(
        "typed Actor subclass: DvActor\n\
         \x20 state: t :: Dynamic = 3\n\
         \x20 state: h :: Dynamic = nil\n",
    );
    let mismatches = type_mismatches(&diags);
    assert!(
        mismatches.is_empty(),
        "explicit Dynamic state should accept any default, got: {mismatches:?} (all: {diags:?})"
    );
}

/// Control: a non-Dynamic declared type still rejects a mismatched
/// literal default — the Dynamic exemption must not suppress real
/// mismatches.
#[test]
fn bt4032_non_dynamic_mismatch_still_warns() {
    let diags = check_source(
        "typed Value subclass: DvControl\n\
         \x20 field: c :: Integer = \"bad\"\n",
    );
    let mismatches = type_mismatches(&diags);
    assert_eq!(
        mismatches.len(),
        1,
        "expected 1 type mismatch for Integer field with String default, got: {diags:?}"
    );
    assert!(mismatches[0].message.contains("`c`"));
}
