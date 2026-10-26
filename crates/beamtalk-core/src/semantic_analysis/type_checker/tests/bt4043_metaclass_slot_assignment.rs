// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4043: assigning an `X class` value to a non-nilable `X class` slot
//! must not warn "Type mismatch: declared as X class, got X class".
//!
//! `check_field_assignment`'s `Meta` arm modeled every metatype value as
//! the tower root `Class` (ADR 0083) before the assignability walk —
//! correct for `:: Class` / `:: Behaviour` / `:: Object` slots, but a
//! nominal `X class` declared type is an unknown class to that walk, so
//! a matching `X class` value falsely warned. Locals never warned
//! because both sides resolve to `Meta{X}` and compare equal there,
//! and `| Nil` hid it because the union-declared arm uses
//! `is_type_compatible` (which has an unknown-class escape hatch).
//! The fix accepts a value whose own `M class` spelling exactly matches
//! the declared slot type, keeping the tower walk for everything else.

use super::common::*;
use crate::test_helpers::test_support::parse_ok;

fn check_source(source: &str) -> Vec<crate::source_analysis::Diagnostic> {
    let module = parse_ok(source);
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

/// The issue's repro shape: `self.kind := Integer` on a
/// `state: kind :: Integer class` slot must not warn.
#[test]
fn bt4043_state_slot_accepts_matching_metaclass_value() {
    let diags = check_source(
        "typed Actor subclass: Holder\n\
         \x20 state: kind :: Integer class\n\
         \x20 setKind -> Object =>\n\
         \x20   self.kind := Integer\n",
    );
    let mismatches = type_mismatches(&diags);
    assert!(
        mismatches.is_empty(),
        "matching `X class` value on a `state:` slot should not warn, got: {mismatches:?} (all: {diags:?})"
    );
}

/// Same acceptance for a `late` slot: `late state: kind :: Integer class`.
#[test]
fn bt4043_late_state_slot_accepts_matching_metaclass_value() {
    let diags = check_source(
        "typed Actor subclass: Holder\n\
         \x20 late state: kind :: Integer class\n\
         \x20 setKind -> Object =>\n\
         \x20   self.kind := Integer\n",
    );
    let mismatches = type_mismatches(&diags);
    assert!(
        mismatches.is_empty(),
        "matching `X class` value on a `late state:` slot should not warn, got: {mismatches:?} (all: {diags:?})"
    );
}

/// Same acceptance for a class-side slot: `classState: kind :: Integer class`.
#[test]
fn bt4043_class_state_slot_accepts_matching_metaclass_value() {
    let diags = check_source(
        "typed Object subclass: Holder\n\
         \x20 classState: kind :: Integer class\n\
         \x20 class setKind -> Object =>\n\
         \x20   self.kind := Integer\n",
    );
    let mismatches = type_mismatches(&diags);
    assert!(
        mismatches.is_empty(),
        "matching `X class` value on a `classState:` slot should not warn, got: {mismatches:?} (all: {diags:?})"
    );
}

/// Same acceptance for a Value field: `field: kind :: Integer class`.
/// (Value slot assignment is rejected by the validator pass, not the
/// type checker, so only `Type mismatch` absence is asserted here.)
#[test]
fn bt4043_field_slot_accepts_matching_metaclass_value() {
    let diags = check_source(
        "typed Value subclass: Holder\n\
         \x20 field: kind :: Integer class\n\
         \x20 setKind -> Object =>\n\
         \x20   self.kind := Integer\n",
    );
    let mismatches = type_mismatches(&diags);
    assert!(
        mismatches.is_empty(),
        "matching `X class` value on a `field:` slot should not warn, got: {mismatches:?} (all: {diags:?})"
    );
}

/// The fix must not silence the arm: a *different* metatype value on an
/// `X class` slot still warns, naming both metatype spellings.
#[test]
fn bt4043_mismatched_metaclass_value_still_warns() {
    let diags = check_source(
        "typed Actor subclass: Holder\n\
         \x20 state: kind :: Integer class\n\
         \x20 setKind -> Object =>\n\
         \x20   self.kind := String\n",
    );
    let mismatches = type_mismatches(&diags);
    assert_eq!(
        mismatches.len(),
        1,
        "mismatched `X class` value should warn exactly once, got: {mismatches:?} (all: {diags:?})"
    );
    assert!(
        mismatches[0]
            .message
            .contains("declared as Integer class, got String class"),
        "warning should name both metatypes, got: {}",
        mismatches[0].message
    );
}

/// Pin the pre-existing ADR 0083 behavior the edited arm also owns: a
/// metatype value still satisfies a `:: Class` slot via the tower walk.
#[test]
fn bt4043_class_declared_slot_still_accepts_metaclass_value() {
    let diags = check_source(
        "typed Actor subclass: Holder\n\
         \x20 state: anything :: Class\n\
         \x20 setKind -> Object =>\n\
         \x20   self.anything := Integer\n",
    );
    let mismatches = type_mismatches(&diags);
    assert!(
        mismatches.is_empty(),
        "`X class` value on a `:: Class` slot should not warn, got: {mismatches:?} (all: {diags:?})"
    );
}
