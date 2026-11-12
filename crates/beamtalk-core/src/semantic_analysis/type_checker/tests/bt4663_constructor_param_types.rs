// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4663: the synthesized keyword constructor checks its arguments against
//! the fields' declared types, like the synthesized `with<Field>:` copy.
//!
//! On a generic class a field typed `T` is pinned here: a class-side send has
//! no receiver type arguments, so the constructor must not reject any value
//! for it (see `generic_field_is_not_flagged`).

use super::common::*;

fn arg_mismatches(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| {
            d.category == Some(DiagnosticCategory::Type)
                && d.message.contains("expects")
                && d.message.contains("got")
        })
        .collect()
}

const POINT: &str = "\
Value subclass: Point\n\
  field: x :: Integer = 0\n\
  field: y :: Integer = 0\n";

#[test]
fn mistyped_keyword_constructor_argument_warns() {
    let diags = diags_with_expect(&format!(
        "{POINT}Object subclass: User\n  run => Point x: \"a\" y: 0\n"
    ));
    assert_eq!(arg_mismatches(&diags).len(), 1, "{diags:#?}");
}

#[test]
fn well_typed_keyword_constructor_does_not_warn() {
    let diags = diags_with_expect(&format!(
        "{POINT}Object subclass: User\n  run => Point x: 1 y: 2\n"
    ));
    assert!(arg_mismatches(&diags).is_empty(), "{diags:#?}");
}

#[test]
fn generic_field_is_not_flagged() {
    let diags = diags_with_expect(
        "Value subclass: Box(T)\n  field: item :: T = nil\n\
         Object subclass: User\n  run => Box item: \"a\"\n",
    );
    assert!(arg_mismatches(&diags).is_empty(), "{diags:#?}");
}

/// A generic class's `T` field stays unchecked, but its concrete fields are
/// still checked: exactly one mismatch, on `count:`.
#[test]
fn concrete_field_on_generic_class_is_still_checked() {
    let diags = diags_with_expect(
        "Value subclass: Box(T)\n  field: item :: T = nil\n  field: count :: Integer = 0\n\
         Object subclass: User\n  run => Box item: \"a\" count: \"b\"\n",
    );
    let hits = arg_mismatches(&diags);
    assert_eq!(hits.len(), 1, "{diags:#?}");
    assert!(hits[0].message.contains("Argument 2"), "{hits:#?}");
}
