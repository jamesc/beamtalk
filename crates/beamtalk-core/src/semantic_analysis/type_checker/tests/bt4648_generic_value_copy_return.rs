// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4648: a synthesized `with<Field>:` copy-setter on a generic value class
//! keeps the receiver's type arguments.
//!
//! `synthetic_selectors::synthesized_copy_return_type` used to record the
//! bare class (`Box`), so on `b :: Box(Integer)` the copy `b withItem: 3`
//! inferred as plain `Box` and `(b withItem: 3) item` lost `Integer`. It is
//! now `Self`, which resolves to the full receiver type. The keyword
//! constructor stays the defining class
//! (`synthetic_selectors::synthesized_constructor_return_type`): it always
//! builds that class, even when a subclass inherits it.

use super::common::*;

fn return_mismatches(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| {
            d.category == Some(DiagnosticCategory::Type)
                && d.message.contains("declares return type")
        })
        .collect()
}

const BOX: &str = "\
Value subclass: Box(T)\n\
  field: item :: T = nil\n";

/// Control: the synthesized getter on `Box(Integer)` answers `Integer`.
#[test]
fn getter_on_parameterized_receiver_answers_type_argument() {
    let diags = diags_with_expect(&format!(
        "{BOX}Object subclass: User\n  run: b :: Box(Integer) -> String => b item\n"
    ));
    let hits = return_mismatches(&diags);
    assert_eq!(hits.len(), 1, "{diags:#?}");
    assert!(
        hits[0].message.contains("body returns Integer"),
        "{hits:#?}"
    );
}

/// The copy keeps `Box(Integer)`, so its getter still answers `Integer`.
#[test]
fn with_star_copy_keeps_type_arguments() {
    let diags = diags_with_expect(&format!(
        "{BOX}Object subclass: User\n  run: b :: Box(Integer) -> String => (b withItem: 3) item\n"
    ));
    let hits = return_mismatches(&diags);
    assert_eq!(hits.len(), 1, "{diags:#?}");
    assert!(
        hits[0].message.contains("body returns Integer"),
        "{hits:#?}"
    );
}

/// The copy itself is `Box(Integer)`, not bare `Box`.
#[test]
fn with_star_copy_is_the_receiver_type() {
    let diags = diags_with_expect(&format!(
        "{BOX}Object subclass: User\n  run: b :: Box(Integer) -> String => b withItem: 3\n"
    ));
    let hits = return_mismatches(&diags);
    assert_eq!(hits.len(), 1, "{diags:#?}");
    assert!(
        hits[0].message.contains("body returns Box(Integer)"),
        "{hits:#?}"
    );
}

/// The class-side keyword constructor still answers an instance of the
/// class (`Self` on a class-side send is the instance type).
#[test]
fn keyword_constructor_answers_the_class() {
    let diags = diags_with_expect(&format!(
        "{BOX}Object subclass: User\n  run -> String => Box item: 3\n"
    ));
    let hits = return_mismatches(&diags);
    assert_eq!(hits.len(), 1, "{diags:#?}");
    assert!(hits[0].message.contains("body returns Box"), "{hits:#?}");
}

/// A non-generic value class's copy is still the class itself.
#[test]
fn with_star_on_plain_value_class_answers_the_class() {
    let diags = diags_with_expect(
        "Value subclass: Point\n  field: x :: Integer = 0\n\
         Object subclass: User\n  run: p :: Point -> String => p withX: 3\n",
    );
    let hits = return_mismatches(&diags);
    assert_eq!(hits.len(), 1, "{diags:#?}");
    assert!(hits[0].message.contains("body returns Point"), "{hits:#?}");
}

const BOX_AND_SUB: &str = "\
Value subclass: Box(T)\n\
  field: item :: T = nil\n\
Box subclass: Sub\n\
  subOnly -> Integer => 1\n";

/// A keyword constructor inherited by a subclass still builds an instance
/// of the *defining* class (its codegen ignores the class-self argument),
/// so `Sub item: 3` is a `Box`, not a `Sub`: `subOnly` on it is a DNU.
#[test]
fn inherited_keyword_constructor_answers_the_defining_class() {
    let diags = diags_with_expect(&format!(
        "{BOX_AND_SUB}Object subclass: User\n  run -> String => Sub item: 3\n"
    ));
    let hits = return_mismatches(&diags);
    assert_eq!(hits.len(), 1, "{diags:#?}");
    assert!(hits[0].message.contains("body returns Box"), "{hits:#?}");
}

/// `with*:` on a subclass instance keeps the subclass: the copy is
/// `maps:put` on the receiver, which keeps its `$beamtalk_class`.
#[test]
fn with_star_on_subclass_instance_answers_the_subclass() {
    let diags = diags_with_expect(&format!(
        "{BOX_AND_SUB}Object subclass: User\n  run: s :: Sub -> String => s withItem: 3\n"
    ));
    let hits = return_mismatches(&diags);
    assert_eq!(hits.len(), 1, "{diags:#?}");
    assert!(hits[0].message.contains("body returns Sub"), "{hits:#?}");
}

fn argument_mismatches(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type) && d.message.contains("expects"))
        .collect()
}

/// With the copy typed `Box(Integer)`, its argument is checked against the
/// substituted field type: a `String` into `Box(Integer)` warns, so
/// `(b withItem: 'hi') item` can't silently infer `Integer`.
#[test]
fn with_star_argument_checked_against_type_argument() {
    let diags = diags_with_expect(&format!(
        "{BOX}Object subclass: User\n  run: b :: Box(Integer) => b withItem: \"hi\"\n"
    ));
    let hits = argument_mismatches(&diags);
    assert_eq!(hits.len(), 1, "{diags:#?}");
    assert!(
        hits[0].message.contains("Integer") && hits[0].message.contains("String"),
        "{hits:#?}"
    );
}

/// A matching argument passes.
#[test]
fn with_star_matching_argument_no_warning() {
    let diags = diags_with_expect(&format!(
        "{BOX}Object subclass: User\n  run: b :: Box(Integer) => b withItem: 3\n"
    ));
    assert!(argument_mismatches(&diags).is_empty(), "{diags:#?}");
}

/// A non-generic class checks the argument against the field's own type.
#[test]
fn with_star_argument_checked_on_plain_value_class() {
    let diags = diags_with_expect(
        "Value subclass: Point\n  field: x :: Integer = 0\n\
         Object subclass: User\n  run: p :: Point => p withX: \"a\"\n",
    );
    assert_eq!(argument_mismatches(&diags).len(), 1, "{diags:#?}");
}
