// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4648: a synthesized `with<Field>:` copy-setter on a generic value class
//! keeps the receiver's type arguments.
//!
//! `synthetic_selectors::synthesized_copy_return_type` used to record the
//! bare class (`Box`), so on `b :: Box(Integer)` the copy `b withItem: 3`
//! inferred as plain `Box` and `(b withItem: 3) item` lost `Integer`. It is
//! now `Self`, which resolves to the full receiver type.

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

fn check(source: &str) -> Vec<Diagnostic> {
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    run_with_expect(&module, &hierarchy)
}

const BOX: &str = "\
Value subclass: Box(T)\n\
  field: item :: T = nil\n";

/// Control: the synthesized getter on `Box(Integer)` answers `Integer`.
#[test]
fn getter_on_parameterized_receiver_answers_type_argument() {
    let diags = check(&format!(
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
    let diags = check(&format!(
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
    let diags = check(&format!(
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
    let diags = check(&format!(
        "{BOX}Object subclass: User\n  run -> String => Box item: 3\n"
    ));
    let hits = return_mismatches(&diags);
    assert_eq!(hits.len(), 1, "{diags:#?}");
    assert!(hits[0].message.contains("body returns Box"), "{hits:#?}");
}

/// A non-generic value class's copy is still the class itself.
#[test]
fn with_star_on_plain_value_class_answers_the_class() {
    let diags = check(
        "Value subclass: Point\n  field: x :: Integer = 0\n\
         Object subclass: User\n  run: p :: Point -> String => p withX: 3\n",
    );
    let hits = return_mismatches(&diags);
    assert_eq!(hits.len(), 1, "{diags:#?}");
    assert!(hits[0].message.contains("body returns Point"), "{hits:#?}");
}
