// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5241: since ADR 0153 decision 7 a class has no fields, so a class-side
//! `fieldAt:` / `fieldAt:put:` / `hasField:` / `clearField:` raises
//! `does_not_understand` at run time unless the class defines a class method
//! of that name. The checker must say so, not resolve it through
//! `Class -> Behaviour -> Object`.

use super::common::*;

fn dnus(source: &str) -> Vec<String> {
    let mut module = parse_source(source);
    let _ = crate::compilation::fold_same_file_extensions(&mut module);
    let hierarchy = ClassHierarchy::build(&module).0;
    run_with_expect(&module, &hierarchy)
        .into_iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Dnu))
        .map(|d| d.message.to_string())
        .collect()
}

const SENDS: [(&str, &str); 4] = [
    ("fieldAt: #x", "fieldAt:"),
    ("fieldAt: #x put: 1", "fieldAt:put:"),
    ("hasField: #x", "hasField:"),
    ("clearField: #x", "clearField:"),
];

#[test]
fn class_literal_receiver_warns() {
    for (send, sel) in SENDS {
        let source = format!(
            "Object subclass: Counter
  value => 1
Object subclass: User
  run => Counter {send}
"
        );
        let d = dnus(&source);
        assert_eq!(d.len(), 1, "{send}: {d:#?}");
        assert!(d[0].contains(&format!("'{sel}'")), "{send}: {d:#?}");
    }
}

#[test]
fn self_in_class_method_warns() {
    for (send, sel) in SENDS {
        let source = format!(
            "Object subclass: Counter
  class run => self {send}
"
        );
        let d = dnus(&source);
        assert_eq!(d.len(), 1, "{send}: {d:#?}");
        assert!(d[0].contains(&format!("'{sel}'")), "{send}: {d:#?}");
    }
}

#[test]
fn class_typed_value_warns() {
    for (send, sel) in SENDS {
        let source = format!(
            "Object subclass: Counter
  value => 1
Object subclass: User
  run: c :: Counter class => c {send}
"
        );
        let d = dnus(&source);
        assert_eq!(d.len(), 1, "{send}: {d:#?}");
        assert!(d[0].contains(&format!("'{sel}'")), "{send}: {d:#?}");
    }
}

#[test]
fn class_with_own_class_method_is_not_flagged() {
    let source = "Object subclass: Counter
  class fieldAt: n => n
  class fieldAt: n put: v => v
  class hasField: n => true
  class clearField: n => n
Counter subclass: Sub
Object subclass: User
  run =>
    Counter fieldAt: #x
    Counter fieldAt: #x put: 1
    Counter hasField: #x
    Counter clearField: #x
    Sub fieldAt: #x
";
    let d = dnus(source);
    assert!(d.is_empty(), "{d:#?}");
}

#[test]
fn instance_side_field_reflection_is_not_flagged() {
    let source = "Object subclass: Counter
  state: x :: Integer = 0
  peek => self fieldAt: #x
Object subclass: User
  run => Counter new fieldAt: #x
";
    let d = dnus(source);
    assert!(d.is_empty(), "{d:#?}");
}
