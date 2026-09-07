// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-2734: synthetic value-accessor doc/signature metadata.

use super::*;
use beamtalk_core::ast::{
    ClassDefinition, DeclaredKeyword, Identifier, KeywordPart, MessageSelector, MethodDefinition,
    ParameterDefinition, StateDeclaration, TypeAnnotation,
};
use beamtalk_core::test_helpers::test_support::make_actor_class;

fn slot(name: &str, ty: Option<&str>) -> StateDeclaration {
    StateDeclaration {
        name: Identifier::new(name, s()),
        type_annotation: ty.map(|t| TypeAnnotation::Simple(Identifier::new(t, s()))),
        default_value: None,
        expect: None,
        comments: beamtalk_core::ast::CommentAttachment::default(),
        doc_comment: None,
        declared_keyword: DeclaredKeyword::default(),
        span: s(),
    }
}

fn value_class(name: &str, slots: Vec<StateDeclaration>) -> ClassDefinition {
    ClassDefinition::new(
        Identifier::new(name, s()),
        Identifier::new("Value", s()),
        slots,
        vec![],
        s(),
    )
}

fn find_entry<'a>(
    entries: &'a [SyntheticAccessorEntry],
    selector: &str,
) -> &'a SyntheticAccessorEntry {
    entries
        .iter()
        .find(|(sel, _, _)| sel == selector)
        .unwrap_or_else(|| panic!("no synthetic entry for {selector}"))
}

#[test]
fn test_synthetic_getter_signature_and_doc() {
    let class = value_class("Point", vec![slot("x", Some("Integer"))]);
    let entries = CoreErlangGenerator::synthetic_value_accessor_entries(&class);
    let (_, sig, doc) = find_entry(&entries.instance, "x");
    assert_eq!(sig, "x -> Integer");
    assert_eq!(
        doc,
        "Compiler-derived accessor. Returns the value of slot `x`."
    );
}

#[test]
fn test_synthetic_setter_signature_and_doc() {
    let class = value_class("Point", vec![slot("x", Some("Integer"))]);
    let entries = CoreErlangGenerator::synthetic_value_accessor_entries(&class);
    let (_, sig, doc) = find_entry(&entries.instance, "withX:");
    assert_eq!(sig, "withX: aValue -> Point");
    assert_eq!(
        doc,
        "Compiler-derived copy-setter. Returns a copy with slot `x` replaced."
    );
}

#[test]
fn test_synthetic_keyword_constructor_is_class_side() {
    let class = value_class(
        "Point",
        vec![slot("x", Some("Integer")), slot("y", Some("Integer"))],
    );
    let entries = CoreErlangGenerator::synthetic_value_accessor_entries(&class);
    assert_eq!(entries.class.len(), 1, "one keyword constructor entry");
    let (sel, sig, doc) = &entries.class[0];
    assert_eq!(sel, "x:y:");
    assert_eq!(sig, "x: x y: y -> Point");
    assert_eq!(
        doc,
        "Compiler-derived keyword constructor. Returns a new Point from the given slot values."
    );
}

#[test]
fn test_synthetic_keyword_constructor_long_selector_is_hashed() {
    // Regression guard alongside BT-1408's `value_many_fields.bt` fixture: a
    // Value class with enough long field names that the raw keyword-
    // constructor selector exceeds Erlang's 255-char atom limit must not
    // surface that raw selector as a `classMethodSignatures`/
    // `classMethodDocs` map *key* atom (it would blow the limit exactly like
    // the dispatch function name did before BT-1408). The key must match
    // `safe_class_method_selector`, the same hash the runtime meta entry and
    // dispatch already use for this selector — the doc/signature *text* still
    // carries the full readable field names since it is a binary, not atom.
    let long_field = "a".repeat(60);
    let field_names: Vec<String> = (0..5).map(|i| format!("{long_field}{i}")).collect();
    let slots: Vec<StateDeclaration> = field_names
        .iter()
        .map(|n| slot(n, Some("Integer")))
        .collect();
    let class = value_class("Big", slots);
    let entries = CoreErlangGenerator::synthetic_value_accessor_entries(&class);
    assert_eq!(entries.class.len(), 1, "one keyword constructor entry");
    let (sel, sig, _doc) = &entries.class[0];
    assert!(
        sel.len() <= 255,
        "keyword constructor map key must stay within the atom limit, got {} bytes",
        sel.len()
    );
    let raw_kw_sel = beamtalk_core::synthetic_selectors::keyword_constructor_selector(
        field_names.iter().map(String::as_str),
    );
    assert_eq!(
        *sel,
        crate::core_erlang::selector_mangler::safe_class_method_selector(&raw_kw_sel),
        "map key must match the same hash the runtime meta entry/dispatch use"
    );
    // The signature text keeps the full readable field names (a binary, not
    // an atom, so it carries no length limit).
    assert!(sig.contains(&format!("{long_field}0")));
}

#[test]
fn test_synthetic_untyped_slot_falls_back_to_object() {
    let class = value_class("Box", vec![slot("v", None)]);
    let entries = CoreErlangGenerator::synthetic_value_accessor_entries(&class);
    let (_, sig, _) = find_entry(&entries.instance, "v");
    assert_eq!(sig, "v -> Object");
}

#[test]
fn test_synthetic_entries_empty_for_non_value_class() {
    let class = make_actor_class("Counter");
    let entries = CoreErlangGenerator::synthetic_value_accessor_entries(&class);
    assert!(
        entries.instance.is_empty() && entries.class.is_empty(),
        "actor classes get no synthetic value accessors"
    );
}

#[test]
fn test_synthetic_skips_user_overridden_getter() {
    // A user-defined `x` getter shadows the auto getter, but the auto
    // `withX:` copy-setter is still synthesized.
    let mut class = value_class("Point", vec![slot("x", Some("Integer"))]);
    class.methods.push(simple_unary_method("x"));
    let entries = CoreErlangGenerator::synthetic_value_accessor_entries(&class);
    assert!(
        entries.instance.iter().all(|(sel, _, _)| sel != "x"),
        "user-defined getter must not be re-synthesized"
    );
    assert!(
        entries.instance.iter().any(|(sel, _, _)| sel == "withX:"),
        "the copy-setter is still auto-generated"
    );
}

#[test]
fn test_synthetic_skips_user_overridden_keyword_constructor() {
    // A user-defined `x:y:` class-side method shadows the auto keyword
    // constructor, so no synthetic class-side entry is injected.
    let mut class = value_class(
        "Point",
        vec![slot("x", Some("Integer")), slot("y", Some("Integer"))],
    );
    class.class_methods.push({
        let mut m = MethodDefinition::new(
            MessageSelector::Keyword(vec![
                KeywordPart::new("x:", s()),
                KeywordPart::new("y:", s()),
            ]),
            vec![
                ParameterDefinition::new(Identifier::new("x", s())),
                ParameterDefinition::new(Identifier::new("y", s())),
            ],
            vec![bare(Expression::Literal(Literal::Integer(42), s()))],
            s(),
        );
        m.is_class_method = true;
        m
    });
    let entries = CoreErlangGenerator::synthetic_value_accessor_entries(&class);
    assert!(
        entries.class.is_empty(),
        "user-defined keyword constructor must suppress the synthetic class-side entry"
    );
}
