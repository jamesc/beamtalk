// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

use super::{DispatchSpec, class_has_catch_all_dnu, generate_has_method_from_spec};
use crate::core_erlang::value_accessors::AutoSlotMethods;
use beamtalk_core::ast::{
    ClassDefinition, ClassKind, CommentAttachment, Expression, ExpressionStatement, Identifier,
    KeywordPart, MessageSelector, MethodDefinition, MethodKind, ParameterDefinition,
};
use beamtalk_core::source_analysis::Span;

fn s() -> Span {
    Span::new(0, 0)
}

fn bare(expr: Expression) -> ExpressionStatement {
    ExpressionStatement::bare(expr)
}

/// A minimal `ClassDefinition` — only the fields `class_has_catch_all_dnu`
/// inspects (`methods`) vary between tests; everything else is a fixed,
/// unused stub.
fn class_with_methods(methods: Vec<MethodDefinition>) -> ClassDefinition {
    ClassDefinition {
        name: Identifier::new("Fixture", s()),
        superclass: Some(Identifier::new("Object", s())),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods,
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        span: s(),
    }
}

fn dnu_method(body: Vec<ExpressionStatement>) -> MethodDefinition {
    MethodDefinition {
        selector: MessageSelector::Keyword(vec![
            KeywordPart::new("doesNotUnderstand:", s()),
            KeywordPart::new("args:", s()),
        ]),
        parameters: vec![
            ParameterDefinition {
                name: Identifier::new("selector", s()),
                type_annotation: None,
            },
            ParameterDefinition {
                name: Identifier::new("args", s()),
                type_annotation: None,
            },
        ],
        body,
        return_type: None,
        is_sealed: false,
        is_internal: false,
        is_class_method: false,
        kind: MethodKind::Primary,
        expect: None,
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: s(),
    }
}

// ── class_has_catch_all_dnu ─────────────────────────────────────────────

#[test]
fn catch_all_dnu_true_for_unquoted_primitive_body() {
    // BT-1763 shape: `doesNotUnderstand: selector args: arguments =>
    // @intrinsic someName` — a single, unquoted (bare-identifier) primitive.
    let class = class_with_methods(vec![dnu_method(vec![bare(Expression::Primitive {
        name: "erlangModuleLookup".into(),
        is_quoted: false,
        is_intrinsic: true,
        is_inferred: false,
        span: s(),
    })])]);
    assert!(class_has_catch_all_dnu(&class));
}

#[test]
fn catch_all_dnu_false_for_quoted_primitive_body() {
    // ProtoObject's own default DNU (`@primitive "doesNotUnderstand:args:"`)
    // is quoted and raises does_not_understand — not a catch-all.
    let class = class_with_methods(vec![dnu_method(vec![bare(Expression::Primitive {
        name: "doesNotUnderstand:args:".into(),
        is_quoted: true,
        is_intrinsic: false,
        is_inferred: false,
        span: s(),
    })])]);
    assert!(!class_has_catch_all_dnu(&class));
}

#[test]
fn catch_all_dnu_false_for_regular_beamtalk_body() {
    // TimeoutProxy-style forwarding DNU handler: a real (non-primitive)
    // Beamtalk method body — not a structural intrinsic, so not a catch-all
    // for has_method/1 purposes even though it *does* handle every selector
    // at message-send time via the runtime's own method-table check.
    let class = class_with_methods(vec![dnu_method(vec![bare(Expression::Identifier(
        Identifier::new("self", s()),
    ))])]);
    assert!(!class_has_catch_all_dnu(&class));
}

#[test]
fn catch_all_dnu_false_when_no_dnu_method_defined() {
    let class = class_with_methods(vec![]);
    assert!(!class_has_catch_all_dnu(&class));
}

// ── generate_has_method_from_spec ───────────────────────────────────────

#[test]
fn dnu_spec_short_circuits_regardless_of_other_fields() {
    let doc = generate_has_method_from_spec(
        &["ignoredMethod".to_string()],
        &DispatchSpec {
            reflection: &["class"],
            class_name: "Proxy",
            superclass: Some("bt@stdlib@actor"),
            dnu: true,
            auto_slots: None,
        },
    );
    let output = doc.to_pretty_string();
    assert_eq!(
        output,
        "'has_method'/1 = fun (_Selector) ->\n    'true'\n\n"
    );
}

#[test]
fn root_class_with_no_superclass_falls_through_to_false() {
    let doc = generate_has_method_from_spec(
        &["increment".to_string()],
        &DispatchSpec {
            reflection: &["class", "respondsTo:"],
            class_name: "Object",
            superclass: None,
            dnu: false,
            auto_slots: None,
        },
    );
    let output = doc.to_pretty_string();
    assert!(output.contains("'increment'"), "Got:\n{output}");
    assert!(output.contains("'class'"), "Got:\n{output}");
    assert!(
        output.contains("<'false'> when 'true' -> 'false'"),
        "no superclass to delegate to. Got:\n{output}"
    );
    assert!(
        !output.contains(":'has_method'(Selector)"),
        "must not emit a delegation call with no superclass. Got:\n{output}"
    );
}

#[test]
fn subclass_delegates_to_superclass_module_on_false() {
    let doc = generate_has_method_from_spec(
        &["increment".to_string()],
        &DispatchSpec {
            reflection: &[],
            class_name: "Counter",
            superclass: Some("bt@stdlib@actor"),
            dnu: false,
            auto_slots: None,
        },
    );
    let output = doc.to_pretty_string();
    assert!(
        output.contains("call 'bt@stdlib@actor':'has_method'(Selector)"),
        "Got:\n{output}"
    );
}

#[test]
fn always_checks_extension_registry_before_delegating() {
    let doc = generate_has_method_from_spec(
        &[],
        &DispatchSpec {
            reflection: &[],
            class_name: "Counter",
            superclass: None,
            dnu: false,
            auto_slots: None,
        },
    );
    let output = doc.to_pretty_string();
    assert!(
        output.contains("call 'beamtalk_extensions':'has'('Counter', Selector)"),
        "Got:\n{output}"
    );
}

#[test]
fn auto_slot_getters_and_setters_are_listed() {
    let auto = AutoSlotMethods {
        getters: vec!["x".to_string()],
        setters: vec!["y".to_string()],
        keyword_constructor: None,
    };
    let doc = generate_has_method_from_spec(
        &[],
        &DispatchSpec {
            reflection: &[],
            class_name: "Point",
            superclass: None,
            dnu: false,
            auto_slots: Some(&auto),
        },
    );
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'x'"),
        "getter selector missing. Got:\n{output}"
    );
    assert!(
        output.contains("'withY:'"),
        "with*: setter selector missing. Got:\n{output}"
    );
}
