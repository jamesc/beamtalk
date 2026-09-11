// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Abstract-class and `native:`-class instantiation diagnostics:
//! `new`/`spawn` on abstract classes, `Object new`, and the
//! opaque-native-class `new` rejection.

use super::*;

// --- Validator coverage tests ─────────────────────────────────────────────

/// Helper: create an abstract class definition for validator tests.
fn make_abstract_class(name: &str) -> ClassDefinition {
    ClassDefinition {
        name: Identifier::new(name, test_span()),
        superclass: Some(Identifier::new("Actor", test_span())),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: true,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        span: test_span(),
    }
}

/// Helper: create a `ClassName spawn` message send.
fn make_spawn_expr(class_name: &str) -> Expression {
    Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new(class_name, test_span()),
            span: test_span(),
            package: None,
        }),
        selector: MessageSelector::Unary("spawn".into()),
        arguments: vec![],
        is_cast: false,
        span: test_span(),
    }
}

#[test]
fn test_abstract_instantiation_in_class_method() {
    use crate::ast::MethodKind;

    // abstract instantiation inside a class-side method should be detected
    let mut shape = make_abstract_class("Shape");
    shape.class_methods.push(MethodDefinition {
        selector: MessageSelector::Unary("create".into()),
        parameters: vec![],
        body: vec![bare(make_spawn_expr("Shape"))],
        return_type: None,
        is_sealed: false,
        is_internal: false,
        is_class_method: true,
        kind: MethodKind::Primary,
        expect: None,
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: test_span(),
    });

    let module = Module {
        classes: vec![shape],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let result = analyse(&module);
    let abstract_errors: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("Cannot instantiate abstract class"))
        .collect();

    assert_eq!(
        abstract_errors.len(),
        1,
        "Should detect abstract instantiation in class method, got: {:?}",
        result.diagnostics
    );
    assert!(abstract_errors[0].message.contains("Shape"));
}

#[test]
fn test_abstract_instantiation_in_string_interpolation() {
    // abstract instantiation inside string interpolation should be detected
    let shape = make_abstract_class("Shape");

    let interp = Expression::StringInterpolation {
        segments: vec![
            StringSegment::Literal("result: ".into()),
            StringSegment::Interpolation(make_spawn_expr("Shape")),
        ],
        span: test_span(),
    };

    let module = Module {
        classes: vec![shape],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![bare(interp)],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let result = analyse(&module);
    let abstract_errors: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("Cannot instantiate abstract class"))
        .collect();

    assert_eq!(
        abstract_errors.len(),
        1,
        "Should detect abstract instantiation in string interpolation, got: {:?}",
        result.diagnostics
    );
    assert!(abstract_errors[0].message.contains("Shape"));
}

#[test]
fn test_abstract_instantiation_in_standalone_method() {
    use crate::ast::{MethodKind, StandaloneMethodDefinition};

    // abstract instantiation inside a standalone method definition should be detected
    let shape = make_abstract_class("Shape");

    let standalone = StandaloneMethodDefinition {
        class_name: Identifier::new("Foo", test_span()),
        package: None,
        is_class_method: false,
        method: MethodDefinition {
            selector: MessageSelector::Unary("build".into()),
            parameters: vec![],
            body: vec![bare(make_spawn_expr("Shape"))],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            is_class_method: false,
            kind: MethodKind::Primary,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        },
        span: test_span(),
    };

    let module = Module {
        classes: vec![shape],
        method_definitions: vec![standalone],
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let result = analyse(&module);
    let abstract_errors: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("Cannot instantiate abstract class"))
        .collect();

    assert_eq!(
        abstract_errors.len(),
        1,
        "Should detect abstract instantiation in standalone method, got: {:?}",
        result.diagnostics
    );
    assert!(abstract_errors[0].message.contains("Shape"));
}

#[test]
fn test_actor_new_error_in_standalone_method() {
    use crate::ast::{MethodKind, StandaloneMethodDefinition};

    // actor `new` usage warning inside standalone method definitions
    let counter = ClassDefinition {
        name: Identifier::new("Counter", test_span()),
        superclass: Some(Identifier::new("Actor", test_span())),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        span: test_span(),
    };

    // Counter new (should warn — use spawn instead)
    let new_expr = Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new("Counter", test_span()),
            span: test_span(),
            package: None,
        }),
        selector: MessageSelector::Unary("new".into()),
        arguments: vec![],
        is_cast: false,
        span: test_span(),
    };

    let standalone = StandaloneMethodDefinition {
        class_name: Identifier::new("Foo", test_span()),
        package: None,
        is_class_method: false,
        method: MethodDefinition {
            selector: MessageSelector::Unary("build".into()),
            parameters: vec![],
            body: vec![bare(new_expr)],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            is_class_method: false,
            kind: MethodKind::Primary,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        },
        span: test_span(),
    };

    let module = Module {
        classes: vec![counter],
        method_definitions: vec![standalone],
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let result = analyse(&module);
    let actor_errors: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| {
            d.message.contains("must use `spawn`")
                && d.severity == crate::source_analysis::Severity::Error
        })
        .collect();

    assert_eq!(
        actor_errors.len(),
        1,
        "Should detect actor new usage in standalone method, got: {:?}",
        result.diagnostics
    );
    assert!(actor_errors[0].message.contains("Counter"));
}

#[test]
fn test_object_new_error() {
    // Object-kind classes cannot use new/new:
    let source = "
Object subclass: MyService
  doStuff => 42

Value subclass: Caller
  test => MyService new
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, _) = crate::source_analysis::parse(tokens);
    let result = analyse(&module);
    let object_errors: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| {
            d.message.contains("cannot be instantiated")
                && d.severity == crate::source_analysis::Severity::Error
        })
        .collect();
    assert_eq!(
        object_errors.len(),
        1,
        "Should detect Object-kind new usage, got: {:?}",
        result.diagnostics
    );
    assert!(object_errors[0].message.contains("MyService"));
}

#[test]
fn test_object_new_allowed_with_own_class_method() {
    // Object-kind classes with their own class-side new: are exempt
    let source = "
Object subclass: Factory
  class new: name => 42

Value subclass: Caller
  test => Factory new: #foo
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, _) = crate::source_analysis::parse(tokens);
    let result = analyse(&module);
    let object_errors: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| {
            d.message.contains("cannot be instantiated")
                && d.severity == crate::source_analysis::Severity::Error
        })
        .collect();
    assert!(
        object_errors.is_empty(),
        "Object-kind class with own class-side new: should be exempt, got: {object_errors:?}",
    );
}

// ── `new` on an opaque `native:` class ──

/// Collects the "cannot be instantiated" errors `analyse` reports for `source`.
fn uninstantiable_new_errors(source: &str) -> Vec<crate::source_analysis::Diagnostic> {
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, _) = crate::source_analysis::parse(tokens);
    analyse(&module)
        .diagnostics
        .into_iter()
        .filter(|d| {
            d.message.contains("cannot be instantiated")
                && d.severity == crate::source_analysis::Severity::Error
        })
        .collect()
}

#[test]
fn test_bt_2998_native_value_class_without_fields_rejects_new() {
    // The instance lives in beamtalk_stopwatch, so `basicNew` would produce a
    // tagged-but-empty map that only fails later, inside an unrelated method.
    let errors = uninstantiable_new_errors(
        "
Value subclass: Stopwatch native: beamtalk_stopwatch
  class sealed started -> Stopwatch => self delegate
  class sealed tickRate -> Integer => self delegate
  elapsed -> Integer => self delegate

Value subclass: Caller
  test => Stopwatch new
",
    );
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert!(
        errors[0].message.contains("`native:` class `Stopwatch`"),
        "{}",
        errors[0].message
    );
    let hint = errors[0].hint.as_ref().expect("should carry a hint");
    assert!(
        hint.contains("Stopwatch started"),
        "hint should name the real constructor, got: {hint}"
    );
    assert!(
        !hint.contains("tickRate"),
        "hint should skip class methods that do not return an instance, got: {hint}"
    );
}

#[test]
fn test_bt_2998_native_class_with_own_new_is_exempt() {
    // `Random`/`Queue` declare a working zero-arg `new` — a real constructor.
    let errors = uninstantiable_new_errors(
        "
Value subclass: Rng native: beamtalk_rng
  class sealed new -> Rng => self delegate

Value subclass: Caller
  test => Rng new
",
    );
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn test_bt_2998_actor_native_class_without_fields_reports_only_spawn_error() {
    // `Subprocess`/`ReactiveSubprocess`/`TranscriptStream` are actor natives
    // with no declared fields, so they match the opaque-native shape exactly.
    // `check_actor_new_usage` already tells the user to `spawn`, which is the
    // actionable fix; a second "use a different constructor" diagnostic would
    // both double-report and point somewhere that doesn't help.
    let source = "
Actor subclass: Proc native: beamtalk_proc
  class sealed open: path :: String -> Proc => self delegate
  pid -> Integer => self delegate

Value subclass: Caller
  test => Proc new
";
    assert!(
        uninstantiable_new_errors(source).is_empty(),
        "actor natives must not draw an opaque-native diagnostic: {:?}",
        uninstantiable_new_errors(source)
    );

    // The pre-existing spawn diagnostic is still the one the user sees.
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, _) = crate::source_analysis::parse(tokens);
    let spawn_errors: Vec<_> = analyse(&module)
        .diagnostics
        .into_iter()
        .filter(|d| d.message.contains("spawn"))
        .collect();
    assert_eq!(spawn_errors.len(), 1, "{spawn_errors:?}");
}

#[test]
fn test_bt_2998_native_class_with_declared_fields_is_exempt() {
    // `Package`/`SupervisionNode`: fields of their own, so `basicNew` builds a
    // genuine default instance.
    let errors = uninstantiable_new_errors(
        "
Value subclass: Pkg native: beamtalk_pkg
  field: name = nil

Value subclass: Caller
  test => Pkg new
",
    );
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn test_bt_2998_plain_value_class_without_fields_is_exempt() {
    // Not native: `~{'$beamtalk_class' => 'Marker'}~` is its complete instance.
    let errors = uninstantiable_new_errors(
        "
Value subclass: Marker
  isMarker -> Boolean => true

Value subclass: Caller
  test => Marker new
",
    );
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn test_bt_2998_native_new_with_args_also_rejected() {
    // `new:` merges over what `new` builds, so it is just as hollow.
    let errors = uninstantiable_new_errors(
        "
Value subclass: Stopwatch native: beamtalk_stopwatch
  class sealed started -> Stopwatch => self delegate

Value subclass: Caller
  test => Stopwatch new: #{}
",
    );
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert!(
        errors[0].message.contains("`new:`"),
        "{}",
        errors[0].message
    );
}

#[test]
fn test_bt_2998_new_with_args_hint_points_at_the_classs_own_new() {
    // `Queue` declares a working `new` but no `new:`, so its `new:` refusal
    // must point back at `Q new` rather than claim there is no constructor.
    let errors = uninstantiable_new_errors(
        "
Value subclass: Q native: beamtalk_q
  class sealed new -> Q => self delegate

Value subclass: Caller
  test => Q new: #{}
",
    );
    assert_eq!(errors.len(), 1, "{errors:?}");
    let hint = errors[0].hint.as_ref().expect("should carry a hint");
    assert!(hint.contains("Q new"), "got: {hint}");
}
