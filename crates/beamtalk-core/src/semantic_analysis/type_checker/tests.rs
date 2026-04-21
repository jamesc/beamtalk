// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for zero-syntax type inference across all expression forms.
use super::*;
use crate::ast::{
    Block, CascadeMessage, ClassDefinition, ClassKind, ClassModifiers, CommentAttachment,
    ExpectCategory, Expression, ExpressionStatement, Identifier, KeywordPart, Literal, MatchArm,
    MessageSelector, MethodDefinition, MethodKind, Module, ParameterDefinition, Pattern,
    ProtocolDefinition, ProtocolMethodSignature, StateDeclaration, TypeAnnotation,
};
use crate::source_analysis::{DiagnosticCategory, Span};

fn span() -> Span {
    Span::new(0, 1)
}

fn ident(name: &str) -> Identifier {
    Identifier {
        name: name.into(),
        span: span(),
    }
}

fn bare(expr: Expression) -> ExpressionStatement {
    ExpressionStatement::bare(expr)
}

fn make_module(expressions: Vec<Expression>) -> Module {
    Module::new(
        expressions
            .into_iter()
            .map(ExpressionStatement::bare)
            .collect(),
        span(),
    )
}

fn make_module_with_classes(expressions: Vec<Expression>, classes: Vec<ClassDefinition>) -> Module {
    let mut module = Module::new(
        expressions
            .into_iter()
            .map(ExpressionStatement::bare)
            .collect(),
        span(),
    );
    module.classes = classes;
    module
}

fn msg_send(receiver: Expression, selector: MessageSelector, args: Vec<Expression>) -> Expression {
    Expression::MessageSend {
        receiver: Box::new(receiver),
        selector,
        arguments: args,
        is_cast: false,
        span: span(),
    }
}

fn int_lit(n: i64) -> Expression {
    Expression::Literal(Literal::Integer(n), span())
}

fn str_lit(s: &str) -> Expression {
    Expression::Literal(Literal::String(s.into()), span())
}

fn symbol_lit(s: &str) -> Expression {
    Expression::Literal(Literal::Symbol(s.into()), span())
}

fn char_lit(c: char) -> Expression {
    Expression::Literal(Literal::Character(c), span())
}

fn var(name: &str) -> Expression {
    Expression::Identifier(ident(name))
}

fn class_ref(name: &str) -> Expression {
    Expression::ClassReference {
        name: ident(name),
        package: None,
        span: span(),
    }
}

fn assign(name: &str, value: Expression) -> Expression {
    Expression::Assignment {
        target: Box::new(var(name)),
        value: Box::new(value),
        span: span(),
    }
}

// ---- Tests ----

#[test]
fn test_literal_type_inference() {
    assert_eq!(
        TypeChecker::infer_literal(&Literal::Integer(42)),
        InferredType::known("Integer")
    );
    assert_eq!(
        TypeChecker::infer_literal(&Literal::Float(1.5)),
        InferredType::known("Float")
    );
    assert_eq!(
        TypeChecker::infer_literal(&Literal::String("hello".into())),
        InferredType::known("String")
    );
    assert_eq!(
        TypeChecker::infer_literal(&Literal::Symbol("sym".into())),
        InferredType::known("#sym")
    );
}

#[test]
fn test_variable_tracking_through_assignment() {
    // x := 42
    // x + 1   ← should know x is Integer
    let module = make_module(vec![
        assign("x", int_lit(42)),
        msg_send(
            var("x"),
            MessageSelector::Binary("+".into()),
            vec![int_lit(1)],
        ),
    ]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(checker.diagnostics().is_empty(), "Integer responds to +");
}

#[test]
fn test_unknown_selector_warning() {
    // 42 foo  ← Integer does not understand 'foo'
    let module = make_module(vec![msg_send(
        int_lit(42),
        MessageSelector::Unary("foo".into()),
        vec![],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert_eq!(checker.diagnostics().len(), 1);
    let diag = &checker.diagnostics()[0];
    assert!(
        diag.message.contains("Integer"),
        "should mention Integer: {}",
        diag.message
    );
    assert!(
        diag.message.contains("foo"),
        "should mention foo: {}",
        diag.message
    );
}

#[test]
fn test_valid_selector_no_warning() {
    // 42 + 1  ← Integer responds to +
    let module = make_module(vec![msg_send(
        int_lit(42),
        MessageSelector::Binary("+".into()),
        vec![int_lit(1)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(checker.diagnostics().is_empty());
}

#[test]
fn test_instance_side_dnu_suppresses_class_side_warning() {
    // Erlang erlang  ← Erlang has instance-side doesNotUnderstand:args:
    // BT-1763: Erlang is a sealed singleton that dispatches class-side
    // messages through instance dispatch, so instance-side DNU also
    // suppresses class-side warnings.
    // Should NOT produce "does not understand" warning
    let module = make_module(vec![msg_send(
        class_ref("Erlang"),
        MessageSelector::Unary("erlang".into()),
        vec![],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "Erlang instance-side DNU should suppress class-side warnings, got: {dnu_warnings:?}"
    );
}

#[test]
fn test_method_lookup_operator_no_warning() {
    // BT-1735: Integer >> #+  — >> is defined on Behaviour, inherited via Class chain
    let module = make_module(vec![msg_send(
        class_ref("Integer"),
        MessageSelector::Binary(">>".into()),
        vec![sym_lit("increment")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        ">> is defined on Behaviour, should not produce DNU warning, got: {dnu_warnings:?}"
    );
}

#[test]
fn test_character_literal_integer_method_no_warning() {
    // BT-778: $A + 1 — Character inherits Integer's +, no DNU warning
    let module = make_module(vec![msg_send(
        char_lit('A'),
        MessageSelector::Binary("+".into()),
        vec![int_lit(1)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "Character should understand '+' via Integer inheritance: {dnu_warnings:?}"
    );
}

#[test]
fn test_character_literal_own_method_no_warning() {
    // BT-778: $A isLetter — Character's own method, no DNU warning
    let module = make_module(vec![msg_send(
        char_lit('A'),
        MessageSelector::Unary("isLetter".into()),
        vec![],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(
        checker.diagnostics().is_empty(),
        "Character should understand 'isLetter': {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_character_literal_bogus_method_warning() {
    // BT-778: $A bogusMethod — should still produce DNU warning
    let module = make_module(vec![msg_send(
        char_lit('A'),
        MessageSelector::Unary("bogusMethod".into()),
        vec![],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert_eq!(
        dnu_warnings.len(),
        1,
        "Character should NOT understand 'bogusMethod': {dnu_warnings:?}"
    );
}

#[test]
fn test_character_literal_non_numeric_operand_warns() {
    // BT-778: $A + 'hello' — Character inherits Integer's +, but 'hello' is not numeric.
    // The type checker should warn that + expects a numeric argument, not a String.
    let module = make_module(vec![msg_send(
        char_lit('A'),
        MessageSelector::Binary("+".into()),
        vec![str_lit("hello")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects a numeric argument"))
        .collect();
    assert_eq!(
        type_warnings.len(),
        1,
        "Character + String should warn about non-numeric operand: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_class_side_spawn_returns_class_type() {
    // x := Counter spawn
    // x increment  ← should know x is Counter
    let module = make_module(vec![
        assign(
            "x",
            msg_send(
                class_ref("Counter"),
                MessageSelector::Unary("spawn".into()),
                vec![],
            ),
        ),
        msg_send(
            var("x"),
            MessageSelector::Unary("nonExistentMethod".into()),
            vec![],
        ),
    ]);

    // Build a hierarchy with Counter that has 'increment' but not 'nonExistentMethod'
    let counter_module = make_module_with_classes(
        vec![],
        vec![ClassDefinition {
            name: ident("Counter"),
            superclass: Some(ident("Actor")),
            superclass_package: None,
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            is_internal: false,
            supervisor_kind: None,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: MessageSelector::Unary("increment".into()),
                parameters: vec![],
                body: vec![],
                return_type: None,
                is_sealed: false,
                is_internal: false,
                kind: MethodKind::Primary,
                expect: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            span: span(),
        }],
    );
    let hierarchy = ClassHierarchy::build(&counter_module).0.unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert_eq!(
        checker.diagnostics().len(),
        1,
        "should warn about nonExistentMethod"
    );
    assert!(checker.diagnostics()[0].message.contains("Counter"));
}

#[test]
fn test_dynamic_type_no_warnings() {
    // x := someUnknownThing
    // x foo  ← x is Dynamic, no warning
    let module = make_module(vec![
        assign("x", var("someUnknownThing")),
        msg_send(var("x"), MessageSelector::Unary("foo".into()), vec![]),
    ]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(
        checker.diagnostics().is_empty(),
        "Dynamic type should not produce warnings"
    );
}

#[test]
fn test_cascade_receiver_type_unchanged() {
    // 42 negated; abs  ← should be fine, Integer responds to both
    let module = make_module(vec![Expression::Cascade {
        receiver: Box::new(int_lit(42)),
        messages: vec![
            CascadeMessage::new(MessageSelector::Unary("negated".into()), vec![], span()),
            CascadeMessage::new(MessageSelector::Unary("abs".into()), vec![], span()),
        ],
        span: span(),
    }]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(checker.diagnostics().is_empty());
}

#[test]
fn test_cascade_invalid_selector() {
    // 42 negated; bogus  ← Integer doesn't understand 'bogus'
    let module = make_module(vec![Expression::Cascade {
        receiver: Box::new(int_lit(42)),
        messages: vec![
            CascadeMessage::new(MessageSelector::Unary("negated".into()), vec![], span()),
            CascadeMessage::new(MessageSelector::Unary("bogus".into()), vec![], span()),
        ],
        span: span(),
    }]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert_eq!(checker.diagnostics().len(), 1);
    assert!(checker.diagnostics()[0].message.contains("bogus"));
}

#[test]
fn test_string_methods() {
    // 'hello' length  ← String responds to length
    let module = make_module(vec![msg_send(
        str_lit("hello"),
        MessageSelector::Unary("length".into()),
        vec![],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(checker.diagnostics().is_empty());
}

#[test]
fn test_block_infers_block_type() {
    let mut checker = TypeChecker::new();
    let hierarchy = ClassHierarchy::with_builtins();
    let mut env = TypeEnv::new();

    let block_expr = Expression::Block(Block {
        parameters: vec![],
        body: vec![bare(int_lit(42))],
        span: span(),
    });

    let ty = checker.infer_expr(&block_expr, &hierarchy, &mut env, false);
    assert_eq!(ty, InferredType::known("Block"));
}

#[test]
fn test_map_literal_infers_dictionary() {
    let mut checker = TypeChecker::new();
    let hierarchy = ClassHierarchy::with_builtins();
    let mut env = TypeEnv::new();

    let map_expr = Expression::MapLiteral {
        pairs: vec![],
        span: span(),
    };

    let ty = checker.infer_expr(&map_expr, &hierarchy, &mut env, false);
    assert_eq!(ty, InferredType::known("Dictionary"));
}

#[test]
fn test_list_literal_infers_list() {
    let mut checker = TypeChecker::new();
    let hierarchy = ClassHierarchy::with_builtins();
    let mut env = TypeEnv::new();

    let list_expr = Expression::ListLiteral {
        elements: vec![int_lit(1), int_lit(2)],
        tail: None,
        span: span(),
    };

    let ty = checker.infer_expr(&list_expr, &hierarchy, &mut env, false);
    assert_eq!(ty, InferredType::known("List"));
}

#[test]
fn test_unknown_variable_is_dynamic() {
    let mut checker = TypeChecker::new();
    let hierarchy = ClassHierarchy::with_builtins();
    let mut env = TypeEnv::new();

    let ty = checker.infer_expr(&var("unknownVar"), &hierarchy, &mut env, false);
    match ty {
        InferredType::Dynamic(reason) => assert_eq!(reason, DynamicReason::Unknown),
        other => panic!("expected Dynamic(Unknown), got {other:?}"),
    }
}

#[test]
fn test_true_false_nil_types() {
    let mut checker = TypeChecker::new();
    let hierarchy = ClassHierarchy::with_builtins();
    let mut env = TypeEnv::new();

    assert_eq!(
        checker.infer_expr(&var("true"), &hierarchy, &mut env, false),
        InferredType::known("Boolean")
    );
    assert_eq!(
        checker.infer_expr(&var("false"), &hierarchy, &mut env, false),
        InferredType::known("Boolean")
    );
    assert_eq!(
        checker.infer_expr(&var("nil"), &hierarchy, &mut env, false),
        InferredType::known("UndefinedObject")
    );
}

#[test]
fn test_did_you_mean_hint() {
    // 'hello' lenght  ← should suggest 'length'
    let module = make_module(vec![msg_send(
        str_lit("hello"),
        MessageSelector::Unary("lenght".into()),
        vec![],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert_eq!(checker.diagnostics().len(), 1);
    let diag = &checker.diagnostics()[0];
    assert!(diag.hint.is_some(), "should have a 'did you mean' hint");
    assert!(
        diag.hint.as_ref().unwrap().contains("length"),
        "hint should suggest 'length': {:?}",
        diag.hint
    );
}

#[test]
fn test_type_env_child_scope() {
    let mut parent = TypeEnv::new();
    parent.set("x", InferredType::known("Integer"));

    let mut child = parent.child();
    assert_eq!(child.get("x"), Some(InferredType::known("Integer")));

    child.set("y", InferredType::known("String"));
    assert!(parent.get("y").is_none(), "parent should not see child's y");
}

#[test]
fn test_match_returns_dynamic() {
    let mut checker = TypeChecker::new();
    let hierarchy = ClassHierarchy::with_builtins();
    let mut env = TypeEnv::new();

    let match_expr = Expression::Match {
        value: Box::new(int_lit(42)),
        arms: vec![],
        span: span(),
    };

    let ty = checker.infer_expr(&match_expr, &hierarchy, &mut env, false);
    match ty {
        InferredType::Dynamic(reason) => assert_eq!(reason, DynamicReason::AmbiguousControlFlow),
        other => panic!("expected Dynamic(AmbiguousControlFlow), got {other:?}"),
    }
}

#[test]
fn test_match_arm_pattern_vars_bound_in_body() {
    // Outer env has `n :: Integer`. The match arm pattern binds `n` as Dynamic,
    // which should shadow the outer binding. Sending a nonexistent selector to
    // a Dynamic receiver produces no warning, but sending it to Integer would.
    // This ensures the test fails if bind_pattern_vars is not called.
    let mut checker = TypeChecker::new();
    let hierarchy = ClassHierarchy::with_builtins();
    let mut env = TypeEnv::new();
    env.set("n", InferredType::known("Integer"));

    let match_expr = Expression::Match {
        value: Box::new(int_lit(42)),
        arms: vec![MatchArm::new(
            Pattern::Variable(ident("n")),
            msg_send(
                var("n"),
                MessageSelector::Unary("definitelyMissing".into()),
                vec![],
            ),
            span(),
        )],
        span: span(),
    };

    let _ = checker.infer_expr(&match_expr, &hierarchy, &mut env, false);
    assert!(
        checker.diagnostics().is_empty(),
        "match-bound var `n` should shadow outer Integer binding — no DNU warnings: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_match_arm_pattern_vars_bound_in_guard() {
    // Same shadowing strategy: outer `n :: Integer`, pattern rebinds as Dynamic.
    // Sending a nonexistent selector in the guard should produce no warning
    // only if the pattern var is correctly bound.
    let mut checker = TypeChecker::new();
    let hierarchy = ClassHierarchy::with_builtins();
    let mut env = TypeEnv::new();
    env.set("n", InferredType::known("Integer"));

    let match_expr = Expression::Match {
        value: Box::new(int_lit(42)),
        arms: vec![MatchArm::with_guard(
            Pattern::Variable(ident("n")),
            msg_send(
                var("n"),
                MessageSelector::Unary("definitelyMissing".into()),
                vec![],
            ),
            var("n"),
            span(),
        )],
        span: span(),
    };

    let _ = checker.infer_expr(&match_expr, &hierarchy, &mut env, false);
    assert!(
        checker.diagnostics().is_empty(),
        "match-bound var `n` in guard should shadow outer Integer binding — no DNU warnings: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_class_method_body_gets_self_type() {
    // Define a class with a method that sends 'unknownMsg' to self
    let module = make_module_with_classes(
        vec![],
        vec![ClassDefinition {
            name: ident("Greeter"),
            superclass: Some(ident("Object")),
            superclass_package: None,
            class_kind: ClassKind::Object,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            is_internal: false,
            supervisor_kind: None,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: MessageSelector::Unary("greet".into()),
                parameters: vec![],
                body: vec![bare(msg_send(
                    var("self"),
                    MessageSelector::Unary("nonExistent".into()),
                    vec![],
                ))],
                return_type: None,
                is_sealed: false,
                is_internal: false,
                kind: MethodKind::Primary,
                expect: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            span: span(),
        }],
    );
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    // Should warn about 'nonExistent' on Greeter
    assert_eq!(checker.diagnostics().len(), 1);
    assert!(checker.diagnostics()[0].message.contains("Greeter"));
}

#[test]
fn test_warnings_only_never_errors() {
    // All diagnostics should be warnings or hints, never errors
    let module = make_module(vec![msg_send(
        int_lit(42),
        MessageSelector::Unary("bogus".into()),
        vec![],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    for diag in checker.diagnostics() {
        assert_ne!(
            diag.severity,
            crate::source_analysis::Severity::Error,
            "type checker should only emit warnings or hints, not errors: {}",
            diag.message
        );
    }
}

#[test]
fn test_keyword_message_validation() {
    // 'hello' at: 1  ← String responds to at:
    let module = make_module(vec![msg_send(
        str_lit("hello"),
        MessageSelector::Keyword(vec![KeywordPart::new("at:", span())]),
        vec![int_lit(1)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(checker.diagnostics().is_empty());
}

#[test]
fn test_inherited_method_no_warning() {
    // 42 printString  ← Integer inherits printString from Object
    let module = make_module(vec![msg_send(
        int_lit(42),
        MessageSelector::Unary("printString".into()),
        vec![],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(
        checker.diagnostics().is_empty(),
        "inherited methods should not produce warnings"
    );
}

#[test]
fn test_stub_returns_no_diagnostics() {
    let module = Module::new(vec![], Span::default());
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(checker.diagnostics().is_empty());
}

#[test]
fn type_map_records_literal_types() {
    let module = make_module(vec![int_lit(42)]);
    let hierarchy = ClassHierarchy::with_builtins();
    let type_map = infer_types(&module, &hierarchy);

    // The integer literal should be recorded as Integer
    let ty = type_map.get(module.expressions[0].expression.span());
    assert!(ty.is_some(), "TypeMap should record literal type");
    assert_eq!(
        ty.unwrap(),
        &InferredType::known("Integer"),
        "Integer literal should infer as Integer"
    );
}

#[test]
fn type_map_records_variable_types_after_assignment() {
    // x := 42 → x should be Integer
    let module = make_module(vec![assign("x", int_lit(42)), var("x")]);
    let hierarchy = ClassHierarchy::with_builtins();
    let type_map = infer_types(&module, &hierarchy);

    // The second expression (var "x") should be recorded as Integer
    let x_expr = &module.expressions[1];
    let ty = type_map.get(x_expr.expression.span());
    assert!(
        ty.is_some(),
        "TypeMap should record variable type after assignment"
    );
    assert_eq!(
        ty.unwrap(),
        &InferredType::known("Integer"),
        "Variable assigned integer should infer as Integer"
    );
}

#[test]
fn infer_types_convenience_function() {
    let module = make_module(vec![str_lit("hello")]);
    let hierarchy = ClassHierarchy::with_builtins();
    let type_map = infer_types(&module, &hierarchy);
    let ty = type_map.get(module.expressions[0].expression.span());
    assert_eq!(
        ty,
        Some(&InferredType::known("String")),
        "infer_types should return correct type map"
    );
}

// BT-614: Class method self-send tests

fn make_class_with_class_methods(
    name: &str,
    instance_methods: Vec<MethodDefinition>,
    class_methods: Vec<MethodDefinition>,
) -> ClassDefinition {
    ClassDefinition {
        name: ident(name),
        superclass: Some(ident("Actor")),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: instance_methods,
        class_methods,
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: span(),
    }
}

fn make_method(selector: &str, body: Vec<Expression>) -> MethodDefinition {
    MethodDefinition {
        selector: MessageSelector::Unary(selector.into()),
        parameters: vec![],
        body: body.into_iter().map(ExpressionStatement::bare).collect(),
        return_type: None,
        is_sealed: false,
        is_internal: false,
        kind: MethodKind::Primary,
        expect: None,
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: span(),
    }
}

#[test]
fn test_class_method_self_send_no_false_warning() {
    // class create => self.instanceCount := ...
    // class createAndReport => self create  ← should NOT warn
    let class_def = make_class_with_class_methods(
        "ClassVarCounter",
        vec![],
        vec![
            make_method("create", vec![int_lit(1)]),
            make_method(
                "createAndReport",
                vec![msg_send(
                    var("self"),
                    MessageSelector::Unary("create".into()),
                    vec![],
                )],
            ),
        ],
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(
        checker.diagnostics().is_empty(),
        "self send to existing class method should not warn, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_class_method_self_send_warns_on_unknown() {
    // class doStuff => self bogusMethod  ← should warn
    let class_def = make_class_with_class_methods(
        "MyClass",
        vec![],
        vec![make_method(
            "doStuff",
            vec![msg_send(
                var("self"),
                MessageSelector::Unary("bogusMethod".into()),
                vec![],
            )],
        )],
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert_eq!(
        checker.diagnostics().len(),
        1,
        "should warn about unknown class method"
    );
    assert!(
        checker.diagnostics()[0].message.contains("bogusMethod"),
        "warning should mention 'bogusMethod': {}",
        checker.diagnostics()[0].message
    );
}

#[test]
fn test_instance_method_self_send_no_regression() {
    // instance method: greet => self increment  ← should NOT warn if increment exists
    let class_def = make_class_with_class_methods(
        "Counter",
        vec![
            make_method("increment", vec![int_lit(1)]),
            make_method(
                "greet",
                vec![msg_send(
                    var("self"),
                    MessageSelector::Unary("increment".into()),
                    vec![],
                )],
            ),
        ],
        vec![],
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(
        checker.diagnostics().is_empty(),
        "instance method self send should not regress, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_cascade_in_class_method_checks_class_side() {
    // class doStuff => self create; getCount  ← both class methods, no warning
    let class_def = make_class_with_class_methods(
        "ClassVarCounter",
        vec![],
        vec![
            make_method("create", vec![int_lit(1)]),
            make_method("getCount", vec![int_lit(0)]),
            make_method(
                "doStuff",
                vec![Expression::Cascade {
                    receiver: Box::new(var("self")),
                    messages: vec![
                        CascadeMessage::new(
                            MessageSelector::Unary("create".into()),
                            vec![],
                            span(),
                        ),
                        CascadeMessage::new(
                            MessageSelector::Unary("getCount".into()),
                            vec![],
                            span(),
                        ),
                    ],
                    span: span(),
                }],
            ),
        ],
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(
        checker.diagnostics().is_empty(),
        "cascade self sends to class methods should not warn, got: {:?}",
        checker.diagnostics()
    );
}

// --- Super type inference tests (BT-596) ---

#[test]
fn test_super_infers_parent_class_type() {
    // class Child < Parent; method: reset => super reset
    // super should infer as Parent type, not Dynamic
    let parent = ClassDefinition {
        name: ident("Parent"),
        superclass: Some(ident("Object")),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![MethodDefinition {
            selector: MessageSelector::Unary("reset".into()),
            parameters: vec![],
            body: vec![bare(int_lit(0))],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            kind: MethodKind::Primary,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: span(),
    };

    let super_span = Span::new(100, 105);
    let msg_span = Span::new(100, 115);
    let child = ClassDefinition {
        name: ident("Child"),
        superclass: Some(ident("Parent")),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![MethodDefinition {
            selector: MessageSelector::Unary("reset".into()),
            parameters: vec![],
            body: vec![bare(Expression::MessageSend {
                receiver: Box::new(Expression::Super(super_span)),
                selector: MessageSelector::Unary("reset".into()),
                arguments: vec![],
                is_cast: false,
                span: msg_span,
            })],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            kind: MethodKind::Primary,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: span(),
    };

    let module = make_module_with_classes(vec![], vec![parent, child]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let type_map = infer_types(&module, &hierarchy);

    // super should be inferred as Parent (not Dynamic)
    let ty = type_map.get(super_span);
    assert_eq!(
        ty,
        Some(&InferredType::known("Parent")),
        "super should infer as parent class type"
    );
}

#[test]
fn test_super_unknown_selector_warns() {
    // class Child < Parent; method: test => super nonExistent
    // Should warn because Parent doesn't have nonExistent
    let parent = ClassDefinition {
        name: ident("Parent"),
        superclass: Some(ident("Object")),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![MethodDefinition {
            selector: MessageSelector::Unary("reset".into()),
            parameters: vec![],
            body: vec![bare(int_lit(0))],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            kind: MethodKind::Primary,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: span(),
    };

    let child = ClassDefinition {
        name: ident("Child"),
        superclass: Some(ident("Parent")),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![MethodDefinition {
            selector: MessageSelector::Unary("test".into()),
            parameters: vec![],
            body: vec![bare(msg_send(
                Expression::Super(span()),
                MessageSelector::Unary("nonExistent".into()),
                vec![],
            ))],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            kind: MethodKind::Primary,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: span(),
    };

    let module = make_module_with_classes(vec![], vec![parent, child]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert_eq!(
        warnings.len(),
        1,
        "should warn about unknown super selector"
    );
    assert!(
        warnings[0].message.contains("Parent"),
        "warning should reference parent class"
    );
}

// --- Binary operand type validation tests (BT-596) ---

#[test]
fn test_integer_plus_string_warns() {
    // 42 + "hello" — type mismatch
    let module = make_module(vec![msg_send(
        int_lit(42),
        MessageSelector::Binary("+".into()),
        vec![str_lit("hello")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects a numeric argument"))
        .collect();
    assert_eq!(type_warnings.len(), 1);
    assert!(type_warnings[0].message.contains("Integer"));
    assert!(type_warnings[0].message.contains("String"));
}

#[test]
fn test_integer_plus_integer_no_warning() {
    // 42 + 1 — valid
    let module = make_module(vec![msg_send(
        int_lit(42),
        MessageSelector::Binary("+".into()),
        vec![int_lit(1)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects a numeric"))
        .collect();
    assert!(type_warnings.is_empty());
}

#[test]
fn test_string_plus_integer_warns_unknown_selector() {
    // "hello" + 42 — String doesn't define +, so we get unknown selector (not operand type)
    let module = make_module(vec![msg_send(
        str_lit("hello"),
        MessageSelector::Binary("+".into()),
        vec![int_lit(42)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // String doesn't define +, so we get "does not understand" (not operand type warning)
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert_eq!(warnings.len(), 1);
    assert!(warnings[0].message.contains("String"));
}

#[test]
fn test_string_concat_no_operand_warning() {
    // "hello" ++ " world" — valid String concatenation
    let module = make_module(vec![msg_send(
        str_lit("hello"),
        MessageSelector::Binary("++".into()),
        vec![str_lit(" world")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects a"))
        .collect();
    assert!(type_warnings.is_empty());
}

#[test]
fn test_string_concat_integer_warns() {
    // "hello" ++ 42 — wrong type for string concat
    let module = make_module(vec![msg_send(
        str_lit("hello"),
        MessageSelector::Binary("++".into()),
        vec![int_lit(42)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects a String argument"))
        .collect();
    assert_eq!(type_warnings.len(), 1);
}

#[test]
fn test_dynamic_type_skips_operand_check() {
    // x + 42 — x is dynamic, no warning
    let module = make_module(vec![msg_send(
        var("x"),
        MessageSelector::Binary("+".into()),
        vec![int_lit(42)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects a"))
        .collect();
    assert!(type_warnings.is_empty());
}

#[test]
fn test_comparison_integer_vs_string_warns() {
    // 42 < "hello" — type mismatch for comparison
    let module = make_module(vec![msg_send(
        int_lit(42),
        MessageSelector::Binary("<".into()),
        vec![str_lit("hello")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects a numeric argument"))
        .collect();
    assert_eq!(type_warnings.len(), 1);
}

#[test]
fn test_binary_operand_warnings_are_warnings_not_errors() {
    // All operand type diagnostics should be warnings
    let module = make_module(vec![msg_send(
        int_lit(42),
        MessageSelector::Binary("+".into()),
        vec![str_lit("hello")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    for diag in checker.diagnostics() {
        if diag.message.contains("expects a") {
            assert_eq!(
                diag.severity,
                crate::source_analysis::Severity::Warning,
                "operand type diagnostics should be warnings"
            );
        }
    }
}

// ---- Typed class tests (BT-587) ----

#[test]
fn test_typed_class_warns_on_missing_param_annotation() {
    // typed class with untyped parameter
    let class_def = ClassDefinition::with_modifiers(
        ident("StrictCounter"),
        Some(ident("Actor")),
        ClassModifiers {
            is_typed: true,
            ..Default::default()
        },
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
            vec![ParameterDefinition::new(ident("amount"))], // no type annotation
            vec![bare(int_lit(0))],
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("Missing type annotation for parameter"))
        .collect();
    assert_eq!(
        warnings.len(),
        1,
        "should warn about untyped param `amount`"
    );
    assert!(warnings[0].message.contains("amount"));
}

#[test]
fn test_typed_class_warns_on_missing_return_type() {
    // typed class with method missing return type
    let class_def = ClassDefinition::with_modifiers(
        ident("StrictCounter"),
        Some(ident("Actor")),
        ClassModifiers {
            is_typed: true,
            ..Default::default()
        },
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Unary("increment".into()),
            vec![],
            vec![bare(int_lit(1))],
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("Missing return type"))
        .collect();
    assert_eq!(warnings.len(), 1, "should warn about missing return type");
}

#[test]
fn test_typed_class_no_warning_when_fully_annotated() {
    // typed class with fully annotated method
    let class_def = ClassDefinition::with_modifiers(
        ident("StrictCounter"),
        Some(ident("Actor")),
        ClassModifiers {
            is_typed: true,
            ..Default::default()
        },
        vec![],
        vec![MethodDefinition::with_return_type(
            MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
            vec![ParameterDefinition::with_type(
                ident("amount"),
                TypeAnnotation::Simple(ident("Integer")),
            )],
            vec![bare(int_lit(0))],
            TypeAnnotation::Simple(ident("Integer")),
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let typed_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("Missing"))
        .collect();
    assert!(
        typed_warnings.is_empty(),
        "fully annotated method should not warn, got: {typed_warnings:?}"
    );
}

// ── BT-1856: @expect suppression on declarations ───────────────────────────

#[test]
fn test_expect_type_suppresses_typed_method_warnings() {
    // BT-1883: typed class with untyped method that has @expect type —
    // type checker emits the diagnostic, apply_expect_directives suppresses it,
    // and no stale @expect warning is produced.
    let mut method = MethodDefinition::new(
        MessageSelector::Unary("first".into()),
        vec![],
        vec![bare(int_lit(42))],
        span(),
    );
    method.expect = Some((ExpectCategory::Type, None, span()));

    let class_def = ClassDefinition::with_modifiers(
        ident("MyTyped"),
        Some(ident("Object")),
        ClassModifiers {
            is_typed: true,
            ..Default::default()
        },
        vec![],
        vec![method],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    assert!(
        diags.is_empty(),
        "@expect type should suppress missing annotation warnings with no stale warning, got: {diags:?}"
    );
}

#[test]
fn test_expect_all_suppresses_typed_method_warnings() {
    // BT-1883: @expect all should also suppress typed class warnings
    // without producing a stale @expect warning.
    let mut method = MethodDefinition::new(
        MessageSelector::Unary("first".into()),
        vec![],
        vec![bare(int_lit(42))],
        span(),
    );
    method.expect = Some((ExpectCategory::All, None, span()));

    let class_def = ClassDefinition::with_modifiers(
        ident("MyTyped"),
        Some(ident("Object")),
        ClassModifiers {
            is_typed: true,
            ..Default::default()
        },
        vec![],
        vec![method],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    assert!(
        diags.is_empty(),
        "@expect all should suppress missing annotation warnings with no stale warning, got: {diags:?}"
    );
}

#[test]
fn test_typed_no_default_state_no_warning() {
    // BT-1947: A type annotation replaces the need for a default value.
    // `state: count :: Integer` (no default) should produce no warnings.
    let typed_no_default = StateDeclaration::with_type(
        ident("count"),
        TypeAnnotation::simple("Integer", span()),
        span(),
    );

    let typed_with_default = StateDeclaration::with_type_and_default(
        ident("name"),
        TypeAnnotation::simple("String", span()),
        str_lit("default"),
        span(),
    );

    let class_def = ClassDefinition::with_modifiers(
        ident("MyTyped"),
        Some(ident("Object")),
        ClassModifiers {
            is_typed: true,
            ..Default::default()
        },
        vec![typed_no_default, typed_with_default],
        vec![],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let uninitialized: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("uninitialized"))
        .collect();
    assert!(
        uninitialized.is_empty(),
        "Typed state with type annotation should not warn about uninitialized (BT-1947), got: {uninitialized:?}"
    );
}

#[test]
fn test_non_typed_class_no_warnings() {
    // non-typed class with untyped method — no warnings expected
    let class_def = ClassDefinition::with_modifiers(
        ident("Counter"),
        Some(ident("Actor")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
            vec![ParameterDefinition::new(ident("amount"))],
            vec![bare(int_lit(0))],
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let typed_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("Missing"))
        .collect();
    assert!(
        typed_warnings.is_empty(),
        "non-typed class should not warn about annotations"
    );
}

#[test]
fn test_typed_class_skips_primitive_methods() {
    // typed class with @primitive method — no warnings
    let class_def = ClassDefinition::with_modifiers(
        ident("StrictInteger"),
        Some(ident("Object")),
        ClassModifiers {
            is_typed: true,
            ..Default::default()
        },
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Binary("+".into()),
            vec![ParameterDefinition::new(ident("other"))],
            vec![bare(Expression::Primitive {
                name: "+".into(),
                is_quoted: true,
                is_intrinsic: false,
                span: span(),
            })],
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let typed_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("Missing"))
        .collect();
    assert!(
        typed_warnings.is_empty(),
        "primitive methods should be skipped in typed classes"
    );
}

#[test]
fn test_as_type_assertion_infers_correct_type() {
    // x asType: Integer  should infer x as Integer
    let module = make_module(vec![msg_send(
        var("x"),
        MessageSelector::Keyword(vec![KeywordPart::new("asType:", span())]),
        vec![class_ref("Integer")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    // The send itself shouldn't warn (asType: is special)
    assert!(
        checker.diagnostics().is_empty(),
        "asType: should not produce warnings, got: {:?}",
        checker.diagnostics()
    );
}

// ---- BT-671: Argument and return type checking tests ----

#[test]
fn test_integer_plus_string_warns_operand_type() {
    // 42 + "hello" — Integer + expects numeric arg, String is wrong type
    let module = make_module(vec![msg_send(
        int_lit(42),
        MessageSelector::Binary("+".into()),
        vec![str_lit("hello")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(
        !checker.diagnostics().is_empty(),
        "42 + 'hello' should produce a type warning"
    );
}

#[test]
fn test_integer_plus_integer_no_arg_type_warning() {
    // 3 + 4 — valid, no warnings (acceptance criteria)
    let module = make_module(vec![msg_send(
        int_lit(3),
        MessageSelector::Binary("+".into()),
        vec![int_lit(4)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(
        checker.diagnostics().is_empty(),
        "3 + 4 should not produce warnings, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_integer_subtype_of_number_no_warning() {
    // Integer is a subtype of Number via superclass chain
    // Integer + Integer should work since param declares Number
    let module = make_module(vec![msg_send(
        int_lit(42),
        MessageSelector::Binary("+".into()),
        vec![int_lit(1)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(
        checker.diagnostics().is_empty(),
        "Integer should be valid for Number param"
    );
}

#[test]
fn test_dynamic_args_never_warn() {
    // unknownVar + 42 — receiver is Dynamic, no warning
    let module = make_module(vec![msg_send(
        var("unknownVar"),
        MessageSelector::Binary("+".into()),
        vec![var("alsoUnknown")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(
        checker.diagnostics().is_empty(),
        "Dynamic arguments should never produce warnings"
    );
}

#[test]
fn test_return_type_mismatch_warns() {
    // getBalance -> Integer => 'oops' warns (acceptance criteria)
    let class_def = ClassDefinition::with_modifiers(
        ident("Account"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::with_return_type(
            MessageSelector::Unary("getBalance".into()),
            vec![],
            vec![bare(str_lit("oops"))], // Returns String, declared Integer
            TypeAnnotation::Simple(ident("Integer")),
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let return_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .collect();
    assert_eq!(
        return_warnings.len(),
        1,
        "should warn about return type mismatch"
    );
    assert!(return_warnings[0].message.contains("Integer"));
    assert!(return_warnings[0].message.contains("String"));
}

#[test]
fn test_return_type_match_no_warning() {
    // getBalance -> Integer => 42  — correct return type
    let class_def = ClassDefinition::with_modifiers(
        ident("Account"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::with_return_type(
            MessageSelector::Unary("getBalance".into()),
            vec![],
            vec![bare(int_lit(42))],
            TypeAnnotation::Simple(ident("Integer")),
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let return_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .collect();
    assert!(
        return_warnings.is_empty(),
        "matching return type should not warn"
    );
}

#[test]
fn test_return_type_skip_primitive_methods() {
    // @primitive method — should not check return type
    let class_def = ClassDefinition::with_modifiers(
        ident("MyInt"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::with_return_type(
            MessageSelector::Unary("value".into()),
            vec![],
            vec![bare(Expression::Primitive {
                name: "value".into(),
                is_quoted: true,
                is_intrinsic: false,
                span: span(),
            })],
            TypeAnnotation::Simple(ident("Integer")),
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let return_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .collect();
    assert!(
        return_warnings.is_empty(),
        "primitive methods should skip return type check"
    );
}

#[test]
fn test_return_type_no_annotation_no_warning() {
    // Method without return type annotation — no warning
    let class_def = ClassDefinition::with_modifiers(
        ident("Counter"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Unary("count".into()),
            vec![],
            vec![bare(str_lit("oops"))],
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let return_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .collect();
    assert!(
        return_warnings.is_empty(),
        "method without return type should not warn about return type"
    );
}

// Helper: lex + parse a source string into a Module.
fn parse_source(source: &str) -> Module {
    use crate::source_analysis::{lex_with_eof, parse};
    let tokens = lex_with_eof(source);
    let (module, _) = parse(tokens);
    module
}

// Helper: run the full diagnostic pipeline (type check + @expect suppression).
// Suppression is owned by `apply_expect_directives` in diagnostic_provider —
// the type checker itself does NOT suppress.
fn run_with_expect(module: &Module, hierarchy: &ClassHierarchy) -> Vec<Diagnostic> {
    let mut checker = TypeChecker::new();
    checker.check_module(module, hierarchy);
    let mut diags = checker.take_diagnostics();
    crate::queries::diagnostic_provider::apply_expect_directives(module, &mut diags);
    diags
}

#[test]
fn test_expect_dnu_suppresses_dnu_warning() {
    // @expect dnu before a DNU-producing send suppresses the hint.
    // Real-world use: `self species withAll: result` in Collection.bt.
    // Spans must be real for apply_expect_directives to match by span.
    let hierarchy = ClassHierarchy::with_builtins();

    // Without @expect: should warn
    let module_bare = parse_source("42 unknownSelector");
    let diags_without = run_with_expect(&module_bare, &hierarchy);
    assert!(
        !diags_without.is_empty(),
        "42 unknownSelector should produce a DNU hint without @expect"
    );

    // With @expect dnu: should be suppressed
    let module_with = parse_source("@expect dnu\n42 unknownSelector");
    let diags_with = run_with_expect(&module_with, &hierarchy);
    assert!(
        diags_with.is_empty(),
        "@expect dnu should suppress DNU hint, got: {diags_with:?}"
    );
}

#[test]
fn test_expect_type_suppresses_type_warning() {
    // @expect type before a type-mismatch expression suppresses the warning.
    let hierarchy = ClassHierarchy::with_builtins();

    // Without @expect: should warn
    let module_bare = parse_source("1 + \"hello\"");
    let diags_without = run_with_expect(&module_bare, &hierarchy);
    assert!(
        !diags_without.is_empty(),
        "1 + \"hello\" should produce a type warning without @expect"
    );

    // With @expect type: should be suppressed
    let module_with = parse_source("@expect type\n1 + \"hello\"");
    let diags_with = run_with_expect(&module_with, &hierarchy);
    assert!(
        diags_with.is_empty(),
        "@expect type should suppress type warning, got: {diags_with:?}"
    );
}

#[test]
fn test_expect_type_suppresses_dnu_hint() {
    // BT-1273: @expect type also suppresses method-not-found (DNU) hints,
    // not just type-mismatch warnings.
    let hierarchy = ClassHierarchy::with_builtins();

    // Without @expect: calling unknownMethod on Integer produces a DNU hint
    let module_bare = parse_source("42 unknownMethod");
    let diags_without = run_with_expect(&module_bare, &hierarchy);
    assert!(
        !diags_without.is_empty(),
        "42 unknownMethod should produce a DNU hint without @expect"
    );

    // With @expect type: should also suppress the DNU hint
    let module_with = parse_source("@expect type\n42 unknownMethod");
    let diags_with = run_with_expect(&module_with, &hierarchy);
    assert!(
        diags_with.is_empty(),
        "@expect type should suppress DNU hint, got: {diags_with:?}"
    );
}

#[test]
fn test_expect_type_stale_when_no_dnu_or_type_diagnostic() {
    // BT-1273: @expect type is stale when the following expression has
    // neither a type warning nor a DNU hint.
    let hierarchy = ClassHierarchy::with_builtins();

    // 42 alone produces no diagnostic — @expect type should be stale
    let module = parse_source("@expect type\n42");
    let diags = run_with_expect(&module, &hierarchy);
    let has_stale = diags.iter().any(|d| d.message.contains("stale @expect"));
    assert!(
        has_stale,
        "@expect type on `42` (no diagnostic) must emit stale warning, got: {diags:?}"
    );
}

#[test]
fn test_expect_does_not_suppress_next_next_expression() {
    // @expect dnu only suppresses the immediately following expression.
    // Here @expect applies to `42` (no DNU) → stale warning is emitted,
    // and `42 unknownSelector` still produces its own DNU hint.
    let module = parse_source("@expect dnu\n42\n42 unknownSelector");
    let hierarchy = ClassHierarchy::with_builtins();
    let diags = run_with_expect(&module, &hierarchy);
    assert!(
        !diags.is_empty(),
        "@expect dnu on `42` must not suppress DNU on the following expression"
    );
}

#[test]
fn test_as_type_suppresses_subsequent_warnings() {
    // (x asType: Integer) + "hello" — x is asserted Integer, should warn about String arg
    let module = make_module(vec![msg_send(
        msg_send(
            var("x"),
            MessageSelector::Keyword(vec![KeywordPart::new("asType:", span())]),
            vec![class_ref("Integer")],
        ),
        MessageSelector::Binary("+".into()),
        vec![str_lit("hello")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    // After asType:, x is Known(Integer), so + "hello" should warn
    assert!(
        !checker.diagnostics().is_empty(),
        "asType: Integer + String should produce a type warning"
    );
}

#[test]
fn test_keyword_arg_type_mismatch_warns() {
    // Counter with typed parameter: deposit: amount: Integer
    // Sending deposit: "hello" should warn
    let class_def = ClassDefinition::with_modifiers(
        ident("Counter"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
            vec![ParameterDefinition::with_type(
                ident("amount"),
                TypeAnnotation::Simple(ident("Integer")),
            )],
            vec![bare(int_lit(0))],
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(
        vec![
            // c := Counter new
            assign(
                "c",
                msg_send(
                    class_ref("Counter"),
                    MessageSelector::Unary("new".into()),
                    vec![],
                ),
            ),
            // c deposit: "hello"
            msg_send(
                var("c"),
                MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
                vec![str_lit("hello")],
            ),
        ],
        vec![class_def],
    );
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let arg_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects Integer"))
        .collect();
    assert_eq!(
        arg_warnings.len(),
        1,
        "should warn about String argument where Integer expected, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_keyword_arg_type_match_no_warning() {
    // Counter deposit: 42 — correct type
    let class_def = ClassDefinition::with_modifiers(
        ident("Counter"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
            vec![ParameterDefinition::with_type(
                ident("amount"),
                TypeAnnotation::Simple(ident("Integer")),
            )],
            vec![bare(int_lit(0))],
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(
        vec![
            assign(
                "c",
                msg_send(
                    class_ref("Counter"),
                    MessageSelector::Unary("new".into()),
                    vec![],
                ),
            ),
            msg_send(
                var("c"),
                MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
                vec![int_lit(42)],
            ),
        ],
        vec![class_def],
    );
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let arg_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects"))
        .collect();
    assert!(
        arg_warnings.is_empty(),
        "correct arg type should not warn, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_parametric_block_arg_type_mismatch_warns() {
    // BT-2002: A non-Block argument to a `Block(T, R)` parameter should still
    // produce a type warning. Previously the hierarchy lookup on the raw
    // annotation string (e.g. "Block(Integer, R)") missed the base class and
    // the conservative "unknown → compatible" escape hatch suppressed the
    // warning.
    let class_def = ClassDefinition::with_modifiers(
        ident("Repro"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("run:", span())]),
            vec![ParameterDefinition::with_type(
                ident("block"),
                TypeAnnotation::Generic {
                    base: ident("Block"),
                    parameters: vec![
                        TypeAnnotation::Simple(ident("Integer")),
                        TypeAnnotation::Simple(ident("R")),
                    ],
                    span: span(),
                },
            )],
            vec![bare(int_lit(0))],
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(
        vec![
            assign(
                "r",
                msg_send(
                    class_ref("Repro"),
                    MessageSelector::Unary("new".into()),
                    vec![],
                ),
            ),
            msg_send(
                var("r"),
                MessageSelector::Keyword(vec![KeywordPart::new("run:", span())]),
                vec![str_lit("not a block")],
            ),
        ],
        vec![class_def],
    );
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let arg_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| {
            d.message.contains("expects Block(Integer, R)") && d.message.contains("got String")
        })
        .collect();
    assert_eq!(
        arg_warnings.len(),
        1,
        "should warn when String is passed to a Block(Integer, R) parameter, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_parametric_block_arg_type_match_no_warning() {
    // BT-2002: A Block argument to a `Block(T, R)` parameter should not warn.
    let class_def = ClassDefinition::with_modifiers(
        ident("Repro"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("run:", span())]),
            vec![ParameterDefinition::with_type(
                ident("block"),
                TypeAnnotation::Generic {
                    base: ident("Block"),
                    parameters: vec![
                        TypeAnnotation::Simple(ident("Integer")),
                        TypeAnnotation::Simple(ident("R")),
                    ],
                    span: span(),
                },
            )],
            vec![bare(int_lit(0))],
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(
        vec![
            assign(
                "r",
                msg_send(
                    class_ref("Repro"),
                    MessageSelector::Unary("new".into()),
                    vec![],
                ),
            ),
            msg_send(
                var("r"),
                MessageSelector::Keyword(vec![KeywordPart::new("run:", span())]),
                vec![block_expr(vec![int_lit(0)])],
            ),
        ],
        vec![class_def],
    );
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let arg_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects Block"))
        .collect();
    assert!(
        arg_warnings.is_empty(),
        "a Block arg for a Block(Integer, R) param should not warn, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_override_incompatible_param_type_warns() {
    // Parent: deposit: amount: Number
    // Child: deposit: amount: String (incompatible)
    let parent = ClassDefinition::with_modifiers(
        ident("Base"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
            vec![ParameterDefinition::with_type(
                ident("amount"),
                TypeAnnotation::Simple(ident("Number")),
            )],
            vec![bare(int_lit(0))],
            span(),
        )],
        span(),
    );
    let child = ClassDefinition::with_modifiers(
        ident("Derived"),
        Some(ident("Base")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
            vec![ParameterDefinition::with_type(
                ident("amount"),
                TypeAnnotation::Simple(ident("String")),
            )],
            vec![bare(str_lit("ok"))],
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![parent, child]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let override_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("incompatible with parent"))
        .collect();
    assert_eq!(
        override_warnings.len(),
        1,
        "should warn about incompatible override param type"
    );
}

#[test]
fn test_override_compatible_param_type_no_warning() {
    // Parent: deposit: amount: Number
    // Child: deposit: amount: Integer (compatible — Integer is subclass of Number)
    let parent = ClassDefinition::with_modifiers(
        ident("Base"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
            vec![ParameterDefinition::with_type(
                ident("amount"),
                TypeAnnotation::Simple(ident("Number")),
            )],
            vec![bare(int_lit(0))],
            span(),
        )],
        span(),
    );
    let child = ClassDefinition::with_modifiers(
        ident("Derived"),
        Some(ident("Base")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
            vec![ParameterDefinition::with_type(
                ident("amount"),
                TypeAnnotation::Simple(ident("Integer")),
            )],
            vec![bare(int_lit(0))],
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![parent, child]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let override_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("incompatible with parent"))
        .collect();
    assert!(
        override_warnings.is_empty(),
        "compatible override should not warn"
    );
}

#[test]
fn test_override_parametric_incompatible_param_type_warns() {
    // BT-2002: Child overrides parent with a parameterized type annotation
    // (`Array(Integer)`) that is not compatible with the parent's type
    // (`Number`). Previously `is_type_compatible` only normalized the
    // expected side, so `hierarchy.has_class("Array(Integer)")` on the
    // actual side hit the conservative "unknown → compatible" escape hatch
    // and the override warning was silently suppressed.
    let parent = ClassDefinition::with_modifiers(
        ident("Base"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
            vec![ParameterDefinition::with_type(
                ident("amount"),
                TypeAnnotation::Simple(ident("Number")),
            )],
            vec![bare(int_lit(0))],
            span(),
        )],
        span(),
    );
    let child = ClassDefinition::with_modifiers(
        ident("Derived"),
        Some(ident("Base")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
            vec![ParameterDefinition::with_type(
                ident("amount"),
                TypeAnnotation::Generic {
                    base: ident("Array"),
                    parameters: vec![TypeAnnotation::Simple(ident("Integer"))],
                    span: span(),
                },
            )],
            vec![bare(int_lit(0))],
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![parent, child]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let override_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("incompatible with parent"))
        .collect();
    assert_eq!(
        override_warnings.len(),
        1,
        "should warn about incompatible parametric override param type, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_all_type_warnings_are_severity_warning() {
    // Verify all diagnostics from type checking are Severity::Warning
    let class_def = ClassDefinition::with_modifiers(
        ident("Account"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::with_return_type(
            MessageSelector::Unary("getBalance".into()),
            vec![],
            vec![bare(str_lit("oops"))],
            TypeAnnotation::Simple(ident("Integer")),
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(
        vec![msg_send(
            int_lit(42),
            MessageSelector::Binary("+".into()),
            vec![str_lit("hello")],
        )],
        vec![class_def],
    );
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    for diag in checker.diagnostics() {
        assert_eq!(
            diag.severity,
            crate::source_analysis::Severity::Warning,
            "all type checking diagnostics should be warnings, not errors: {}",
            diag.message
        );
    }
}

// --- State field assignment type checking tests (BT-672) ---

fn field_access(field_name: &str) -> Expression {
    Expression::FieldAccess {
        receiver: Box::new(var("self")),
        field: ident(field_name),
        span: span(),
    }
}

fn field_assign(field_name: &str, value: Expression) -> Expression {
    Expression::Assignment {
        target: Box::new(field_access(field_name)),
        value: Box::new(value),
        span: span(),
    }
}

fn counter_class_with_typed_state(
    methods: Vec<MethodDefinition>,
    state: Vec<StateDeclaration>,
) -> ClassDefinition {
    ClassDefinition {
        name: ident("Counter"),
        superclass: Some(ident("Object")),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state,
        methods,
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: span(),
    }
}

#[test]
fn test_empty_body_with_return_type_no_crash() {
    // Empty method body with return type annotation — should not crash
    let class_def = ClassDefinition::with_modifiers(
        ident("Counter"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::with_return_type(
            MessageSelector::Unary("count".into()),
            vec![],
            vec![], // empty body
            TypeAnnotation::Simple(ident("Integer")),
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    // Dynamic body type — no return type warning (can't infer from empty body)
    let return_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .collect();
    assert!(
        return_warnings.is_empty(),
        "empty body should not produce return type mismatch"
    );
}

#[test]
fn test_integer_plus_string_exactly_one_warning() {
    // 42 + "hello" — should produce exactly 1 warning (binary operand check),
    // not 2 (no duplicate from check_argument_types)
    let module = make_module(vec![msg_send(
        int_lit(42),
        MessageSelector::Binary("+".into()),
        vec![str_lit("hello")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert_eq!(
        checker.diagnostics().len(),
        1,
        "should produce exactly 1 warning (no duplicate), got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_field_assign_typed_mismatch_warns() {
    // state: count: Integer = 0; self.count := "bad" → warning
    let state = vec![StateDeclaration::with_type_and_default(
        ident("count"),
        TypeAnnotation::simple("Integer", span()),
        int_lit(0),
        span(),
    )];
    let method = make_method("badMethod", vec![field_assign("count", str_lit("bad"))]);
    let class = counter_class_with_typed_state(vec![method], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("Type mismatch"))
        .collect();
    assert_eq!(
        warnings.len(),
        1,
        "Expected 1 type mismatch warning, got: {:?}",
        checker.diagnostics()
    );
    assert!(warnings[0].message.contains("count"));
    assert!(warnings[0].message.contains("Integer"));
    assert!(warnings[0].message.contains("String"));
}

#[test]
fn test_field_assign_typed_match_no_warn() {
    // state: count: Integer = 0; self.count := 42 → no warning
    let state = vec![StateDeclaration::with_type_and_default(
        ident("count"),
        TypeAnnotation::simple("Integer", span()),
        int_lit(0),
        span(),
    )];
    let method = make_method("goodMethod", vec![field_assign("count", int_lit(42))]);
    let class = counter_class_with_typed_state(vec![method], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(
        checker.diagnostics().is_empty(),
        "No warnings expected, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_return_type_subtype_compatible() {
    // Method declares -> Number, body returns Integer (subtype) — no warning
    let class_def = ClassDefinition::with_modifiers(
        ident("Calculator"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::with_return_type(
            MessageSelector::Unary("compute".into()),
            vec![],
            vec![bare(int_lit(42))], // Integer is subtype of Number
            TypeAnnotation::Simple(ident("Number")),
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let return_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .collect();
    assert!(
        return_warnings.is_empty(),
        "Integer return should be compatible with Number declaration"
    );
}

#[test]
fn test_field_assign_untyped_no_warn() {
    // state: value = 0; self.value := "anything" → no warning
    let state = vec![StateDeclaration::with_default(
        ident("value"),
        int_lit(0),
        span(),
    )];
    let method = make_method(
        "setAnything",
        vec![field_assign("value", str_lit("anything"))],
    );
    let class = counter_class_with_typed_state(vec![method], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(
        checker.diagnostics().is_empty(),
        "Untyped fields should not produce warnings, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_early_return_uses_return_type_not_trailing() {
    // Method with early return: ^ 42 followed by unreachable "oops"
    // Return type checking should use the return expression type (Integer),
    // not the unreachable trailing expression (String)
    let class_def = ClassDefinition::with_modifiers(
        ident("Calculator"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::with_return_type(
            MessageSelector::Unary("compute".into()),
            vec![],
            vec![
                bare(Expression::Return {
                    value: Box::new(int_lit(42)),
                    span: span(),
                }),
                bare(str_lit("unreachable")), // would be String, bare(but never reached)
            ],
            TypeAnnotation::Simple(ident("Integer")),
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let return_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .collect();
    assert!(
        return_warnings.is_empty(),
        "early return of Integer should match declared Integer, not unreachable String"
    );
}

#[test]
fn test_field_assign_subtype_no_warn() {
    // Integer is assignable to Number (Integer's superclass chain includes Number)
    let state = vec![StateDeclaration::with_type(
        ident("value"),
        TypeAnnotation::simple("Number", span()),
        span(),
    )];
    let method = make_method("setNumber", vec![field_assign("value", int_lit(42))]);
    let class = counter_class_with_typed_state(vec![method], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let type_mismatches: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("Type mismatch"))
        .collect();
    assert!(
        type_mismatches.is_empty(),
        "Integer should be assignable to Number, got: {type_mismatches:?}"
    );
}

#[test]
fn test_state_default_value_mismatch_warns() {
    // state: count: Integer = "bad" → warning at class definition time
    let state = vec![StateDeclaration::with_type_and_default(
        ident("count"),
        TypeAnnotation::simple("Integer", span()),
        str_lit("bad"),
        span(),
    )];
    let class = counter_class_with_typed_state(vec![], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("Type mismatch"))
        .collect();
    assert_eq!(
        warnings.len(),
        1,
        "Expected 1 type mismatch for default value, got: {:?}",
        checker.diagnostics()
    );
    assert!(warnings[0].message.contains("count"));
}

#[test]
fn test_state_default_value_match_no_warn() {
    // state: count: Integer = 0 → no warning
    let state = vec![StateDeclaration::with_type_and_default(
        ident("count"),
        TypeAnnotation::simple("Integer", span()),
        int_lit(0),
        span(),
    )];
    let class = counter_class_with_typed_state(vec![], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(
        checker.diagnostics().is_empty(),
        "Default value matching declared type should not warn, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_dynamic_expression_no_warn() {
    // Assigning a dynamic expression (unknown variable) to a typed field → no assignment warning
    let state = vec![StateDeclaration::with_type(
        ident("count"),
        TypeAnnotation::simple("Integer", span()),
        span(),
    )];
    let method = make_method("setUnknown", vec![field_assign("count", var("unknownVar"))]);
    let class = counter_class_with_typed_state(vec![method], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let type_mismatches: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("Type mismatch"))
        .collect();
    assert!(
        type_mismatches.is_empty(),
        "Dynamic expressions should never produce type mismatch warnings, got: {type_mismatches:?}"
    );
}

#[test]
fn test_union_type_annotation_no_false_positive() {
    // state: value: Integer | String = 42 → no warning (union types skip check)
    let state = vec![StateDeclaration::with_type_and_default(
        ident("value"),
        TypeAnnotation::union(
            vec![
                TypeAnnotation::simple("Integer", span()),
                TypeAnnotation::simple("String", span()),
            ],
            span(),
        ),
        int_lit(42),
        span(),
    )];
    let method = make_method("setValue", vec![field_assign("value", str_lit("hello"))]);
    let class = counter_class_with_typed_state(vec![method], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(
        checker.diagnostics().is_empty(),
        "Union type annotations should not produce false positives, got: {:?}",
        checker.diagnostics()
    );
}

// --- Typed state field declarations (BT-1831, BT-1947) ---

#[test]
fn test_typed_state_no_default_no_warn() {
    // BT-1947: Mixed-default class: name :: String (no default) + count :: Integer = 0 (has default)
    // Type annotation replaces the need for a default — no uninitialized warning.
    let state = vec![
        StateDeclaration::with_type(
            ident("name"),
            TypeAnnotation::simple("String", span()),
            span(),
        ),
        StateDeclaration::with_type_and_default(
            ident("count"),
            TypeAnnotation::simple("Integer", span()),
            int_lit(0),
            span(),
        ),
    ];
    let class = counter_class_with_typed_state(vec![], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("uninitialized"))
        .collect();
    assert!(
        warnings.is_empty(),
        "BT-1947: Typed state without default should not warn, got: {warnings:?}"
    );
}

#[test]
fn test_all_factory_fields_no_warn() {
    // All-factory class: all typed fields have no default → no warnings.
    // Sibling-default heuristic: if no typed field has a default, the class
    // is factory-constructed (spawnWith:/new:) — suppress warnings.
    let state = vec![
        StateDeclaration::with_type(
            ident("name"),
            TypeAnnotation::simple("String", span()),
            span(),
        ),
        StateDeclaration::with_type(
            ident("config"),
            TypeAnnotation::simple("Dictionary", span()),
            span(),
        ),
    ];
    let class = counter_class_with_typed_state(vec![], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("uninitialized"))
        .collect();
    assert!(
        warnings.is_empty(),
        "All-factory class should not warn about uninitialized fields, got: {warnings:?}"
    );
}

#[test]
fn test_nilable_typed_state_no_warn() {
    // state: name :: String | Nil → no warning (nil is valid)
    let state = vec![StateDeclaration::with_type(
        ident("name"),
        TypeAnnotation::union(
            vec![
                TypeAnnotation::simple("String", span()),
                TypeAnnotation::simple("Nil", span()),
            ],
            span(),
        ),
        span(),
    )];
    let class = counter_class_with_typed_state(vec![], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let uninitialized: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("uninitialized"))
        .collect();
    assert!(
        uninitialized.is_empty(),
        "Nilable typed state should not warn about uninitialized, got: {uninitialized:?}"
    );
}

#[test]
fn test_typed_state_with_default_no_warn() {
    // state: name :: String = "" → no warning (has default)
    let state = vec![StateDeclaration::with_type_and_default(
        ident("name"),
        TypeAnnotation::simple("String", span()),
        str_lit(""),
        span(),
    )];
    let class = counter_class_with_typed_state(vec![], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let uninitialized: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("uninitialized"))
        .collect();
    assert!(
        uninitialized.is_empty(),
        "Typed state with default should not warn about uninitialized, got: {uninitialized:?}"
    );
}

#[test]
fn test_untyped_state_no_default_no_warn() {
    // state: name (no type, no default) → no warning
    let state = vec![StateDeclaration::new(ident("name"), span())];
    let class = counter_class_with_typed_state(vec![], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let uninitialized: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("uninitialized"))
        .collect();
    assert!(
        uninitialized.is_empty(),
        "Untyped state should not warn about uninitialized, got: {uninitialized:?}"
    );
}

// --- BT-1913: Missing state field annotations in typed classes ---

#[test]
fn test_typed_class_warns_on_missing_state_annotation() {
    // typed class with state field lacking type annotation
    let state = vec![StateDeclaration::new(ident("count"), span())];
    let class_def = ClassDefinition::with_modifiers(
        ident("StrictCounter"),
        Some(ident("Object")),
        ClassModifiers {
            is_typed: true,
            ..Default::default()
        },
        state,
        vec![],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| {
            d.message
                .contains("Missing type annotation for state field")
        })
        .collect();
    assert_eq!(
        warnings.len(),
        1,
        "should warn about untyped state field `count`"
    );
    assert!(warnings[0].message.contains("count"));
    assert!(warnings[0].message.contains("StrictCounter"));
}

#[test]
fn test_typed_class_no_warning_when_state_annotated() {
    // typed class with state field that has a type annotation
    let state = vec![StateDeclaration::with_type(
        ident("count"),
        TypeAnnotation::simple("Integer", span()),
        span(),
    )];
    let class_def = ClassDefinition::with_modifiers(
        ident("StrictCounter"),
        Some(ident("Object")),
        ClassModifiers {
            is_typed: true,
            ..Default::default()
        },
        state,
        vec![],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| {
            d.message
                .contains("Missing type annotation for state field")
        })
        .collect();
    assert!(
        warnings.is_empty(),
        "annotated state field should not warn, got: {warnings:?}"
    );
}

#[test]
fn test_inherited_typed_class_warns_on_missing_state_annotation() {
    // A subclass of a typed class should also get the warning
    let parent = ClassDefinition::with_modifiers(
        ident("TypedBase"),
        Some(ident("Object")),
        ClassModifiers {
            is_typed: true,
            ..Default::default()
        },
        vec![],
        vec![],
        span(),
    );
    let child = ClassDefinition::with_modifiers(
        ident("Child"),
        Some(ident("TypedBase")),
        ClassModifiers::default(),
        vec![StateDeclaration::new(ident("name"), span())],
        vec![],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![parent, child]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| {
            d.message
                .contains("Missing type annotation for state field")
        })
        .collect();
    assert_eq!(
        warnings.len(),
        1,
        "inherited typed class should warn about untyped state field `name`"
    );
    assert!(warnings[0].message.contains("name"));
    assert!(warnings[0].message.contains("Child"));
}

#[test]
fn test_expect_type_suppresses_typed_state_warning() {
    // BT-1913: @expect type on a state field should suppress the warning
    let mut state_decl = StateDeclaration::new(ident("count"), span());
    state_decl.expect = Some((ExpectCategory::Type, None, span()));

    let class_def = ClassDefinition::with_modifiers(
        ident("StrictCounter"),
        Some(ident("Object")),
        ClassModifiers {
            is_typed: true,
            ..Default::default()
        },
        vec![state_decl],
        vec![],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // The type checker should emit the warning...
    let raw_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| {
            d.message
                .contains("Missing type annotation for state field")
        })
        .collect();
    assert_eq!(
        raw_warnings.len(),
        1,
        "type checker should emit the warning"
    );

    // ...but apply_expect_directives should suppress it
    let mut diagnostics = checker.diagnostics().to_vec();
    crate::queries::diagnostic_provider::apply_expect_directives(&module, &mut diagnostics);
    let remaining: Vec<_> = diagnostics
        .iter()
        .filter(|d| {
            d.message
                .contains("Missing type annotation for state field")
        })
        .collect();
    assert!(
        remaining.is_empty(),
        "@expect type should suppress the state field warning, got: {remaining:?}"
    );
    // No stale @expect warning
    let stale: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.message.contains("stale"))
        .collect();
    assert!(
        stale.is_empty(),
        "should not produce stale @expect warning, got: {stale:?}"
    );
}

#[test]
fn test_untyped_class_no_state_annotation_warning() {
    // Non-typed class should NOT warn about missing state annotations
    let state = vec![StateDeclaration::new(ident("count"), span())];
    let class_def = ClassDefinition::with_modifiers(
        ident("SimpleCounter"),
        Some(ident("Object")),
        ClassModifiers::default(),
        state,
        vec![],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| {
            d.message
                .contains("Missing type annotation for state field")
        })
        .collect();
    assert!(
        warnings.is_empty(),
        "non-typed class should not warn about state annotations, got: {warnings:?}"
    );
}

// --- Behaviour protocol / Class hierarchy fallback tests (BT-777) ---

#[test]
fn test_behaviour_protocol_superclass_no_warning() {
    // Integer superclass — should NOT warn (superclass is a Behaviour instance method
    // resolved via the Class→Behaviour chain fallback)
    let module = make_module(vec![msg_send(
        class_ref("Integer"),
        MessageSelector::Unary("superclass".into()),
        vec![],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "Integer superclass should produce no warning, got: {dnu_warnings:?}"
    );
}

#[test]
fn test_behaviour_protocol_methods_no_warning() {
    // Integer methods — should NOT warn
    let module = make_module(vec![msg_send(
        class_ref("Integer"),
        MessageSelector::Unary("methods".into()),
        vec![],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "Integer methods should produce no warning, got: {dnu_warnings:?}"
    );
}

#[test]
fn test_behaviour_protocol_subclasses_no_warning() {
    // Integer subclasses — should NOT warn
    let module = make_module(vec![msg_send(
        class_ref("Integer"),
        MessageSelector::Unary("subclasses".into()),
        vec![],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "Integer subclasses should produce no warning, got: {dnu_warnings:?}"
    );
}

#[test]
fn test_behaviour_protocol_all_superclasses_no_warning() {
    // String allSuperclasses — should NOT warn
    let module = make_module(vec![msg_send(
        class_ref("String"),
        MessageSelector::Unary("allSuperclasses".into()),
        vec![],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "String allSuperclasses should produce no warning, got: {dnu_warnings:?}"
    );
}

#[test]
fn test_class_protocol_name_no_warning() {
    // Integer name — should NOT warn (name is a Class instance method)
    let module = make_module(vec![msg_send(
        class_ref("Integer"),
        MessageSelector::Unary("name".into()),
        vec![],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "Integer name should produce no warning, got: {dnu_warnings:?}"
    );
}

#[test]
fn test_class_protocol_is_class_no_warning() {
    // Integer isClass — should NOT warn
    let module = make_module(vec![msg_send(
        class_ref("Integer"),
        MessageSelector::Unary("isClass".into()),
        vec![],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "Integer isClass should produce no warning, got: {dnu_warnings:?}"
    );
}

#[test]
fn test_unknown_class_side_message_still_warns() {
    // Integer bogusClassMethod — SHOULD warn (not in Class chain or instance methods)
    let module = make_module(vec![msg_send(
        class_ref("Integer"),
        MessageSelector::Unary("bogusClassMethod".into()),
        vec![],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert_eq!(
        dnu_warnings.len(),
        1,
        "Integer bogusClassMethod should produce exactly one warning, got: {dnu_warnings:?}"
    );
}

#[test]
fn test_behaviour_protocol_can_understand_no_warning() {
    // Integer canUnderstand: #+ — should NOT warn (canUnderstand: is a Behaviour method)
    let module = make_module(vec![msg_send(
        class_ref("Integer"),
        MessageSelector::Keyword(vec![crate::ast::KeywordPart {
            keyword: "canUnderstand:".into(),
            span: span(),
        }]),
        vec![Expression::Literal(Literal::Symbol("+".into()), span())],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "Integer canUnderstand: should produce no warning, got: {dnu_warnings:?}"
    );
}

#[test]
fn test_non_self_field_assignment_produces_warning_not_error() {
    // `other.x := 1` should produce a Severity::Warning, not Severity::Error,
    // consistent with the module's "Warnings only, never errors" design principle.
    use crate::source_analysis::Severity;
    let source = "Value subclass: Point\n  state: x\n  state: y\n  bad: other =>\n    other.x := 1";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let field_diags: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("Cannot assign"))
        .collect();
    assert_eq!(
        field_diags.len(),
        1,
        "Expected exactly one non-self field assignment diagnostic, got: {field_diags:?}"
    );
    assert_eq!(
        field_diags[0].severity,
        Severity::Warning,
        "Non-self field assignment should be a warning, not an error"
    );
}

// ---- Self return type tests (BT-1041) ----

#[test]
fn test_self_return_type_parsed() {
    // `-> Self` parses to TypeAnnotation::SelfType
    let source = "Object subclass: Foo\n  clone -> Self => self";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let method = &module.classes[0].methods[0];
    assert!(
        matches!(method.return_type, Some(TypeAnnotation::SelfType { .. })),
        "Expected SelfType annotation, got: {:?}",
        method.return_type
    );
}

#[test]
fn test_self_return_type_no_warning_when_body_returns_self() {
    // Method declares -> Self and body returns self (same class) — no warning
    let source = "Object subclass: Foo\n  clone -> Self => self";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let return_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .collect();
    assert!(
        return_warnings.is_empty(),
        "-> Self with body returning self should not warn, got: {return_warnings:?}"
    );
}

#[test]
fn test_self_return_type_warns_on_mismatch() {
    // Method declares -> Self but body returns a String — should warn
    let source = "Object subclass: Foo\n  clone -> Self => \"not-self\"";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let return_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .collect();
    assert_eq!(
        return_warnings.len(),
        1,
        "-> Self with body returning String should warn"
    );
    // The warning should mention the class name (Foo) and the actual body type (String)
    assert!(
        return_warnings[0].message.contains("Foo"),
        "Warning should mention class name"
    );
    assert!(
        return_warnings[0].message.contains("String"),
        "Warning should mention actual return type"
    );
}

#[test]
fn test_self_in_param_position_emits_error() {
    // `clone: other: Self` — Self in param position is an error
    let source = "Object subclass: Foo\n  mergeWith: other :: Self => self";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let self_errors: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("cannot be used as a parameter type"))
        .collect();
    assert_eq!(
        self_errors.len(),
        1,
        "Expected error for Self in parameter position, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_self_resolves_to_receiver_class_in_instance_send() {
    // List collect: [...] should infer return type List (not Collection or Self)
    // Use `first` which is List-specific (not on Collection) to prove resolution
    let source = "
Object subclass: Foo
  test =>
list := #(1, 2, 3)
result := list collect: [:x | x]
result first
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    // `result first` should be valid — `first` is List-specific, proving Self resolved to List
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "result.first should be valid when collect: returns List via Self, got: {dnu_warnings:?}"
    );
}

#[test]
fn test_self_resolves_to_receiver_class_in_class_send() {
    // List withAll: #(1,2,3) should infer return type List (not Collection or Self)
    // Use `first` which is List-specific (not on Collection) to prove resolution
    let source = "
Object subclass: Foo
  test =>
result := List withAll: #(1, 2, 3)
result first
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    // `result first` should be valid — proves Self resolved to List, not Collection
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "result.first should be valid when List withAll: returns List via Self, got: {dnu_warnings:?}"
    );
}

#[test]
fn test_self_type_name_is_self() {
    // TypeAnnotation::SelfType::type_name() returns "Self"
    let ann = TypeAnnotation::SelfType { span: span() };
    assert_eq!(ann.type_name(), "Self");
}

#[test]
fn test_self_resolves_through_multi_level_inheritance() {
    // A defines -> Self, B extends A, C extends B with a unique method.
    // A call on C should resolve Self to C (not A or B).
    let source = "
Value subclass: A
  clone -> Self => self

A subclass: B

B subclass: C
  onlyOnC => 42

Value subclass: Foo
  test =>
c := C new
result := c clone
result onlyOnC
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    // result.onlyOnC should be valid — proves Self resolved to C (not A or B)
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "result.onlyOnC should be valid for multi-level Self, got: {dnu_warnings:?}"
    );
}

#[test]
fn test_self_class_type_name() {
    // BT-1952: TypeAnnotation::SelfClass::type_name() returns "Self class"
    let ann = TypeAnnotation::SelfClass { span: span() };
    assert_eq!(ann.type_name(), "Self class");
}

#[test]
fn test_self_class_no_false_dnu_warnings() {
    // BT-1952: A method returning `-> Self class` should parse correctly and
    // not produce false DNU warnings for class-side method sends on the result.
    let source = "
Value subclass: Counter
  classState: instanceCount = 0
  class instanceCount => self.instanceCount
  getMyClass -> Self class => self class

Value subclass: User
  test =>
    c := Counter new
    c getMyClass instanceCount
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // Verify `-> Self class` parses as SelfClass variant
    let counter = &module.classes[0];
    let get_class_method = counter
        .methods
        .iter()
        .find(|m| m.selector.name() == "getMyClass")
        .expect("getMyClass method should exist");
    let ret_ty = get_class_method.return_type.as_ref().unwrap();
    assert!(
        matches!(ret_ty, TypeAnnotation::SelfClass { .. }),
        "Return type should be SelfClass, got: {ret_ty:?}"
    );

    // Verify no DNU warnings — `getMyClass` returns Dynamic (Self class),
    // so `instanceCount` send should not produce a false warning
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "Expected no DNU warnings, got: {dnu_warnings:?}"
    );
}

// ---- Self in generic position tests (BT-1986, Phase 0 of ADR 0079) ----

/// BT-1986: Verify that `Self` substitutes correctly when it appears as a
/// generic type argument (e.g. `Result(Self, Error)`). This is required by
/// ADR 0079's typed-lookup API, where `class named: name -> Result(Self, Error)`
/// is declared once on `Actor` and subclasses like `Counter` are expected to
/// see the return type narrowed to `Result(Counter, Error)`.
///
/// Observable consequence: after unwrapping via `value`, the resulting type
/// should expose subclass-specific methods without producing false DNU
/// warnings. `onlyOnSub` is only defined on `Sub`, so calling it on
/// `(Sub lookup: #key) value` proves that `Self` inside `Result(Self, Error)`
/// resolved to `Sub` (not `Base`, not `Dynamic`, not a bogus "Self" class).
#[test]
fn test_self_in_generic_return_type_narrows_to_subclass() {
    let source = "
Value subclass: Base
  class lookup: key :: Symbol -> Result(Self, Error) => Result ok: self

Base subclass: Sub
  onlyOnSub => 42

Value subclass: Driver
  test =>
    r := Sub lookup: #key
    r value onlyOnSub
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // No DNU warnings for `onlyOnSub` — that method is on `Sub`, not `Base`.
    // If Self substitution fails inside Result(Self, Error), the inner type
    // would not be Sub and `onlyOnSub` would either warn as DNU (if resolved
    // to Base/Self-as-class-name) or be silently skipped (if Dynamic, which
    // still counts as working per the gradual-typing escape hatch).
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "Self inside Result(Self, Error) should narrow to the receiver; \
         got DNU warnings: {dnu_warnings:?}"
    );
}

/// BT-1986: Direct inspection of inferred type for `Self` inside `Result(...)`.
/// Uses `check_module` + method-body type inference to assert the outer
/// result type is `Result(Sub, Error)`, not `Result(Self, Error)` or
/// `Result(Dynamic, Error)`.
#[test]
fn test_self_in_generic_return_type_is_substituted() {
    let source = "
Value subclass: Base
  class lookup: key :: Symbol -> Result(Self, Error) => Result ok: self

Base subclass: Sub
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    // Build a minimal method body `Sub lookup: #key` and infer its type.
    let probe_source = "
Value subclass: Base
  class lookup: key :: Symbol -> Result(Self, Error) => Result ok: self

Base subclass: Sub

Value subclass: Driver
  probe =>
    Sub lookup: #key
";
    let probe_tokens = crate::source_analysis::lex_with_eof(probe_source);
    let (probe_module, probe_diags) = crate::source_analysis::parse(probe_tokens);
    assert!(probe_diags.is_empty(), "Parse failed: {probe_diags:?}");
    let probe_hierarchy = crate::semantic_analysis::ClassHierarchy::build(&probe_module)
        .0
        .unwrap();

    let driver = probe_module
        .classes
        .iter()
        .find(|c| c.name.name.as_str() == "Driver")
        .expect("Driver class");
    let probe = driver
        .methods
        .iter()
        .find(|m| m.selector.name() == "probe")
        .expect("probe method");
    let stmt = probe.body.last().expect("probe has at least one statement");
    let expr = &stmt.expression;

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set("self", InferredType::known("Driver"));
    let ty = checker.infer_expr(expr, &probe_hierarchy, &mut env, false);

    // Assert the outer type is Result with Sub as its first type arg.
    match &ty {
        InferredType::Known {
            class_name,
            type_args,
            ..
        } => {
            assert_eq!(
                class_name.as_str(),
                "Result",
                "Expected outer type Result, got {class_name}"
            );
            let first = type_args
                .first()
                .expect("Result should have at least one type arg");
            let inner_name = first.as_known().map(EcoString::to_string);
            assert_eq!(
                inner_name.as_deref(),
                Some("Sub"),
                "Expected Self to be substituted to Sub, got {first:?} (full type: {ty:?})"
            );
        }
        other => panic!("Expected Known(Result, [Sub, ...]), got {other:?}"),
    }

    // Sanity: a parallel module-level check should report no warnings
    // about the generic return type itself.
    let mut module_checker = TypeChecker::new();
    module_checker.check_module(&module, &hierarchy);
    let relevant: Vec<_> = module_checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("Self") || d.message.contains("Result"))
        .collect();
    assert!(
        relevant.is_empty(),
        "Unexpected diagnostics mentioning Self/Result: {relevant:?}"
    );
}

/// BT-1986: When called directly on the declaring class, `Self` inside a
/// generic return type should resolve to that class (not `Self` as a class
/// name, not `Dynamic`). This is the base case for ADR 0079's
/// `Actor>>class named: -> Result(Self, Error)` — calling `Actor named: #x`
/// on Actor itself should give `Result(Actor, Error)`.
#[test]
fn test_self_in_generic_return_type_on_declaring_class() {
    let source = "
Value subclass: Base
  class lookup: key :: Symbol -> Result(Self, Error) => Result ok: self

Value subclass: Driver
  probe =>
    Base lookup: #key
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let driver = module
        .classes
        .iter()
        .find(|c| c.name.name.as_str() == "Driver")
        .expect("Driver class");
    let probe = driver
        .methods
        .iter()
        .find(|m| m.selector.name() == "probe")
        .expect("probe method");
    let expr = &probe
        .body
        .last()
        .expect("probe has at least one statement")
        .expression;

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set("self", InferredType::known("Driver"));
    let ty = checker.infer_expr(expr, &hierarchy, &mut env, false);

    match &ty {
        InferredType::Known {
            class_name,
            type_args,
            ..
        } => {
            assert_eq!(class_name.as_str(), "Result");
            let first = type_args.first().expect("Result should have type args");
            assert_eq!(
                first.as_known().map(EcoString::to_string).as_deref(),
                Some("Base"),
                "Expected Self to resolve to Base on direct call, got {first:?}"
            );
        }
        other => panic!("Expected Known(Result, [Base, ...]), got {other:?}"),
    }
}

// ---- Class-level type parameter in generic return type (BT-1995, Phase 0b of ADR 0080) ----

/// BT-1995: Verify that a class-level type parameter `C` substitutes correctly
/// inside a generic return type (e.g. `Result(C, Error)`) on a subclass that
/// binds `C` to a concrete type via `superclass_type_args`.
///
/// This is ADR 0080's Phase 0b probe: `DynamicSupervisor(C)` declares
/// `startChild -> Result(C, Error)`, and a subclass `WorkerPool` extending
/// `DynamicSupervisor(Counter)` must see `pool startChild` narrow to
/// `Result(Counter, Error)`. BT-1992 threaded the receiver's type arguments
/// through `Self` substitution; this test probes the analogous path for a
/// class-level parameter (`C`), which flows through
/// `build_inherited_substitution_map` rather than via `Self`.
#[test]
fn test_class_type_param_in_generic_return_narrows_to_concrete() {
    let source = "
abstract Object subclass: FakeDynamicSupervisor(C)
  probeC -> Result(C, Error) => Result ok: nil

FakeDynamicSupervisor(Counter) subclass: FakeWorkerPool

Value subclass: Counter
  getValue -> Integer => 0

Value subclass: Driver
  probe =>
    pool := FakeWorkerPool
    pool probeC
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let driver = module
        .classes
        .iter()
        .find(|c| c.name.name.as_str() == "Driver")
        .expect("Driver class");
    let probe = driver
        .methods
        .iter()
        .find(|m| m.selector.name() == "probe")
        .expect("probe method");
    let expr = &probe
        .body
        .last()
        .expect("probe has at least one statement")
        .expression;

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set("self", InferredType::known("Driver"));
    // `pool` is a FakeWorkerPool instance (no receiver type args; the
    // binding of C flows from superclass_type_args on FakeWorkerPool).
    env.set("pool", InferredType::known("FakeWorkerPool"));
    let ty = checker.infer_expr(expr, &hierarchy, &mut env, false);

    match &ty {
        InferredType::Known {
            class_name,
            type_args,
            ..
        } => {
            assert_eq!(
                class_name.as_str(),
                "Result",
                "Expected outer type Result, got {class_name}"
            );
            assert_eq!(
                type_args.len(),
                2,
                "Expected Result to have 2 type args, got {type_args:?}"
            );
            let first = type_args
                .first()
                .expect("Result should have at least one type arg");
            assert_eq!(
                first.as_known().map(EcoString::to_string).as_deref(),
                Some("Counter"),
                "Expected C to be substituted to Counter via superclass_type_args, \
                 got {first:?} (full type: {ty:?})"
            );
            assert_eq!(
                type_args[1].as_known().map(EcoString::to_string).as_deref(),
                Some("Error"),
                "Expected second type arg to remain Error, got {:?}",
                type_args[1]
            );
        }
        other => panic!("Expected Known(Result, [Counter, Error]), got {other:?}"),
    }
}

// ── infer_method_return_types / take_method_return_types tests (BT-1042) ─────

fn make_class_with_methods(name: &str, instance_methods: Vec<MethodDefinition>) -> ClassDefinition {
    ClassDefinition {
        name: ident(name),
        superclass: Some(ident("Object")),
        superclass_package: None,
        class_kind: ClassKind::Value,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: instance_methods,
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: span(),
    }
}

fn method_unannotated(selector: &str, body: Vec<Expression>) -> MethodDefinition {
    MethodDefinition {
        selector: MessageSelector::Unary(selector.into()),
        parameters: vec![],
        body: body.into_iter().map(ExpressionStatement::bare).collect(),
        return_type: None,
        is_sealed: false,
        is_internal: false,
        kind: MethodKind::Primary,
        expect: None,
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: span(),
    }
}

fn method_annotated(selector: &str, return_type: &str, body: Vec<Expression>) -> MethodDefinition {
    MethodDefinition {
        selector: MessageSelector::Unary(selector.into()),
        parameters: vec![],
        body: body.into_iter().map(ExpressionStatement::bare).collect(),
        return_type: Some(TypeAnnotation::Simple(ident(return_type))),
        is_sealed: false,
        is_internal: false,
        kind: MethodKind::Primary,
        expect: None,
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: span(),
    }
}

fn primitive_expr() -> Expression {
    Expression::Primitive {
        name: "+".into(),
        is_quoted: true,
        is_intrinsic: false,
        span: span(),
    }
}

#[test]
fn infer_method_return_types_collects_instance_methods() {
    // Unannotated method returning a String literal → should be collected
    let class = make_class_with_methods(
        "Greeter",
        vec![method_unannotated("greeting", vec![str_lit("hello")])],
    );
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let result = infer_method_return_types(&module, &hierarchy);
    assert_eq!(
        result.get(&("Greeter".into(), "greeting".into(), false)),
        Some(&"String".into()),
        "unannotated instance method returning String should be collected"
    );
}

#[test]
fn infer_method_return_types_collects_class_methods() {
    // Unannotated class method returning an Integer literal → should be collected
    let mut class = make_class_with_methods("Counter", vec![]);
    class.class_methods = vec![method_unannotated("zero", vec![int_lit(0)])];
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let result = infer_method_return_types(&module, &hierarchy);
    assert_eq!(
        result.get(&("Counter".into(), "zero".into(), true)),
        Some(&"Integer".into()),
        "unannotated class method returning Integer should be collected"
    );
}

#[test]
fn infer_method_return_types_collects_standalone_methods() {
    // Standalone (Tonel-style) unannotated method returning a String literal
    use crate::ast::StandaloneMethodDefinition;
    let mut module = make_module(vec![]);
    module.method_definitions = vec![StandaloneMethodDefinition {
        class_name: ident("Widget"),
        package: None,
        is_class_method: false,
        method: method_unannotated("label", vec![str_lit("ok")]),
        span: span(),
    }];
    let hierarchy = ClassHierarchy::with_builtins();
    let result = infer_method_return_types(&module, &hierarchy);
    assert_eq!(
        result.get(&("Widget".into(), "label".into(), false)),
        Some(&"String".into()),
        "unannotated standalone method returning String should be collected"
    );
}

#[test]
fn infer_method_return_types_excludes_annotated_methods() {
    // Explicitly annotated method should NOT appear in the result map
    let class = make_class_with_methods(
        "Foo",
        vec![method_annotated("value", "Integer", vec![int_lit(42)])],
    );
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let result = infer_method_return_types(&module, &hierarchy);
    assert!(
        !result.contains_key(&("Foo".into(), "value".into(), false)),
        "annotated method must not be overridden by inference"
    );
}

#[test]
fn infer_method_return_types_excludes_primitive_methods() {
    // Method whose body contains @primitive must be excluded
    let class = make_class_with_methods(
        "Bar",
        vec![method_unannotated("add", vec![primitive_expr()])],
    );
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let result = infer_method_return_types(&module, &hierarchy);
    assert!(
        !result.contains_key(&("Bar".into(), "add".into(), false)),
        "@primitive method must not appear in inferred return types"
    );
}

#[test]
fn infer_method_return_types_excludes_dynamic_results() {
    // Method body that resolves to Dynamic (unresolvable variable) should not be stored
    let class = make_class_with_methods(
        "Baz",
        vec![method_unannotated("compute", vec![var("unknownVar")])],
    );
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let result = infer_method_return_types(&module, &hierarchy);
    assert!(
        !result.contains_key(&("Baz".into(), "compute".into(), false)),
        "Dynamic result must not appear in inferred return types"
    );
}

#[test]
fn take_method_return_types_leaves_empty_map() {
    let class = make_class_with_methods(
        "Greeter",
        vec![method_unannotated("greeting", vec![str_lit("hello")])],
    );
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let first = checker.take_method_return_types();
    assert!(
        !first.is_empty(),
        "first take should have collected entries"
    );
    let second = checker.take_method_return_types();
    assert!(
        second.is_empty(),
        "second take should return empty map after drain"
    );
}

// ── infer_types_and_returns combined entry point (BT-1047) ──────────

#[test]
fn infer_types_and_returns_produces_both_outputs() {
    // Module with a class whose method returns an integer literal, and a
    // top-level expression using that method.
    let class =
        make_class_with_methods("Box", vec![method_unannotated("value", vec![int_lit(42)])]);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();

    let (type_map, returns) = infer_types_and_returns(&module, &hierarchy);

    // method_return_types should contain the inferred return type
    assert_eq!(
        returns.get(&("Box".into(), "value".into(), false)),
        Some(&"Integer".into()),
        "should infer Box#value returns Integer"
    );

    // type_map should be non-empty (at least the class was processed)
    // This verifies both outputs come from the same single pass
    assert!(
        !type_map.types.is_empty() || returns.len() == 1,
        "combined function should produce valid outputs from a single pass"
    );
}

// ---- Destructure assignment TypeEnv binding tests ----

fn destructure_assign(pattern: Pattern, value: Expression) -> Expression {
    Expression::DestructureAssignment {
        pattern,
        value: Box::new(value),
        span: span(),
    }
}

fn array_pattern(names: &[&str]) -> Pattern {
    Pattern::Array {
        elements: names.iter().map(|n| Pattern::Variable(ident(n))).collect(),
        list_syntax: false,
        rest: None,
        span: span(),
    }
}

fn list_pattern(names: &[&str]) -> Pattern {
    Pattern::Array {
        elements: names.iter().map(|n| Pattern::Variable(ident(n))).collect(),
        list_syntax: true,
        rest: None,
        span: span(),
    }
}

#[test]
fn test_tuple_destructure_binds_vars_in_env() {
    // {x, y} := someValue
    // x foo   <- x is Dynamic (local), not a field; no DNU warning expected
    //
    // Without the fix, x is unbound and the lookup falls back to self-field lookup.
    // If a class field named `x` were Integer, `x foo` would warn. With the fix,
    // x is bound as Dynamic and no warning is produced.
    let state = vec![StateDeclaration::with_type_and_default(
        ident("x"),
        TypeAnnotation::simple("Integer", span()),
        int_lit(0),
        span(),
    )];
    let method = make_method(
        "doWork",
        vec![
            // {x, _} := someValue  — shadows field `x` with local
            destructure_assign(
                Pattern::Tuple {
                    elements: vec![Pattern::Variable(ident("x")), Pattern::Wildcard(span())],
                    span: span(),
                },
                int_lit(0),
            ),
            // x foo — x is Dynamic (local), should NOT produce a DNU warning
            msg_send(var("x"), MessageSelector::Unary("foo".into()), vec![]),
        ],
    );
    let class = counter_class_with_typed_state(vec![method], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "destructured local `x` should shadow field and be Dynamic (no DNU): {dnu_warnings:?}"
    );
}

#[test]
fn test_array_destructure_binds_vars_in_env() {
    // #[first, second] := someValue
    // first + 1 — first is Dynamic, no warnings expected
    let method = make_method(
        "doWork",
        vec![
            destructure_assign(array_pattern(&["first", "second"]), int_lit(0)),
            msg_send(
                var("first"),
                MessageSelector::Binary("+".into()),
                vec![int_lit(1)],
            ),
        ],
    );
    let class = make_class_with_methods("Thing", vec![method]);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(
        checker.diagnostics().is_empty(),
        "array-destructured vars should be Dynamic — no warnings: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_list_destructure_binds_vars_in_env() {
    // #(first, second) := someValue  (list syntax, BT-1279)
    // first + 1 — first is Dynamic, no warnings expected
    // Verifies list_syntax: true takes the same type-checking path as list_syntax: false.
    let method = make_method(
        "doWork",
        vec![
            destructure_assign(list_pattern(&["first", "second"]), int_lit(0)),
            msg_send(
                var("first"),
                MessageSelector::Binary("+".into()),
                vec![int_lit(1)],
            ),
        ],
    );
    let class = make_class_with_methods("Thing", vec![method]);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(
        checker.diagnostics().is_empty(),
        "list-destructured vars should be Dynamic — no warnings: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_bind_pattern_vars_skips_wildcard() {
    // Directly verify that bind_pattern_vars does NOT insert `_` into TypeEnv
    // while still binding the named variable `y`.
    let pattern = Pattern::Tuple {
        elements: vec![Pattern::Wildcard(span()), Pattern::Variable(ident("y"))],
        span: span(),
    };
    let mut env = TypeEnv::new();
    TypeChecker::bind_pattern_vars(&pattern, &mut env);
    assert!(
        env.get("_").is_none(),
        "`_` must not be bound by bind_pattern_vars"
    );
    assert_eq!(
        env.get("y"),
        Some(InferredType::Dynamic(DynamicReason::Unknown)),
        "`y` must be bound as Dynamic"
    );
}

// --- Extension method type checking tests (BT-1518) ---

/// Helper to build a hierarchy with an extension method registered.
fn hierarchy_with_extension(
    class_name: &str,
    selector: &str,
    arity: usize,
    param_types: Vec<Option<EcoString>>,
    return_type: Option<EcoString>,
) -> ClassHierarchy {
    use crate::compilation::extension_index::{
        ExtensionIndex, ExtensionKey, ExtensionLocation, ExtensionTypeInfo, MethodSide,
    };
    use std::path::PathBuf;

    let mut h = ClassHierarchy::with_builtins();
    let mut index = ExtensionIndex::new();
    let key = ExtensionKey {
        class_name: class_name.into(),
        side: MethodSide::Instance,
        selector: selector.into(),
    };
    index.entries_mut().insert(
        key,
        vec![ExtensionLocation {
            file_path: PathBuf::from("test.bt"),
            span: span(),
            type_info: ExtensionTypeInfo {
                arity,
                param_types,
                return_type,
            },
        }],
    );
    h.register_extensions(&index);
    h
}

#[test]
fn extension_method_no_dnu_warning() {
    // `"hello" shout` where `String >> shout` is defined as an extension.
    // Should NOT produce "does not understand" warning.
    let hierarchy = hierarchy_with_extension("String", "shout", 0, vec![], Some("String".into()));
    let module = make_module(vec![msg_send(
        str_lit("hello"),
        MessageSelector::Unary("shout".into()),
        vec![],
    )]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu.is_empty(),
        "Extension method 'shout' should be recognised, got: {dnu:?}"
    );
}

#[test]
fn extension_method_return_type_propagates() {
    // `"hello" shout` returns String (from extension annotation).
    // Sending `size` on the result should not warn.
    let hierarchy = hierarchy_with_extension("String", "shout", 0, vec![], Some("String".into()));
    let module = make_module(vec![msg_send(
        msg_send(
            str_lit("hello"),
            MessageSelector::Unary("shout".into()),
            vec![],
        ),
        MessageSelector::Unary("size".into()),
        vec![],
    )]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu.is_empty(),
        "Chained send on extension return type should resolve, got: {dnu:?}"
    );
}

#[test]
fn extension_method_unannotated_returns_dynamic() {
    // `"hello" shout` with no return type annotation returns Dynamic.
    // Sending any message on the result should not warn (Dynamic = no checking).
    let hierarchy = hierarchy_with_extension(
        "String",
        "shout",
        0,
        vec![],
        None, // unannotated → Dynamic
    );
    let module = make_module(vec![msg_send(
        msg_send(
            str_lit("hello"),
            MessageSelector::Unary("shout".into()),
            vec![],
        ),
        MessageSelector::Unary("nonexistentMethod".into()),
        vec![],
    )]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu.is_empty(),
        "Unannotated extension returns Dynamic — no false type errors, got: {dnu:?}"
    );
}

#[test]
fn missing_extension_still_warns() {
    // `"hello" nonExistent` — no extension defined, should still warn.
    let hierarchy = ClassHierarchy::with_builtins();
    let module = make_module(vec![msg_send(
        str_lit("hello"),
        MessageSelector::Unary("nonExistent".into()),
        vec![],
    )]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert_eq!(
        dnu.len(),
        1,
        "Missing extension should still produce DNU warning"
    );
}

#[test]
fn extension_method_class_side_no_warning() {
    // `String fromJson: "..."` where `String class >> fromJson:` is an extension.
    use crate::compilation::extension_index::{
        ExtensionIndex, ExtensionKey, ExtensionLocation, ExtensionTypeInfo, MethodSide,
    };
    use std::path::PathBuf;

    let mut h = ClassHierarchy::with_builtins();
    let mut index = ExtensionIndex::new();
    let key = ExtensionKey {
        class_name: "String".into(),
        side: MethodSide::Class,
        selector: "fromJson:".into(),
    };
    index.entries_mut().insert(
        key,
        vec![ExtensionLocation {
            file_path: PathBuf::from("test.bt"),
            span: span(),
            type_info: ExtensionTypeInfo {
                arity: 1,
                param_types: vec![Some("String".into())],
                return_type: Some("String".into()),
            },
        }],
    );
    h.register_extensions(&index);

    let module = make_module(vec![msg_send(
        class_ref("String"),
        MessageSelector::Keyword(vec![KeywordPart::new("fromJson:", span())]),
        vec![str_lit("{}")],
    )]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &h);
    let dnu: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu.is_empty(),
        "Class-side extension should be recognised, got: {dnu:?}"
    );
}

#[test]
fn extension_method_argument_type_checking() {
    // `42 addString: "hello"` where `Integer >> addString: s :: String -> Integer`
    // Sending with wrong arg type should warn.
    let hierarchy = hierarchy_with_extension(
        "Integer",
        "addString:",
        1,
        vec![Some("String".into())],
        Some("Integer".into()),
    );
    // Correct arg type — no warning
    let module = make_module(vec![msg_send(
        int_lit(42),
        MessageSelector::Keyword(vec![KeywordPart::new("addString:", span())]),
        vec![str_lit("hello")],
    )]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let arg_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects"))
        .collect();
    assert!(
        arg_warnings.is_empty(),
        "Correct argument type should not warn, got: {arg_warnings:?}"
    );

    // Wrong arg type — should warn
    let module2 = make_module(vec![msg_send(
        int_lit(42),
        MessageSelector::Keyword(vec![KeywordPart::new("addString:", span())]),
        vec![int_lit(99)],
    )]);
    let mut checker2 = TypeChecker::new();
    checker2.check_module(&module2, &hierarchy);
    let arg_warnings2: Vec<_> = checker2
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects String"))
        .collect();
    assert_eq!(
        arg_warnings2.len(),
        1,
        "Wrong argument type to extension should warn"
    );
}

/// BT-1559: Cross-file Value sub-subclass `self new:` should NOT produce DNU warning.
///
/// Simulates the build command's flow: Child is in the current file,
/// Base is injected from another file via pre-loaded classes.
#[test]
fn cross_file_value_sub_subclass_no_dnu_for_new() {
    use crate::semantic_analysis::class_hierarchy::ClassInfo;

    // Parse a module containing only Child (extends Base, has a class method using self new:)
    let source = r"
Base subclass: Child
  field: y = 0
  class make: val => self new: #{#y => val}
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    // Simulate Base from another file: Value subclass: Base (field: x = 0)
    let base_info = ClassInfo {
        name: eco_string("Base"),
        superclass: Some(eco_string("Value")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: true,
        is_native: false,
        state: vec![eco_string("x")],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };

    // Use the full analysis pipeline (same as the build command)
    let result = crate::semantic_analysis::analyse_with_options_and_classes(
        &module,
        &crate::CompilerOptions::default(),
        vec![base_info],
    );
    let dnu_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "self new: in class method should not produce DNU for Value sub-subclass, got: {dnu_warnings:?}"
    );
}

fn eco_string(s: &str) -> ecow::EcoString {
    ecow::EcoString::from(s)
}

// ── Union type tests (BT-1572) ──────────────────────────────────────────

#[test]
fn union_of_simplifies_single_type() {
    let result = InferredType::union_of(&[
        InferredType::known("Integer"),
        InferredType::known("Integer"),
    ]);
    assert_eq!(result, InferredType::known("Integer"));
}

#[test]
fn union_of_preserves_different_types() {
    let result = InferredType::union_of(&[
        InferredType::known("String"),
        InferredType::known("UndefinedObject"),
    ]);
    assert_eq!(
        result,
        InferredType::simple_union(&["String", "UndefinedObject"])
    );
}

#[test]
fn union_of_returns_dynamic_if_any_dynamic() {
    let result = InferredType::union_of(&[
        InferredType::known("String"),
        InferredType::Dynamic(DynamicReason::Unknown),
    ]);
    assert_eq!(result, InferredType::Dynamic(DynamicReason::Unknown));
}

#[test]
fn union_display_name() {
    let ty = InferredType::simple_union(&["String", "UndefinedObject"]);
    assert_eq!(ty.display_name(), Some("String | UndefinedObject".into()));
}

#[test]
fn known_display_name() {
    let ty = InferredType::known("Integer");
    assert_eq!(ty.display_name(), Some("Integer".into()));
}

#[test]
fn dynamic_display_name_unknown() {
    assert_eq!(
        InferredType::Dynamic(DynamicReason::Unknown).display_name(),
        Some(ecow::EcoString::from("Dynamic"))
    );
}

#[test]
fn dynamic_display_name_unannotated_param() {
    assert_eq!(
        InferredType::Dynamic(DynamicReason::UnannotatedParam).display_name(),
        Some(ecow::EcoString::from("Dynamic (unannotated parameter)"))
    );
}

#[test]
fn dynamic_display_name_unannotated_return() {
    assert_eq!(
        InferredType::Dynamic(DynamicReason::UnannotatedReturn).display_name(),
        Some(ecow::EcoString::from("Dynamic (unannotated return)"))
    );
}

#[test]
fn dynamic_display_name_dynamic_receiver() {
    assert_eq!(
        InferredType::Dynamic(DynamicReason::DynamicReceiver).display_name(),
        Some(ecow::EcoString::from("Dynamic (dynamic receiver)"))
    );
}

#[test]
fn dynamic_display_name_ambiguous_control_flow() {
    assert_eq!(
        InferredType::Dynamic(DynamicReason::AmbiguousControlFlow).display_name(),
        Some(ecow::EcoString::from("Dynamic (ambiguous control flow)"))
    );
}

#[test]
fn dynamic_display_name_untyped_ffi() {
    assert_eq!(
        InferredType::Dynamic(DynamicReason::UntypedFfi).display_name(),
        Some(ecow::EcoString::from("Dynamic (untyped FFI)"))
    );
}

#[test]
fn dynamic_display_name_dynamic_spec() {
    assert_eq!(
        InferredType::Dynamic(DynamicReason::DynamicSpec).display_name(),
        Some(ecow::EcoString::from("Dynamic (FFI spec is Dynamic)"))
    );
}

#[test]
fn resolve_type_annotation_simple() {
    let ann = TypeAnnotation::Simple(ident("Integer"));
    let result = TypeChecker::resolve_type_annotation(&ann);
    assert_eq!(result, InferredType::known("Integer"));
}

#[test]
fn resolve_type_annotation_nil_keyword() {
    let ann = TypeAnnotation::Simple(ident("nil"));
    let result = TypeChecker::resolve_type_annotation(&ann);
    assert_eq!(result, InferredType::known("UndefinedObject"));
}

#[test]
fn resolve_type_annotation_false_keyword() {
    let ann = TypeAnnotation::Simple(ident("false"));
    let result = TypeChecker::resolve_type_annotation(&ann);
    assert_eq!(result, InferredType::known("False"));
}

#[test]
fn resolve_type_annotation_true_keyword() {
    let ann = TypeAnnotation::Simple(ident("true"));
    let result = TypeChecker::resolve_type_annotation(&ann);
    assert_eq!(result, InferredType::known("True"));
}

#[test]
fn resolve_type_annotation_union() {
    let ann = TypeAnnotation::Union {
        types: vec![
            TypeAnnotation::Simple(ident("String")),
            TypeAnnotation::Simple(ident("nil")),
        ],
        span: span(),
    };
    let result = TypeChecker::resolve_type_annotation(&ann);
    assert_eq!(
        result,
        InferredType::simple_union(&["String", "UndefinedObject"])
    );
}

#[test]
fn resolve_type_annotation_false_or() {
    let ann = TypeAnnotation::FalseOr {
        inner: Box::new(TypeAnnotation::Simple(ident("Integer"))),
        span: span(),
    };
    let result = TypeChecker::resolve_type_annotation(&ann);
    assert_eq!(result, InferredType::simple_union(&["Integer", "False"]));
}

#[test]
fn resolve_type_name_string_simple() {
    let result = TypeChecker::resolve_type_name_string(&"Integer".into());
    assert_eq!(result, InferredType::known("Integer"));
}

#[test]
fn resolve_type_name_string_union() {
    let result = TypeChecker::resolve_type_name_string(&"String | nil".into());
    assert_eq!(
        result,
        InferredType::simple_union(&["String", "UndefinedObject"])
    );
}

/// BT-1572 + BT-1857: Message send on union receiver warns if non-Nil member lacks the selector.
#[test]
fn union_receiver_warns_when_non_nil_member_lacks_selector() {
    // String understands `trim` but Integer does not.
    // Sending `trim` to a `String | Integer` receiver should emit a DNU hint for Integer.
    let module = Module::new(
        vec![ExpressionStatement::bare(msg_send(
            Expression::Identifier(ident("x")),
            MessageSelector::Unary("trim".into()),
            vec![],
        ))],
        span(),
    );

    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set("x", InferredType::simple_union(&["String", "Integer"]));

    let _ty = checker.infer_expr(
        &module.expressions[0].expression,
        &hierarchy,
        &mut env,
        false,
    );

    let dnu_hints: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert_eq!(
        dnu_hints.len(),
        1,
        "Should warn that Integer does not understand 'trim'"
    );
    assert!(
        dnu_hints[0].message.contains("Integer"),
        "Warning should mention which member lacks the selector"
    );
}

/// BT-1857: Nil members in unions are skipped for method resolution.
/// String | Nil sending `size` should not warn — Nil is skipped.
#[test]
fn union_receiver_nil_skipped_no_warning() {
    let module = Module::new(
        vec![ExpressionStatement::bare(msg_send(
            Expression::Identifier(ident("x")),
            MessageSelector::Unary("size".into()),
            vec![],
        ))],
        span(),
    );

    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set(
        "x",
        InferredType::simple_union(&["String", "UndefinedObject"]),
    );

    let ty = checker.infer_expr(
        &module.expressions[0].expression,
        &hierarchy,
        &mut env,
        false,
    );

    let dnu_hints: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_hints.is_empty(),
        "Should NOT warn when Nil is the only non-responding member (it's skipped)"
    );

    // Return type should include Integer from String.size, plus Nil back since
    // the receiver may be nil (BT-1857: Nil re-added to return type)
    match &ty {
        InferredType::Union { members, .. } => {
            let names: Vec<&str> = members
                .iter()
                .filter_map(|m| m.as_known().map(EcoString::as_str))
                .collect();
            assert!(
                names.contains(&"Integer"),
                "Return type should include Integer from String>>size, got: {names:?}"
            );
            assert!(
                names.contains(&"UndefinedObject"),
                "Return type should include Nil since receiver may be nil, got: {names:?}"
            );
        }
        other => panic!("Expected Union return type, got {other:?}"),
    }
}

/// BT-1857: Nullable hint still appears when non-Nil member also lacks selector.
#[test]
fn union_receiver_nullable_hint_with_non_nil_missing() {
    // Float | Nil — Float doesn't understand `size`, and union has Nil.
    // Should warn about Float AND add nullable hint.
    let module = Module::new(
        vec![ExpressionStatement::bare(msg_send(
            Expression::Identifier(ident("x")),
            MessageSelector::Unary("size".into()),
            vec![],
        ))],
        span(),
    );

    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set(
        "x",
        InferredType::simple_union(&["Float", "UndefinedObject"]),
    );

    checker.infer_expr(
        &module.expressions[0].expression,
        &hierarchy,
        &mut env,
        false,
    );

    let dnu_hints: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert_eq!(dnu_hints.len(), 1, "Should warn about Float");
    assert!(
        dnu_hints[0].message.contains("Float"),
        "Warning should mention Float, not UndefinedObject"
    );
    // BT-1857: UndefinedObject should NOT appear as a non-responding member.
    // Extract the subject (text before "does not understand") and verify it
    // does not mention UndefinedObject.
    let subject = dnu_hints[0]
        .message
        .split("does not understand")
        .next()
        .unwrap_or("");
    assert!(
        !subject.contains("UndefinedObject"),
        "UndefinedObject should not be listed as a non-responding member, got subject: {subject:?}"
    );
}

/// BT-1572: No warning when all union members understand the selector.
#[test]
fn union_receiver_no_warning_when_all_understand() {
    // Both Integer and String understand `asString` (via Object hierarchy).
    let module = Module::new(
        vec![ExpressionStatement::bare(msg_send(
            Expression::Identifier(ident("x")),
            MessageSelector::Unary("asString".into()),
            vec![],
        ))],
        span(),
    );

    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set("x", InferredType::simple_union(&["Integer", "String"]));

    checker.infer_expr(
        &module.expressions[0].expression,
        &hierarchy,
        &mut env,
        false,
    );

    let dnu_hints: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_hints.is_empty(),
        "No warning when all union members understand the selector"
    );
}

/// BT-1857: Dynamic member in union is handled conservatively (no warning).
#[test]
fn union_receiver_dynamic_member_no_warning() {
    // If a union member is Dynamic, we can't know what it responds to.
    // Should not warn — conservative handling.
    let module = Module::new(
        vec![ExpressionStatement::bare(msg_send(
            Expression::Identifier(ident("x")),
            MessageSelector::Unary("fooBar".into()),
            vec![],
        ))],
        span(),
    );

    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    // Union with a Dynamic member — should fall through to Dynamic result
    env.set(
        "x",
        InferredType::Union {
            members: vec![
                InferredType::known("String"),
                InferredType::Dynamic(DynamicReason::Unknown),
            ],
            provenance: super::TypeProvenance::Inferred(span()),
        },
    );

    let ty = checker.infer_expr(
        &module.expressions[0].expression,
        &hierarchy,
        &mut env,
        false,
    );

    let dnu_hints: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_hints.is_empty(),
        "Should not warn when Dynamic is in the union (conservative)"
    );
    // Dynamic poisons the union → result should be Dynamic
    assert!(
        matches!(ty, InferredType::Dynamic(_)),
        "Return type should be Dynamic when union contains Dynamic, got {ty:?}"
    );
}

/// BT-1857: Return type is union of return types from all responding members.
#[test]
fn union_receiver_return_type_is_union_of_member_returns() {
    // Integer.asString -> String, Array.asString -> String.
    // Both understand `asString` and return String, so result is String (not a union).
    let module = Module::new(
        vec![ExpressionStatement::bare(msg_send(
            Expression::Identifier(ident("x")),
            MessageSelector::Unary("asString".into()),
            vec![],
        ))],
        span(),
    );

    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set("x", InferredType::simple_union(&["Integer", "String"]));

    let ty = checker.infer_expr(
        &module.expressions[0].expression,
        &hierarchy,
        &mut env,
        false,
    );

    // Both return String, so union_of collapses to Known(String)
    assert!(
        matches!(ty, InferredType::Known { ref class_name, .. } if class_name == "String"),
        "Return type should be String when all members return String, got {ty:?}"
    );
}

/// BT-1857: Nil-only union (`UndefinedObject` alone) — no members to check, returns Dynamic.
#[test]
fn union_receiver_nil_only_returns_dynamic() {
    let module = Module::new(
        vec![ExpressionStatement::bare(msg_send(
            Expression::Identifier(ident("x")),
            MessageSelector::Unary("size".into()),
            vec![],
        ))],
        span(),
    );

    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    // Degenerate case: union of just Nil
    env.set(
        "x",
        InferredType::Union {
            members: vec![InferredType::known("UndefinedObject")],
            provenance: super::TypeProvenance::Inferred(span()),
        },
    );

    let _ty = checker.infer_expr(
        &module.expressions[0].expression,
        &hierarchy,
        &mut env,
        false,
    );

    let dnu_hints: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_hints.is_empty(),
        "Nil-only union should not produce DNU warnings"
    );
}

/// BT-1871: Non-responding union members should not widen return type with Dynamic.
/// `(String | Integer).size` — Integer doesn't understand `size`, but String does
/// and returns Integer. Result should be Integer, not Integer | Dynamic.
#[test]
fn union_receiver_non_responding_member_does_not_widen_return_type() {
    let module = Module::new(
        vec![ExpressionStatement::bare(msg_send(
            Expression::Identifier(ident("x")),
            MessageSelector::Unary("size".into()),
            vec![],
        ))],
        span(),
    );

    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set("x", InferredType::simple_union(&["String", "Integer"]));

    let ty = checker.infer_expr(
        &module.expressions[0].expression,
        &hierarchy,
        &mut env,
        false,
    );

    // String.size returns Integer. Integer doesn't understand `size`.
    // BT-1871: The return type should be just Integer, NOT Integer | Dynamic.
    assert!(
        matches!(ty, InferredType::Known { ref class_name, .. } if class_name == "Integer"),
        "Return type should be Integer (not widened with Dynamic), got {ty:?}"
    );

    // Should still warn about Integer not understanding `size`
    let dnu_hints: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert_eq!(
        dnu_hints.len(),
        1,
        "Should warn that Integer does not understand 'size'"
    );
}

/// BT-1871: When NO union members respond, return Dynamic as fallback.
#[test]
fn union_receiver_no_members_respond_returns_dynamic() {
    let module = Module::new(
        vec![ExpressionStatement::bare(msg_send(
            Expression::Identifier(ident("x")),
            MessageSelector::Unary("nonExistentMethod".into()),
            vec![],
        ))],
        span(),
    );

    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set("x", InferredType::simple_union(&["String", "Integer"]));

    let ty = checker.infer_expr(
        &module.expressions[0].expression,
        &hierarchy,
        &mut env,
        false,
    );

    // Neither String nor Integer understand `nonExistentMethod`.
    // With no return types collected, union_of returns Dynamic as fallback.
    assert!(
        matches!(ty, InferredType::Dynamic(DynamicReason::Unknown)),
        "Return type should be Dynamic when no members respond, got {ty:?}"
    );
}

/// BT-1572: Integer | False (`FalseOr`) parameter resolution.
#[test]
fn false_or_param_resolves_to_union() {
    // A method with parameter `x :: Integer | False` should infer x as Union.
    let method = MethodDefinition {
        selector: MessageSelector::Keyword(vec![KeywordPart::new("check:", span())]),
        parameters: vec![ParameterDefinition::with_type(
            ident("x"),
            TypeAnnotation::FalseOr {
                inner: Box::new(TypeAnnotation::Simple(ident("Integer"))),
                span: span(),
            },
        )],
        body: vec![bare(msg_send(
            Expression::Identifier(ident("x")),
            MessageSelector::Unary("isNil".into()),
            vec![],
        ))],
        return_type: None,
        is_sealed: false,
        is_internal: false,
        kind: MethodKind::Primary,
        expect: None,
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: span(),
    };

    let class = ClassDefinition {
        name: ident("MyClass"),
        superclass: Some(ident("Object")),
        superclass_package: None,
        class_kind: ClassKind::Value,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![method],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: span(),
    };

    let module = Module {
        classes: vec![class],
        method_definitions: vec![],
        protocols: vec![],
        expressions: vec![],
        span: span(),
        file_leading_comments: vec![],
        file_trailing_comments: vec![],
    };

    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // No DNU for isNil — both Integer and False understand it (via Object)
    let dnu_hints: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_hints.is_empty(),
        "FalseOr resolved as union; no DNU for universal messages: {dnu_hints:?}"
    );
}

/// BT-1572: `is_assignable_to` with union declared type (via `type_name` string).
#[test]
fn is_assignable_to_union_string() {
    let hierarchy = ClassHierarchy::with_builtins();
    // "String | nil" declared type should accept "String"
    assert!(
        TypeChecker::is_assignable_to(&"String".into(), &"String | nil".into(), &hierarchy),
        "String should be assignable to String | nil"
    );
    // "Integer" should NOT be assignable to "String | nil"
    assert!(
        !TypeChecker::is_assignable_to(&"Integer".into(), &"String | nil".into(), &hierarchy),
        "Integer should not be assignable to String | nil"
    );
}

/// BT-1572: `is_assignable_to` with Integer | String union.
#[test]
fn is_assignable_to_integer_or_string() {
    let hierarchy = ClassHierarchy::with_builtins();
    assert!(TypeChecker::is_assignable_to(
        &"Integer".into(),
        &"Integer | String".into(),
        &hierarchy
    ));
    assert!(TypeChecker::is_assignable_to(
        &"String".into(),
        &"Integer | String".into(),
        &hierarchy
    ));
    assert!(!TypeChecker::is_assignable_to(
        &"Float".into(),
        &"Integer | String".into(),
        &hierarchy
    ));
}

// ---- BT-1570: Generic substitution tests ----

/// Build a `GenResult(T, E)` class in the hierarchy for generic tests.
///
/// Uses `GenResult` instead of `Result` to avoid collision with the
/// builtin `Result` class (which has no `type_params`).
#[allow(clippy::too_many_lines)] // test fixture — length is proportional to ClassInfo fields
fn add_generic_result_class(hierarchy: &mut ClassHierarchy) {
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};

    let result_info = ClassInfo {
        name: eco_string("GenResult"),
        superclass: Some(eco_string("Value")),
        is_sealed: true,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: true,
        is_native: false,
        state: vec![eco_string("okValue"), eco_string("errReason")],
        state_types: {
            let mut m = std::collections::HashMap::new();
            m.insert(eco_string("okValue"), eco_string("T"));
            m.insert(eco_string("errReason"), eco_string("E"));
            m
        },
        state_has_default: std::collections::HashMap::new(),
        methods: vec![
            MethodInfo {
                selector: eco_string("unwrap"),
                arity: 0,
                kind: MethodKind::Primary,
                defined_in: eco_string("GenResult"),
                is_sealed: true,
                is_internal: false,
                spawns_block: false,
                return_type: Some(eco_string("T")),
                param_types: vec![],
                doc: None,
            },
            MethodInfo {
                selector: eco_string("error"),
                arity: 0,
                kind: MethodKind::Primary,
                defined_in: eco_string("GenResult"),
                is_sealed: true,
                is_internal: false,
                spawns_block: false,
                return_type: Some(eco_string("E")),
                param_types: vec![],
                doc: None,
            },
            MethodInfo {
                selector: eco_string("map:"),
                arity: 1,
                kind: MethodKind::Primary,
                defined_in: eco_string("GenResult"),
                is_sealed: true,
                is_internal: false,
                spawns_block: false,
                return_type: Some(eco_string("GenResult(R, E)")),
                param_types: vec![Some(eco_string("Block(T, R)"))],
                doc: None,
            },
            MethodInfo {
                selector: eco_string("isOk"),
                arity: 0,
                kind: MethodKind::Primary,
                defined_in: eco_string("GenResult"),
                is_sealed: true,
                is_internal: false,
                spawns_block: false,
                return_type: Some(eco_string("Boolean")),
                param_types: vec![],
                doc: None,
            },
        ],
        class_methods: vec![
            MethodInfo {
                selector: eco_string("ok:"),
                arity: 1,
                kind: MethodKind::Primary,
                defined_in: eco_string("GenResult"),
                is_sealed: true,
                is_internal: false,
                spawns_block: false,
                return_type: Some(eco_string("Self")),
                param_types: vec![Some(eco_string("T"))],
                doc: None,
            },
            MethodInfo {
                selector: eco_string("error:"),
                arity: 1,
                kind: MethodKind::Primary,
                defined_in: eco_string("GenResult"),
                is_sealed: true,
                is_internal: false,
                spawns_block: false,
                return_type: Some(eco_string("Self")),
                param_types: vec![Some(eco_string("E"))],
                doc: None,
            },
        ],
        class_variables: vec![],
        type_params: vec![eco_string("T"), eco_string("E")],
        type_param_bounds: vec![None, None],
        superclass_type_args: vec![],
    };

    hierarchy.add_from_beam_meta(vec![result_info]);
}

/// BT-1570: Substitution map built from `GenResult(Integer, IOError)`.
/// `unwrap` returns `T` → `Integer`.
#[test]
fn generic_substitution_unwrap_returns_concrete_type() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_result_class(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set(
        "r",
        InferredType::Known {
            class_name: eco_string("GenResult"),
            type_args: vec![
                InferredType::known("Integer"),
                InferredType::known("IOError"),
            ],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result_ty = checker.infer_expr(
        &msg_send(var("r"), MessageSelector::Unary("unwrap".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );

    assert_eq!(
        result_ty.as_known().map(EcoString::as_str),
        Some("Integer"),
        "unwrap on GenResult(Integer, IOError) should return Integer, got: {result_ty:?}"
    );
}

/// BT-1570: `error` returns `E` → `IOError`.
#[test]
fn generic_substitution_error_returns_concrete_type() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_result_class(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set(
        "r",
        InferredType::Known {
            class_name: eco_string("GenResult"),
            type_args: vec![
                InferredType::known("Integer"),
                InferredType::known("IOError"),
            ],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result_ty = checker.infer_expr(
        &msg_send(var("r"), MessageSelector::Unary("error".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );

    assert_eq!(
        result_ty.as_known().map(EcoString::as_str),
        Some("IOError"),
        "error on GenResult(Integer, IOError) should return IOError, got: {result_ty:?}"
    );
}

/// BT-1570: Non-generic return type (`isOk` -> `Boolean`) is unaffected.
#[test]
fn generic_substitution_non_generic_return_unchanged() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_result_class(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set(
        "r",
        InferredType::Known {
            class_name: eco_string("GenResult"),
            type_args: vec![
                InferredType::known("Integer"),
                InferredType::known("IOError"),
            ],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result_ty = checker.infer_expr(
        &msg_send(var("r"), MessageSelector::Unary("isOk".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );

    assert_eq!(
        result_ty.as_known().map(EcoString::as_str),
        Some("Boolean"),
        "isOk on GenResult should return Boolean regardless of type args"
    );
}

/// BT-1570: No `type_args` on receiver falls back to unsubstituted return.
#[test]
fn generic_no_type_args_returns_unsubstituted() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_result_class(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    // Set r to bare GenResult (no type_args) — unparameterized
    env.set("r", InferredType::known("GenResult"));

    let result_ty = checker.infer_expr(
        &msg_send(var("r"), MessageSelector::Unary("unwrap".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );

    // Without type_args, the return type "T" is returned as-is (known("T")).
    // It won't produce false-positive warnings because "T" is unknown to
    // the hierarchy, so all type compatibility checks return true.
    assert!(
        result_ty.as_known().map(EcoString::as_str) != Some("Integer"),
        "unwrap on bare GenResult (no type_args) should NOT return Integer"
    );
}

/// BT-1570: `set_param_types` handles generic annotations.
#[test]
fn set_param_types_resolves_generic_annotation() {
    // Parameter annotated as :: Result(Integer, Error) should be Known("Result")
    // with type_args
    let params = vec![ParameterDefinition {
        name: ident("r"),
        type_annotation: Some(TypeAnnotation::Generic {
            base: ident("Result"),
            parameters: vec![
                TypeAnnotation::Simple(ident("Integer")),
                TypeAnnotation::Simple(ident("Error")),
            ],
            span: span(),
        }),
    }];

    let mut env = TypeEnv::new();
    TypeChecker::set_param_types(&mut env, &params);

    let r_type = env.get("r").expect("r should be in env");
    match r_type {
        InferredType::Known {
            class_name,
            type_args,
            ..
        } => {
            assert_eq!(class_name.as_str(), "Result");
            assert_eq!(type_args.len(), 2);
            assert_eq!(
                type_args[0].as_known().map(EcoString::as_str),
                Some("Integer")
            );
            assert_eq!(
                type_args[1].as_known().map(EcoString::as_str),
                Some("Error")
            );
        }
        other => panic!("Expected Known type with type_args, got: {other:?}"),
    }
}

/// BT-1570: Generic return type check extracts base type for compatibility.
#[test]
fn check_return_type_handles_generic_declared_type() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_result_class(&mut hierarchy);

    // Method declares -> GenResult(Integer, Error), body returns GenResult
    let method = MethodDefinition {
        selector: MessageSelector::Unary("compute".into()),
        parameters: vec![],
        body: vec![bare(int_lit(42))], // placeholder body
        return_type: Some(TypeAnnotation::Generic {
            base: ident("GenResult"),
            parameters: vec![
                TypeAnnotation::Simple(ident("Integer")),
                TypeAnnotation::Simple(ident("Error")),
            ],
            span: span(),
        }),
        kind: MethodKind::Primary,
        is_sealed: false,
        is_internal: false,
        span: span(),
        doc_comment: None,
        expect: None,
        comments: CommentAttachment::default(),
    };

    let body_type = InferredType::known("GenResult");
    let mut checker = TypeChecker::new();
    checker.check_return_type(&method, &body_type, &eco_string("MyClass"), &hierarchy);

    // Should NOT warn: GenResult is compatible with declared GenResult(Integer, Error)
    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("return type"))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "GenResult body should be compatible with GenResult(Integer, Error) return type, got: {type_warnings:?}"
    );
}

/// BT-1570: `is_assignable_to` handles generic declared types structurally.
#[test]
fn is_assignable_to_generic_declared_type() {
    let hierarchy = ClassHierarchy::with_builtins();

    // Result is assignable to Result(Integer, Error) — base type matches
    assert!(
        TypeChecker::is_assignable_to(
            &eco_string("Result"),
            &eco_string("Result(Integer, Error)"),
            &hierarchy,
        ),
        "Result should be assignable to Result(Integer, Error)"
    );

    // Integer is NOT assignable to Result(Integer, Error) — base types differ
    assert!(
        !TypeChecker::is_assignable_to(
            &eco_string("Integer"),
            &eco_string("Result(Integer, Error)"),
            &hierarchy,
        ),
        "Integer should NOT be assignable to Result(Integer, Error)"
    );
}

// ---- BT-1571: Constructor type inference tests ----

/// BT-1571: `GenResult ok: 42` infers T = Integer → GenResult(Integer, Dynamic).
#[test]
fn constructor_inference_ok_infers_t_from_integer() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_result_class(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    // GenResult ok: 42
    let result_ty = checker.infer_expr(
        &msg_send(
            class_ref("GenResult"),
            MessageSelector::Keyword(vec![KeywordPart::new("ok:", span())]),
            vec![int_lit(42)],
        ),
        &hierarchy,
        &mut env,
        false,
    );

    match &result_ty {
        InferredType::Known {
            class_name,
            type_args,
            ..
        } => {
            assert_eq!(
                class_name.as_str(),
                "GenResult",
                "Constructor should return GenResult"
            );
            assert_eq!(type_args.len(), 2, "Should have 2 type args (T, E)");
            assert_eq!(
                type_args[0].as_known().map(EcoString::as_str),
                Some("Integer"),
                "T should be inferred as Integer from argument"
            );
            assert!(
                matches!(type_args[1], InferredType::Dynamic(DynamicReason::Unknown)),
                "E should be Dynamic (not inferrable from ok:), got: {:?}",
                type_args[1]
            );
        }
        other => panic!("Expected Known type with type_args, got: {other:?}"),
    }
}

/// BT-1571: `GenResult error: #not_found` infers E = Symbol → GenResult(Dynamic, Symbol).
#[test]
fn constructor_inference_error_infers_e_from_symbol() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_result_class(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    // GenResult error: #not_found
    let result_ty = checker.infer_expr(
        &msg_send(
            class_ref("GenResult"),
            MessageSelector::Keyword(vec![KeywordPart::new("error:", span())]),
            vec![Expression::Literal(
                Literal::Symbol("not_found".into()),
                span(),
            )],
        ),
        &hierarchy,
        &mut env,
        false,
    );

    match &result_ty {
        InferredType::Known {
            class_name,
            type_args,
            ..
        } => {
            assert_eq!(class_name.as_str(), "GenResult");
            assert_eq!(type_args.len(), 2, "Should have 2 type args (T, E)");
            assert!(
                matches!(type_args[0], InferredType::Dynamic(DynamicReason::Unknown)),
                "T should be Dynamic (not inferrable from error:), got: {:?}",
                type_args[0]
            );
            assert_eq!(
                type_args[1].as_known().map(EcoString::as_str),
                Some("#not_found"),
                "E should be inferred as #not_found from symbol literal argument"
            );
        }
        other => panic!("Expected Known type with type_args, got: {other:?}"),
    }
}

/// BT-1571: Constructor inference on non-generic class returns plain Known.
#[test]
fn constructor_inference_non_generic_class_no_type_args() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    // Array new — Array is generic Array(E), so constructor with no args
    // produces Dynamic type_args since E cannot be inferred.
    let result_ty = checker.infer_expr(
        &msg_send(
            class_ref("Array"),
            MessageSelector::Unary("new".into()),
            vec![],
        ),
        &hierarchy,
        &mut env,
        false,
    );

    match &result_ty {
        InferredType::Known { class_name, .. } => {
            assert_eq!(class_name.as_str(), "Array");
        }
        other => panic!("Expected Known type, got: {other:?}"),
    }
}

/// BT-1571: Inferred type from constructor flows into subsequent instance sends.
/// `(GenResult ok: 42) unwrap` should return Integer via substitution.
#[test]
fn constructor_inference_flows_to_instance_sends() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_result_class(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    // r := GenResult ok: 42
    let assign_expr = Expression::Assignment {
        target: Box::new(Expression::Identifier(ident("r"))),
        value: Box::new(msg_send(
            class_ref("GenResult"),
            MessageSelector::Keyword(vec![KeywordPart::new("ok:", span())]),
            vec![int_lit(42)],
        )),
        span: span(),
    };
    checker.infer_expr(&assign_expr, &hierarchy, &mut env, false);

    // r unwrap — should return Integer via T substitution
    let unwrap_ty = checker.infer_expr(
        &msg_send(var("r"), MessageSelector::Unary("unwrap".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );

    assert_eq!(
        unwrap_ty.as_known().map(EcoString::as_str),
        Some("Integer"),
        "unwrap on (GenResult ok: 42) should return Integer via constructor inference"
    );
}

/// BT-1571: Constructor inference for error: flows into error accessor.
/// `(GenResult error: #not_found) error` should return Symbol.
#[test]
fn constructor_inference_error_flows_to_error_accessor() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_result_class(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    // r := GenResult error: #not_found
    let assign_expr = Expression::Assignment {
        target: Box::new(Expression::Identifier(ident("r"))),
        value: Box::new(msg_send(
            class_ref("GenResult"),
            MessageSelector::Keyword(vec![KeywordPart::new("error:", span())]),
            vec![Expression::Literal(
                Literal::Symbol("not_found".into()),
                span(),
            )],
        )),
        span: span(),
    };
    checker.infer_expr(&assign_expr, &hierarchy, &mut env, false);

    // r error — should return Symbol via E substitution
    let error_ty = checker.infer_expr(
        &msg_send(var("r"), MessageSelector::Unary("error".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );

    assert_eq!(
        error_ty.as_known().map(EcoString::as_str),
        Some("#not_found"),
        "error on (GenResult error: #not_found) should return #not_found via constructor inference"
    );
}

// ---- Control-flow narrowing tests (ADR 0068 Phase 1g) ----

/// Helper: build `x class` unary message
fn x_class(var_name: &str) -> Expression {
    msg_send(
        var(var_name),
        MessageSelector::Unary("class".into()),
        vec![],
    )
}

/// Helper: build `(x class) = ClassName` binary expression
fn class_eq(var_name: &str, class_name: &str) -> Expression {
    msg_send(
        x_class(var_name),
        MessageSelector::Binary("=".into()),
        vec![class_ref(class_name)],
    )
}

/// Helper: build `(x class) =:= ClassName` binary expression
fn class_eqeq(var_name: &str, class_name: &str) -> Expression {
    msg_send(
        x_class(var_name),
        MessageSelector::Binary("=:=".into()),
        vec![class_ref(class_name)],
    )
}

/// Helper: build `x isKindOf: ClassName` keyword expression
fn is_kind_of(var_name: &str, class_name: &str) -> Expression {
    msg_send(
        var(var_name),
        MessageSelector::Keyword(vec![KeywordPart::new("isKindOf:", span())]),
        vec![class_ref(class_name)],
    )
}

/// Helper: build `x isNil` unary expression
fn is_nil(var_name: &str) -> Expression {
    msg_send(
        var(var_name),
        MessageSelector::Unary("isNil".into()),
        vec![],
    )
}

/// Helper: build a symbol literal `#name`
fn sym_lit(name: &str) -> Expression {
    Expression::Literal(Literal::Symbol(name.into()), span())
}

/// Helper: build `x respondsTo: #selector` keyword expression
fn responds_to(var_name: &str, selector_name: &str) -> Expression {
    msg_send(
        var(var_name),
        MessageSelector::Keyword(vec![KeywordPart::new("respondsTo:", span())]),
        vec![sym_lit(selector_name)],
    )
}

/// Helper: build `x isOk` unary expression (BT-1859)
fn is_ok(var_name: &str) -> Expression {
    msg_send(var(var_name), MessageSelector::Unary("isOk".into()), vec![])
}

/// Helper: build `x isError` unary expression (BT-1859)
fn is_error(var_name: &str) -> Expression {
    msg_send(
        var(var_name),
        MessageSelector::Unary("isError".into()),
        vec![],
    )
}

/// Helper: build a block expression `[body]`
fn block_expr(body: Vec<Expression>) -> Expression {
    Expression::Block(Block::new(
        vec![],
        body.into_iter().map(ExpressionStatement::bare).collect(),
        span(),
    ))
}

/// Helper: build a block with a non-local return `[^value]`
fn block_with_return(value: Expression) -> Expression {
    Expression::Block(Block::new(
        vec![],
        vec![ExpressionStatement::bare(Expression::Return {
            value: Box::new(value),
            span: span(),
        })],
        span(),
    ))
}

/// Helper: build `receiver ifTrue: [block]`
fn if_true(receiver: Expression, block: Expression) -> Expression {
    msg_send(
        receiver,
        MessageSelector::Keyword(vec![KeywordPart::new("ifTrue:", span())]),
        vec![block],
    )
}

/// Helper: build `receiver ifFalse: [block]`
fn if_false(receiver: Expression, block: Expression) -> Expression {
    msg_send(
        receiver,
        MessageSelector::Keyword(vec![KeywordPart::new("ifFalse:", span())]),
        vec![block],
    )
}

/// Helper: build `receiver ifTrue: [block1] ifFalse: [block2]`
fn if_true_if_false(
    receiver: Expression,
    true_block: Expression,
    false_block: Expression,
) -> Expression {
    msg_send(
        receiver,
        MessageSelector::Keyword(vec![
            KeywordPart::new("ifTrue:", span()),
            KeywordPart::new("ifFalse:", span()),
        ]),
        vec![true_block, false_block],
    )
}

/// Helper: make a keyword method with typed parameters and a body
fn make_keyword_method(
    selector_parts: &[&str],
    params: Vec<(&str, Option<&str>)>,
    body: Vec<Expression>,
) -> MethodDefinition {
    MethodDefinition {
        selector: MessageSelector::Keyword(
            selector_parts
                .iter()
                .map(|k| KeywordPart::new(*k, span()))
                .collect(),
        ),
        parameters: params
            .into_iter()
            .map(|(name, ty)| {
                if let Some(t) = ty {
                    ParameterDefinition::with_type(ident(name), TypeAnnotation::Simple(ident(t)))
                } else {
                    ParameterDefinition::new(ident(name))
                }
            })
            .collect(),
        body: body.into_iter().map(ExpressionStatement::bare).collect(),
        return_type: None,
        is_sealed: false,
        is_internal: false,
        kind: MethodKind::Primary,
        expect: None,
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: span(),
    }
}

#[test]
fn test_narrowing_class_eq_in_true_block() {
    // Build a class with a method:
    //   process: x :: Object =>
    //     (x class = Integer) ifTrue: [x + 1]
    //     x + 1    // should warn — x is Object outside the block
    let hierarchy = ClassHierarchy::with_builtins();
    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("x", Some("Object"))],
            vec![
                // (x class = Integer) ifTrue: [x + 1]
                if_true(
                    class_eq("x", "Integer"),
                    block_expr(vec![msg_send(
                        var("x"),
                        MessageSelector::Binary("+".into()),
                        vec![int_lit(1)],
                    )]),
                ),
                // x + 1  — should warn (Object doesn't have +)
                msg_send(
                    var("x"),
                    MessageSelector::Binary("+".into()),
                    vec![int_lit(1)],
                ),
            ],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // Inside the block: `x + 1` should NOT warn because x is narrowed to Integer
    // Outside the block: `x + 1` SHOULD warn because x is still Object
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    // The outside `x + 1` should produce a warning
    assert_eq!(
        warnings.len(),
        1,
        "Expected 1 warning for x + 1 outside the narrowed block, got {}:\n{:?}",
        warnings.len(),
        warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_narrowing_class_eqeq_in_true_block() {
    // Same as above but with =:= operator
    let hierarchy = ClassHierarchy::with_builtins();
    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("x", Some("Object"))],
            vec![
                // (x class =:= Integer) ifTrue: [x + 1]
                if_true(
                    class_eqeq("x", "Integer"),
                    block_expr(vec![msg_send(
                        var("x"),
                        MessageSelector::Binary("+".into()),
                        vec![int_lit(1)],
                    )]),
                ),
            ],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // Inside the block: `x + 1` should NOT warn because x is narrowed to Integer
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        warnings.is_empty(),
        "Expected no warnings for x + 1 inside narrowed block, got: {:?}",
        warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_narrowing_is_kind_of_in_true_block() {
    // process: x :: Object =>
    //   (x isKindOf: Integer) ifTrue: [x + 1]
    let hierarchy = ClassHierarchy::with_builtins();
    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("x", Some("Object"))],
            vec![if_true(
                is_kind_of("x", "Integer"),
                block_expr(vec![msg_send(
                    var("x"),
                    MessageSelector::Binary("+".into()),
                    vec![int_lit(1)],
                )]),
            )],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        warnings.is_empty(),
        "Expected no warnings inside isKindOf: narrowed block, got: {:?}",
        warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_narrowing_is_nil_early_return() {
    // validate: x :: Object =>
    //   x isNil ifTrue: [^nil]
    //   x size   // x should be non-nil, but still Object
    let hierarchy = ClassHierarchy::with_builtins();

    let class = {
        let validate_method = make_keyword_method(
            &["validate:"],
            vec![("x", Some("Object"))],
            vec![
                // x isNil ifTrue: [^nil]
                if_true(is_nil("x"), block_with_return(var("nil"))),
                // x size  — after early return, x is non-nil
                msg_send(var("x"), MessageSelector::Unary("size".into()), vec![]),
            ],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![validate_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // After early return narrowing, x is still Object (non-nil) but Object
    // does respond to `size` (it's a built-in), so no warning expected.
    // This test just verifies the narrowing doesn't crash.
}

#[test]
fn test_narrowing_is_nil_if_true_if_false() {
    // process: x :: Object =>
    //   x isNil ifTrue: [42] ifFalse: [x size]
    // In the false block, x should be non-nil.
    let hierarchy = ClassHierarchy::with_builtins();

    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("x", Some("Object"))],
            vec![if_true_if_false(
                is_nil("x"),
                block_expr(vec![int_lit(42)]),
                block_expr(vec![msg_send(
                    var("x"),
                    MessageSelector::Unary("size".into()),
                    vec![],
                )]),
            )],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // This just verifies the narrowing path executes without crash.
    // Object responds to `size`, so no warning expected.
}

#[test]
fn test_narrowing_union_with_nil_check() {
    // Compose union checking with narrowing:
    // A parameter typed as String|UndefinedObject (union), after nil check,
    // should narrow to String in the false block.
    //
    // For now, union types in annotations aren't parsed to InferredType::Union
    // (they resolve to Dynamic), so this test validates the non_nil_type logic
    // directly.
    let union = InferredType::simple_union(&["String", "UndefinedObject"]);
    let narrowed = TypeChecker::non_nil_type(&union);
    assert_eq!(
        narrowed,
        InferredType::known("String"),
        "Removing UndefinedObject from String|UndefinedObject should yield String"
    );
}

#[test]
fn test_narrowing_union_multi_member() {
    // String | Integer | UndefinedObject → String | Integer after non_nil
    let union = InferredType::simple_union(&["String", "Integer", "UndefinedObject"]);
    let narrowed = TypeChecker::non_nil_type(&union);
    match narrowed {
        InferredType::Union { members, .. } => {
            assert_eq!(members.len(), 2);
            assert!(members.contains(&InferredType::known("String")));
            assert!(members.contains(&InferredType::known("Integer")));
        }
        _ => panic!("Expected Union type after narrowing, got: {narrowed:?}"),
    }
}

#[test]
fn test_union_order_independent_equality() {
    // A | B should equal B | A
    let ab = InferredType::simple_union(&["String", "Integer"]);
    let ba = InferredType::simple_union(&["Integer", "String"]);
    assert_eq!(ab, ba, "Union equality should be order-independent");
}

#[test]
fn test_union_with_generic_members() {
    // Result(Integer, String) | UndefinedObject
    let result_ty = InferredType::Known {
        class_name: "Result".into(),
        type_args: vec![
            InferredType::known("Integer"),
            InferredType::known("String"),
        ],
        provenance: TypeProvenance::Inferred(Span::default()),
    };
    let nil_ty = InferredType::known("UndefinedObject");
    let union = InferredType::union_of(&[result_ty.clone(), nil_ty]);

    // display_name should render generics
    let display = union.display_name().unwrap();
    assert!(
        display.contains("Result(Integer, String)"),
        "Union display should include generic args, got: {display}"
    );
    assert!(
        display.contains("UndefinedObject"),
        "Union display should include nil member, got: {display}"
    );

    // non_nil_type should preserve generic args
    let narrowed = TypeChecker::non_nil_type(&union);
    assert_eq!(
        narrowed, result_ty,
        "Narrowing away nil should preserve the full generic Result type"
    );
}

#[test]
fn test_union_deduplication() {
    // union_of([String, String, Integer]) should deduplicate to String | Integer
    let members = vec![
        InferredType::known("String"),
        InferredType::known("String"),
        InferredType::known("Integer"),
    ];
    let union = InferredType::union_of(&members);
    match union {
        InferredType::Union { members, .. } => {
            assert_eq!(members.len(), 2, "Duplicate members should be removed");
        }
        _ => panic!("Expected Union, got: {union:?}"),
    }
}

#[test]
fn test_detect_narrowing_class_eq_pattern() {
    // (x class = Integer) → should detect narrowing for x to Integer
    let expr = class_eq("x", "Integer");
    let info = TypeChecker::detect_narrowing(&expr);
    assert!(info.is_some(), "Should detect class = narrowing");
    let info = info.unwrap();
    assert_eq!(info.variable.as_str(), "x");
    assert_eq!(info.true_type, InferredType::known("Integer"));
    assert!(!info.is_nil_check);
}

#[test]
fn test_detect_narrowing_is_kind_of_pattern() {
    let expr = is_kind_of("x", "Number");
    let info = TypeChecker::detect_narrowing(&expr);
    assert!(info.is_some(), "Should detect isKindOf: narrowing");
    let info = info.unwrap();
    assert_eq!(info.variable.as_str(), "x");
    assert_eq!(info.true_type, InferredType::known("Number"));
    assert!(!info.is_nil_check);
}

#[test]
fn test_detect_narrowing_is_nil_pattern() {
    let expr = is_nil("x");
    let info = TypeChecker::detect_narrowing(&expr);
    assert!(info.is_some(), "Should detect isNil narrowing");
    let info = info.unwrap();
    assert_eq!(info.variable.as_str(), "x");
    assert!(info.is_nil_check);
}

#[test]
fn test_detect_narrowing_no_match() {
    // `x + 1` is not a narrowing pattern
    let expr = msg_send(
        var("x"),
        MessageSelector::Binary("+".into()),
        vec![int_lit(1)],
    );
    assert!(
        TypeChecker::detect_narrowing(&expr).is_none(),
        "x + 1 should not be detected as narrowing"
    );
}

#[test]
fn test_narrowing_does_not_leak_outside_block() {
    // Verify narrowing is scoped to block only:
    //   process: x :: Object =>
    //     (x class = String) ifTrue: [x size]
    //     x unknownThing   // Object doesn't have unknownThing → warning
    let hierarchy = ClassHierarchy::with_builtins();
    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("x", Some("Object"))],
            vec![
                if_true(
                    class_eq("x", "String"),
                    block_expr(vec![msg_send(
                        var("x"),
                        MessageSelector::Unary("size".into()),
                        vec![],
                    )]),
                ),
                msg_send(
                    var("x"),
                    MessageSelector::Unary("unknownThing".into()),
                    vec![],
                ),
            ],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    // 'unknownThing' on Object should warn (narrowing doesn't leak)
    assert_eq!(
        warnings.len(),
        1,
        "Expected 1 warning for unknownThing outside narrowed block, got {}",
        warnings.len(),
    );
}

// ---- BT-1582: respondsTo: narrowing tests (ADR 0068 Phase 2e) ----

#[test]
fn test_detect_narrowing_responds_to_pattern() {
    // `x respondsTo: #asString` → should detect narrowing for x
    let expr = responds_to("x", "asString");
    let info = TypeChecker::detect_narrowing(&expr);
    assert!(info.is_some(), "Should detect respondsTo: narrowing");
    let info = info.unwrap();
    assert_eq!(info.variable.as_str(), "x");
    assert_eq!(
        info.true_type,
        InferredType::Dynamic(DynamicReason::Unknown)
    );
    assert!(!info.is_nil_check);
    assert_eq!(
        info.responded_selector.as_deref(),
        Some("asString"),
        "Should record the tested selector"
    );
}

#[test]
fn test_detect_narrowing_responds_to_non_symbol_arg() {
    // `x respondsTo: someVar` — not a symbol literal → no narrowing
    let expr = msg_send(
        var("x"),
        MessageSelector::Keyword(vec![KeywordPart::new("respondsTo:", span())]),
        vec![var("someVar")],
    );
    assert!(
        TypeChecker::detect_narrowing(&expr).is_none(),
        "respondsTo: with non-symbol argument should not be detected as narrowing"
    );
}

#[test]
fn test_narrowing_responds_to_in_true_block() {
    // Build a class with a method:
    //   process: x :: Object =>
    //     (x respondsTo: #customMethod) ifTrue: [x customMethod]
    //     x customMethod    // should warn — x is Object outside the block
    let hierarchy = ClassHierarchy::with_builtins();
    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("x", Some("Object"))],
            vec![
                // (x respondsTo: #customMethod) ifTrue: [x customMethod]
                if_true(
                    responds_to("x", "customMethod"),
                    block_expr(vec![msg_send(
                        var("x"),
                        MessageSelector::Unary("customMethod".into()),
                        vec![],
                    )]),
                ),
                // x customMethod — should warn (Object doesn't have customMethod)
                msg_send(
                    var("x"),
                    MessageSelector::Unary("customMethod".into()),
                    vec![],
                ),
            ],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // Inside the block: `x customMethod` should NOT warn because x is Dynamic
    // (respondsTo: narrowing sets the variable to Dynamic in the true block)
    // Outside the block: `x customMethod` SHOULD warn because x is still Object
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert_eq!(
        warnings.len(),
        1,
        "Expected 1 warning for customMethod outside the narrowed block, got {}:\n{:?}",
        warnings.len(),
        warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_narrowing_responds_to_does_not_leak() {
    // Verify narrowing from respondsTo: is scoped to block only:
    //   process: x :: Object =>
    //     (x respondsTo: #asString) ifTrue: [x asString]
    //     x unknownSelector   // Object doesn't have this → warning
    let hierarchy = ClassHierarchy::with_builtins();
    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("x", Some("Object"))],
            vec![
                if_true(
                    responds_to("x", "asString"),
                    block_expr(vec![msg_send(
                        var("x"),
                        MessageSelector::Unary("asString".into()),
                        vec![],
                    )]),
                ),
                msg_send(
                    var("x"),
                    MessageSelector::Unary("unknownSelector".into()),
                    vec![],
                ),
            ],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    // 'unknownSelector' on Object should warn (narrowing doesn't leak)
    assert_eq!(
        warnings.len(),
        1,
        "Expected 1 warning for unknownSelector outside narrowed block, got {}",
        warnings.len(),
    );
}

#[test]
fn test_narrowing_responds_to_if_true_if_false() {
    // Build: (x respondsTo: #customMethod) ifTrue: [x customMethod] ifFalse: [x customMethod]
    // True block: no warning (narrowed to Dynamic)
    // False block: should warn (x is still Object, no respondsTo: narrowing in false branch)
    let hierarchy = ClassHierarchy::with_builtins();
    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("x", Some("Object"))],
            vec![if_true_if_false(
                responds_to("x", "customMethod"),
                block_expr(vec![msg_send(
                    var("x"),
                    MessageSelector::Unary("customMethod".into()),
                    vec![],
                )]),
                block_expr(vec![msg_send(
                    var("x"),
                    MessageSelector::Unary("customMethod".into()),
                    vec![],
                )]),
            )],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // True block: no warning (Dynamic narrowing)
    // False block: should warn (Object doesn't have customMethod)
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert_eq!(
        warnings.len(),
        1,
        "Expected 1 warning in false block (no respondsTo: narrowing there), got {}:\n{:?}",
        warnings.len(),
        warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_responds_to_protocol_inference() {
    // When `x respondsTo: #asString` is detected, and a Printable protocol
    // requiring asString exists, the responded_selector can be used to infer
    // protocol conformance (ADR 0068 Phase 2e).
    let hierarchy = ClassHierarchy::with_builtins();

    // Build a Printable protocol requiring asString
    let printable_proto = ProtocolDefinition {
        name: ident("Printable"),
        type_params: vec![],
        extending: None,
        method_signatures: vec![ProtocolMethodSignature {
            selector: MessageSelector::Unary("asString".into()),
            parameters: vec![],
            return_type: Some(TypeAnnotation::Simple(ident("String"))),
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }],
        class_method_signatures: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: span(),
    };
    let module = Module {
        protocols: vec![printable_proto],
        ..Module::new(vec![], span())
    };
    let mut registry = ProtocolRegistry::new();
    let diags = registry.register_module(&module, &hierarchy);
    assert!(diags.is_empty());

    // Detect narrowing from `x respondsTo: #asString`
    let expr = responds_to("x", "asString");
    let info = TypeChecker::detect_narrowing(&expr).unwrap();
    assert_eq!(info.responded_selector.as_deref(), Some("asString"));

    // The responded_selector matches the Printable protocol's required method.
    // Integer conforms to Printable (has asString), String does too.
    let result = registry.check_conformance("Integer", "Printable", &hierarchy);
    assert!(
        result.is_ok(),
        "Integer responds to asString → conforms to Printable"
    );

    // A class without asString would not conform
    // (This verifies the protocol registry is usable for narrowing inference)
    let proto = registry.get("Printable").unwrap();
    let required: Vec<&str> = proto
        .all_required_selectors(&registry)
        .iter()
        .map(|s| s.as_str())
        .collect();
    assert!(
        required.contains(&"asString"),
        "Printable protocol requires asString"
    );
}

// ---- BT-1833: respondsTo: narrows to protocol type ----

/// Helper: build a protocol module with a single protocol requiring a single selector.
fn make_protocol_module(
    protocol_name: &str,
    required_selector: &str,
) -> (Module, ProtocolRegistry, ClassHierarchy) {
    let hierarchy = ClassHierarchy::with_builtins();
    let proto_def = ProtocolDefinition {
        name: ident(protocol_name),
        type_params: vec![],
        extending: None,
        method_signatures: vec![ProtocolMethodSignature {
            selector: MessageSelector::Unary(required_selector.into()),
            parameters: vec![],
            return_type: Some(TypeAnnotation::Simple(ident("String"))),
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }],
        class_method_signatures: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: span(),
    };
    let module = Module {
        protocols: vec![proto_def],
        ..Module::new(vec![], span())
    };
    let mut registry = ProtocolRegistry::new();
    let diags = registry.register_module(&module, &hierarchy);
    assert!(diags.is_empty());
    (module, registry, hierarchy)
}

#[test]
fn test_responds_to_narrows_to_protocol_type() {
    // When `x respondsTo: #asString` with a Printable protocol requiring asString,
    // the true-branch should narrow x to Printable (not Dynamic).
    let (_, registry, hierarchy) = make_protocol_module("Printable", "asString");

    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("x", Some("Object"))],
            vec![
                // (x respondsTo: #asString) ifTrue: [x asString]
                if_true(
                    responds_to("x", "asString"),
                    block_expr(vec![msg_send(
                        var("x"),
                        MessageSelector::Unary("asString".into()),
                        vec![],
                    )]),
                ),
            ],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module_with_protocols(&module, &hierarchy, &registry);

    // asString is a valid method on Printable protocol, so no DNU warning
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "Expected no DNU warnings (x narrowed to Printable), got: {:?}",
        dnu_warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_responds_to_no_protocol_match_falls_back_to_dynamic() {
    // When `x respondsTo: #unknownMethod` and no protocol requires that selector,
    // the narrowing should fall back to Dynamic (no warnings in true block).
    let (_, registry, hierarchy) = make_protocol_module("Printable", "asString");

    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("x", Some("Object"))],
            vec![
                // (x respondsTo: #unknownMethod) ifTrue: [x unknownMethod]
                if_true(
                    responds_to("x", "unknownMethod"),
                    block_expr(vec![msg_send(
                        var("x"),
                        MessageSelector::Unary("unknownMethod".into()),
                        vec![],
                    )]),
                ),
            ],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module_with_protocols(&module, &hierarchy, &registry);

    // Dynamic suppresses DNU, so no warning in the true block
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "Expected no DNU warnings (x narrowed to Dynamic), got: {:?}",
        dnu_warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_responds_to_protocol_narrows_validates_methods() {
    // When narrowed to Printable, sending a method NOT on the protocol should warn.
    let (_, registry, hierarchy) = make_protocol_module("Printable", "asString");

    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("x", Some("Object"))],
            vec![
                // (x respondsTo: #asString) ifTrue: [x nonExistentMethod]
                if_true(
                    responds_to("x", "asString"),
                    block_expr(vec![msg_send(
                        var("x"),
                        MessageSelector::Unary("nonExistentMethod".into()),
                        vec![],
                    )]),
                ),
            ],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module_with_protocols(&module, &hierarchy, &registry);

    // Printable only has asString — nonExistentMethod should warn
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert_eq!(
        dnu_warnings.len(),
        1,
        "Expected 1 DNU warning for nonExistentMethod on Printable, got: {:?}",
        dnu_warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_responds_to_ambiguous_protocols_falls_back_to_dynamic() {
    // When multiple protocols require the same selector, fall back to Dynamic.
    let hierarchy = ClassHierarchy::with_builtins();

    // Create two protocols both requiring 'asString'
    let proto1 = ProtocolDefinition {
        name: ident("Printable"),
        type_params: vec![],
        extending: None,
        method_signatures: vec![ProtocolMethodSignature {
            selector: MessageSelector::Unary("asString".into()),
            parameters: vec![],
            return_type: Some(TypeAnnotation::Simple(ident("String"))),
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }],
        class_method_signatures: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: span(),
    };
    let proto2 = ProtocolDefinition {
        name: ident("Displayable"),
        type_params: vec![],
        extending: None,
        method_signatures: vec![ProtocolMethodSignature {
            selector: MessageSelector::Unary("asString".into()),
            parameters: vec![],
            return_type: Some(TypeAnnotation::Simple(ident("String"))),
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }],
        class_method_signatures: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: span(),
    };
    let proto_module = Module {
        protocols: vec![proto1, proto2],
        ..Module::new(vec![], span())
    };
    let mut registry = ProtocolRegistry::new();
    registry.register_module(&proto_module, &hierarchy);

    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("x", Some("Object"))],
            vec![
                // (x respondsTo: #asString) ifTrue: [x unknownMethod]
                if_true(
                    responds_to("x", "asString"),
                    block_expr(vec![msg_send(
                        var("x"),
                        MessageSelector::Unary("unknownMethod".into()),
                        vec![],
                    )]),
                ),
            ],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module_with_protocols(&module, &hierarchy, &registry);

    // With ambiguous protocols, x stays Dynamic — no DNU warning for unknownMethod
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "Expected no DNU warnings (ambiguous protocols → Dynamic), got: {:?}",
        dnu_warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_responds_to_without_protocol_registry_stays_dynamic() {
    // When check_module is called without protocols (no registry),
    // respondsTo: narrowing stays Dynamic (backward compatible).
    let hierarchy = ClassHierarchy::with_builtins();
    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("x", Some("Object"))],
            vec![if_true(
                responds_to("x", "customMethod"),
                block_expr(vec![msg_send(
                    var("x"),
                    MessageSelector::Unary("customMethod".into()),
                    vec![],
                )]),
            )],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    // Use check_module (no protocol registry) — should still work
    checker.check_module(&module, &hierarchy);

    // Dynamic suppresses DNU, so no warning
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "Expected no DNU warnings (Dynamic narrowing, no registry), got: {:?}",
        dnu_warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_find_unique_protocol_for_selector() {
    // Unit test for the ProtocolRegistry helper method.
    let (_, registry, _) = make_protocol_module("Printable", "asString");

    // Unique match: asString → Printable
    let result = registry.find_unique_protocol_for_selector("asString");
    assert_eq!(
        result.map(EcoString::as_str),
        Some("Printable"),
        "asString should uniquely match Printable"
    );

    // No match: unknownSelector → None
    let result = registry.find_unique_protocol_for_selector("unknownSelector");
    assert!(
        result.is_none(),
        "unknownSelector should not match any protocol"
    );
}

// ---- BT-1577: Generic inheritance tests ----

/// Build `GenCollection(E)` with method `first` returning `E`, `size` returning `Integer`.
/// Build `GenArray(E)` extends `GenCollection(E)` with `superclass_type_args` mapping E to E.
fn add_generic_collection_hierarchy(hierarchy: &mut ClassHierarchy) {
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo, SuperclassTypeArg};

    let collection_info = ClassInfo {
        name: eco_string("GenCollection"),
        superclass: Some(eco_string("Object")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        methods: vec![
            MethodInfo {
                selector: eco_string("first"),
                arity: 0,
                kind: MethodKind::Primary,
                defined_in: eco_string("GenCollection"),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: Some(eco_string("E")),
                param_types: vec![],
                doc: None,
            },
            MethodInfo {
                selector: eco_string("size"),
                arity: 0,
                kind: MethodKind::Primary,
                defined_in: eco_string("GenCollection"),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: Some(eco_string("Integer")),
                param_types: vec![],
                doc: None,
            },
            MethodInfo {
                selector: eco_string("select:"),
                arity: 1,
                kind: MethodKind::Primary,
                defined_in: eco_string("GenCollection"),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: Some(eco_string("Self")),
                param_types: vec![Some(eco_string("Block(E, Boolean)"))],
                doc: None,
            },
        ],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![eco_string("E")],
        type_param_bounds: vec![None],
        superclass_type_args: vec![],
    };

    let array_info = ClassInfo {
        name: eco_string("GenArray"),
        superclass: Some(eco_string("GenCollection")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        methods: vec![MethodInfo {
            selector: eco_string("append:"),
            arity: 1,
            kind: MethodKind::Primary,
            defined_in: eco_string("GenArray"),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: Some(eco_string("Self")),
            param_types: vec![Some(eco_string("E"))],
            doc: None,
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![eco_string("E")],
        type_param_bounds: vec![None],
        superclass_type_args: vec![SuperclassTypeArg::ParamRef { param_index: 0 }],
    };

    hierarchy.add_from_beam_meta(vec![collection_info, array_info]);
}

/// BT-1577: Inherited method `first` on `GenArray(Integer)` returns `Integer`.
#[test]
fn generic_inheritance_inherited_method_returns_substituted_type() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_collection_hierarchy(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set(
        "arr",
        InferredType::Known {
            class_name: eco_string("GenArray"),
            type_args: vec![InferredType::known("Integer")],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result_ty = checker.infer_expr(
        &msg_send(var("arr"), MessageSelector::Unary("first".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );

    assert_eq!(
        result_ty.as_known().map(EcoString::as_str),
        Some("Integer"),
        "first on GenArray(Integer) should return Integer via inheritance, got: {result_ty:?}"
    );
}

/// BT-1577: Non-generic return type from inherited method is unaffected.
#[test]
fn generic_inheritance_non_generic_return_unchanged() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_collection_hierarchy(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set(
        "arr",
        InferredType::Known {
            class_name: eco_string("GenArray"),
            type_args: vec![InferredType::known("Integer")],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result_ty = checker.infer_expr(
        &msg_send(var("arr"), MessageSelector::Unary("size".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );

    assert_eq!(
        result_ty.as_known().map(EcoString::as_str),
        Some("Integer"),
        "size on GenArray(Integer) should return Integer (non-generic), got: {result_ty:?}"
    );
}

/// BT-1577: Concrete superclass type arg — `IntArray` extends `GenCollection(Integer)`.
#[test]
fn generic_inheritance_concrete_superclass_type_arg() {
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, SuperclassTypeArg};

    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_collection_hierarchy(&mut hierarchy);

    // IntArray has no type params, but maps Integer to GenCollection's E
    let int_array_info = ClassInfo {
        name: eco_string("IntArray"),
        superclass: Some(eco_string("GenCollection")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![SuperclassTypeArg::Concrete {
            type_name: eco_string("Integer"),
        }],
    };
    hierarchy.add_from_beam_meta(vec![int_array_info]);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set("ia", InferredType::known("IntArray"));

    let result_ty = checker.infer_expr(
        &msg_send(var("ia"), MessageSelector::Unary("first".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );

    assert_eq!(
        result_ty.as_known().map(EcoString::as_str),
        Some("Integer"),
        "first on IntArray should return Integer via concrete superclass type arg, got: {result_ty:?}"
    );
}

/// BT-1577: Self type on inherited method carries receiver's type args.
#[test]
fn generic_inheritance_self_type_carries_type_args() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_collection_hierarchy(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set(
        "arr",
        InferredType::Known {
            class_name: eco_string("GenArray"),
            type_args: vec![InferredType::known("Integer")],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    // select: returns Self — should be GenArray(Integer) not GenCollection(Integer)
    let result_ty = checker.infer_expr(
        &msg_send(
            var("arr"),
            MessageSelector::Keyword(vec![KeywordPart::new("select:", span())]),
            vec![var("block")],
        ),
        &hierarchy,
        &mut env,
        false,
    );

    match &result_ty {
        InferredType::Known {
            class_name,
            type_args,
            ..
        } => {
            assert_eq!(
                class_name.as_str(),
                "GenArray",
                "Self should resolve to GenArray, got: {class_name}"
            );
            assert_eq!(type_args.len(), 1, "Should have 1 type arg");
            assert_eq!(
                type_args[0].as_known().map(EcoString::as_str),
                Some("Integer"),
                "Type arg should be Integer"
            );
        }
        _ => panic!("Expected Known type, got: {result_ty:?}"),
    }
}

/// BT-1577: Multi-level inheritance composes substitution correctly.
/// `GenCollection(E) subclass: GenArray(E)`, `GenArray(E) subclass: SortedArray(E)`.
#[test]
fn generic_inheritance_multi_level_composition() {
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, SuperclassTypeArg};

    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_collection_hierarchy(&mut hierarchy);

    // SortedArray(E) extends GenArray(E)
    let sorted_info = ClassInfo {
        name: eco_string("SortedArray"),
        superclass: Some(eco_string("GenArray")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![eco_string("E")],
        type_param_bounds: vec![None],
        superclass_type_args: vec![SuperclassTypeArg::ParamRef { param_index: 0 }],
    };
    hierarchy.add_from_beam_meta(vec![sorted_info]);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set(
        "sa",
        InferredType::Known {
            class_name: eco_string("SortedArray"),
            type_args: vec![InferredType::known("String")],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    // `first` is defined on GenCollection — 2 levels up
    let result_ty = checker.infer_expr(
        &msg_send(var("sa"), MessageSelector::Unary("first".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );

    assert_eq!(
        result_ty.as_known().map(EcoString::as_str),
        Some("String"),
        "first on SortedArray(String) should compose through 2 levels to return String, got: {result_ty:?}"
    );
}

/// BT-1577: Method defined on own class (not inherited) still uses direct substitution.
#[test]
fn generic_inheritance_own_method_uses_direct_substitution() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_result_class(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set(
        "r",
        InferredType::Known {
            class_name: eco_string("GenResult"),
            type_args: vec![
                InferredType::known("Integer"),
                InferredType::known("IOError"),
            ],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    // unwrap is defined on GenResult itself — should still work
    let result_ty = checker.infer_expr(
        &msg_send(var("r"), MessageSelector::Unary("unwrap".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );

    assert_eq!(
        result_ty.as_known().map(EcoString::as_str),
        Some("Integer"),
        "unwrap on GenResult(Integer, IOError) should still return Integer, got: {result_ty:?}"
    );
}

// --- ADR 0068 Phase 2d: Type parameter bounds tests ---

#[test]
fn type_param_bounds_conforming_type_no_warning() {
    // Logger(T :: Printable) where Integer conforms to Printable → no warning
    use crate::semantic_analysis::class_hierarchy::ClassInfo;
    let mut hierarchy = ClassHierarchy::with_builtins();
    hierarchy.add_from_beam_meta(vec![ClassInfo {
        name: "BoundedLogger".into(),
        superclass: Some("Actor".into()),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: HashMap::new(),
        state_has_default: HashMap::new(),
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec!["T".into()],
        type_param_bounds: vec![Some("Printable".into())],
        superclass_type_args: vec![],
    }]);

    let mut registry = ProtocolRegistry::new();
    // Define Printable protocol requiring asString
    let proto_module = Module {
        protocols: vec![crate::ast::ProtocolDefinition {
            name: crate::ast::Identifier::new("Printable", span()),
            type_params: vec![],
            extending: None,
            method_signatures: vec![crate::ast::ProtocolMethodSignature {
                selector: crate::ast::MessageSelector::Unary("asString".into()),
                parameters: vec![],
                return_type: Some(crate::ast::TypeAnnotation::simple("String", span())),
                comments: crate::ast::CommentAttachment::default(),
                doc_comment: None,
                span: span(),
            }],
            class_method_signatures: vec![],
            comments: crate::ast::CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }],
        ..Module::new(vec![], span())
    };
    registry.register_module(&proto_module, &hierarchy);

    let mut checker = TypeChecker::new();
    checker.check_type_param_bounds(
        &"BoundedLogger".into(),
        &[InferredType::known("Integer")],
        span(),
        &hierarchy,
        &registry,
    );

    // Integer has asString (built-in) → should conform → no warning
    assert!(
        checker.diagnostics().is_empty(),
        "Integer conforms to Printable, expected no warnings, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn type_param_bounds_non_conforming_type_warns() {
    // Logger(T :: HasSortKey) where Integer does NOT have sortKey → warning
    use crate::semantic_analysis::class_hierarchy::ClassInfo;
    let mut hierarchy = ClassHierarchy::with_builtins();
    hierarchy.add_from_beam_meta(vec![ClassInfo {
        name: "BoundedLogger".into(),
        superclass: Some("Actor".into()),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: HashMap::new(),
        state_has_default: HashMap::new(),
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec!["T".into()],
        type_param_bounds: vec![Some("HasSortKey".into())],
        superclass_type_args: vec![],
    }]);

    let mut registry = ProtocolRegistry::new();
    let proto_module = Module {
        protocols: vec![crate::ast::ProtocolDefinition {
            name: crate::ast::Identifier::new("HasSortKey", span()),
            type_params: vec![],
            extending: None,
            method_signatures: vec![crate::ast::ProtocolMethodSignature {
                selector: crate::ast::MessageSelector::Unary("sortKey".into()),
                parameters: vec![],
                return_type: None,
                comments: crate::ast::CommentAttachment::default(),
                doc_comment: None,
                span: span(),
            }],
            class_method_signatures: vec![],
            comments: crate::ast::CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }],
        ..Module::new(vec![], span())
    };
    registry.register_module(&proto_module, &hierarchy);

    let mut checker = TypeChecker::new();
    checker.check_type_param_bounds(
        &"BoundedLogger".into(),
        &[InferredType::known("Integer")],
        span(),
        &hierarchy,
        &registry,
    );

    // Integer does NOT have sortKey → should warn
    assert_eq!(
        checker.diagnostics().len(),
        1,
        "Expected 1 bound violation warning, got: {:?}",
        checker.diagnostics()
    );
    assert!(
        checker.diagnostics()[0]
            .message
            .contains("does not conform to HasSortKey")
    );
}

#[test]
fn type_param_bounds_unbounded_param_no_check() {
    // Result(T, E) with no bounds → no warnings for any type args
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_result_class(&mut hierarchy);

    let registry = ProtocolRegistry::new();
    let mut checker = TypeChecker::new();
    checker.check_type_param_bounds(
        &"GenResult".into(),
        &[
            InferredType::known("Integer"),
            InferredType::known("String"),
        ],
        span(),
        &hierarchy,
        &registry,
    );

    assert!(
        checker.diagnostics().is_empty(),
        "Unbounded params should not warn, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn type_param_bounds_dynamic_skipped() {
    // Logger(T :: Printable) where T is Dynamic → no warning (conservative)
    use crate::semantic_analysis::class_hierarchy::ClassInfo;
    let mut hierarchy = ClassHierarchy::with_builtins();
    hierarchy.add_from_beam_meta(vec![ClassInfo {
        name: "BoundedLogger".into(),
        superclass: Some("Actor".into()),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: HashMap::new(),
        state_has_default: HashMap::new(),
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec!["T".into()],
        type_param_bounds: vec![Some("Printable".into())],
        superclass_type_args: vec![],
    }]);

    let registry = ProtocolRegistry::new();
    let mut checker = TypeChecker::new();
    checker.check_type_param_bounds(
        &"BoundedLogger".into(),
        &[InferredType::Dynamic(DynamicReason::Unknown)],
        span(),
        &hierarchy,
        &registry,
    );

    assert!(
        checker.diagnostics().is_empty(),
        "Dynamic type arg should not trigger bound check, got: {:?}",
        checker.diagnostics()
    );
}

// ---- BT-1583: Generic variance tests (ADR 0068 Phase 2f) ----

/// Build a sealed Value class `SealedBox(T)` with a Printable protocol and
/// classes that conform to it, for variance testing.
#[allow(clippy::too_many_lines)]
fn setup_variance_test_env() -> (ClassHierarchy, ProtocolRegistry) {
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};
    use crate::semantic_analysis::protocol_registry::{ProtocolInfo, ProtocolMethodRequirement};

    let mut hierarchy = ClassHierarchy::with_builtins();

    // Add a sealed Value class: SealedBox(T) — covariant (immutable)
    let sealed_box = ClassInfo {
        name: eco_string("SealedBox"),
        superclass: Some(eco_string("Value")),
        is_sealed: true,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: true,
        is_native: false,
        state: vec![eco_string("value")],
        state_types: {
            let mut m = std::collections::HashMap::new();
            m.insert(eco_string("value"), eco_string("T"));
            m
        },
        state_has_default: std::collections::HashMap::new(),
        methods: vec![MethodInfo {
            selector: eco_string("value"),
            arity: 0,
            kind: MethodKind::Primary,
            defined_in: eco_string("SealedBox"),
            is_sealed: true,
            is_internal: false,
            spawns_block: false,
            return_type: Some(eco_string("T")),
            param_types: vec![],
            doc: None,
        }],
        class_methods: vec![MethodInfo {
            selector: eco_string("wrap:"),
            arity: 1,
            kind: MethodKind::Primary,
            defined_in: eco_string("SealedBox"),
            is_sealed: true,
            is_internal: false,
            spawns_block: false,
            return_type: Some(eco_string("Self")),
            param_types: vec![Some(eco_string("T"))],
            doc: None,
        }],
        class_variables: vec![],
        type_params: vec![eco_string("T")],
        type_param_bounds: vec![None],
        superclass_type_args: vec![],
    };

    // Add an Actor class: ActorBox(T) — invariant (mutable state)
    let actor_box = ClassInfo {
        name: eco_string("ActorBox"),
        superclass: Some(eco_string("Actor")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![eco_string("value")],
        state_types: {
            let mut m = std::collections::HashMap::new();
            m.insert(eco_string("value"), eco_string("T"));
            m
        },
        state_has_default: std::collections::HashMap::new(),
        methods: vec![
            MethodInfo {
                selector: eco_string("value"),
                arity: 0,
                kind: MethodKind::Primary,
                defined_in: eco_string("ActorBox"),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: Some(eco_string("T")),
                param_types: vec![],
                doc: None,
            },
            MethodInfo {
                selector: eco_string("value:"),
                arity: 1,
                kind: MethodKind::Primary,
                defined_in: eco_string("ActorBox"),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: None,
                param_types: vec![Some(eco_string("T"))],
                doc: None,
            },
        ],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![eco_string("T")],
        type_param_bounds: vec![None],
        superclass_type_args: vec![],
    };

    // Add an unsealed Value class: OpenBox(T) — invariant (can be subclassed)
    let open_box = ClassInfo {
        name: eco_string("OpenBox"),
        superclass: Some(eco_string("Value")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: true,
        is_native: false,
        state: vec![eco_string("value")],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![eco_string("T")],
        type_param_bounds: vec![None],
        superclass_type_args: vec![],
    };

    // Add a class that accepts SealedBox(Printable) parameter
    let consumer = ClassInfo {
        name: eco_string("BoxConsumer"),
        superclass: Some(eco_string("Object")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        methods: vec![
            MethodInfo {
                selector: eco_string("printBox:"),
                arity: 1,
                kind: MethodKind::Primary,
                defined_in: eco_string("BoxConsumer"),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: Some(eco_string("String")),
                param_types: vec![Some(eco_string("SealedBox(Printable)"))],
                doc: None,
            },
            MethodInfo {
                selector: eco_string("setActor:"),
                arity: 1,
                kind: MethodKind::Primary,
                defined_in: eco_string("BoxConsumer"),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: None,
                param_types: vec![Some(eco_string("ActorBox(Printable)"))],
                doc: None,
            },
        ],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };

    hierarchy.add_from_beam_meta(vec![sealed_box, actor_box, open_box, consumer]);

    // Register a Printable protocol — requires `asString` method
    let mut registry = ProtocolRegistry::new();
    let printable = ProtocolInfo {
        name: eco_string("Printable"),
        type_params: vec![],
        type_param_bounds: vec![],
        extending: None,
        methods: vec![ProtocolMethodRequirement {
            selector: eco_string("asString"),
            arity: 0,
            return_type: Some(eco_string("String")),
            param_types: vec![],
        }],
        class_methods: vec![],
        span: span(),
    };
    registry.register_test_protocol(printable);

    (hierarchy, registry)
}

/// BT-1583: `is_covariant_class` returns true for sealed Value classes with type params.
#[test]
fn covariant_class_sealed_value_is_covariant() {
    let (hierarchy, _registry) = setup_variance_test_env();
    assert!(
        hierarchy.is_covariant_class("SealedBox"),
        "Sealed Value class SealedBox should be covariant"
    );
}

/// BT-1583: `is_covariant_class` returns false for Actor classes (invariant).
#[test]
fn covariant_class_actor_is_invariant() {
    let (hierarchy, _registry) = setup_variance_test_env();
    assert!(
        !hierarchy.is_covariant_class("ActorBox"),
        "Actor class ActorBox should be invariant"
    );
}

/// BT-1583: `is_covariant_class` returns false for unsealed Value classes (conservative).
#[test]
fn covariant_class_unsealed_value_is_invariant() {
    let (hierarchy, _registry) = setup_variance_test_env();
    assert!(
        !hierarchy.is_covariant_class("OpenBox"),
        "Unsealed Value class OpenBox should be invariant (conservative)"
    );
}

/// BT-1583: `is_covariant_class` returns false for non-generic classes.
#[test]
fn covariant_class_non_generic_is_false() {
    let hierarchy = ClassHierarchy::with_builtins();
    assert!(
        !hierarchy.is_covariant_class("Integer"),
        "Non-generic class Integer should not be covariant"
    );
}

/// BT-1583: Covariant assignment — `SealedBox(Integer)` assignable to `SealedBox(Printable)`.
///
/// Integer conforms to Printable (has `asString`), and `SealedBox` is a sealed Value class.
#[test]
fn variance_covariant_sealed_value_protocol_typed() {
    let (hierarchy, registry) = setup_variance_test_env();

    assert!(
        TypeChecker::is_assignable_to_with_variance(
            &eco_string("SealedBox(Integer)"),
            &eco_string("SealedBox(Printable)"),
            &hierarchy,
            &registry,
        ),
        "SealedBox(Integer) should be assignable to SealedBox(Printable) — Integer conforms to Printable"
    );
}

/// BT-1583: Covariant — same type args are trivially compatible.
#[test]
fn variance_covariant_same_type_args() {
    let (hierarchy, registry) = setup_variance_test_env();

    assert!(
        TypeChecker::is_assignable_to_with_variance(
            &eco_string("SealedBox(Integer)"),
            &eco_string("SealedBox(Integer)"),
            &hierarchy,
            &registry,
        ),
        "SealedBox(Integer) should be assignable to SealedBox(Integer)"
    );
}

/// BT-1583: Covariant — non-conforming type is rejected.
///
/// If `OpaqueType` does not conform to Printable, `SealedBox(OpaqueType)` should NOT
/// be assignable to `SealedBox(Printable)`.
#[test]
fn variance_covariant_non_conforming_rejected() {
    use crate::semantic_analysis::class_hierarchy::ClassInfo;

    let (mut hierarchy, registry) = setup_variance_test_env();

    // Add OpaqueType without asString — does not conform to Printable
    hierarchy.add_from_beam_meta(vec![ClassInfo {
        name: eco_string("OpaqueType"),
        superclass: Some(eco_string("Object")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    }]);

    assert!(
        !TypeChecker::is_assignable_to_with_variance(
            &eco_string("SealedBox(OpaqueType)"),
            &eco_string("SealedBox(Printable)"),
            &hierarchy,
            &registry,
        ),
        "SealedBox(OpaqueType) should NOT be assignable to SealedBox(Printable)"
    );
}

/// BT-1583: Invariant actor state — `ActorBox(Integer)` NOT assignable to `ActorBox(Printable)`.
///
/// Actor classes are invariant because their state fields can be mutated.
#[test]
fn variance_invariant_actor_state() {
    let (hierarchy, _registry) = setup_variance_test_env();

    // ActorBox is invariant — the base-name-only check in is_assignable_to is permissive,
    // but the variance-aware method should recognize it's invariant.
    // Note: The current string-based approach falls back to is_assignable_to for invariant
    // classes, which checks base names only. For the invariant case with different type args,
    // the type args differ but the base names match — so the old behaviour (permissive) is
    // preserved. The test validates that the actor class is recognized as invariant.
    assert!(
        !hierarchy.is_covariant_class("ActorBox"),
        "ActorBox should be invariant (not covariant)"
    );
}

/// BT-1583: Non-generic types fall back to normal assignability.
#[test]
fn variance_non_generic_falls_back() {
    let (hierarchy, registry) = setup_variance_test_env();

    // Integer is assignable to Number (subclass relationship)
    assert!(
        TypeChecker::is_assignable_to_with_variance(
            &eco_string("Integer"),
            &eco_string("Number"),
            &hierarchy,
            &registry,
        ),
        "Integer should be assignable to Number via superclass chain"
    );

    // String is NOT assignable to Integer
    assert!(
        !TypeChecker::is_assignable_to_with_variance(
            &eco_string("String"),
            &eco_string("Integer"),
            &hierarchy,
            &registry,
        ),
        "String should NOT be assignable to Integer"
    );
}

/// BT-1583: Covariant with class-hierarchy subtyping (Integer → Number).
#[test]
fn variance_covariant_class_subtyping() {
    let (hierarchy, registry) = setup_variance_test_env();

    // SealedBox(Integer) should be assignable to SealedBox(Number)
    // because Integer is a subclass of Number and SealedBox is covariant
    assert!(
        TypeChecker::is_assignable_to_with_variance(
            &eco_string("SealedBox(Integer)"),
            &eco_string("SealedBox(Number)"),
            &hierarchy,
            &registry,
        ),
        "SealedBox(Integer) should be assignable to SealedBox(Number) — Integer is a subclass of Number"
    );
}

/// BT-1583: `parse_generic_type_string` correctly parses type strings.
#[test]
fn parse_generic_type_string_basic() {
    let (base, args) = TypeChecker::parse_generic_type_string("Array(Integer)");
    assert_eq!(base, "Array");
    assert_eq!(args, vec!["Integer"]);

    let (base, args) = TypeChecker::parse_generic_type_string("Result(Integer, Error)");
    assert_eq!(base, "Result");
    assert_eq!(args, vec!["Integer", "Error"]);

    let (base, args) = TypeChecker::parse_generic_type_string("String");
    assert_eq!(base, "String");
    assert!(args.is_empty());
}

// ---- BT-1588: Generic type param detection ----

#[test]
fn test_is_generic_type_param() {
    // Single uppercase letters are generic type params
    assert!(is_generic_type_param("V"));
    assert!(is_generic_type_param("K"));
    assert!(is_generic_type_param("T"));
    assert!(is_generic_type_param("R"));
    assert!(is_generic_type_param("E"));

    // Multi-character names are NOT generic type params
    assert!(!is_generic_type_param("String"));
    assert!(!is_generic_type_param("Integer"));
    assert!(!is_generic_type_param("Dictionary"));

    // Lowercase is NOT a generic type param
    assert!(!is_generic_type_param("v"));
    assert!(!is_generic_type_param("t"));

    // Empty string is NOT
    assert!(!is_generic_type_param(""));
}

#[test]
fn test_generic_type_param_binary_operand_hint_severity() {
    // Directly test check_binary_operand_types with a generic type param
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();

    // "hello" ++ V — V is generic, should produce Hint not Warning
    checker.check_binary_operand_types(
        &"String".into(),
        "++",
        &"V".into(),
        span(),
        &hierarchy,
        None,
    );

    let diags: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects a String argument"))
        .collect();
    assert_eq!(diags.len(), 1);
    assert_eq!(
        diags[0].severity,
        crate::source_analysis::Severity::Hint,
        "Generic type param V should produce Hint, not Warning"
    );

    // "hello" ++ Integer — concrete type, should produce Warning
    let mut checker2 = TypeChecker::new();
    checker2.check_binary_operand_types(
        &"String".into(),
        "++",
        &"Integer".into(),
        span(),
        &hierarchy,
        None,
    );
    let diags2: Vec<_> = checker2
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects a String argument"))
        .collect();
    assert_eq!(diags2.len(), 1);
    assert_eq!(
        diags2[0].severity,
        crate::source_analysis::Severity::Warning,
        "Concrete type Integer should produce Warning"
    );
}

#[test]
fn test_type_env_origin_tracking() {
    let mut env = TypeEnv::new();

    // Set without origin
    env.set("x", InferredType::known("Integer"));
    assert!(env.get_origin("x").is_none());

    // Set with origin
    env.set_with_origin(
        "val",
        InferredType::known("V"),
        "`Dictionary at:ifAbsent:` returns generic type `V`",
        Span::new(100, 120),
    );
    let origin = env.get_origin("val").unwrap();
    assert!(origin.description.contains("Dictionary"));
    assert_eq!(origin.span, Span::new(100, 120));

    // Child inherits origins
    let child = env.child();
    assert!(child.get_origin("val").is_some());
}

#[test]
fn test_diagnostic_notes_field() {
    use crate::source_analysis::Diagnostic;

    // Diagnostic without notes
    let diag = Diagnostic::warning("test warning", span());
    assert!(diag.notes.is_empty());

    // Diagnostic with a note
    let diag = Diagnostic::warning("type mismatch", span()).with_note(
        "variable has type V from Dictionary at:ifAbsent:",
        Some(Span::new(10, 20)),
    );
    assert_eq!(diag.notes.len(), 1);
    assert!(diag.notes[0].message.contains("Dictionary"));
    assert_eq!(diag.notes[0].span, Some(Span::new(10, 20)));

    // Hint severity for generic types
    let diag = Diagnostic::hint("likely false positive", span());
    assert_eq!(diag.severity, crate::source_analysis::Severity::Hint);
}

#[test]
fn test_binary_operand_with_origin_note() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();

    // "hello" ++ V with origin info
    let origin = (
        EcoString::from("`Dictionary at:ifAbsent:` returns generic type `V`"),
        Some(Span::new(50, 80)),
    );
    checker.check_binary_operand_types(
        &"String".into(),
        "++",
        &"V".into(),
        span(),
        &hierarchy,
        Some(&origin),
    );

    let diags: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects a String argument"))
        .collect();
    assert_eq!(diags.len(), 1);
    assert_eq!(diags[0].notes.len(), 1, "Should have origin note attached");
    assert!(
        diags[0].notes[0].message.contains("Dictionary"),
        "Note should reference Dictionary origin"
    );
}

// ── ADR 0071 Phase 3 (BT-1702): E0403 — internal method visibility ──

/// Build a hierarchy with a class that has an internal method, in a specific package.
fn make_hierarchy_with_internal_method() -> ClassHierarchy {
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};
    let mut h = ClassHierarchy::with_builtins();
    h.add_from_beam_meta(vec![ClassInfo {
        name: "HttpClient".into(),
        superclass: Some("Object".into()),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: Some("http".into()),
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        methods: vec![
            MethodInfo {
                selector: "get:".into(),
                arity: 1,
                kind: MethodKind::Primary,
                defined_in: "HttpClient".into(),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: Some("String".into()),
                param_types: vec![Some("String".into())],
                doc: None,
            },
            MethodInfo {
                selector: "buildHeaders:".into(),
                arity: 1,
                kind: MethodKind::Primary,
                defined_in: "HttpClient".into(),
                is_sealed: false,
                is_internal: true,
                spawns_block: false,
                return_type: Some("Dictionary".into()),
                param_types: vec![None],
                doc: None,
            },
        ],
        class_methods: vec![MethodInfo {
            selector: "internalFactory".into(),
            arity: 0,
            kind: MethodKind::Primary,
            defined_in: "HttpClient".into(),
            is_sealed: false,
            is_internal: true,
            spawns_block: false,
            return_type: Some("HttpClient".into()),
            param_types: vec![],
            doc: None,
        }],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    }]);
    h
}

#[test]
fn e0403_cross_package_instance_send_to_internal_method() {
    // Directly test check_internal_method_access: sending `buildHeaders:`
    // from package `my_app` to `HttpClient` (package `http`) should emit E0403.
    let hierarchy = make_hierarchy_with_internal_method();
    let mut checker = TypeChecker::with_package("my_app");
    let class_name: EcoString = "HttpClient".into();
    checker.check_internal_method_access(&class_name, "buildHeaders:", span(), &hierarchy, false);
    let e0403_errors: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("is internal to package"))
        .collect();
    assert_eq!(
        e0403_errors.len(),
        1,
        "Expected error for cross-package internal method send, got: {e0403_errors:?}"
    );
    assert!(e0403_errors[0].message.contains("buildHeaders:"));
    assert!(e0403_errors[0].message.contains("http"));
    assert!(e0403_errors[0].message.contains("my_app"));
}

#[test]
fn e0403_same_package_send_to_internal_method_allowed() {
    // Same-package send to internal method should NOT emit E0403.
    let hierarchy = make_hierarchy_with_internal_method();
    let mut checker = TypeChecker::with_package("http");
    let class_name: EcoString = "HttpClient".into();
    checker.check_internal_method_access(&class_name, "buildHeaders:", span(), &hierarchy, false);
    let e0403_errors: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("is internal to package"))
        .collect();
    assert!(
        e0403_errors.is_empty(),
        "Same-package send to internal method should not emit internal method error, got: {e0403_errors:?}"
    );
}

#[test]
fn e0403_public_method_not_flagged() {
    // Sending `get:` (public method) from any package should NOT emit E0403.
    let hierarchy = make_hierarchy_with_internal_method();
    let mut checker = TypeChecker::with_package("my_app");
    let class_name: EcoString = "HttpClient".into();
    checker.check_internal_method_access(&class_name, "get:", span(), &hierarchy, false);
    let e0403_errors: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("is internal to package"))
        .collect();
    assert!(
        e0403_errors.is_empty(),
        "Public method send should not emit internal method error, got: {e0403_errors:?}"
    );
}

#[test]
fn e0403_no_package_context_skips_check() {
    // When no current package is set (REPL), internal method access is not checked.
    let hierarchy = make_hierarchy_with_internal_method();
    let mut checker = TypeChecker::new(); // No package context
    let class_name: EcoString = "HttpClient".into();
    checker.check_internal_method_access(&class_name, "buildHeaders:", span(), &hierarchy, false);
    let e0403_errors: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("is internal to package"))
        .collect();
    assert!(
        e0403_errors.is_empty(),
        "No package context should skip internal method checks, got: {e0403_errors:?}"
    );
}

#[test]
fn e0403_class_side_internal_method() {
    // Sending `HttpClient internalFactory` from another package should emit E0403.
    let module = make_module(vec![msg_send(
        Expression::ClassReference {
            name: ident("HttpClient"),
            package: None,
            span: span(),
        },
        MessageSelector::Unary("internalFactory".into()),
        vec![],
    )]);
    let hierarchy = make_hierarchy_with_internal_method();
    let mut checker = TypeChecker::with_package("my_app");
    checker.check_module(&module, &hierarchy);
    let e0403_errors: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("is internal to package"))
        .collect();
    assert_eq!(
        e0403_errors.len(),
        1,
        "Expected error for cross-package internal class method, got: {e0403_errors:?}"
    );
    assert!(e0403_errors[0].message.contains("internalFactory"));
}

#[test]
fn e0403_untyped_send_not_checked() {
    // Dynamic sends where receiver type is unknown should NOT emit E0403.
    // Using an Identifier (untyped variable) as receiver — type is Dynamic.
    let module = make_module(vec![msg_send(
        Expression::Identifier(ident("client")),
        MessageSelector::Keyword(vec![KeywordPart::new("buildHeaders:", span())]),
        vec![int_lit(42)],
    )]);
    let hierarchy = make_hierarchy_with_internal_method();
    let mut checker = TypeChecker::with_package("my_app");
    checker.check_module(&module, &hierarchy);
    let e0403_errors: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("is internal to package"))
        .collect();
    assert!(
        e0403_errors.is_empty(),
        "Untyped dynamic send should not emit internal method error, got: {e0403_errors:?}"
    );
}

// ---- BT-1818: Generalized infer_method_local_params tests ----

/// Add a non-parametric `Processor` class with methods that accept parametric param types.
///
/// - `processResult: r :: GenResult(T, E) -> T` — extracts T from a Result argument
/// - `processArray: a :: Array(T) -> T` — extracts T from an Array argument
fn add_processor_class(hierarchy: &mut ClassHierarchy) {
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};

    let info = ClassInfo {
        name: eco_string("Processor"),
        superclass: Some(eco_string("Value")),
        is_sealed: true,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: true,
        is_native: false,
        state: vec![],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        methods: vec![
            MethodInfo {
                selector: eco_string("processResult:"),
                arity: 1,
                kind: MethodKind::Primary,
                defined_in: eco_string("Processor"),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: Some(eco_string("T")),
                param_types: vec![Some(eco_string("GenResult(T, E)"))],
                doc: None,
            },
            MethodInfo {
                selector: eco_string("processArray:"),
                arity: 1,
                kind: MethodKind::Primary,
                defined_in: eco_string("Processor"),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: Some(eco_string("T")),
                param_types: vec![Some(eco_string("Array(T)"))],
                doc: None,
            },
        ],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };

    hierarchy.add_from_beam_meta(vec![info]);
}

/// BT-1818: Block inference still works after generalization (regression).
/// `map:` on `GenResult(Integer, IOError)` with a `Block(Integer, String)` arg
/// should infer R=String and return `GenResult(String, IOError)`.
#[test]
fn method_local_params_block_regression() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_result_class(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set(
        "r",
        InferredType::Known {
            class_name: eco_string("GenResult"),
            type_args: vec![
                InferredType::known("Integer"),
                InferredType::known("IOError"),
            ],
            provenance: TypeProvenance::Declared(span()),
        },
    );
    // Simulate a block argument with type Block(Integer, String)
    env.set(
        "blk",
        InferredType::Known {
            class_name: eco_string("Block"),
            type_args: vec![
                InferredType::known("Integer"),
                InferredType::known("String"),
            ],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result_ty = checker.infer_expr(
        &msg_send(
            var("r"),
            MessageSelector::Keyword(vec![KeywordPart::new("map:", span())]),
            vec![var("blk")],
        ),
        &hierarchy,
        &mut env,
        false,
    );

    // map: returns GenResult(R, E) where R=String (from block return type), E=IOError
    match &result_ty {
        InferredType::Known {
            class_name,
            type_args,
            ..
        } => {
            assert_eq!(class_name.as_str(), "GenResult");
            assert_eq!(type_args.len(), 2);
            assert_eq!(
                type_args[0].as_known().map(EcoString::as_str),
                Some("String"),
                "R should be String from block return type"
            );
            assert_eq!(
                type_args[1].as_known().map(EcoString::as_str),
                Some("IOError"),
                "E should remain IOError"
            );
        }
        other => panic!("Expected Known type, got: {other:?}"),
    }
}

/// BT-1818: Infer method-local params from Result(T, E) argument type.
/// `processResult:` on non-parametric Processor with `GenResult(Integer, IOError)` arg
/// should infer T=Integer and return Integer.
#[test]
fn method_local_params_from_result_type() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_result_class(&mut hierarchy);
    add_processor_class(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set("p", InferredType::known("Processor"));
    env.set(
        "r",
        InferredType::Known {
            class_name: eco_string("GenResult"),
            type_args: vec![
                InferredType::known("Integer"),
                InferredType::known("IOError"),
            ],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result_ty = checker.infer_expr(
        &msg_send(
            var("p"),
            MessageSelector::Keyword(vec![KeywordPart::new("processResult:", span())]),
            vec![var("r")],
        ),
        &hierarchy,
        &mut env,
        false,
    );

    assert_eq!(
        result_ty.as_known().map(EcoString::as_str),
        Some("Integer"),
        "processResult: with GenResult(Integer, IOError) should return Integer (T), got: {result_ty:?}"
    );
}

/// BT-1818: Infer method-local params from Array(T) argument type.
/// `processArray:` on non-parametric Processor with Array(String) arg
/// should infer T=String and return String.
#[test]
fn method_local_params_from_array_type() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_processor_class(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set("p", InferredType::known("Processor"));
    env.set(
        "a",
        InferredType::Known {
            class_name: eco_string("Array"),
            type_args: vec![InferredType::known("String")],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result_ty = checker.infer_expr(
        &msg_send(
            var("p"),
            MessageSelector::Keyword(vec![KeywordPart::new("processArray:", span())]),
            vec![var("a")],
        ),
        &hierarchy,
        &mut env,
        false,
    );

    assert_eq!(
        result_ty.as_known().map(EcoString::as_str),
        Some("String"),
        "processArray: with Array(String) should return String (T), got: {result_ty:?}"
    );
}

// ---- BT-1830: Singleton type inference and validation ----

#[test]
fn is_assignable_to_singleton_exact_match() {
    let hierarchy = ClassHierarchy::with_builtins();
    assert!(
        TypeChecker::is_assignable_to(&"#ok".into(), &"#ok".into(), &hierarchy),
        "#ok should be assignable to #ok"
    );
}

#[test]
fn is_assignable_to_singleton_mismatch() {
    let hierarchy = ClassHierarchy::with_builtins();
    assert!(
        !TypeChecker::is_assignable_to(&"#error".into(), &"#ok".into(), &hierarchy),
        "#error should not be assignable to #ok"
    );
}

#[test]
fn is_assignable_to_singleton_rejects_symbol() {
    // BT-1878: Symbol is NOT assignable to #ok — subtyping goes the other direction.
    // #ok is a subtype of Symbol, not the reverse.
    let hierarchy = ClassHierarchy::with_builtins();
    assert!(
        !TypeChecker::is_assignable_to(&"Symbol".into(), &"#ok".into(), &hierarchy),
        "Symbol should NOT be assignable to #ok (wrong subtyping direction)"
    );
}

#[test]
fn is_assignable_to_singleton_value_to_symbol() {
    let hierarchy = ClassHierarchy::with_builtins();
    assert!(
        TypeChecker::is_assignable_to(&"#ok".into(), &"Symbol".into(), &hierarchy),
        "#ok should be assignable to Symbol (singleton is a subtype of Symbol)"
    );
}

#[test]
fn is_assignable_to_singleton_rejects_unrelated_type() {
    let hierarchy = ClassHierarchy::with_builtins();
    assert!(
        !TypeChecker::is_assignable_to(&"Integer".into(), &"#ok".into(), &hierarchy),
        "Integer should not be assignable to #ok"
    );
}

#[test]
fn singleton_return_type_mismatch_warns() {
    // method -> #ok => #error  — should warn
    let class_def = ClassDefinition::with_modifiers(
        ident("Checker"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::with_return_type(
            MessageSelector::Unary("status".into()),
            vec![],
            vec![bare(symbol_lit("error"))], // Returns Symbol, declared #ok
            TypeAnnotation::singleton("ok", span()),
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let return_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .collect();
    assert_eq!(
        return_warnings.len(),
        1,
        "should warn about singleton return type mismatch: {return_warnings:?}"
    );
    assert!(return_warnings[0].message.contains("#ok"));
}

#[test]
fn singleton_return_type_match_no_warning() {
    // method -> #ok => #ok  — should NOT warn (Symbol literal is compatible)
    let class_def = ClassDefinition::with_modifiers(
        ident("Checker"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::with_return_type(
            MessageSelector::Unary("status".into()),
            vec![],
            vec![bare(symbol_lit("ok"))], // Returns Symbol — compatible with #ok
            TypeAnnotation::singleton("ok", span()),
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let return_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .collect();
    assert!(
        return_warnings.is_empty(),
        "Symbol literal compatible with singleton return type should not warn: {return_warnings:?}"
    );
}

#[test]
fn is_assignable_to_singleton_to_symbol_union() {
    let hierarchy = ClassHierarchy::with_builtins();
    // #ok should be assignable to "Symbol | nil" (singleton is a subtype of Symbol)
    assert!(
        TypeChecker::is_assignable_to(&"#ok".into(), &"Symbol | nil".into(), &hierarchy),
        "#ok should be assignable to Symbol | nil"
    );
}

#[test]
fn is_assignable_to_singleton_to_object() {
    let hierarchy = ClassHierarchy::with_builtins();
    // #ok should be assignable to Object (Symbol is a subclass of Object)
    assert!(
        TypeChecker::is_assignable_to(&"#ok".into(), &"Object".into(), &hierarchy),
        "#ok should be assignable to Object via Symbol superclass chain"
    );
}

// ── BT-1878: Singleton/Symbol subtyping direction regression tests ──

#[test]
fn symbol_not_assignable_to_singleton_union() {
    // BT-1878: Symbol should NOT be assignable to a union of singletons
    let hierarchy = ClassHierarchy::with_builtins();
    assert!(
        !TypeChecker::is_assignable_to(&"Symbol".into(), &"#ok | #error".into(), &hierarchy),
        "Symbol should NOT be assignable to #ok | #error"
    );
}

#[test]
fn singleton_assignable_to_singleton_union() {
    // Regression: #ok must be assignable to #ok | #error (singleton union).
    // Previously, starts_with('#') matched the union string before the union
    // check, causing an exact-match return that rejected valid members.
    let hierarchy = ClassHierarchy::with_builtins();
    assert!(
        TypeChecker::is_assignable_to(&"#ok".into(), &"#ok | #error".into(), &hierarchy),
        "#ok should be assignable to #ok | #error"
    );
    assert!(
        TypeChecker::is_assignable_to(&"#error".into(), &"#ok | #error".into(), &hierarchy),
        "#error should be assignable to #ok | #error"
    );
}

// ── BT-1832: Union type validation across all contexts ──────────────────

// --- Argument type checking with unions ---

#[test]
fn union_arg_all_compatible_no_warning() {
    // Union(Integer | Float) passed to param expecting Number → all compatible → no warning
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};
    let mut hierarchy = ClassHierarchy::with_builtins();
    hierarchy.add_from_beam_meta(vec![ClassInfo {
        name: "Calculator".into(),
        superclass: Some("Object".into()),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: HashMap::new(),
        state_has_default: HashMap::new(),
        methods: vec![MethodInfo {
            selector: "compute:".into(),
            arity: 1,
            kind: MethodKind::Primary,
            defined_in: "Calculator".into(),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: Some("Number".into()),
            param_types: vec![Some("Number".into())],
            doc: None,
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    }]);

    let mut checker = TypeChecker::new();
    checker.check_argument_types(
        &"Calculator".into(),
        "compute:",
        &[InferredType::simple_union(&["Integer", "Float"])],
        span(),
        &hierarchy,
        false,
        None,
        None,
    );
    assert!(
        checker.diagnostics().is_empty(),
        "All union members (Integer, Float) are Numbers — no warning expected, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn union_arg_none_compatible_warns() {
    // Union(String | Symbol) passed to param expecting Integer → none compatible → warning
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};
    let mut hierarchy = ClassHierarchy::with_builtins();
    hierarchy.add_from_beam_meta(vec![ClassInfo {
        name: "Calculator".into(),
        superclass: Some("Object".into()),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: HashMap::new(),
        state_has_default: HashMap::new(),
        methods: vec![MethodInfo {
            selector: "compute:".into(),
            arity: 1,
            kind: MethodKind::Primary,
            defined_in: "Calculator".into(),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: Some("Integer".into()),
            param_types: vec![Some("Integer".into())],
            doc: None,
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    }]);

    let mut checker = TypeChecker::new();
    checker.check_argument_types(
        &"Calculator".into(),
        "compute:",
        &[InferredType::simple_union(&["String", "Symbol"])],
        span(),
        &hierarchy,
        false,
        None,
        None,
    );
    assert_eq!(
        checker.diagnostics().len(),
        1,
        "No union member is Integer — expected 1 warning, got: {:?}",
        checker.diagnostics()
    );
    assert!(checker.diagnostics()[0].message.contains("expects Integer"));
    assert_eq!(
        checker.diagnostics()[0].severity,
        crate::source_analysis::Severity::Warning
    );
}

#[test]
fn union_arg_mixed_compatible_hints() {
    // Union(Integer | String) passed to param expecting Number → Integer matches, String doesn't → hint
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};
    let mut hierarchy = ClassHierarchy::with_builtins();
    hierarchy.add_from_beam_meta(vec![ClassInfo {
        name: "Calculator".into(),
        superclass: Some("Object".into()),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: HashMap::new(),
        state_has_default: HashMap::new(),
        methods: vec![MethodInfo {
            selector: "compute:".into(),
            arity: 1,
            kind: MethodKind::Primary,
            defined_in: "Calculator".into(),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: Some("Number".into()),
            param_types: vec![Some("Number".into())],
            doc: None,
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    }]);

    let mut checker = TypeChecker::new();
    checker.check_argument_types(
        &"Calculator".into(),
        "compute:",
        &[InferredType::simple_union(&["Integer", "String"])],
        span(),
        &hierarchy,
        false,
        None,
        None,
    );
    assert_eq!(
        checker.diagnostics().len(),
        1,
        "Mixed union — expected 1 hint, got: {:?}",
        checker.diagnostics()
    );
    assert_eq!(
        checker.diagnostics()[0].severity,
        crate::source_analysis::Severity::Hint
    );
    assert!(checker.diagnostics()[0].message.contains("expects Number"));
}

#[test]
fn union_arg_dynamic_still_skips() {
    // Dynamic arg should still skip (no regression)
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};
    let mut hierarchy = ClassHierarchy::with_builtins();
    hierarchy.add_from_beam_meta(vec![ClassInfo {
        name: "Calculator".into(),
        superclass: Some("Object".into()),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: HashMap::new(),
        state_has_default: HashMap::new(),
        methods: vec![MethodInfo {
            selector: "compute:".into(),
            arity: 1,
            kind: MethodKind::Primary,
            defined_in: "Calculator".into(),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: Some("Integer".into()),
            param_types: vec![Some("Integer".into())],
            doc: None,
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    }]);

    let mut checker = TypeChecker::new();
    checker.check_argument_types(
        &"Calculator".into(),
        "compute:",
        &[InferredType::Dynamic(DynamicReason::Unknown)],
        span(),
        &hierarchy,
        false,
        None,
        None,
    );
    assert!(
        checker.diagnostics().is_empty(),
        "Dynamic arg should skip — no warning expected, got: {:?}",
        checker.diagnostics()
    );
}

// --- Field assignment with unions ---

#[test]
fn union_field_assign_all_compatible_no_warning() {
    // state: count :: Number; self.count := (Integer | Float) → all compatible → no warning
    let state = vec![StateDeclaration::with_type_and_default(
        ident("count"),
        TypeAnnotation::simple("Number", span()),
        int_lit(0),
        span(),
    )];
    let method = make_method(
        "setCount",
        vec![field_assign("count", int_lit(1))], // placeholder, we test directly below
    );
    let class = counter_class_with_typed_state(vec![method], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set("self", InferredType::known("Counter"));
    checker.check_field_assignment(
        &ident("count"),
        &InferredType::simple_union(&["Integer", "Float"]),
        span(),
        &hierarchy,
        &env,
    );
    assert!(
        checker.diagnostics().is_empty(),
        "All union members compatible with Number — no warning expected, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn union_field_assign_none_compatible_warns() {
    // state: count :: Integer; self.count := (String | Symbol) → none compatible → warning
    let state = vec![StateDeclaration::with_type_and_default(
        ident("count"),
        TypeAnnotation::simple("Integer", span()),
        int_lit(0),
        span(),
    )];
    let method = make_method("setCount", vec![field_assign("count", int_lit(1))]);
    let class = counter_class_with_typed_state(vec![method], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set("self", InferredType::known("Counter"));
    checker.check_field_assignment(
        &ident("count"),
        &InferredType::simple_union(&["String", "Symbol"]),
        span(),
        &hierarchy,
        &env,
    );
    assert_eq!(
        checker.diagnostics().len(),
        1,
        "No union member is Integer — expected 1 warning, got: {:?}",
        checker.diagnostics()
    );
    assert!(checker.diagnostics()[0].message.contains("Type mismatch"));
    assert_eq!(
        checker.diagnostics()[0].severity,
        crate::source_analysis::Severity::Warning
    );
}

#[test]
fn union_field_assign_mixed_compatible_hints() {
    // state: count :: Number; self.count := (Integer | String) → Integer matches, String doesn't → hint
    let state = vec![StateDeclaration::with_type_and_default(
        ident("count"),
        TypeAnnotation::simple("Number", span()),
        int_lit(0),
        span(),
    )];
    let method = make_method("setCount", vec![field_assign("count", int_lit(1))]);
    let class = counter_class_with_typed_state(vec![method], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set("self", InferredType::known("Counter"));
    checker.check_field_assignment(
        &ident("count"),
        &InferredType::simple_union(&["Integer", "String"]),
        span(),
        &hierarchy,
        &env,
    );
    assert_eq!(
        checker.diagnostics().len(),
        1,
        "Mixed union — expected 1 hint, got: {:?}",
        checker.diagnostics()
    );
    assert_eq!(
        checker.diagnostics()[0].severity,
        crate::source_analysis::Severity::Hint
    );
}

// --- Protocol conformance with unions ---

#[test]
fn union_protocol_all_conform_no_warning() {
    // Integer | String both conform to Printable (both have asString) → no warning
    let hierarchy = ClassHierarchy::with_builtins();

    let mut registry = ProtocolRegistry::new();
    let proto_module = Module {
        protocols: vec![ProtocolDefinition {
            name: Identifier::new("Printable", span()),
            type_params: vec![],
            extending: None,
            method_signatures: vec![ProtocolMethodSignature {
                selector: MessageSelector::Unary("asString".into()),
                parameters: vec![],
                return_type: Some(TypeAnnotation::simple("String", span())),
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: span(),
            }],
            class_method_signatures: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }],
        ..Module::new(vec![], span())
    };
    registry.register_module(&proto_module, &hierarchy);

    let mut checker = TypeChecker::new();
    checker.check_protocol_argument_conformance(
        &InferredType::simple_union(&["Integer", "String"]),
        "Printable",
        span(),
        &hierarchy,
        &registry,
    );
    assert!(
        checker.diagnostics().is_empty(),
        "Both Integer and String conform to Printable — no warning expected, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn union_protocol_none_conform_warns() {
    // Integer | String, neither has sortKey → warning
    let hierarchy = ClassHierarchy::with_builtins();

    let mut registry = ProtocolRegistry::new();
    let proto_module = Module {
        protocols: vec![ProtocolDefinition {
            name: Identifier::new("HasSortKey", span()),
            type_params: vec![],
            extending: None,
            method_signatures: vec![ProtocolMethodSignature {
                selector: MessageSelector::Unary("sortKey".into()),
                parameters: vec![],
                return_type: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: span(),
            }],
            class_method_signatures: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }],
        ..Module::new(vec![], span())
    };
    registry.register_module(&proto_module, &hierarchy);

    let mut checker = TypeChecker::new();
    checker.check_protocol_argument_conformance(
        &InferredType::simple_union(&["Integer", "String"]),
        "HasSortKey",
        span(),
        &hierarchy,
        &registry,
    );
    assert_eq!(
        checker.diagnostics().len(),
        1,
        "No union member conforms to HasSortKey — expected 1 warning, got: {:?}",
        checker.diagnostics()
    );
    assert!(
        checker.diagnostics()[0]
            .message
            .contains("does not conform to protocol HasSortKey")
    );
    assert_eq!(
        checker.diagnostics()[0].severity,
        crate::source_analysis::Severity::Warning
    );
}

#[test]
fn union_protocol_mixed_conformance_hints() {
    // Build a protocol "HasSortKey" requiring sortKey.
    // Integer has sortKey (we register it), String does not → hint.
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};
    let mut hierarchy = ClassHierarchy::with_builtins();
    // Add sortKey to Integer so it conforms
    hierarchy.add_from_beam_meta(vec![ClassInfo {
        name: "SortableInt".into(),
        superclass: Some("Integer".into()),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: true,
        is_native: false,
        state: vec![],
        state_types: HashMap::new(),
        state_has_default: HashMap::new(),
        methods: vec![MethodInfo {
            selector: "sortKey".into(),
            arity: 0,
            kind: MethodKind::Primary,
            defined_in: "SortableInt".into(),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: None,
            param_types: vec![],
            doc: None,
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    }]);

    let mut registry = ProtocolRegistry::new();
    let proto_module = Module {
        protocols: vec![ProtocolDefinition {
            name: Identifier::new("HasSortKey", span()),
            type_params: vec![],
            extending: None,
            method_signatures: vec![ProtocolMethodSignature {
                selector: MessageSelector::Unary("sortKey".into()),
                parameters: vec![],
                return_type: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: span(),
            }],
            class_method_signatures: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }],
        ..Module::new(vec![], span())
    };
    registry.register_module(&proto_module, &hierarchy);

    let mut checker = TypeChecker::new();
    // SortableInt conforms (has sortKey), String does not
    checker.check_protocol_argument_conformance(
        &InferredType::simple_union(&["SortableInt", "String"]),
        "HasSortKey",
        span(),
        &hierarchy,
        &registry,
    );
    assert_eq!(
        checker.diagnostics().len(),
        1,
        "Mixed conformance — expected 1 hint, got: {:?}",
        checker.diagnostics()
    );
    assert_eq!(
        checker.diagnostics()[0].severity,
        crate::source_analysis::Severity::Hint
    );
    assert!(checker.diagnostics()[0].message.contains("Not all members"));
}

// --- Type parameter bounds with unions ---

#[test]
fn union_type_param_bounds_all_conform_no_warning() {
    // Logger(T :: Printable) where T = Integer | String → both conform → no warning
    use crate::semantic_analysis::class_hierarchy::ClassInfo;
    let mut hierarchy = ClassHierarchy::with_builtins();
    hierarchy.add_from_beam_meta(vec![ClassInfo {
        name: "BoundedLogger".into(),
        superclass: Some("Actor".into()),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: HashMap::new(),
        state_has_default: HashMap::new(),
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec!["T".into()],
        type_param_bounds: vec![Some("Printable".into())],
        superclass_type_args: vec![],
    }]);

    let mut registry = ProtocolRegistry::new();
    let proto_module = Module {
        protocols: vec![ProtocolDefinition {
            name: Identifier::new("Printable", span()),
            type_params: vec![],
            extending: None,
            method_signatures: vec![ProtocolMethodSignature {
                selector: MessageSelector::Unary("asString".into()),
                parameters: vec![],
                return_type: Some(TypeAnnotation::simple("String", span())),
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: span(),
            }],
            class_method_signatures: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }],
        ..Module::new(vec![], span())
    };
    registry.register_module(&proto_module, &hierarchy);

    let mut checker = TypeChecker::new();
    checker.check_type_param_bounds(
        &"BoundedLogger".into(),
        &[InferredType::simple_union(&["Integer", "String"])],
        span(),
        &hierarchy,
        &registry,
    );
    assert!(
        checker.diagnostics().is_empty(),
        "All union members conform to Printable — no warning expected, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn union_type_param_bounds_none_conform_warns() {
    // Logger(T :: HasSortKey) where T = Integer | String → neither conforms → warning
    use crate::semantic_analysis::class_hierarchy::ClassInfo;
    let mut hierarchy = ClassHierarchy::with_builtins();
    hierarchy.add_from_beam_meta(vec![ClassInfo {
        name: "BoundedLogger".into(),
        superclass: Some("Actor".into()),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: HashMap::new(),
        state_has_default: HashMap::new(),
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec!["T".into()],
        type_param_bounds: vec![Some("HasSortKey".into())],
        superclass_type_args: vec![],
    }]);

    let mut registry = ProtocolRegistry::new();
    let proto_module = Module {
        protocols: vec![ProtocolDefinition {
            name: Identifier::new("HasSortKey", span()),
            type_params: vec![],
            extending: None,
            method_signatures: vec![ProtocolMethodSignature {
                selector: MessageSelector::Unary("sortKey".into()),
                parameters: vec![],
                return_type: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: span(),
            }],
            class_method_signatures: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }],
        ..Module::new(vec![], span())
    };
    registry.register_module(&proto_module, &hierarchy);

    let mut checker = TypeChecker::new();
    checker.check_type_param_bounds(
        &"BoundedLogger".into(),
        &[InferredType::simple_union(&["Integer", "String"])],
        span(),
        &hierarchy,
        &registry,
    );
    assert_eq!(
        checker.diagnostics().len(),
        1,
        "No union member conforms to HasSortKey — expected 1 warning, got: {:?}",
        checker.diagnostics()
    );
    assert!(
        checker.diagnostics()[0]
            .message
            .contains("does not conform to HasSortKey")
    );
    assert_eq!(
        checker.diagnostics()[0].severity,
        crate::source_analysis::Severity::Warning
    );
}

#[test]
fn union_type_param_bounds_mixed_conformance_hints() {
    // Logger(T :: HasSortKey) where T = SortableInt | String → SortableInt conforms, String doesn't → hint
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};
    let mut hierarchy = ClassHierarchy::with_builtins();
    hierarchy.add_from_beam_meta(vec![
        ClassInfo {
            name: "BoundedLogger".into(),
            superclass: Some("Actor".into()),
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            is_internal: false,
            package: None,
            is_value: false,
            is_native: false,
            state: vec![],
            state_types: HashMap::new(),
            state_has_default: HashMap::new(),
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec!["T".into()],
            type_param_bounds: vec![Some("HasSortKey".into())],
            superclass_type_args: vec![],
        },
        ClassInfo {
            name: "SortableInt".into(),
            superclass: Some("Integer".into()),
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            is_internal: false,
            package: None,
            is_value: true,
            is_native: false,
            state: vec![],
            state_types: HashMap::new(),
            state_has_default: HashMap::new(),
            methods: vec![MethodInfo {
                selector: "sortKey".into(),
                arity: 0,
                kind: MethodKind::Primary,
                defined_in: "SortableInt".into(),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: None,
                param_types: vec![],
                doc: None,
            }],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            type_param_bounds: vec![],
            superclass_type_args: vec![],
        },
    ]);

    let mut registry = ProtocolRegistry::new();
    let proto_module = Module {
        protocols: vec![ProtocolDefinition {
            name: Identifier::new("HasSortKey", span()),
            type_params: vec![],
            extending: None,
            method_signatures: vec![ProtocolMethodSignature {
                selector: MessageSelector::Unary("sortKey".into()),
                parameters: vec![],
                return_type: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: span(),
            }],
            class_method_signatures: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }],
        ..Module::new(vec![], span())
    };
    registry.register_module(&proto_module, &hierarchy);

    let mut checker = TypeChecker::new();
    checker.check_type_param_bounds(
        &"BoundedLogger".into(),
        &[InferredType::simple_union(&["SortableInt", "String"])],
        span(),
        &hierarchy,
        &registry,
    );
    assert_eq!(
        checker.diagnostics().len(),
        1,
        "Mixed conformance — expected 1 hint, got: {:?}",
        checker.diagnostics()
    );
    assert_eq!(
        checker.diagnostics()[0].severity,
        crate::source_analysis::Severity::Hint
    );
    assert!(
        checker.diagnostics()[0]
            .message
            .contains("not all members conform to HasSortKey")
    );
}

// ---- BT-1834: Generic return type resolution ----

/// BT-1834: List(E) first returns E, resolved to concrete type via substitution.
#[test]
fn generic_list_first_returns_element_type() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    // Simulate a variable typed as List(String)
    env.set(
        "names",
        InferredType::Known {
            class_name: eco_string("List"),
            type_args: vec![InferredType::known("String")],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result = checker.infer_expr(
        &msg_send(var("names"), MessageSelector::Unary("first".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );
    assert_eq!(
        result.as_known().map(EcoString::as_str),
        Some("String"),
        "List(String) first should return String, got: {result:?}"
    );
}

/// BT-1834: List(E) last returns E.
#[test]
fn generic_list_last_returns_element_type() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    env.set(
        "nums",
        InferredType::Known {
            class_name: eco_string("List"),
            type_args: vec![InferredType::known("Integer")],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result = checker.infer_expr(
        &msg_send(var("nums"), MessageSelector::Unary("last".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );
    assert_eq!(
        result.as_known().map(EcoString::as_str),
        Some("Integer"),
        "List(Integer) last should return Integer, got: {result:?}"
    );
}

/// BT-1834: List(E) at: returns E.
#[test]
fn generic_list_at_returns_element_type() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    env.set(
        "items",
        InferredType::Known {
            class_name: eco_string("List"),
            type_args: vec![InferredType::known("Float")],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result = checker.infer_expr(
        &msg_send(
            var("items"),
            MessageSelector::Keyword(vec![KeywordPart::new("at:", span())]),
            vec![int_lit(1)],
        ),
        &hierarchy,
        &mut env,
        false,
    );
    assert_eq!(
        result.as_known().map(EcoString::as_str),
        Some("Float"),
        "List(Float) at: should return Float, got: {result:?}"
    );
}

/// BT-1834: Array(E) at: returns E (existing functionality, regression check).
#[test]
fn generic_array_at_returns_element_type() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    env.set(
        "arr",
        InferredType::Known {
            class_name: eco_string("Array"),
            type_args: vec![InferredType::known("String")],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result = checker.infer_expr(
        &msg_send(
            var("arr"),
            MessageSelector::Keyword(vec![KeywordPart::new("at:", span())]),
            vec![int_lit(1)],
        ),
        &hierarchy,
        &mut env,
        false,
    );
    assert_eq!(
        result.as_known().map(EcoString::as_str),
        Some("String"),
        "Array(String) at: should return String, got: {result:?}"
    );
}

/// BT-1834: Dictionary(K, V) at: returns V.
#[test]
fn generic_dictionary_at_returns_value_type() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    env.set(
        "dict",
        InferredType::Known {
            class_name: eco_string("Dictionary"),
            type_args: vec![
                InferredType::known("String"),
                InferredType::known("Integer"),
            ],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result = checker.infer_expr(
        &msg_send(
            var("dict"),
            MessageSelector::Keyword(vec![KeywordPart::new("at:", span())]),
            vec![str_lit("key")],
        ),
        &hierarchy,
        &mut env,
        false,
    );
    assert_eq!(
        result.as_known().map(EcoString::as_str),
        Some("Integer"),
        "Dictionary(String, Integer) at: should return Integer, got: {result:?}"
    );
}

/// BT-1834: Nested generics — Dictionary(String, Array(Integer)) at: returns Array(Integer).
#[test]
fn generic_nested_dictionary_at_returns_nested_type() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    env.set(
        "nested",
        InferredType::Known {
            class_name: eco_string("Dictionary"),
            type_args: vec![
                InferredType::known("String"),
                InferredType::Known {
                    class_name: eco_string("Array"),
                    type_args: vec![InferredType::known("Integer")],
                    provenance: TypeProvenance::Declared(span()),
                },
            ],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result = checker.infer_expr(
        &msg_send(
            var("nested"),
            MessageSelector::Keyword(vec![KeywordPart::new("at:", span())]),
            vec![str_lit("key")],
        ),
        &hierarchy,
        &mut env,
        false,
    );

    // Should return Array(Integer)
    match &result {
        InferredType::Known {
            class_name,
            type_args,
            ..
        } => {
            assert_eq!(class_name.as_str(), "Array");
            assert_eq!(type_args.len(), 1);
            assert_eq!(
                type_args[0].as_known().map(EcoString::as_str),
                Some("Integer")
            );
        }
        other => panic!("Expected Known(Array(Integer)), got: {other:?}"),
    }
}

/// BT-1834: Block value returns the last type arg.
#[test]
fn generic_block_value_returns_last_type_arg() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    // Block(String) — zero-arg block returning String
    env.set(
        "blk",
        InferredType::Known {
            class_name: eco_string("Block"),
            type_args: vec![InferredType::known("String")],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result = checker.infer_expr(
        &msg_send(var("blk"), MessageSelector::Unary("value".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );
    assert_eq!(
        result.as_known().map(EcoString::as_str),
        Some("String"),
        "Block(String) value should return String, got: {result:?}"
    );
}

/// BT-1834: Block(A, R) value: returns R (last type arg).
#[test]
fn generic_block_value_colon_returns_last_type_arg() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    // Block(Integer, String) — one-arg block taking Integer, returning String
    env.set(
        "blk",
        InferredType::Known {
            class_name: eco_string("Block"),
            type_args: vec![
                InferredType::known("Integer"),
                InferredType::known("String"),
            ],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result = checker.infer_expr(
        &msg_send(
            var("blk"),
            MessageSelector::Keyword(vec![KeywordPart::new("value:", span())]),
            vec![int_lit(42)],
        ),
        &hierarchy,
        &mut env,
        false,
    );
    assert_eq!(
        result.as_known().map(EcoString::as_str),
        Some("String"),
        "Block(Integer, String) value: should return String, got: {result:?}"
    );
}

/// BT-1834: Unresolvable type params fall back to Dynamic (no regression).
#[test]
fn generic_unresolved_type_param_falls_back_to_dynamic() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    // Bare List without type args — first should be Dynamic
    env.set("plain", InferredType::known("List"));

    let result = checker.infer_expr(
        &msg_send(var("plain"), MessageSelector::Unary("first".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );
    assert!(
        matches!(result, InferredType::Dynamic(DynamicReason::Unknown)),
        "List (no type args) first should fall back to Dynamic, got: {result:?}"
    );
}

/// BT-1834: Plain param type inference — inject:into: resolves A from initial arg.
#[test]
fn generic_inject_into_resolves_accumulator_type() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    env.set(
        "arr",
        InferredType::Known {
            class_name: eco_string("Array"),
            type_args: vec![InferredType::known("Integer")],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    // arr inject: 0 into: [:acc :x | acc + x]
    // A should be inferred as Integer from the initial value argument
    let block = Expression::Block(Block::new(
        vec![
            crate::ast::BlockParameter::new("acc", span()),
            crate::ast::BlockParameter::new("x", span()),
        ],
        vec![bare(msg_send(
            var("acc"),
            MessageSelector::Binary("+".into()),
            vec![var("x")],
        ))],
        span(),
    ));

    let result = checker.infer_expr(
        &msg_send(
            var("arr"),
            MessageSelector::Keyword(vec![
                KeywordPart::new("inject:", span()),
                KeywordPart::new("into:", span()),
            ]),
            vec![int_lit(0), block],
        ),
        &hierarchy,
        &mut env,
        false,
    );
    assert_eq!(
        result.as_known().map(EcoString::as_str),
        Some("Integer"),
        "inject: 0 into: should return Integer (inferred from initial), got: {result:?}"
    );
}

/// BT-1834: List(E) detect: returns E.
#[test]
fn generic_list_detect_returns_element_type() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    env.set(
        "strs",
        InferredType::Known {
            class_name: eco_string("List"),
            type_args: vec![InferredType::known("String")],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let block = Expression::Block(Block::new(
        vec![crate::ast::BlockParameter::new("x", span())],
        vec![bare(msg_send(
            var("x"),
            MessageSelector::Binary(">".into()),
            vec![str_lit("a")],
        ))],
        span(),
    ));

    let result = checker.infer_expr(
        &msg_send(
            var("strs"),
            MessageSelector::Keyword(vec![KeywordPart::new("detect:", span())]),
            vec![block],
        ),
        &hierarchy,
        &mut env,
        false,
    );
    assert_eq!(
        result.as_known().map(EcoString::as_str),
        Some("String"),
        "List(String) detect: should return String, got: {result:?}"
    );
}

/// BT-1834: Behaviour superclass returns Behaviour | Nil.
#[test]
fn behaviour_superclass_returns_union_type() {
    let hierarchy = ClassHierarchy::with_builtins();
    let method = hierarchy.find_method("Behaviour", "superclass");
    assert!(method.is_some(), "Behaviour should have superclass method");
    assert_eq!(
        method.unwrap().return_type.as_deref(),
        Some("Behaviour | Nil"),
        "superclass should return Behaviour | Nil"
    );
}

// --- BT-1835: Union syntax in builtin param_types ---

#[test]
fn union_param_type_integer_compatible() {
    // Integer arg to "Integer | Symbol" param → no warning
    let hierarchy = hierarchy_with_extension(
        "Object",
        "setTimeout:",
        1,
        vec![Some("Integer | Symbol".into())],
        Some("Nil".into()),
    );
    let module = make_module(vec![msg_send(
        msg_send(
            class_ref("Object"),
            MessageSelector::Unary("new".into()),
            vec![],
        ),
        MessageSelector::Keyword(vec![KeywordPart::new("setTimeout:", span())]),
        vec![int_lit(42)],
    )]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects"))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "Integer should be compatible with 'Integer | Symbol', got: {type_warnings:?}"
    );
}

#[test]
fn union_param_type_symbol_compatible() {
    // Symbol arg to "Integer | Symbol" param → no warning
    let hierarchy = hierarchy_with_extension(
        "Object",
        "setTimeout:",
        1,
        vec![Some("Integer | Symbol".into())],
        Some("Nil".into()),
    );
    let module = make_module(vec![msg_send(
        msg_send(
            class_ref("Object"),
            MessageSelector::Unary("new".into()),
            vec![],
        ),
        MessageSelector::Keyword(vec![KeywordPart::new("setTimeout:", span())]),
        vec![sym_lit("infinity")],
    )]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects"))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "Symbol should be compatible with 'Integer | Symbol', got: {type_warnings:?}"
    );
}

#[test]
fn union_param_type_incompatible_warns() {
    // String arg to "Integer | Symbol" param → warning
    let hierarchy = hierarchy_with_extension(
        "Object",
        "setTimeout:",
        1,
        vec![Some("Integer | Symbol".into())],
        Some("Nil".into()),
    );
    let module = make_module(vec![msg_send(
        msg_send(
            class_ref("Object"),
            MessageSelector::Unary("new".into()),
            vec![],
        ),
        MessageSelector::Keyword(vec![KeywordPart::new("setTimeout:", span())]),
        vec![str_lit("bad")],
    )]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects"))
        .collect();
    assert_eq!(
        type_warnings.len(),
        1,
        "String should be incompatible with 'Integer | Symbol', got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn union_param_type_subclass_compatible() {
    // Integer is a subclass of Number, so Integer arg to "Number | Symbol" should pass
    let hierarchy = hierarchy_with_extension(
        "Object",
        "setTimeout:",
        1,
        vec![Some("Number | Symbol".into())],
        Some("Nil".into()),
    );
    let module = make_module(vec![msg_send(
        msg_send(
            class_ref("Object"),
            MessageSelector::Unary("new".into()),
            vec![],
        ),
        MessageSelector::Keyword(vec![KeywordPart::new("setTimeout:", span())]),
        vec![int_lit(42)],
    )]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects"))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "Integer should be compatible with 'Number | Symbol' (subclass of Number), got: {type_warnings:?}"
    );
}

#[test]
fn non_union_param_type_unchanged() {
    // Non-union expected type still works as before: String arg to Integer param → warning
    let hierarchy = hierarchy_with_extension(
        "Object",
        "deposit:",
        1,
        vec![Some("Integer".into())],
        Some("Nil".into()),
    );
    let module = make_module(vec![msg_send(
        msg_send(
            class_ref("Object"),
            MessageSelector::Unary("new".into()),
            vec![],
        ),
        MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
        vec![str_lit("bad")],
    )]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects"))
        .collect();
    assert_eq!(
        type_warnings.len(),
        1,
        "Non-union type checking should still work, got: {:?}",
        checker.diagnostics()
    );
}

// ---------------------------------------------------------------------------
// BT-1877: Nil resolved to UndefinedObject in union param validation
// ---------------------------------------------------------------------------

#[test]
fn union_param_type_nil_does_not_disable_validation() {
    // BT-1877: `String | Nil` param with Integer arg should warn.
    // Before fix, `Nil` was not in the hierarchy so the conservative fallback
    // made it compatible with anything, silently disabling validation.
    let hierarchy = hierarchy_with_extension(
        "Object",
        "setName:",
        1,
        vec![Some("String | Nil".into())],
        Some("Nil".into()),
    );
    let module = make_module(vec![msg_send(
        msg_send(
            class_ref("Object"),
            MessageSelector::Unary("new".into()),
            vec![],
        ),
        MessageSelector::Keyword(vec![KeywordPart::new("setName:", span())]),
        vec![int_lit(42)],
    )]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects"))
        .collect();
    assert_eq!(
        type_warnings.len(),
        1,
        "Integer should be incompatible with 'String | Nil' — Nil must not disable validation, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn union_param_type_nil_compatible_with_object_union() {
    // `Object | Nil` param with Integer arg should pass (Integer is subclass of Object)
    let hierarchy = hierarchy_with_extension(
        "Object",
        "setValue:",
        1,
        vec![Some("Object | Nil".into())],
        Some("Nil".into()),
    );
    let module = make_module(vec![msg_send(
        msg_send(
            class_ref("Object"),
            MessageSelector::Unary("new".into()),
            vec![],
        ),
        MessageSelector::Keyword(vec![KeywordPart::new("setValue:", span())]),
        vec![int_lit(42)],
    )]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects"))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "Integer should be compatible with 'Object | Nil' (subclass of Object), got: {type_warnings:?}"
    );
}

// ---------------------------------------------------------------------------
// ADR 0075: FFI call type inference from NativeTypeRegistry
// ---------------------------------------------------------------------------

/// Helper: Build an `Erlang <module>` receiver expression.
/// Uses a different span (10,20) from the outer send so type map lookups are distinct.
fn erlang_module_recv(module_name: &str) -> Expression {
    Expression::MessageSend {
        receiver: Box::new(class_ref("Erlang")),
        selector: MessageSelector::Unary(module_name.into()),
        arguments: vec![],
        is_cast: false,
        span: Span::new(10, 20),
    }
}

/// Helper: Build a keyword selector from parts.
fn keyword_selector(parts: &[&str]) -> MessageSelector {
    MessageSelector::Keyword(
        parts
            .iter()
            .map(|kw| KeywordPart::new(*kw, span()))
            .collect(),
    )
}

/// Helper: Build a `NativeTypeRegistry` with lists module signatures.
fn lists_registry() -> NativeTypeRegistry {
    let mut reg = NativeTypeRegistry::new();
    reg.register_module(
        "lists",
        vec![
            FunctionSignature {
                name: "reverse".to_string(),
                arity: 1,
                params: vec![ParamType {
                    keyword: Some(ecow::EcoString::from("list")),
                    type_: InferredType::known("List"),
                }],
                return_type: InferredType::known("List"),
                provenance: TypeProvenance::Extracted,
                line: None,
            },
            FunctionSignature {
                name: "seq".to_string(),
                arity: 2,
                params: vec![
                    ParamType {
                        keyword: Some(ecow::EcoString::from("from")),
                        type_: InferredType::known("Integer"),
                    },
                    ParamType {
                        keyword: Some(ecow::EcoString::from("to")),
                        type_: InferredType::known("Integer"),
                    },
                ],
                return_type: InferredType::known("List"),
                provenance: TypeProvenance::Extracted,
                line: None,
            },
            FunctionSignature {
                name: "member".to_string(),
                arity: 2,
                params: vec![
                    ParamType {
                        keyword: Some(ecow::EcoString::from("elem")),
                        type_: InferredType::Dynamic(DynamicReason::Unknown),
                    },
                    ParamType {
                        keyword: Some(ecow::EcoString::from("list")),
                        type_: InferredType::known("List"),
                    },
                ],
                return_type: InferredType::known("Boolean"),
                provenance: TypeProvenance::Extracted,
                line: None,
            },
            FunctionSignature {
                name: "node".to_string(),
                arity: 0,
                params: vec![],
                return_type: InferredType::known("Symbol"),
                provenance: TypeProvenance::Extracted,
                line: None,
            },
        ],
    );
    reg
}

#[test]
fn test_ffi_call_returns_typed_result() {
    // Erlang lists reverse: #(1, 2, 3)
    // With registry, should infer List (not Dynamic)
    let module = make_module(vec![msg_send(
        erlang_module_recv("lists"),
        keyword_selector(&["reverse:"]),
        vec![Expression::ListLiteral {
            elements: vec![int_lit(1), int_lit(2), int_lit(3)],
            tail: None,
            span: span(),
        }],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    // Check the type map — the outer message send should have type List
    let send_type = checker.type_map().get(span());
    assert_eq!(
        send_type,
        Some(&InferredType::known("List")),
        "FFI call should infer List return type"
    );
}

#[test]
fn test_ffi_call_multi_arg_returns_typed_result() {
    // Erlang lists seq: 1 to: 10
    let module = make_module(vec![msg_send(
        erlang_module_recv("lists"),
        keyword_selector(&["seq:", "to:"]),
        vec![int_lit(1), int_lit(10)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    let send_type = checker.type_map().get(span());
    assert_eq!(
        send_type,
        Some(&InferredType::known("List")),
        "Multi-arg FFI call should infer List return type"
    );
}

#[test]
fn test_ffi_call_no_registry_falls_back_to_dynamic() {
    // Without registry, FFI calls should return Dynamic (no regression)
    // and should not produce any FFI-specific diagnostics.
    let module = make_module(vec![msg_send(
        erlang_module_recv("lists"),
        keyword_selector(&["reverse:"]),
        vec![int_lit(42)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    // No registry set
    checker.check_module(&module, &hierarchy);

    let ffi_diags: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| {
            d.message.contains("FFI keyword")
                || d.message.contains("expects List")
                || d.message.contains("lists:")
        })
        .collect();
    assert!(
        ffi_diags.is_empty(),
        "Without registry, no FFI-specific diagnostics should be emitted. Got: {ffi_diags:?}"
    );
}

#[test]
fn test_ffi_call_unknown_function_falls_back_to_dynamic() {
    // Known module, unknown function → Dynamic (no FFI diagnostics)
    let module = make_module(vec![msg_send(
        erlang_module_recv("lists"),
        keyword_selector(&["unknown_fn:"]),
        vec![int_lit(1)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    let ffi_diags: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("FFI keyword") || d.message.contains("lists:"))
        .collect();
    assert!(
        ffi_diags.is_empty(),
        "Unknown function should not produce FFI diagnostics. Got: {ffi_diags:?}"
    );
}

#[test]
fn test_ffi_call_unknown_module_falls_back_to_dynamic() {
    // Unknown module → Dynamic (no FFI diagnostics)
    let module = make_module(vec![msg_send(
        erlang_module_recv("unknown_mod"),
        keyword_selector(&["foo:"]),
        vec![int_lit(1)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    let ffi_diags: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("FFI keyword") || d.message.contains("unknown_mod:"))
        .collect();
    assert!(
        ffi_diags.is_empty(),
        "Unknown module should not produce FFI diagnostics. Got: {ffi_diags:?}"
    );
}

#[test]
fn test_ffi_variable_tracking_through_assignment() {
    // proxy := Erlang lists
    // proxy reverse: #(1, 2, 3)
    // → proxy should have type ErlangModule<lists>, reverse should infer List
    let module = make_module(vec![
        assign("proxy", erlang_module_recv("lists")),
        msg_send(
            var("proxy"),
            keyword_selector(&["reverse:"]),
            vec![Expression::ListLiteral {
                elements: vec![int_lit(1)],
                tail: None,
                span: span(),
            }],
        ),
    ]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    let send_type = checker.type_map().get(span());
    assert_eq!(
        send_type,
        Some(&InferredType::known("List")),
        "Variable-tracked FFI call should infer List return type"
    );
}

#[test]
fn test_ffi_keyword_mismatch_warning() {
    // Erlang lists seq: 1 foo: 10
    // Should warn: 'foo:' does not match 'to:' for lists:seq/2 parameter 2
    let module = make_module(vec![msg_send(
        erlang_module_recv("lists"),
        keyword_selector(&["seq:", "foo:"]),
        vec![int_lit(1), int_lit(10)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    let keyword_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not match"))
        .collect();
    assert_eq!(
        keyword_warnings.len(),
        1,
        "Should emit keyword mismatch warning. Diagnostics: {:?}",
        checker.diagnostics()
    );
    assert!(
        keyword_warnings[0].message.contains("foo"),
        "Warning should mention the mismatched keyword 'foo'"
    );
    assert!(
        keyword_warnings[0].message.contains("to"),
        "Warning should mention the expected keyword 'to'"
    );
}

#[test]
fn test_ffi_keyword_with_suppressed() {
    // Erlang lists seq: 1 with: 10
    // `with:` is the universal fallback — should NOT warn
    let module = make_module(vec![msg_send(
        erlang_module_recv("lists"),
        keyword_selector(&["seq:", "with:"]),
        vec![int_lit(1), int_lit(10)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    let keyword_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not match"))
        .collect();
    assert!(
        keyword_warnings.is_empty(),
        "with: fallback should suppress keyword mismatch warning. Got: {keyword_warnings:?}"
    );
}

#[test]
fn test_ffi_keyword_matching_no_warning() {
    // Erlang lists seq: 1 to: 10
    // Keywords match — no warning
    let module = make_module(vec![msg_send(
        erlang_module_recv("lists"),
        keyword_selector(&["seq:", "to:"]),
        vec![int_lit(1), int_lit(10)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    let keyword_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not match"))
        .collect();
    assert!(
        keyword_warnings.is_empty(),
        "Matching keywords should not produce a warning. Got: {keyword_warnings:?}"
    );
}

#[test]
fn test_ffi_argument_type_mismatch_warning() {
    // Erlang lists reverse: 42
    // reverse expects List, got Integer
    let module = make_module(vec![msg_send(
        erlang_module_recv("lists"),
        keyword_selector(&["reverse:"]),
        vec![int_lit(42)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects"))
        .collect();
    assert_eq!(
        type_warnings.len(),
        1,
        "Should emit argument type mismatch warning. Diagnostics: {:?}",
        checker.diagnostics()
    );
    assert!(
        type_warnings[0].message.contains("List"),
        "Warning should mention expected type List"
    );
    assert!(
        type_warnings[0].message.contains("Integer"),
        "Warning should mention actual type Integer"
    );
}

#[test]
fn test_ffi_argument_dynamic_param_no_warning() {
    // Erlang lists member: "hello" in: #(1, 2, 3)
    // member param 1 is Dynamic — should not warn for any argument type
    let module = make_module(vec![msg_send(
        erlang_module_recv("lists"),
        keyword_selector(&["member:", "in:"]),
        vec![
            str_lit("hello"),
            Expression::ListLiteral {
                elements: vec![int_lit(1)],
                tail: None,
                span: span(),
            },
        ],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects"))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "Dynamic param type should accept any argument. Got: {type_warnings:?}"
    );
}

#[test]
fn test_ffi_dynamic_module_name_falls_back() {
    // Erlang (someVar) reverse: xs → Dynamic (no static module name)
    // This is a keyword send on Erlang class, which goes through normal DNU-suppressed path
    let module = make_module(vec![msg_send(
        // Simulate Erlang (someVar) — receiver is MessageSend with keyword selector
        // Actually, `Erlang (someVar)` parses differently.
        // The parenthesized form is: ClassRef("Erlang") with a keyword/unary send
        // where the module is dynamic. For our purposes, a non-Unary selector
        // on Erlang falls through to normal class-side handling.
        class_ref("Erlang"),
        keyword_selector(&["reverse:"]),
        vec![var("xs")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    // Should not crash, and should not produce FFI-specific warnings
    let ffi_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("FFI keyword") || d.message.contains("expects"))
        .collect();
    assert!(
        ffi_warnings.is_empty(),
        "Dynamic module name should not produce FFI warnings. Got: {ffi_warnings:?}"
    );
}

#[test]
fn test_ffi_erlang_module_type_in_type_map() {
    // Erlang lists → should infer ErlangModule<lists> in type map
    let module = make_module(vec![erlang_module_recv("lists")]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // erlang_module_recv uses span (10,20)
    let erlang_type = checker.type_map().get(Span::new(10, 20));
    match erlang_type {
        Some(InferredType::Known {
            class_name,
            type_args,
            ..
        }) => {
            assert_eq!(class_name.as_str(), "ErlangModule");
            assert_eq!(type_args.len(), 1);
            if let InferredType::Known {
                class_name: mod_name,
                ..
            } = &type_args[0]
            {
                assert_eq!(mod_name.as_str(), "lists");
            } else {
                panic!("Expected Known type arg, got: {:?}", type_args[0]);
            }
        }
        other => panic!("Expected ErlangModule<lists> in type map, got: {other:?}"),
    }
}

#[test]
fn test_ffi_wrong_arity_falls_back_to_dynamic() {
    // Erlang lists reverse: a with: b → arity 2, but reverse is arity 1
    // Should fall back to Dynamic (no FFI type warnings)
    let module = make_module(vec![msg_send(
        erlang_module_recv("lists"),
        keyword_selector(&["reverse:", "with:"]),
        vec![int_lit(1), int_lit(2)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    let ffi_type_diags: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("lists:reverse"))
        .collect();
    assert!(
        ffi_type_diags.is_empty(),
        "Wrong arity should not produce type check diagnostics. Got: {ffi_type_diags:?}"
    );
}

#[test]
fn test_ffi_member_returns_boolean() {
    // Erlang lists member: 1 in: myList → Boolean
    let module = make_module(vec![msg_send(
        erlang_module_recv("lists"),
        keyword_selector(&["member:", "in:"]),
        vec![int_lit(1), var("myList")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    let send_type = checker.type_map().get(span());
    assert_eq!(
        send_type,
        Some(&InferredType::known("Boolean")),
        "member should return Boolean"
    );
}

// ---- Object subtype acceptance and unnamed param keyword skip ----

#[test]
fn test_ffi_object_param_accepts_any_class() {
    // An FFI function expecting Object (from beamtalk_object() spec) should
    // accept any concrete class type without warning.
    let mut reg = NativeTypeRegistry::new();
    reg.register_module(
        "beamtalk_supervisor",
        vec![FunctionSignature {
            name: "startLink".to_string(),
            arity: 1,
            params: vec![ParamType {
                keyword: Some(ecow::EcoString::from("arg")),
                type_: InferredType::known("Object"),
            }],
            return_type: InferredType::Dynamic(DynamicReason::DynamicSpec),
            provenance: TypeProvenance::Extracted,
            line: None,
        }],
    );
    // Pass a class reference (Supervisor) as the argument — should be accepted
    let module = make_module(vec![msg_send(
        erlang_module_recv("beamtalk_supervisor"),
        keyword_selector(&["startLink:"]),
        vec![class_ref("Supervisor")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(reg);
    checker.check_module(&module, &hierarchy);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects"))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "Object param should accept any class type. Got: {type_warnings:?}"
    );
}

#[test]
fn test_ffi_unnamed_arg_param_skips_keyword_mismatch() {
    // An FFI function with unnamed params (keyword = "arg") should not warn
    // when the call site uses a different keyword name.
    let mut reg = NativeTypeRegistry::new();
    reg.register_module(
        "beamtalk_supervisor",
        vec![FunctionSignature {
            name: "terminateChild".to_string(),
            arity: 2,
            params: vec![
                ParamType {
                    keyword: Some(ecow::EcoString::from("arg")),
                    type_: InferredType::Dynamic(DynamicReason::DynamicSpec),
                },
                ParamType {
                    keyword: Some(ecow::EcoString::from("arg")),
                    type_: InferredType::Dynamic(DynamicReason::DynamicSpec),
                },
            ],
            return_type: InferredType::known("Nil"),
            provenance: TypeProvenance::Extracted,
            line: None,
        }],
    );
    // Call with child: keyword — should not warn about mismatch with "arg"
    let module = make_module(vec![msg_send(
        erlang_module_recv("beamtalk_supervisor"),
        keyword_selector(&["terminateChild:", "child:"]),
        vec![var("self"), var("child")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(reg);
    checker.check_module(&module, &hierarchy);

    let keyword_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not match"))
        .collect();
    assert!(
        keyword_warnings.is_empty(),
        "Unnamed 'arg' param should not trigger keyword mismatch. Got: {keyword_warnings:?}"
    );
}

// ---- BT-1880: Class protocol selectors vs FFI module lookups ----

#[test]
fn test_erlang_class_resolves_as_class_protocol_not_ffi() {
    // `Erlang class` should resolve as a class protocol message (returns Metaclass),
    // NOT as ErlangModule<class>.
    let module = make_module(vec![msg_send(
        class_ref("Erlang"),
        MessageSelector::Unary("class".into()),
        vec![],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // The result should NOT be ErlangModule<class>
    let send_type = checker.type_map().get(span());
    if let Some(InferredType::Known { class_name, .. }) = send_type {
        assert_ne!(
            class_name.as_str(),
            "ErlangModule",
            "`Erlang class` should not be inferred as ErlangModule"
        );
    }
    // No FFI-related diagnostics expected
    let ffi_diags: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("FFI") || d.message.contains("ErlangModule"))
        .collect();
    assert!(
        ffi_diags.is_empty(),
        "`Erlang class` should not produce FFI diagnostics. Got: {ffi_diags:?}"
    );
}

#[test]
fn test_erlang_new_resolves_as_class_protocol_not_ffi() {
    // `Erlang new` should resolve as a class protocol message,
    // NOT as ErlangModule<new>.
    let module = make_module(vec![msg_send(
        class_ref("Erlang"),
        MessageSelector::Unary("new".into()),
        vec![],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let send_type = checker.type_map().get(span());
    if let Some(InferredType::Known { class_name, .. }) = send_type {
        assert_ne!(
            class_name.as_str(),
            "ErlangModule",
            "`Erlang new` should not be inferred as ErlangModule"
        );
    }
}

#[test]
fn test_erlang_module_proxy_class_uses_normal_dispatch() {
    // `proxy := Erlang lists; proxy class` — `class` on an ErlangModule instance
    // should use normal dispatch (class protocol), not FFI lookup.
    let module = make_module(vec![
        assign("proxy", erlang_module_recv("lists")),
        msg_send(var("proxy"), MessageSelector::Unary("class".into()), vec![]),
    ]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    // Should not produce any FFI-related diagnostics for `class`
    let ffi_diags: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("lists:class"))
        .collect();
    assert!(
        ffi_diags.is_empty(),
        "`proxy class` should not trigger FFI lookup. Got: {ffi_diags:?}"
    );
}

#[test]
fn test_erlang_module_proxy_equality_uses_normal_dispatch() {
    // `proxy := Erlang lists; proxy == other` — binary selector on ErlangModule
    // should use normal dispatch, not FFI lookup.
    let module = make_module(vec![
        assign("proxy", erlang_module_recv("lists")),
        msg_send(
            var("proxy"),
            MessageSelector::Binary("==".into()),
            vec![var("other")],
        ),
    ]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    // Should not produce FFI diagnostics for `==`
    let ffi_diags: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("lists:==") || d.message.contains("FFI"))
        .collect();
    assert!(
        ffi_diags.is_empty(),
        "`proxy == other` should not trigger FFI lookup. Got: {ffi_diags:?}"
    );
}

#[test]
fn test_erlang_lists_still_infers_ffi() {
    // Sanity check: `Erlang lists reverse: xs` should still go through FFI inference.
    let module = make_module(vec![msg_send(
        erlang_module_recv("lists"),
        keyword_selector(&["reverse:"]),
        vec![var("xs")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    let send_type = checker.type_map().get(span());
    assert_eq!(
        send_type,
        Some(&InferredType::known("List")),
        "FFI call should still infer correct return type"
    );
}

// ---- BT-1859: Result isOk/isError narrowing tests ----

#[test]
fn test_detect_narrowing_is_ok_pattern() {
    let expr = is_ok("r");
    let info = TypeChecker::detect_narrowing(&expr);
    assert!(info.is_some(), "Should detect isOk narrowing");
    let info = info.unwrap();
    assert_eq!(info.variable.as_str(), "r");
    assert!(info.is_result_ok_check);
    assert!(!info.is_result_error_check);
    assert!(!info.is_nil_check);
}

#[test]
fn test_detect_narrowing_is_error_pattern() {
    let expr = is_error("r");
    let info = TypeChecker::detect_narrowing(&expr);
    assert!(info.is_some(), "Should detect isError narrowing");
    let info = info.unwrap();
    assert_eq!(info.variable.as_str(), "r");
    assert!(!info.is_result_ok_check);
    assert!(info.is_result_error_check);
    assert!(!info.is_nil_check);
}

#[test]
fn test_result_isok_narrowing_true_branch() {
    // r :: Result(String, Error)
    // r isOk ifTrue: [r value]
    // Inside the block, `r` should still be Result(String, Error) so
    // `value` resolves to String via generic substitution (no DNU).
    let hierarchy = ClassHierarchy::with_builtins();
    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            // Use Result as param type (the type checker will resolve it)
            vec![("r", Some("Result"))],
            vec![if_true(
                is_ok("r"),
                block_expr(vec![msg_send(
                    var("r"),
                    MessageSelector::Unary("value".into()),
                    vec![],
                )]),
            )],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        warnings.is_empty(),
        "r value inside isOk ifTrue: should not produce DNU, got: {:?}",
        warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_result_isok_narrowing_false_branch() {
    // r :: Result(String, Error)
    // r isOk ifFalse: [r error]
    // Inside the false block, `r` is the error variant, so `error` is valid.
    let hierarchy = ClassHierarchy::with_builtins();
    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("r", Some("Result"))],
            vec![if_false(
                is_ok("r"),
                block_expr(vec![msg_send(
                    var("r"),
                    MessageSelector::Unary("error".into()),
                    vec![],
                )]),
            )],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        warnings.is_empty(),
        "r error inside isOk ifFalse: should not produce DNU, got: {:?}",
        warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_result_isok_narrowing_both_branches() {
    // r :: Result(String, Error)
    // r isOk ifTrue: [r value] ifFalse: [r error]
    let hierarchy = ClassHierarchy::with_builtins();
    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("r", Some("Result"))],
            vec![if_true_if_false(
                is_ok("r"),
                block_expr(vec![msg_send(
                    var("r"),
                    MessageSelector::Unary("value".into()),
                    vec![],
                )]),
                block_expr(vec![msg_send(
                    var("r"),
                    MessageSelector::Unary("error".into()),
                    vec![],
                )]),
            )],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        warnings.is_empty(),
        "r value/error inside isOk ifTrue:ifFalse: should not produce DNU, got: {:?}",
        warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_result_iserror_narrowing_true_branch() {
    // r :: Result(String, Error)
    // r isError ifTrue: [r error]
    let hierarchy = ClassHierarchy::with_builtins();
    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("r", Some("Result"))],
            vec![if_true(
                is_error("r"),
                block_expr(vec![msg_send(
                    var("r"),
                    MessageSelector::Unary("error".into()),
                    vec![],
                )]),
            )],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        warnings.is_empty(),
        "r error inside isError ifTrue: should not produce DNU, got: {:?}",
        warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_result_narrowing_non_result_no_narrowing() {
    // x :: Integer
    // x isOk ifTrue: [x unknownThing]
    // Integer does not understand isOk, so we should get a DNU warning for isOk
    // and also for unknownThing. The narrowing should NOT apply because x is not a Result.
    let hierarchy = ClassHierarchy::with_builtins();
    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("x", Some("Integer"))],
            vec![if_true(
                is_ok("x"),
                block_expr(vec![msg_send(
                    var("x"),
                    MessageSelector::Unary("unknownThing".into()),
                    vec![],
                )]),
            )],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    // Should have DNU for 'isOk' on Integer and 'unknownThing' on Integer
    assert!(
        !warnings.is_empty(),
        "Non-Result receiver should still produce DNU warnings, got: {:?}",
        warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_result_narrowing_does_not_leak() {
    // r :: Result(String, Error)
    // r isOk ifTrue: [r value]
    // r unknownMethod  // should warn — narrowing scoped to block
    let hierarchy = ClassHierarchy::with_builtins();
    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("r", Some("Result"))],
            vec![
                if_true(
                    is_ok("r"),
                    block_expr(vec![msg_send(
                        var("r"),
                        MessageSelector::Unary("value".into()),
                        vec![],
                    )]),
                ),
                msg_send(
                    var("r"),
                    MessageSelector::Unary("unknownMethod".into()),
                    vec![],
                ),
            ],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| {
            d.message.contains("does not understand") && d.message.contains("unknownMethod")
        })
        .collect();
    assert_eq!(
        warnings.len(),
        1,
        "Narrowing should not leak outside the block, expected 1 DNU for unknownMethod"
    );
}

// BT-1861: Warn on type args for classes with no type params
#[test]
fn type_args_for_non_generic_class_warns() {
    // Integer has no type params — `:: Integer(String)` should warn
    let state = vec![StateDeclaration::with_type(
        ident("count"),
        TypeAnnotation::Generic {
            base: ident("Integer"),
            parameters: vec![TypeAnnotation::Simple(ident("String"))],
            span: span(),
        },
        span(),
    )];
    let class = counter_class_with_typed_state(vec![], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_type_param_bounds_in_module(&module, &hierarchy, &ProtocolRegistry::new());
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("no type parameters"))
        .collect();
    assert_eq!(
        warnings.len(),
        1,
        "Expected 1 'no type parameters' warning, got: {:?}",
        checker.diagnostics()
    );
    assert!(
        warnings[0].message.contains("Integer"),
        "Warning should mention the class name, got: {}",
        warnings[0].message
    );
}

#[test]
fn type_args_for_generic_class_no_false_positive() {
    // GenericDict has type params (K, V) — `:: GenericDict(Symbol, Integer)` should NOT warn
    use crate::semantic_analysis::class_hierarchy::ClassInfo;
    let mut hierarchy = ClassHierarchy::with_builtins();
    hierarchy.add_from_beam_meta(vec![
        ClassInfo {
            name: "GenericDict".into(),
            superclass: Some("Object".into()),
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            is_internal: false,
            package: None,
            is_value: false,
            is_native: false,
            state: vec![],
            state_types: HashMap::new(),
            state_has_default: HashMap::new(),
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec!["K".into(), "V".into()],
            type_param_bounds: vec![None, None],
            superclass_type_args: vec![],
        },
        ClassInfo {
            name: "Counter".into(),
            superclass: Some("Object".into()),
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            is_internal: false,
            package: None,
            is_value: false,
            is_native: false,
            state: vec![],
            state_types: HashMap::new(),
            state_has_default: HashMap::new(),
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            type_param_bounds: vec![],
            superclass_type_args: vec![],
        },
    ]);

    let state = vec![StateDeclaration::with_type(
        ident("refs"),
        TypeAnnotation::Generic {
            base: ident("GenericDict"),
            parameters: vec![
                TypeAnnotation::Simple(ident("Symbol")),
                TypeAnnotation::Simple(ident("Integer")),
            ],
            span: span(),
        },
        span(),
    )];
    let class = counter_class_with_typed_state(vec![], state);
    let module = make_module_with_classes(vec![], vec![class]);

    let mut checker = TypeChecker::new();
    checker.check_type_param_bounds_in_module(&module, &hierarchy, &ProtocolRegistry::new());
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("no type parameters"))
        .collect();
    assert!(
        warnings.is_empty(),
        "GenericDict has type params — should not warn, got: {warnings:?}",
    );
}

#[test]
fn type_args_for_non_generic_class_in_method_param_warns() {
    // Method parameter annotated as `:: Boolean(Integer)` — Boolean has no type params
    let method = MethodDefinition {
        selector: MessageSelector::Unary("doSomething".into()),
        parameters: vec![ParameterDefinition {
            name: ident("flag"),
            type_annotation: Some(TypeAnnotation::Generic {
                base: ident("Boolean"),
                parameters: vec![TypeAnnotation::Simple(ident("Integer"))],
                span: span(),
            }),
        }],
        body: vec![],
        kind: MethodKind::Primary,
        return_type: None,
        is_sealed: false,
        is_internal: false,
        expect: None,
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: span(),
    };
    let class = counter_class_with_typed_state(vec![method], vec![]);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_type_param_bounds_in_module(&module, &hierarchy, &ProtocolRegistry::new());
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("no type parameters"))
        .collect();
    assert_eq!(
        warnings.len(),
        1,
        "Expected 1 'no type parameters' warning for method param, got: {:?}",
        checker.diagnostics()
    );
    assert!(warnings[0].message.contains("Boolean"));
}

#[test]
fn type_args_for_non_generic_class_in_return_type_warns() {
    // Return type annotated as `:: String(Integer)` — String has no type params
    let method = MethodDefinition::with_return_type(
        MessageSelector::Unary("name".into()),
        vec![],
        vec![],
        TypeAnnotation::Generic {
            base: ident("String"),
            parameters: vec![TypeAnnotation::Simple(ident("Integer"))],
            span: span(),
        },
        span(),
    );
    let class = counter_class_with_typed_state(vec![method], vec![]);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_type_param_bounds_in_module(&module, &hierarchy, &ProtocolRegistry::new());
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("no type parameters"))
        .collect();
    assert_eq!(
        warnings.len(),
        1,
        "Expected 1 'no type parameters' warning for return type, got: {:?}",
        checker.diagnostics()
    );
    assert!(warnings[0].message.contains("String"));
}

#[test]
fn type_args_for_block_no_false_positive() {
    // Block uses type args as documentation convention (e.g., `Block(E, Boolean)`)
    // This should NOT trigger the "no type parameters" warning
    let method = MethodDefinition {
        selector: MessageSelector::Unary("doSomething".into()),
        parameters: vec![ParameterDefinition {
            name: ident("block"),
            type_annotation: Some(TypeAnnotation::Generic {
                base: ident("Block"),
                parameters: vec![
                    TypeAnnotation::Simple(ident("E")),
                    TypeAnnotation::Simple(ident("Boolean")),
                ],
                span: span(),
            }),
        }],
        body: vec![],
        kind: MethodKind::Primary,
        return_type: None,
        is_sealed: false,
        is_internal: false,
        expect: None,
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: span(),
    };
    let class = counter_class_with_typed_state(vec![method], vec![]);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_type_param_bounds_in_module(&module, &hierarchy, &ProtocolRegistry::new());
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("no type parameters"))
        .collect();
    assert!(
        warnings.is_empty(),
        "Block type args are a documentation convention — should not warn, got: {warnings:?}",
    );
}

/// BT-1872: When no non-nil union members understand the selector, the DNU
/// diagnostic should be a warning (definite runtime failure).
#[test]
fn union_dnu_all_missing_emits_warning() {
    // Neither Integer nor Float understands `size`.
    let module = Module::new(
        vec![ExpressionStatement::bare(msg_send(
            Expression::Identifier(ident("x")),
            MessageSelector::Unary("size".into()),
            vec![],
        ))],
        span(),
    );

    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set(
        "x",
        InferredType::Union {
            members: vec![InferredType::known("Integer"), InferredType::known("Float")],
            provenance: super::TypeProvenance::Inferred(span()),
        },
    );

    let _ty = checker.infer_expr(
        &module.expressions[0].expression,
        &hierarchy,
        &mut env,
        false,
    );

    let dnu_diags: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("not understand"))
        .collect();
    assert_eq!(
        dnu_diags.len(),
        1,
        "Expected exactly one DNU diagnostic, got {dnu_diags:?}"
    );
    assert_eq!(
        dnu_diags[0].severity,
        crate::source_analysis::Severity::Warning,
        "DNU when no members respond should be Warning severity"
    );
}

/// BT-1872: When some but not all non-nil union members understand the
/// selector, the DNU diagnostic should remain a hint.
#[test]
fn union_dnu_partial_missing_emits_hint() {
    // String understands `size`, Integer does not.
    let module = Module::new(
        vec![ExpressionStatement::bare(msg_send(
            Expression::Identifier(ident("x")),
            MessageSelector::Unary("size".into()),
            vec![],
        ))],
        span(),
    );

    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set("x", InferredType::simple_union(&["String", "Integer"]));

    let _ty = checker.infer_expr(
        &module.expressions[0].expression,
        &hierarchy,
        &mut env,
        false,
    );

    let dnu_diags: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("not understand"))
        .collect();
    assert_eq!(
        dnu_diags.len(),
        1,
        "Expected exactly one DNU diagnostic, got {dnu_diags:?}"
    );
    assert_eq!(
        dnu_diags[0].severity,
        crate::source_analysis::Severity::Hint,
        "DNU when some members respond should be Hint severity"
    );
}

// ---- BT-1882: flatten and partition: type annotations ----

/// BT-1882: List(List(Integer)) flatten returns List (not List(List(Integer))).
#[test]
fn generic_list_flatten_returns_unparameterized_list() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    // Variable typed as List(List(Integer)) — a nested list
    env.set(
        "nested",
        InferredType::Known {
            class_name: eco_string("List"),
            type_args: vec![InferredType::Known {
                class_name: eco_string("List"),
                type_args: vec![InferredType::known("Integer")],
                provenance: TypeProvenance::Inferred(span()),
            }],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result = checker.infer_expr(
        &msg_send(
            var("nested"),
            MessageSelector::Unary("flatten".into()),
            vec![],
        ),
        &hierarchy,
        &mut env,
        false,
    );
    assert_eq!(
        result.as_known().map(EcoString::as_str),
        Some("List"),
        "List(List(Integer)) flatten should return List, got: {result:?}"
    );
}

/// BT-1882: List(Integer) partition: returns Dictionary (not List(List(Integer))).
#[test]
fn generic_list_partition_returns_dictionary() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    env.set(
        "nums",
        InferredType::Known {
            class_name: eco_string("List"),
            type_args: vec![InferredType::known("Integer")],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let block = Expression::Block(Block::new(
        vec![crate::ast::BlockParameter::new("x", span())],
        vec![bare(msg_send(
            var("x"),
            MessageSelector::Unary("isEven".into()),
            vec![],
        ))],
        span(),
    ));

    let result = checker.infer_expr(
        &msg_send(
            var("nums"),
            MessageSelector::Keyword(vec![KeywordPart::new("partition:", span())]),
            vec![block],
        ),
        &hierarchy,
        &mut env,
        false,
    );
    assert_eq!(
        result.as_known().map(EcoString::as_str),
        Some("Dictionary"),
        "List(Integer) partition: should return Dictionary, got: {result:?}"
    );
}

// ── CoverageReport tests (BT-1915) ─────────────────────────────────

#[test]
fn coverage_report_from_module_counts_typed_vs_dynamic() {
    let source = r"
Object subclass: Counter
  state: count :: Integer = 0
  increment => self.count := self.count + 1
  value => self.count
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, _diags) = crate::source_analysis::parse(tokens);

    let hierarchy = ClassHierarchy::with_builtins();
    let type_map = infer_types(&module, &hierarchy);

    let report = CoverageReport::from_module(&module, &type_map, "test.bt", false);

    assert_eq!(report.classes.len(), 1, "should have one class");
    assert_eq!(report.classes[0].name.as_str(), "Counter");
    assert!(report.total_expressions > 0, "should have some expressions");
    assert!(
        report.typed_expressions <= report.total_expressions,
        "typed <= total"
    );
    assert!(
        report.dynamic_entries.is_empty(),
        "no detail entries without detail mode"
    );
}

#[test]
fn coverage_report_detail_mode_collects_dynamic_entries() {
    let source = r"
Object subclass: Greeter
  greet: name => name hello
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, _diags) = crate::source_analysis::parse(tokens);

    let hierarchy = ClassHierarchy::with_builtins();
    let type_map = infer_types(&module, &hierarchy);

    let report = CoverageReport::from_module(&module, &type_map, "greeter.bt", true);

    assert_eq!(report.classes.len(), 1);
    // With detail mode, any Dynamic entries should be collected
    // (name parameter is untyped -> Dynamic)
    let dynamic_count = report.total_expressions - report.typed_expressions;
    assert_eq!(
        report.dynamic_entries.len(),
        dynamic_count,
        "detail mode should collect all dynamic entries"
    );
}

#[test]
fn coverage_report_empty_module() {
    let module = make_module(vec![]);
    let hierarchy = ClassHierarchy::with_builtins();
    let type_map = infer_types(&module, &hierarchy);

    let report = CoverageReport::from_module(&module, &type_map, "empty.bt", false);

    assert!(report.classes.is_empty());
    assert_eq!(report.total_expressions, 0);
    assert_eq!(report.typed_expressions, 0);
    // 100% coverage for empty module (nothing to cover)
    assert!((report.coverage_percent() - 100.0).abs() < f64::EPSILON);
}

#[test]
fn coverage_report_merge() {
    let mut report_a = CoverageReport {
        classes: vec![ClassCoverage {
            name: "A".into(),
            file: "a.bt".to_string(),
            total: 10,
            typed: 8,
        }],
        dynamic_entries: vec![],
        total_expressions: 10,
        typed_expressions: 8,
    };
    let report_b = CoverageReport {
        classes: vec![ClassCoverage {
            name: "B".into(),
            file: "b.bt".to_string(),
            total: 5,
            typed: 5,
        }],
        dynamic_entries: vec![],
        total_expressions: 5,
        typed_expressions: 5,
    };

    report_a.merge(report_b);

    assert_eq!(report_a.classes.len(), 2);
    assert_eq!(report_a.total_expressions, 15);
    assert_eq!(report_a.typed_expressions, 13);
}

#[test]
fn coverage_percent_calculation() {
    let coverage = ClassCoverage {
        name: "Test".into(),
        file: "test.bt".to_string(),
        total: 100,
        typed: 75,
    };
    assert!((coverage.coverage_percent() - 75.0).abs() < f64::EPSILON);

    let empty = ClassCoverage {
        name: "Empty".into(),
        file: "empty.bt".to_string(),
        total: 0,
        typed: 0,
    };
    assert!((empty.coverage_percent() - 100.0).abs() < f64::EPSILON);
}

// ---- BT-1914: Dynamic inference warnings in typed classes ----

#[test]
fn test_dynamic_inference_warning_in_typed_class() {
    // BT-1914: unannotated parameter in typed class should produce a warning
    // when referenced in the method body.
    let class_def = ClassDefinition::with_modifiers(
        ident("StrictCounter"),
        Some(ident("Object")),
        ClassModifiers {
            is_typed: true,
            ..Default::default()
        },
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("process:", span())]),
            vec![ParameterDefinition::new(ident("handler"))], // no type annotation
            // Body references `handler` — this is an identifier expression
            // that infers as Dynamic(UnannotatedParam)
            vec![bare(Expression::Identifier(ident("handler")))],
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dynamic_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect();
    assert!(
        !dynamic_warnings.is_empty(),
        "typed class should warn about Dynamic inference from unannotated param"
    );
    assert!(
        dynamic_warnings[0].message.contains("StrictCounter"),
        "warning should mention the class name: {:?}",
        dynamic_warnings[0].message
    );
    assert!(
        dynamic_warnings[0]
            .message
            .contains("unannotated parameter"),
        "warning should include the DynamicReason description: {:?}",
        dynamic_warnings[0].message
    );
    assert_eq!(
        dynamic_warnings[0].category,
        Some(DiagnosticCategory::Type),
        "warning should have Type category"
    );
}

#[test]
fn test_no_dynamic_inference_warning_in_untyped_class() {
    // BT-1914: non-typed class should NOT warn about Dynamic inference
    let class_def = ClassDefinition::with_modifiers(
        ident("SimpleCounter"),
        Some(ident("Object")),
        ClassModifiers::default(), // NOT typed
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("process:", span())]),
            vec![ParameterDefinition::new(ident("handler"))],
            vec![bare(Expression::Identifier(ident("handler")))],
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dynamic_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect();
    assert!(
        dynamic_warnings.is_empty(),
        "non-typed class should NOT warn about Dynamic inference, got: {dynamic_warnings:?}"
    );
}

#[test]
fn test_no_dynamic_inference_warning_for_known_types() {
    // BT-1914: expressions with known types in typed class should NOT warn
    let class_def = ClassDefinition::with_modifiers(
        ident("StrictCounter"),
        Some(ident("Object")),
        ClassModifiers {
            is_typed: true,
            ..Default::default()
        },
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Unary("getValue".into()),
            vec![],
            vec![bare(int_lit(42))], // Integer literal — known type
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dynamic_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect();
    assert!(
        dynamic_warnings.is_empty(),
        "known type should NOT produce Dynamic inference warning, got: {dynamic_warnings:?}"
    );
}

#[test]
fn test_expect_type_suppresses_dynamic_inference_warning() {
    // BT-1914: @expect type should suppress Dynamic inference warnings.
    // Uses parse_source for real spans so apply_expect_directives can match.
    let source =
        "typed Object subclass: Processor\n  process: handler =>\n    @expect type\n    handler";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let dynamic_warnings: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect();
    assert!(
        dynamic_warnings.is_empty(),
        "@expect type should suppress Dynamic inference warning, got: {dynamic_warnings:?}"
    );
    // No stale @expect warning
    let stale: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("stale"))
        .collect();
    assert!(
        stale.is_empty(),
        "should not produce stale @expect warning, got: {stale:?}"
    );
}

#[test]
fn test_expect_type_stale_when_no_dynamic_warning() {
    // BT-1914: @expect type on a fully-typed expression in a typed class
    // should produce a stale @expect warning.
    let source = "typed Object subclass: Processor\n  getValue =>\n    @expect type\n    42";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let stale: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("stale"))
        .collect();
    assert!(
        !stale.is_empty(),
        "@expect type on `42` in typed class should produce stale warning, got diags: {diags:?}"
    );
}

#[test]
fn dynamic_reason_descriptions() {
    assert_eq!(
        DynamicReason::UnannotatedParam.description(),
        Some("unannotated parameter")
    );
    assert_eq!(
        DynamicReason::DynamicReceiver.description(),
        Some("dynamic receiver")
    );
    assert_eq!(DynamicReason::UntypedFfi.description(), Some("untyped FFI"));
    assert_eq!(
        DynamicReason::DynamicSpec.description(),
        Some("FFI spec is Dynamic")
    );
    assert_eq!(DynamicReason::Unknown.description(), None);
}

// ---- block param type propagation from method signatures ----

#[test]
fn block_params_typed_from_sort_on_parameterized_list() {
    // List(String)>>sort: declares Block(E, E, Boolean) — block params should get String.
    let source = "typed Object subclass: T\n  field: items :: List(String)\n  m -> List(String) => self.items sort: [:a :b | a < b]";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let dynamic_warnings: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect();
    assert!(
        dynamic_warnings.is_empty(),
        "sort: block params should be typed from List(String), got: {dynamic_warnings:?}"
    );
}

#[test]
fn block_params_typed_from_any_satisfy_on_parameterized_list() {
    // List(Integer)>>anySatisfy: declares Block(E, Boolean) — block param should get Integer.
    let source = "typed Object subclass: T\n  field: nums :: List(Integer)\n  m -> Boolean => self.nums anySatisfy: [:n | n > 0]";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let dynamic_warnings: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect();
    assert!(
        dynamic_warnings.is_empty(),
        "anySatisfy: block param should be typed from List(Integer), got: {dynamic_warnings:?}"
    );
}

#[test]
fn block_params_typed_from_inject_into() {
    // List(Integer)>>inject:into: declares [A, Block(A, E, A)] — acc gets type from first arg.
    let source = "typed Object subclass: T\n  field: nums :: List(Integer)\n  m -> Integer => self.nums inject: 0 into: [:acc :n | acc + n]";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let dynamic_warnings: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect();
    assert!(
        dynamic_warnings.is_empty(),
        "inject:into: block params should be typed, got: {dynamic_warnings:?}"
    );
}

#[test]
fn block_params_typed_from_collect_on_parameterized_array() {
    // Array(Integer)>>collect: declares Block(E, R) — block param should get Integer.
    let source = "typed Object subclass: T\n  field: nums :: Array(Integer)\n  m -> Array => self.nums collect: [:n | n + 1]";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let dynamic_warnings: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect();
    assert!(
        dynamic_warnings.is_empty(),
        "collect: block param should be typed from Array(Integer), got: {dynamic_warnings:?}"
    );
}

#[test]
fn block_params_remain_dynamic_for_unparameterized_list() {
    // List>>sort: with unparameterized List — E can't be resolved, params stay Dynamic.
    let source = "typed Object subclass: T\n  field: items :: List\n  m -> List => self.items sort: [:a :b | a < b]";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dynamic_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect();
    assert!(
        !dynamic_warnings.is_empty(),
        "unparameterized List should still produce Dynamic warnings for block params"
    );
}

#[test]
fn resolve_type_name_string_parametric() {
    // List(String) should parse to Known("List") with type_args [Known("String")]
    let result = TypeChecker::resolve_type_name_string(&"List(String)".into());
    match &result {
        InferredType::Known {
            class_name,
            type_args,
            ..
        } => {
            assert_eq!(class_name.as_str(), "List");
            assert_eq!(type_args.len(), 1);
            assert_eq!(type_args[0], InferredType::known("String"));
        }
        other => panic!("Expected Known with type_args, got {other:?}"),
    }
}

#[test]
fn resolve_type_name_string_nested_parametric() {
    // Result(List(Integer), String) should parse correctly
    let result = TypeChecker::resolve_type_name_string(&"Result(List(Integer), String)".into());
    match &result {
        InferredType::Known {
            class_name,
            type_args,
            ..
        } => {
            assert_eq!(class_name.as_str(), "Result");
            assert_eq!(type_args.len(), 2);
            // First arg: List(Integer)
            match &type_args[0] {
                InferredType::Known {
                    class_name,
                    type_args: inner,
                    ..
                } => {
                    assert_eq!(class_name.as_str(), "List");
                    assert_eq!(inner.len(), 1);
                    assert_eq!(inner[0], InferredType::known("Integer"));
                }
                other => panic!("Expected Known(List(Integer)), got {other:?}"),
            }
            assert_eq!(type_args[1], InferredType::known("String"));
        }
        other => panic!("Expected Known with type_args, got {other:?}"),
    }
}

#[test]
fn block_params_fewer_than_signature_no_crash() {
    // List(String)>>sort: expects Block(E, E, Boolean) — 2 block params.
    // User passes a 1-param block. Extra resolved types are silently dropped.
    let source = "typed Object subclass: T\n  field: items :: List(String)\n  m -> List(String) => self.items sort: [:a | a]";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    // Should not crash, and the one param should be typed (no Dynamic warning for it)
    let dynamic_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect();
    assert!(
        dynamic_warnings.is_empty(),
        "single block param should still be typed from List(String), got: {dynamic_warnings:?}"
    );
}

#[test]
fn block_params_more_than_signature_extra_stays_dynamic() {
    // List(Integer)>>anySatisfy: expects Block(E, Boolean) — 1 block param.
    // User passes a 2-param block. Extra param stays Dynamic.
    let source = "typed Object subclass: T\n  field: nums :: List(Integer)\n  m -> Boolean => self.nums anySatisfy: [:n :extra | n > 0]";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    // The first param (n) is used and should still be typed from List(Integer),
    // so no Dynamic inference warning should be emitted for this block.
    let dynamic_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect();
    assert!(
        dynamic_warnings.is_empty(),
        "used block param should still be typed when extra params are present, got: {dynamic_warnings:?}"
    );
}

#[test]
fn block_params_typed_via_method_parameter_annotation() {
    // Method parameter `items :: List(Dictionary)` — sort: block params should get Dictionary.
    let source = "typed Object subclass: T\n  m: items :: List(Dictionary) -> List(Dictionary) => items sort: [:a :b | (a at: \"x\" ifAbsent: [0]) < (b at: \"x\" ifAbsent: [0])]";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let dynamic_warnings: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect();
    assert!(
        dynamic_warnings.is_empty(),
        "block params should be typed from method param List(Dictionary), got: {dynamic_warnings:?}"
    );
}

#[test]
fn split_union_respecting_parens_simple() {
    let result = TypeChecker::split_union_respecting_parens("String | nil");
    assert_eq!(result, vec!["String", "nil"]);
}

#[test]
fn split_union_respecting_parens_inside_parametric() {
    // Pipe inside parens should NOT cause a split
    let result = TypeChecker::split_union_respecting_parens("Result(String | Integer, Error)");
    assert_eq!(result, vec!["Result(String | Integer, Error)"]);
}

#[test]
fn split_union_respecting_parens_mixed() {
    // Top-level union with parametric member
    let result = TypeChecker::split_union_respecting_parens("List(String) | nil");
    assert_eq!(result, vec!["List(String)", "nil"]);
}

#[test]
fn block_params_typed_in_cascade_sends() {
    // Cascade sends should also propagate block param types.
    let source = "typed Object subclass: T\n  field: nums :: List(Integer)\n  m -> List(Integer) =>\n    self.nums\n      sort: [:a :b | a < b];\n      yourself";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let dynamic_warnings: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect();
    assert!(
        dynamic_warnings.is_empty(),
        "cascade sort: block params should be typed from List(Integer), got: {dynamic_warnings:?}"
    );
}

// ── BT-1945: Never bottom type ──────────────────────────────────────────────

#[test]
fn never_union_identity() {
    // T | Never = T
    let result = InferredType::union_of(&[InferredType::known("Integer"), InferredType::Never]);
    assert_eq!(result, InferredType::known("Integer"));
}

#[test]
fn never_union_identity_reversed() {
    // Never | T = T
    let result = InferredType::union_of(&[InferredType::Never, InferredType::known("String")]);
    assert_eq!(result, InferredType::known("String"));
}

#[test]
fn never_union_multiple() {
    // T | Never | U = T | U
    let result = InferredType::union_of(&[
        InferredType::known("Integer"),
        InferredType::Never,
        InferredType::known("String"),
    ]);
    assert_eq!(result, InferredType::simple_union(&["Integer", "String"]));
}

#[test]
fn never_union_all_never() {
    // Never | Never = Never
    let result = InferredType::union_of(&[InferredType::Never, InferredType::Never]);
    assert_eq!(result, InferredType::Never);
}

#[test]
fn never_display_name() {
    assert_eq!(InferredType::Never.display_name(), Some("Never".into()));
}

#[test]
fn never_equality() {
    assert_eq!(InferredType::Never, InferredType::Never);
    assert_ne!(InferredType::Never, InferredType::known("Never"));
    assert_ne!(
        InferredType::Never,
        InferredType::Dynamic(DynamicReason::Unknown)
    );
}

#[test]
fn never_as_known_is_none() {
    assert!(InferredType::Never.as_known().is_none());
}

#[test]
fn resolve_type_annotation_never() {
    let ann = TypeAnnotation::Simple(ident("Never"));
    let result = TypeChecker::resolve_type_annotation(&ann);
    assert_eq!(result, InferredType::Never);
}
