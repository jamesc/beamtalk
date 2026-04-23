// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Basic type inference: literals, variables, DNU, cascade, matches, class methods.

use super::super::*;
use super::common::*;

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
fn test_cascade_complex_receiver_diagnostic_emitted_once() {
    // BT-2035 regression: a cascade whose inner receiver subtree is itself
    // a message send with a DNU must emit the DNU diagnostic exactly once.
    // Previously the Cascade arm called `infer_expr` twice on the inner
    // receiver — once via the outer MessageSend, once directly to extract
    // the dispatch type — doubling any diagnostics produced by the subtree.
    //
    // AST for `(42 bogus) negated; abs`:
    //   Cascade {
    //     receiver: MessageSend { receiver: MessageSend(42, bogus),
    //                             selector: negated, .. },
    //     messages: [abs],
    //   }
    let inner_bad = msg_send(int_lit(42), MessageSelector::Unary("bogus".into()), vec![]);
    let first_send = msg_send(inner_bad, MessageSelector::Unary("negated".into()), vec![]);
    let module = make_module(vec![Expression::Cascade {
        receiver: Box::new(first_send),
        messages: vec![CascadeMessage::new(
            MessageSelector::Unary("abs".into()),
            vec![],
            span(),
        )],
        span: span(),
    }]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let bogus_diagnostics: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("bogus"))
        .collect();
    assert_eq!(
        bogus_diagnostics.len(),
        1,
        "expected exactly one diagnostic for the DNU `bogus` on the inner cascade receiver, got {}: {:?}",
        bogus_diagnostics.len(),
        bogus_diagnostics
            .iter()
            .map(|d| d.message.as_str())
            .collect::<Vec<_>>()
    );
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
    parent.set_local("x", InferredType::known("Integer"));

    let mut child = parent.child();
    assert_eq!(child.get_local("x"), Some(InferredType::known("Integer")));

    child.set_local("y", InferredType::known("String"));
    assert!(
        parent.get_local("y").is_none(),
        "parent should not see child's y"
    );
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
    env.set_local("n", InferredType::known("Integer"));

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
    env.set_local("n", InferredType::known("Integer"));

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
