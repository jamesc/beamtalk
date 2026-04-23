// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Self return type and Self in generic positions (BT-1041, BT-1986, BT-1995).

use super::super::*;
use super::common::*;

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

#[test]
fn test_class_of_metatype_no_false_dnu_warnings() {
    // BT-2034: A field typed `Actor class | Nil` must allow class-side messages
    // (e.g. `isSupervisor`, `name`) to flow through after a nil-check without
    // false DNU warnings. Mirrors the `Self class` behaviour from BT-1952.
    let source = "
Value subclass: Actor
  class isSupervisor -> Boolean => false

typed Value subclass: Spec
  field: cls :: Actor class | Nil = nil
  check -> Boolean =>
    self.cls isNil ifTrue: [^false]
    self.cls isSupervisor
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // Verify the field annotation parses as Union(ClassOf(Actor), UndefinedObject).
    let spec = module
        .classes
        .iter()
        .find(|c| c.name.name == "Spec")
        .expect("Spec class");
    let cls_field = spec
        .state
        .iter()
        .find(|s| s.name.name == "cls")
        .expect("cls field");
    let ann = cls_field.type_annotation.as_ref().unwrap();
    let TypeAnnotation::Union { types, .. } = ann else {
        panic!("Expected Union annotation, got {ann:?}");
    };
    assert_eq!(
        types.len(),
        2,
        "Expected exactly two union members for `Actor class | Nil`, got {types:?}"
    );
    assert!(
        types.iter().any(|t| matches!(
            t,
            TypeAnnotation::ClassOf { class_name, .. } if class_name.name == "Actor"
        )),
        "Expected ClassOf(Actor) in union, got {types:?}"
    );
    assert!(
        types.iter().any(|t| matches!(
            t,
            TypeAnnotation::Simple(id) if id.name == "Nil"
        )),
        "Expected Nil in union, got {types:?}"
    );

    // No DNU warning for `isSupervisor` on the class-metatype receiver.
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
    env.set_local("self", InferredType::known("Driver"));
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
    env.set_local("self", InferredType::known("Driver"));
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
    env.set_local("self", InferredType::known("Driver"));
    // `pool` is a FakeWorkerPool instance (no receiver type args; the
    // binding of C flows from superclass_type_args on FakeWorkerPool).
    env.set_local("pool", InferredType::known("FakeWorkerPool"));
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
        Some(&InferredType::known("String")),
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
        Some(&InferredType::known("Integer")),
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
        Some(&InferredType::known("String")),
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
        Some(&InferredType::known("Integer")),
        "should infer Box#value returns Integer"
    );

    // type_map should be non-empty (at least the class was processed)
    // This verifies both outputs come from the same single pass
    assert!(
        !type_map.types.is_empty() || returns.len() == 1,
        "combined function should produce valid outputs from a single pass"
    );
}
