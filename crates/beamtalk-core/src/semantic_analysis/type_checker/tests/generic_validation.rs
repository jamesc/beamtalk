// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Generic return type validation, Never returns, and self/super threading in generics (BT-2022, BT-2033, BT-2021).

use super::super::*;
use super::common::*;

// ---- BT-2022: Generic return type validation checks inner type args ----

/// BT-2022 Bug A: Declared `-> Result(Integer, Error)` with body returning
/// `Result(String, Error)` must warn about the type arg mismatch.
#[test]
fn bt2022_generic_return_type_inner_arg_mismatch_warns() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_result_class(&mut hierarchy);

    let method = MethodDefinition {
        selector: MessageSelector::Unary("compute".into()),
        parameters: vec![],
        body: vec![bare(int_lit(42))],
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

    // Body infers as GenResult(String, Error) — inner arg mismatch
    let body_type = InferredType::Known {
        class_name: "GenResult".into(),
        type_args: vec![InferredType::known("String"), InferredType::known("Error")],
        provenance: TypeProvenance::Inferred(span()),
    };
    let mut checker = TypeChecker::new();
    checker.check_return_type(&method, &body_type, &eco_string("MyClass"), &hierarchy);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .collect();
    assert_eq!(
        type_warnings.len(),
        1,
        "Should warn when inner type arg (Integer vs String) mismatches: {type_warnings:?}"
    );
    assert!(
        type_warnings[0]
            .message
            .contains("GenResult(Integer, Error)"),
        "Warning should mention the declared type with args: {}",
        type_warnings[0].message
    );
    assert!(
        type_warnings[0]
            .message
            .contains("GenResult(String, Error)"),
        "Warning should mention the actual type with args: {}",
        type_warnings[0].message
    );
}

/// BT-2022: When both declared and body have matching inner type args,
/// no warning should be produced.
#[test]
fn bt2022_generic_return_type_matching_inner_args_no_warning() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_result_class(&mut hierarchy);

    let method = MethodDefinition {
        selector: MessageSelector::Unary("compute".into()),
        parameters: vec![],
        body: vec![bare(int_lit(42))],
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

    // Body infers as GenResult(Integer, Error) — exact match
    let body_type = InferredType::Known {
        class_name: "GenResult".into(),
        type_args: vec![InferredType::known("Integer"), InferredType::known("Error")],
        provenance: TypeProvenance::Inferred(span()),
    };
    let mut checker = TypeChecker::new();
    checker.check_return_type(&method, &body_type, &eco_string("MyClass"), &hierarchy);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "Matching inner type args should produce no warning: {type_warnings:?}"
    );
}

/// BT-2022: Body with bare `GenResult` (no `type_args`) is still compatible
/// with declared `GenResult(Integer, Error)` — only warn when both sides
/// have `type_args` and they mismatch.
#[test]
fn bt2022_generic_return_type_bare_body_no_warning() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_result_class(&mut hierarchy);

    let method = MethodDefinition {
        selector: MessageSelector::Unary("compute".into()),
        parameters: vec![],
        body: vec![bare(int_lit(42))],
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

    // Body infers as bare GenResult (no type_args) — no enough info to warn
    let body_type = InferredType::known("GenResult");
    let mut checker = TypeChecker::new();
    checker.check_return_type(&method, &body_type, &eco_string("MyClass"), &hierarchy);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "Bare GenResult body should be compatible with GenResult(Integer, Error): {type_warnings:?}"
    );
}

/// BT-2022: Dictionary(Symbol, Integer) declared, body returns
/// Dictionary(Symbol, String) — should warn about the value type mismatch.
#[test]
fn bt2022_dictionary_inner_arg_mismatch_warns() {
    let hierarchy = ClassHierarchy::with_builtins();

    let method = MethodDefinition {
        selector: MessageSelector::Unary("data".into()),
        parameters: vec![],
        body: vec![bare(int_lit(42))],
        return_type: Some(TypeAnnotation::Generic {
            base: ident("Dictionary"),
            parameters: vec![
                TypeAnnotation::Simple(ident("Symbol")),
                TypeAnnotation::Simple(ident("Integer")),
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

    // Body returns Dictionary(Symbol, String) — value type mismatch
    let body_type = InferredType::Known {
        class_name: "Dictionary".into(),
        type_args: vec![InferredType::known("Symbol"), InferredType::known("String")],
        provenance: TypeProvenance::Inferred(span()),
    };
    let mut checker = TypeChecker::new();
    checker.check_return_type(&method, &body_type, &eco_string("MyClass"), &hierarchy);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .collect();
    assert_eq!(
        type_warnings.len(),
        1,
        "Should warn about Dictionary value type mismatch (Integer vs String): {type_warnings:?}"
    );
}

/// BT-2022: Array(Integer) declared, body returns Array(String) — should warn.
#[test]
fn bt2022_list_inner_arg_mismatch_warns() {
    let hierarchy = ClassHierarchy::with_builtins();

    let method = MethodDefinition {
        selector: MessageSelector::Unary("items".into()),
        parameters: vec![],
        body: vec![bare(int_lit(42))],
        return_type: Some(TypeAnnotation::Generic {
            base: ident("Array"),
            parameters: vec![TypeAnnotation::Simple(ident("Integer"))],
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

    // Body returns Array(String) — type arg mismatch
    let body_type = InferredType::Known {
        class_name: "Array".into(),
        type_args: vec![InferredType::known("String")],
        provenance: TypeProvenance::Inferred(span()),
    };
    let mut checker = TypeChecker::new();
    checker.check_return_type(&method, &body_type, &eco_string("MyClass"), &hierarchy);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .collect();
    assert_eq!(
        type_warnings.len(),
        1,
        "Should warn about Array element type mismatch (Integer vs String): {type_warnings:?}"
    );
}

/// BT-2022 Bug B: `method_return_types` cache preserves `type_args`.
/// An unannotated method whose body infers as a generic type should have
/// the full `InferredType` (with `type_args`) stored in the cache.
#[test]
fn bt2022_method_return_type_cache_preserves_type_args() {
    // Parse source with an unannotated method that returns an array literal
    let src = "Object subclass: Holder\n  items => #(\"a\" \"b\" \"c\")";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();

    let result = infer_method_return_types(&module, &hierarchy);

    // The cache should contain the inferred return type for `items`
    let key = ("Holder".into(), "items".into(), false);
    let cached = result.get(&key);
    assert!(
        cached.is_some(),
        "Unannotated method `items` should have an entry in method_return_types"
    );
    let cached_ty = cached.unwrap();
    // BT-2022: The cache must store an InferredType (not the previous EcoString)
    // so that any type_args produced by the inference path can flow to callers.
    // Verifies the structural fix; element-type inference for array literals is
    // a separate concern outside this issue's scope.
    let InferredType::Known { class_name, .. } = cached_ty else {
        panic!("Cached type should be Known, got: {cached_ty:?}");
    };
    assert_eq!(class_name.as_str(), "List", "expected List base class");
}

/// BT-2022: When declared inner args include a generic type parameter (T, E),
/// the comparison should skip that arg rather than warn (type params are
/// symbolic placeholders, not concrete types).
#[test]
fn bt2022_generic_type_param_in_declared_args_not_warned() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_result_class(&mut hierarchy);

    let method = MethodDefinition {
        selector: MessageSelector::Unary("compute".into()),
        parameters: vec![],
        body: vec![bare(int_lit(42))],
        return_type: Some(TypeAnnotation::Generic {
            base: ident("GenResult"),
            parameters: vec![
                // T is a generic type param placeholder — should be skipped
                TypeAnnotation::Simple(ident("T")),
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

    // Body returns GenResult(String, Error) — T should be compatible with String
    let body_type = InferredType::Known {
        class_name: "GenResult".into(),
        type_args: vec![InferredType::known("String"), InferredType::known("Error")],
        provenance: TypeProvenance::Inferred(span()),
    };
    let mut checker = TypeChecker::new();
    checker.check_return_type(&method, &body_type, &eco_string("MyClass"), &hierarchy);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "Type param T in declared type should not trigger a mismatch warning: {type_warnings:?}"
    );
}

// ---- BT-2033: `-> Never` with non-divergent body must warn ----

/// BT-2033: A method declared `-> Never` with a `Known` body type (e.g. `42`)
/// is not divergent — warn so the mislabelled declaration is caught.
#[test]
fn bt2033_never_return_with_known_body_warns() {
    let hierarchy = ClassHierarchy::with_builtins();

    let method = MethodDefinition {
        selector: MessageSelector::Unary("diverge".into()),
        parameters: vec![],
        body: vec![bare(int_lit(42))],
        return_type: Some(TypeAnnotation::Simple(ident("Never"))),
        kind: MethodKind::Primary,
        is_sealed: false,
        is_internal: false,
        span: span(),
        doc_comment: None,
        expect: None,
        comments: CommentAttachment::default(),
    };

    // Body infers as Integer — clearly not divergent
    let body_type = InferredType::known("Integer");
    let mut checker = TypeChecker::new();
    checker.check_return_type(&method, &body_type, &eco_string("NeverProbe"), &hierarchy);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .collect();
    assert_eq!(
        type_warnings.len(),
        1,
        "Declared -> Never with Integer body should produce one warning: {type_warnings:?}"
    );
    assert!(
        type_warnings[0].message.contains("Never"),
        "Warning should mention Never: {}",
        type_warnings[0].message
    );
    assert!(
        type_warnings[0].message.contains("Integer"),
        "Warning should mention the actual Integer body type: {}",
        type_warnings[0].message
    );
}

/// BT-2033: A method declared `-> Never` whose body is truly divergent
/// (inferred as `InferredType::Never`) must not warn — the declaration is
/// honest.
#[test]
fn bt2033_never_return_with_never_body_no_warning() {
    let hierarchy = ClassHierarchy::with_builtins();

    let method = MethodDefinition {
        selector: MessageSelector::Unary("diverge".into()),
        parameters: vec![],
        body: vec![bare(int_lit(42))],
        return_type: Some(TypeAnnotation::Simple(ident("Never"))),
        kind: MethodKind::Primary,
        is_sealed: false,
        is_internal: false,
        span: span(),
        doc_comment: None,
        expect: None,
        comments: CommentAttachment::default(),
    };

    // Body is divergent (e.g. always `self error: ...`)
    let body_type = InferredType::Never;
    let mut checker = TypeChecker::new();
    checker.check_return_type(&method, &body_type, &eco_string("NeverProbe"), &hierarchy);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "Declared -> Never with Never body should not warn: {type_warnings:?}"
    );
}

// =====================================================================
// BT-2021 — Receiver type_args dropped for self / super in generic classes
// =====================================================================
//
// Sub-bug A (self): a method body inside a generic class `Box(T)` that
// `self`-sends a `-> T` helper used to see `self` as `Known("Box", [])`,
// dropping the symbolic type arg and resolving T to Dynamic. After
// BT-2025's `receiver_type_for_class` migration, `self` carries
// `[Known("T")]` placeholders so substitution can rewrite T correctly.
//
// Sub-bug B (super): from a child generic class `Sub(R)` extending
// `Base(E)`, `super`-sending a `-> E`-returning method has to thread the
// child's type-param placeholder (via `superclass_type_args`) into the
// parent's `E` slot — otherwise the parent and child end up using
// unrelated symbolic placeholders.
//
// Sub-bug C (Self class): intentionally still resolves to Dynamic. The
// previous attempt to make it `Known(class_name, type_args)` broke four
// narrowing tests (`x class = Integer` semantics) so full metatype
// support stays deferred. The regression test below pins that contract
// down so a future change can't silently flip it.

/// BT-2021 sub-bug A: inside a generic class `Box(T)`, calling a self-send
/// that returns `T` must yield the symbolic `T` placeholder rather than
/// `Dynamic`. We probe the cached return type of the wrapper method —
/// without the fix, the wrapper's return type was bare `Dynamic`.
#[test]
fn bt2021_self_send_in_generic_class_preserves_type_param() {
    let source = "
Object subclass: Box(T)
  state: x :: T = nil
  value -> T => x
  wrapped -> T => self value
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // The fix should produce no return-type-mismatch warnings — the wrapped
    // method's body type must be the same `T` placeholder as its declared
    // return type. Without the fix, `self value` resolves to Dynamic and
    // the typed-class-context would warn.
    let mismatches: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| {
            d.message.contains("does not match declared")
                || d.message.contains("does not understand")
        })
        .collect();
    assert!(
        mismatches.is_empty(),
        "self-send in generic class should preserve T; got: {mismatches:?}"
    );
}

/// BT-2021 sub-bug A direct check: probe the inferred type of `self value`
/// inside a method of `Box(T)`. Must be `Known("T", [])`, never `Dynamic`.
#[test]
fn bt2021_self_send_inferred_type_is_symbolic_param() {
    let source = "
Object subclass: Box(T)
  state: x :: T = nil
  value -> T => x
  wrapped -> T => self value
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let class = module
        .classes
        .iter()
        .find(|c| c.name.name.as_str() == "Box")
        .expect("Box class");
    let wrapped = class
        .methods
        .iter()
        .find(|m| m.selector.name() == "wrapped")
        .expect("wrapped method");
    let stmt = wrapped.body.last().expect("wrapped has at least one stmt");
    let expr = &stmt.expression;

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    // Mirror what `check_module` does: seed `self` with the receiver type
    // produced by the centralised helper.
    env.set_local(
        "self",
        super::super::type_resolver::receiver_type_for_class(&"Box".into(), &hierarchy),
    );
    let ty = checker.infer_expr(expr, &hierarchy, &mut env, false);

    match &ty {
        InferredType::Known { class_name, .. } => assert_eq!(
            class_name.as_str(),
            "T",
            "expected `self value` to resolve to symbolic T placeholder, got {ty:?}"
        ),
        other => panic!("expected Known(T), got {other:?}"),
    }
}

/// BT-2021 sub-bug A nested case: a multi-param generic class. Inside
/// `Pair(K, V)`, calling `self second` (returning `V`) must resolve to
/// the symbolic V placeholder, not Dynamic and not the wrong slot.
#[test]
fn bt2021_self_send_multi_param_generic_resolves_correct_slot() {
    let source = "
Object subclass: Pair(K, V)
  state: k :: K = nil
  state: v :: V = nil
  first -> K => k
  second -> V => v
  rewrap -> V => self second
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let class = module
        .classes
        .iter()
        .find(|c| c.name.name.as_str() == "Pair")
        .expect("Pair class");
    let rewrap = class
        .methods
        .iter()
        .find(|m| m.selector.name() == "rewrap")
        .expect("rewrap method");
    let stmt = rewrap.body.last().expect("rewrap has at least one stmt");
    let expr = &stmt.expression;

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local(
        "self",
        super::super::type_resolver::receiver_type_for_class(&"Pair".into(), &hierarchy),
    );
    let ty = checker.infer_expr(expr, &hierarchy, &mut env, false);

    match &ty {
        InferredType::Known { class_name, .. } => assert_eq!(
            class_name.as_str(),
            "V",
            "expected `self second` on Pair(K,V) to resolve to V, got {ty:?}"
        ),
        other => panic!("expected Known(V), got {other:?}"),
    }
}

/// BT-2021 sub-bug B: a child generic class calling `super`-send on a
/// parent method that returns the parent's type param must yield the
/// child's matching type-param placeholder (mapped via
/// `superclass_type_args`).
///
/// Source: `Base(E)` declares `peek -> E`; `Sub(R)` extends `Base(R)` —
/// Sub's R is forwarded to Base's E. Inside a method on Sub, `super peek`
/// should infer as `Known("R", [])`, not `Known("E", [])` and not Dynamic.
#[test]
fn bt2021_super_send_in_generic_class_threads_child_param() {
    let source = "
Object subclass: Base(E)
  state: x :: E = nil
  peek -> E => x

Base(R) subclass: Sub(R)
  borrow -> R => super peek
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let sub = module
        .classes
        .iter()
        .find(|c| c.name.name.as_str() == "Sub")
        .expect("Sub class");
    let borrow = sub
        .methods
        .iter()
        .find(|m| m.selector.name() == "borrow")
        .expect("borrow method");
    let stmt = borrow.body.last().expect("borrow has at least one stmt");
    let expr = &stmt.expression;

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local(
        "self",
        super::super::type_resolver::receiver_type_for_class(&"Sub".into(), &hierarchy),
    );
    let ty = checker.infer_expr(expr, &hierarchy, &mut env, false);

    match &ty {
        InferredType::Known { class_name, .. } => assert_eq!(
            class_name.as_str(),
            "R",
            "expected `super peek` from Sub(R) extends Base(R) to resolve to R, got {ty:?}"
        ),
        other => panic!("expected Known(R), got {other:?}"),
    }
}

/// BT-2021 sub-bug B with a *concrete* mapping: child binds parent's type
/// param to a fixed type. `IntBase` extends `Base(Integer)`. Inside an
/// `IntBase` method, `super peek` should resolve to `Integer`, not the
/// parent's symbolic `E` placeholder.
#[test]
fn bt2021_super_send_with_concrete_superclass_arg_resolves_to_concrete() {
    let source = "
Object subclass: Base(E)
  state: x :: E = nil
  peek -> E => x

Base(Integer) subclass: IntBase
  borrow -> Integer => super peek
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let int_base = module
        .classes
        .iter()
        .find(|c| c.name.name.as_str() == "IntBase")
        .expect("IntBase class");
    let borrow = int_base
        .methods
        .iter()
        .find(|m| m.selector.name() == "borrow")
        .expect("borrow method");
    let stmt = borrow.body.last().expect("borrow has at least one stmt");
    let expr = &stmt.expression;

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local(
        "self",
        super::super::type_resolver::receiver_type_for_class(&"IntBase".into(), &hierarchy),
    );
    let ty = checker.infer_expr(expr, &hierarchy, &mut env, false);

    match &ty {
        InferredType::Known { class_name, .. } => assert_eq!(
            class_name.as_str(),
            "Integer",
            "expected `super peek` from IntBase extends Base(Integer) to resolve to Integer, got {ty:?}"
        ),
        other => panic!("expected Known(Integer), got {other:?}"),
    }
}

/// BT-2021 sub-bug C contract pin: `Self class` (as returned by `Object>>class`)
/// stays `Dynamic`. Promoting it to `Known(class_name, type_args)` would
/// regress narrowing tests like `x class = Integer` (BT-1952 constraint).
/// Full metatype support is tracked separately.
#[test]
fn bt2021_self_class_remains_dynamic_for_factory_pattern() {
    // Probe `self class` directly (not `self class new`) so a future change
    // that resolves the metatype to a Known type but leaves `new` Dynamic
    // would still cause this test to fail. CodeRabbit on PR #2064.
    let source = "
Object subclass: Box(T)
  state: x :: T = nil
  metatype => self class
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let class = module
        .classes
        .iter()
        .find(|c| c.name.name.as_str() == "Box")
        .expect("Box class");
    let metatype = class
        .methods
        .iter()
        .find(|m| m.selector.name() == "metatype")
        .expect("metatype method");
    let stmt = metatype
        .body
        .last()
        .expect("metatype has at least one stmt");
    let expr = &stmt.expression;

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local(
        "self",
        super::super::type_resolver::receiver_type_for_class(&"Box".into(), &hierarchy),
    );
    let ty = checker.infer_expr(expr, &hierarchy, &mut env, false);

    // `self class` on `Box(T)` resolves through `class -> Self class`
    // (Object>>class) which intentionally returns Dynamic per BT-1952.
    assert!(
        matches!(&ty, InferredType::Dynamic(_)),
        "self class should stay Dynamic (Self class metatype is deferred); got {ty:?}"
    );
}
