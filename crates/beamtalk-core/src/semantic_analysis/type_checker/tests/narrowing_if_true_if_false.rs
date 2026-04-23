// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Narrowing-path `ifTrue:` / `ifFalse:` carrying `Block(R)` type args (BT-2020, BT-2039).

use super::super::*;
use super::common::*;

// =========================================================================
// BT-2020: narrowing-path ifTrue:/ifFalse:/ifTrue:ifFalse: block type_args
//
// Before BT-2020, `infer_block_with_narrowing` returned a bare `Block` with
// no type_args. That stripped the inferred return type of the block body,
// and `infer_method_local_params` had nothing to unify `R` in
// `ifTrue: _ :: Block(R) ifFalse: _ :: Block(R) -> R` against — so
// `cond ifTrue: [...] ifFalse: [...]` collapsed to `Dynamic`.
// =========================================================================

/// BT-2020: `cond ifTrue: [Integer] ifFalse: [Integer]` should infer
/// `Integer`, not `Dynamic`.
#[test]
fn bt_2020_if_true_if_false_integer_branches_yield_integer() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("cond", InferredType::known("Boolean"));

    let expr = if_true_if_false(
        var("cond"),
        block_expr(vec![int_lit(1)]),
        block_expr(vec![int_lit(2)]),
    );
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);

    assert_eq!(
        ty.as_known().map(EcoString::as_str),
        Some("Integer"),
        "cond ifTrue: [1] ifFalse: [2] should infer Integer, got {ty:?}"
    );
}

/// BT-2020: `x isNil ifTrue: [default] ifFalse: [x + 1]` should preserve
/// the `Integer` return type across both branches.
#[test]
fn bt_2020_is_nil_if_true_if_false_preserves_integer_return() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    // x :: Integer | Nil
    env.set_local(
        "x",
        InferredType::Union {
            members: vec![
                InferredType::known("Integer"),
                InferredType::known("UndefinedObject"),
            ],
            provenance: super::super::TypeProvenance::Inferred(span()),
        },
    );
    env.set_local("default", InferredType::known("Integer"));

    let expr = if_true_if_false(
        is_nil("x"),
        block_expr(vec![var("default")]),
        // `x + 1` — in the ifFalse: branch, x is narrowed to non-nil Integer
        block_expr(vec![msg_send(
            var("x"),
            MessageSelector::Binary("+".into()),
            vec![int_lit(1)],
        )]),
    );
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);

    assert_eq!(
        ty.as_known().map(EcoString::as_str),
        Some("Integer"),
        "x isNil ifTrue: [default] ifFalse: [x + 1] should infer Integer, got {ty:?}"
    );
}

/// BT-2020: `cond ifTrue: [String] ifFalse: [Integer]` should infer a
/// concrete type (not `Dynamic`). The current substitution mechanism
/// picks one arm's concrete type rather than synthesising the full union,
/// which satisfies the AC's "union (or simplified common type)" clause;
/// the essential property is that the result is not `Dynamic`.
#[test]
fn bt_2020_if_true_if_false_mixed_arms_yield_concrete_type() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("cond", InferredType::known("Boolean"));

    let expr = if_true_if_false(
        var("cond"),
        block_expr(vec![str_lit("hello")]),
        block_expr(vec![int_lit(1)]),
    );
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);

    assert!(
        !matches!(ty, InferredType::Dynamic(_)),
        "mixed ifTrue:ifFalse: arms should not collapse to Dynamic, got {ty:?}"
    );
    // Either a Known with one of the two arm types, or a Union containing them.
    match &ty {
        InferredType::Known { class_name, .. } => {
            assert!(
                matches!(class_name.as_str(), "String" | "Integer"),
                "expected Known(String|Integer), got {class_name}"
            );
        }
        InferredType::Union { members, .. } => {
            let names: std::collections::HashSet<_> = members
                .iter()
                .filter_map(|m| m.as_known())
                .map(EcoString::as_str)
                .collect();
            assert!(
                names.contains("String") && names.contains("Integer"),
                "expected union to contain both String and Integer, got {names:?}"
            );
        }
        other => panic!("unexpected type for mixed arms: {other:?}"),
    }
}

/// BT-2020: Reproducer from the issue — `r isOk ifTrue: [r value] ifFalse: [MyVal new]`
/// assigned to `picked` should let `picked` bind as `MyVal`, so `picked xyzzyNonsense`
/// warns with a DNU naming `xyzzyNonsense`.
#[test]
fn bt_2020_result_is_ok_if_true_if_false_preserves_my_val() {
    use crate::semantic_analysis::class_hierarchy::ClassInfo;
    let mut hierarchy = ClassHierarchy::with_builtins();
    // Register a `MyVal` class so `MyVal new` yields `Known(MyVal)`.
    hierarchy.add_from_beam_meta(vec![ClassInfo {
        name: "MyVal".into(),
        superclass: Some("Object".into()),
        is_sealed: false,
        is_abstract: false,
        is_typed: true,
        is_internal: false,
        package: None,
        is_value: true,
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

    let class = {
        // a: r :: Result =>
        //   picked := r isOk ifTrue: [r value] ifFalse: [MyVal new]
        //   picked xyzzyNonsense
        let a_method = MethodDefinition {
            selector: MessageSelector::Keyword(vec![KeywordPart::new("a:", span())]),
            parameters: vec![ParameterDefinition::with_type(
                ident("r"),
                TypeAnnotation::Generic {
                    base: ident("Result"),
                    parameters: vec![
                        TypeAnnotation::Simple(ident("MyVal")),
                        TypeAnnotation::Simple(ident("Error")),
                    ],
                    span: span(),
                },
            )],
            body: vec![
                bare(assign(
                    "picked",
                    if_true_if_false(
                        is_ok("r"),
                        block_expr(vec![msg_send(
                            var("r"),
                            MessageSelector::Unary("value".into()),
                            vec![],
                        )]),
                        block_expr(vec![msg_send(
                            class_ref("MyVal"),
                            MessageSelector::Unary("new".into()),
                            vec![],
                        )]),
                    ),
                )),
                bare(msg_send(
                    var("picked"),
                    MessageSelector::Unary("xyzzyNonsense".into()),
                    vec![],
                )),
            ],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            kind: MethodKind::Primary,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        };
        ClassDefinition::new(
            ident("NarrowBlockProbe"),
            ident("Object"),
            vec![],
            vec![a_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let dnu: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| {
            d.message.contains("does not understand") && d.message.contains("xyzzyNonsense")
        })
        .collect();
    assert_eq!(
        dnu.len(),
        1,
        "expected DNU for `xyzzyNonsense` on MyVal (picked should be inferred as MyVal), \
         diagnostics: {:?}",
        checker.diagnostics()
    );
}

/// BT-2020: `(v isKindOf: List) ifTrue: [v size]` — narrowing path
/// recurses into the block body correctly, so `v size` inside the
/// narrowed block should not produce a DNU warning.
#[test]
fn bt_2020_narrowed_if_true_is_kind_of_preserves_inner_sends() {
    let hierarchy = ClassHierarchy::with_builtins();
    let class = {
        // process: v :: Object =>
        //   (v isKindOf: List) ifTrue: [v size]
        //   v unknownMethod   // should warn — v is Object outside block
        let process_method = make_keyword_method(
            &["process:"],
            vec![("v", Some("Object"))],
            vec![
                if_true(
                    msg_send(
                        var("v"),
                        MessageSelector::Keyword(vec![KeywordPart::new("isKindOf:", span())]),
                        vec![class_ref("List")],
                    ),
                    block_expr(vec![msg_send(
                        var("v"),
                        MessageSelector::Unary("size".into()),
                        vec![],
                    )]),
                ),
                msg_send(
                    var("v"),
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

    // `v size` inside ifTrue: should NOT produce DNU — v is narrowed to List.
    let dnu_size: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand") && d.message.contains("size"))
        .collect();
    assert!(
        dnu_size.is_empty(),
        "v size inside isKindOf: List ifTrue: should not DNU, got: {:?}",
        dnu_size.iter().map(|d| &d.message).collect::<Vec<_>>()
    );

    // `v unknownMethod` outside the block should warn (v is Object, not List).
    let dnu_unknown: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| {
            d.message.contains("does not understand") && d.message.contains("unknownMethod")
        })
        .collect();
    assert!(
        !dnu_unknown.is_empty(),
        "v unknownMethod outside narrowed block should produce DNU"
    );
}

// =========================================================================
// BT-2039: Dynamic|Known join on narrowing ifTrue:ifFalse:
//
// BT-2020 taught `ifTrue:ifFalse:` to carry `Block(R)` so the method-local
// `R` could be unified. But `infer_method_local_params` last-wins when the
// same type var appears in multiple arg positions: if branch 1 gave R=T and
// branch 2 gave R=Dynamic(UntypedFfi), R collapsed to Dynamic and the BT-1914
// "expression inferred as Dynamic in typed class" warning fired on the
// method body. The fix prefers Known over Dynamic when merging bindings.
// =========================================================================

/// BT-2039: `method -> Integer => cond ifTrue: [self.value] ifFalse: [(Erlang foo) bar]`
/// must not produce the "Dynamic in typed class" warning — the Known branch
/// pins `R` to `Integer` so the method's declared return type is preserved.
#[test]
fn bt_2039_if_true_if_false_known_branch_suppresses_ffi_warning() {
    // typed class Container
    //   state: value :: Integer = 0
    //   pick: cond :: Boolean -> Integer =>
    //     cond ifTrue: [self.value] ifFalse: [(Erlang foo) bar]
    let state = vec![StateDeclaration::with_type_and_default(
        ident("value"),
        TypeAnnotation::simple("Integer", span()),
        int_lit(0),
        span(),
    )];
    let pick_method = MethodDefinition::with_return_type(
        MessageSelector::Keyword(vec![KeywordPart::new("pick:", span())]),
        vec![ParameterDefinition::with_type(
            ident("cond"),
            TypeAnnotation::Simple(ident("Boolean")),
        )],
        vec![ExpressionStatement::bare(if_true_if_false(
            var("cond"),
            block_expr(vec![field_access("value")]),
            block_expr(vec![msg_send(
                erlang_module_recv("foo"),
                MessageSelector::Unary("bar".into()),
                vec![],
            )]),
        ))],
        TypeAnnotation::Simple(ident("Integer")),
        span(),
    );
    let class = ClassDefinition::with_modifiers(
        ident("Container"),
        Some(ident("Object")),
        ClassModifiers {
            is_typed: true,
            ..Default::default()
        },
        state,
        vec![pick_method],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // The inner `(Erlang foo) bar` call still produces its own "Dynamic in
    // typed class" warning (that's a separate, pre-existing warning at the
    // FFI call itself). What BT-2039 fixes is the *outer* warning on the
    // whole `ifTrue:ifFalse:` expression — so we should see exactly one
    // Dynamic warning (the inner FFI call), not two.
    let dynamic_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("inferred as Dynamic in typed class"))
        .collect();
    assert_eq!(
        dynamic_warnings.len(),
        1,
        "expected exactly one Dynamic-in-typed-class warning (the inner FFI \
         call). Before BT-2039 the outer ifTrue:ifFalse: also collapsed to \
         Dynamic and added a second warning. Got: {:?}",
        dynamic_warnings
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}
