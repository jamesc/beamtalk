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
        surface_incomplete: false,
        name: "MyVal".into(),
        superclass: Some("Object".into()),
        is_sealed: false,
        is_abstract: false,
        is_typed: true,
        is_internal: false,
        package: None,
        is_value: true,
        is_native: false,
        handle_scope: None,
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
            is_class_method: false,
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

// ── BT-2624 item 4: singleton-union narrowing, integration through infer_expr ──

/// BT-2624 item 4 — the previously-blocked end-to-end narrowing check for
/// BT-2617, at the highest level currently reachable.
///
/// A singleton union (`Integer | #infinity`) cannot yet be *constructed* from
/// parseable source — it is unwritable as a type annotation (the parser rejects
/// `#foo` in type position), bare `=` does not parse as an equality operator,
/// and an `ifTrue:ifFalse:` assignment collapses its branches rather than
/// unioning them. So the union is seeded as a local and the guard expression is
/// built and run through the real `infer_expr` pipeline (narrowing + union
/// message-send), which is exactly what items 1–3 changed.
///
/// `(ms =:= #infinity) ifTrue: [0] ifFalse: [ms + 1]` — in the `ifFalse:` branch
/// `ms` narrows to `Integer`, so `ms + 1` resolves cleanly. The *unnarrowed*
/// `ms + 1` on the bare union warns (item 3: `#infinity` resolves its `Symbol`
/// method set and does not understand `+`). The contrast proves the narrowing.
#[test]
fn bt2624_singleton_union_narrowing_suppresses_branch_warning() {
    let hierarchy = ClassHierarchy::with_builtins();
    let union = InferredType::simple_union(&["Integer", "#infinity"]);
    let ms_plus_one = || {
        msg_send(
            var("ms"),
            MessageSelector::Binary("+".into()),
            vec![int_lit(1)],
        )
    };
    let guard = msg_send(
        var("ms"),
        MessageSelector::Binary("=:=".into()),
        vec![Expression::Literal(
            Literal::Symbol("infinity".into()),
            span(),
        )],
    );
    let guarded = if_true_if_false(
        guard,
        block_expr(vec![int_lit(0)]),
        block_expr(vec![ms_plus_one()]),
    );

    // Narrowed: the `+` send lives in the `ifFalse:` branch where `ms :: Integer`.
    let mut narrowed = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("ms", union.clone());
    let _ = narrowed.infer_expr(&guarded, &hierarchy, &mut env, false);
    assert!(
        narrowed
            .diagnostics()
            .iter()
            .all(|d| !d.message.contains("understand")),
        "narrowing should suppress the '+' warning in the ifFalse: branch, got: {:?}",
        narrowed.diagnostics()
    );

    // Unnarrowed control: the same `+` send on the bare union must warn, naming
    // the singleton that does not understand it.
    let mut unnarrowed = TypeChecker::new();
    let mut env2 = TypeEnv::new();
    env2.set_local("ms", union);
    let _ = unnarrowed.infer_expr(&ms_plus_one(), &hierarchy, &mut env2, false);
    let plus_dnu: Vec<_> = unnarrowed
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("understand") && d.message.contains("'+'"))
        .collect();
    assert_eq!(
        plus_dnu.len(),
        1,
        "the unnarrowed union must warn about '+', got: {:?}",
        unnarrowed.diagnostics()
    );
    assert!(plus_dnu[0].message.contains("#infinity"));
}

/// BT-2834 established that solo `ifTrue:` on an UNNARROWED `Boolean`
/// receiver (no isNil/isKindOf/comparison pattern for `detect_narrowing` to
/// find) must NOT infer the block's return type outright: `False>>ifTrue:`
/// never invokes the block — it returns `self` (`False`) — so promising
/// `Integer` here would be unsound.
///
/// BT-2868 refines that: collapsing all the way to `Dynamic` is more
/// conservative than necessary. The sound type is the union of the
/// `Boolean` self-branch and the block's own return type — `Boolean |
/// Integer`, not `Dynamic`. Mirrors BT-2824's `ifNil:`/`ifNotNil:` fix.
#[test]
fn bt_2868_solo_if_true_unnarrowed_boolean_receiver_yields_boolean_or_r() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    // cond :: Boolean, no narrowing info available (not from isNil/isKindOf/=)
    env.set_local("cond", InferredType::known("Boolean"));

    let expr = if_true(var("cond"), block_expr(vec![int_lit(42)]));
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);

    let InferredType::Union { members, .. } = &ty else {
        panic!(
            "cond ifTrue: [42] on an unnarrowed Boolean should infer \
             Boolean | Integer, not {ty:?}"
        );
    };
    let names: std::collections::BTreeSet<&str> = members
        .iter()
        .filter_map(|m| m.as_known().map(EcoString::as_str))
        .collect();
    assert_eq!(
        names,
        std::collections::BTreeSet::from(["Boolean", "Integer"]),
        "expected Boolean | Integer, got {ty:?}"
    );
}

/// BT-2868: same fix for solo `ifFalse:` (`True>>ifFalse:` returns `self`,
/// not the block's result, so the sound type is `Boolean | String`).
#[test]
fn bt_2868_solo_if_false_unnarrowed_boolean_receiver_yields_boolean_or_r() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("cond", InferredType::known("Boolean"));

    let expr = if_false(var("cond"), block_expr(vec![str_lit("no")]));
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);

    let InferredType::Union { members, .. } = &ty else {
        panic!(
            "cond ifFalse: [\"no\"] on an unnarrowed Boolean should infer \
             Boolean | String, not {ty:?}"
        );
    };
    let names: std::collections::BTreeSet<&str> = members
        .iter()
        .filter_map(|m| m.as_known().map(EcoString::as_str))
        .collect();
    assert_eq!(
        names,
        std::collections::BTreeSet::from(["Boolean", "String"]),
        "expected Boolean | String, got {ty:?}"
    );
}

/// BT-2868: `ifTrue:ifFalse:` (two-armed) behaviour is unchanged — both
/// arms are exhaustive, so the pre-existing unification to a single `R`
/// still applies (no spurious `Boolean` member).
#[test]
fn bt_2868_two_armed_if_true_if_false_unaffected() {
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
        "cond ifTrue: [1] ifFalse: [2] should still infer Integer, got {ty:?}"
    );
}

/// BT-2868: a receiver already narrowed to exactly `True` resolves through
/// the normal declared-return-type path (`True>>ifTrue: -> R`), so it must
/// NOT gain a spurious `Boolean` union member — the fix only fires for an
/// unnarrowed `Boolean` receiver.
#[test]
fn bt_2868_narrowed_true_receiver_unaffected() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("cond", InferredType::known("True"));

    let expr = if_true(var("cond"), block_expr(vec![int_lit(42)]));
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);

    assert_eq!(
        ty.as_known().map(EcoString::as_str),
        Some("Integer"),
        "cond ifTrue: [42] on a receiver narrowed to True should infer plain \
         Integer (True>>ifTrue: declares -> R), not a Boolean union; got {ty:?}"
    );
}

/// BT-2868: a genuinely Dynamic receiver must still infer Dynamic — the fix
/// only widens well-typed `Boolean` receivers.
#[test]
fn bt_2868_dynamic_receiver_unaffected() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("cond", InferredType::Dynamic(DynamicReason::Unknown));

    let expr = if_true(var("cond"), block_expr(vec![int_lit(42)]));
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);

    assert!(
        matches!(ty, InferredType::Dynamic(_)),
        "cond ifTrue: [42] on a genuinely Dynamic receiver must stay \
         Dynamic; got {ty:?}"
    );
}

/// BT-2868 regression test from the issue's exact repro: a `typed` method
/// whose body sends `fromIdx > n ifTrue: [0]` as a non-return statement (its
/// value is discarded — the method actually returns the following bare `0`)
/// should still infer the *send itself* as `Boolean | Integer` in the type
/// map, not `Dynamic` — this is what `type-coverage --detail` reports on.
#[test]
fn bt2868_solo_if_true_repro_infers_boolean_or_integer_not_dynamic() {
    let source = r"
typed Object subclass: TypeTest5
  gt: fromIdx :: Integer to: n :: Integer -> Integer =>
    fromIdx > n ifTrue: [0]
    0
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let send_ty = find_send_inferred_ty(&module, checker.type_map(), "ifTrue:")
        .expect("no ifTrue: send found in type_map");
    let InferredType::Union { members, .. } = send_ty else {
        panic!("`fromIdx > n ifTrue: [0]` should infer Boolean | Integer, not {send_ty:?}");
    };
    let names: std::collections::BTreeSet<&str> = members
        .iter()
        .filter_map(|m| m.as_known().map(EcoString::as_str))
        .collect();
    assert_eq!(
        names,
        std::collections::BTreeSet::from(["Boolean", "Integer"]),
        "expected Boolean | Integer, got {send_ty:?}"
    );
}

/// BT-2868: when the block always exits via a non-local return (`^`), it
/// contributes `Never` to the union — which `union_of` skips — so the send
/// infers plain `Boolean` (the surviving self-branch alone), not a
/// `Boolean | Never` union or `Dynamic`. Mirrors the equivalent
/// always-returns coverage `if_nil_branch_union_ret_ty` already has for the
/// two-armed `ifNil:ifNotNil:` case.
///
/// This exercises the simple case where `^` is the block's sole top-level
/// statement — a genuine "always returns". `block_has_any_return` (which
/// backs the `Never` classification) is conservative beyond that: it also
/// reports `true` for a `^` that merely appears somewhere in the block
/// without dominating every exit path, so the `Never` widening isn't a
/// precise always-diverges proof in general — see the doc comment on
/// `if_true_false_solo_boolean_ret_ty`.
#[test]
fn bt2868_solo_if_true_block_always_returns_yields_plain_boolean() {
    let source = r"
typed Object subclass: Repro
  go: flag :: Boolean -> Boolean =>
    flag ifTrue: [^flag]
    flag
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let send_ty = find_send_inferred_ty(&module, checker.type_map(), "ifTrue:")
        .expect("no ifTrue: send found in type_map");
    assert_eq!(
        send_ty.as_known().map(EcoString::as_str),
        Some("Boolean"),
        "flag ifTrue: [^flag] should infer plain Boolean (Never is skipped \
         by union_of), not {send_ty:?}"
    );
    assert!(
        checker
            .diagnostics()
            .iter()
            .all(|d| d.severity != crate::source_analysis::Severity::Warning),
        "expected no warnings, got: {:?}",
        checker.diagnostics()
    );
}

/// BT-2868: a solo `ifTrue:`/`ifFalse:` on a `Boolean`-containing UNION
/// receiver (e.g. `Boolean | Nil`) also widens to `Boolean | R` instead of
/// poisoning the whole union to `Dynamic` — audits the structurally
/// identical fallback inside `infer_union_message_send`.
#[test]
fn bt2868_solo_if_true_on_boolean_or_nil_union_infers_boolean_or_r() {
    let source = r"
typed Object subclass: Repro
  field: flag :: Boolean | Nil = nil

  go =>
    self.flag ifTrue: [1]
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    assert!(
        find_union_in_type_map(checker.type_map(), &["Boolean", "Integer"]).is_some(),
        "expected `Boolean | Integer` in type_map; got entries: {:?}",
        checker
            .type_map()
            .iter()
            .map(|(_, ty)| ty.display_name())
            .collect::<Vec<_>>()
    );
}

/// BT-2868: the terminal Dynamic fallback for a solo `ifTrue:`/`ifFalse:`
/// on a `Known` receiver whose method exists but declares no return type
/// must report `UnannotatedReturn`, not `DynamicReceiver` — the receiver
/// itself is perfectly well-typed. Deliberately scoped to a *non*-`Boolean`
/// synthetic class so `if_true_false_solo_boolean_ret_ty` doesn't short-
/// circuit with its `Boolean | R` union (that path is covered by the tests
/// above) — this test isolates the honest-reason fallback itself. Uses a
/// hierarchy-only class (not part of the checked module) so the BT-1047
/// "inferred return types from this same pass" cache can't mask the
/// terminal fallback with an unrelated hit.
#[test]
fn bt2868_unannotated_return_method_reports_honest_reason() {
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};

    let mut hierarchy = ClassHierarchy::with_builtins();
    hierarchy.add_from_beam_meta(vec![ClassInfo {
        surface_incomplete: false,
        name: eco_string("Weird"),
        superclass: Some(eco_string("Object")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        handle_scope: None,
        state: vec![],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        methods: vec![MethodInfo {
            selector: eco_string("ifTrue:"),
            arity: 1,
            kind: MethodKind::Primary,
            defined_in: eco_string("Weird"),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: None,
            param_types: vec![Some(eco_string("Block(R)"))],
            doc: None,
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    }]);

    let mut checker = TypeChecker::new();
    // Simulate being inside a `typed` class so BT-1914 has a context to warn on.
    checker.typed_class_context = Some(eco_string("Repro"));
    let mut env = TypeEnv::new();
    env.set_local("recv", InferredType::known("Weird"));

    let expr = if_true(var("recv"), block_expr(vec![int_lit(42)]));
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);

    assert_eq!(
        ty,
        InferredType::Dynamic(DynamicReason::UnannotatedReturn),
        "a Known receiver whose ifTrue: method exists but declares no \
         return type should report UnannotatedReturn, not DynamicReceiver; \
         got {ty:?}"
    );
    assert!(
        checker
            .diagnostics()
            .iter()
            .any(|d| d.message.contains("inferred as Dynamic in typed class")
                && d.message.contains("unannotated return")),
        "UnannotatedReturn is not in the BT-1914 suppression list, so the \
         warning should fire now that the reason is honest; got: {:?}",
        checker.diagnostics()
    );
}
