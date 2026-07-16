// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ifNil:ifNotNil: branch-union return types (BT-2047).

use super::common::*;

// ── BT-2047: `ifNil:ifNotNil:` / `ifNotNil:ifNil:` return-type unification ──
//
// The whole `receiver ifNil: [a] ifNotNil: [:x | b]` expression should type as
// `typeof(a) | typeof(b)` (deduplicated) instead of `Dynamic`. BT-2046 already
// inferred both branches as `Block(..., R)` with narrowed params — this test
// family locks in that the outer send's return type is the branch union.

/// Minimal repro from the BT-2047 issue: `self.snapshot ifNil: [42] ifNotNil:
/// [:snap | "got one"]` should type as `Integer | String`, not Dynamic.
#[test]
fn bt2047_if_nil_if_not_nil_returns_branch_union() {
    let source = r#"
typed Object subclass: ReplaySnapshot
  workflowId -> String =>
    [^"wf-1"]

typed Object subclass: Repro
  field: snapshot :: ReplaySnapshot | Nil = nil

  go =>
    self.snapshot
      ifNil: [42]
      ifNotNil: [:snap | "got one"]
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    assert!(
        find_union_in_type_map(checker.type_map(), &["Integer", "String"]).is_some(),
        "expected `Integer | String` in type_map; got entries: {:?}",
        checker
            .type_map()
            .iter()
            .map(|(_, ty)| ty.display_name())
            .collect::<Vec<_>>()
    );
}

/// Reversed keyword order: `ifNotNil: [:x | a] ifNil: [b]` yields the same
/// union (acceptance criterion #2).
#[test]
fn bt2047_if_not_nil_if_nil_returns_branch_union() {
    let source = r#"
typed Object subclass: ReplaySnapshot
  workflowId -> String =>
    [^"wf-1"]

typed Object subclass: Repro
  field: snapshot :: ReplaySnapshot | Nil = nil

  go =>
    self.snapshot
      ifNotNil: [:snap | "got one"]
      ifNil: [42]
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    assert!(
        find_union_in_type_map(checker.type_map(), &["Integer", "String"]).is_some(),
        "expected `Integer | String` in type_map; got entries: {:?}",
        checker
            .type_map()
            .iter()
            .map(|(_, ty)| ty.display_name())
            .collect::<Vec<_>>()
    );
}

/// When both branches produce the same type, `union_of` dedups to that type
/// (no spurious `String | String`).
#[test]
fn bt2047_same_branch_types_deduplicate() {
    let source = r#"
typed Object subclass: ReplaySnapshot

typed Object subclass: Repro
  field: snapshot :: ReplaySnapshot | Nil = nil

  go -> String =>
    ^self.snapshot
      ifNil: ["none"]
      ifNotNil: [:snap | "some"]
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // Assert the inferred type of the `ifNil:ifNotNil:` send directly —
    // "no Type diagnostics" alone would pass even if the expression fell
    // back to `Dynamic` (which skips return-type mismatch checks).
    let send_ty = find_send_inferred_ty(&module, checker.type_map(), "ifNil:ifNotNil:")
        .expect("no ifNil:ifNotNil: send found in type_map");
    assert!(
        matches!(send_ty, InferredType::Known { class_name, .. } if class_name.as_str() == "String"),
        "both branches typed String — expression should dedup to Known(\"String\"), got: {send_ty:?}",
    );
}

/// Early-returning branch: `ifNil: [^42] ifNotNil: [:x | "ok"]` should type
/// as `String` alone (the non-diverging branch), NOT `Integer | String`. The
/// `^` exits the method before the expression value is observed, so the
/// diverging branch contributes `Never` to the union (AC #3).
#[test]
fn bt2047_diverging_if_nil_branch_contributes_only_other_arm() {
    let source = r#"
typed Object subclass: ReplaySnapshot

typed Object subclass: Repro
  field: snapshot :: ReplaySnapshot | Nil = nil

  go -> String =>
    ^self.snapshot
      ifNil: [^"early"]
      ifNotNil: [:snap | "got one"]
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let send_ty = find_send_inferred_ty(&module, checker.type_map(), "ifNil:ifNotNil:")
        .expect("no ifNil:ifNotNil: send found in type_map");
    assert!(
        matches!(send_ty, InferredType::Known { class_name, .. } if class_name.as_str() == "String"),
        "diverging ifNil: branch should project to Never — expression should be Known(\"String\"), got: {send_ty:?}",
    );
}

/// Key test: the divergent branch has a DIFFERENT type than the surviving
/// branch. `ifNil: [^42]` (Integer) with `ifNotNil: [:x | "ok"]` (String) —
/// without the `Never` projection, the expression would type as
/// `Integer | String` and mismatch the method's declared `-> String`. With
/// it, only the non-diverging branch contributes.
#[test]
fn bt2047_diverging_branch_with_distinct_type_projects_to_never() {
    let source = r#"
typed Object subclass: ReplaySnapshot

typed Object subclass: Repro
  field: snapshot :: ReplaySnapshot | Nil = nil

  go -> String =>
    ^self.snapshot
      ifNil: [^42]
      ifNotNil: [:snap | "got one"]
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // The key assertion: the send's inferred type is Known("String"), not
    // `Integer | String`. Without the `Never` projection, the union would
    // include Integer and the return type mismatch would surface; with it,
    // the diverging branch drops out.
    let send_ty = find_send_inferred_ty(&module, checker.type_map(), "ifNil:ifNotNil:")
        .expect("no ifNil:ifNotNil: send found in type_map");
    assert!(
        matches!(send_ty, InferredType::Known { class_name, .. } if class_name.as_str() == "String"),
        "diverging `ifNil: [^42]` with distinct `ifNotNil:` type should project to \
         Known(\"String\"), not `Integer | String`; got: {send_ty:?}",
    );
}

// ── BT-2824: solo `ifNil:` / `ifNotNil:` branch-union return types ──
//
// Solo `recv ifNil: [block]` / `recv ifNotNil: [:v | block]` on a `T | Nil`
// receiver previously fell through to the generic union-send dispatch, which
// has no way to read the block's actual return type from a bare `Block`
// stdlib signature — the whole expression collapsed to `Dynamic`. These
// tests lock in that solo sends now infer `T | R` / `R | Nil` the same way
// the two-arm combinators already do (BT-2047).

/// Regression test from the BT-2824 issue: a `typed` method with a declared
/// concrete return type (`-> String`) whose body is `x ifNil: ["default"]`
/// on a `String | Nil` field should type-check with the inferred type as
/// `String`, not `Dynamic`.
#[test]
fn bt2824_solo_if_nil_dedups_to_field_type() {
    let source = r#"
typed Object subclass: Repro
  field: name :: String | Nil = nil

  go -> String =>
    ^self.name ifNil: [""]
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let send_ty = find_send_inferred_ty(&module, checker.type_map(), "ifNil:")
        .expect("no ifNil: send found in type_map");
    assert!(
        matches!(send_ty, InferredType::Known { class_name, .. } if class_name.as_str() == "String"),
        "`x ifNil: [\"\"]` on a `String | Nil` field should dedup to Known(\"String\"), \
         not Dynamic; got: {send_ty:?}",
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

/// Distinct branch types: `Integer | Nil` field's `ifNil: ["none"]` should
/// infer as `Integer | String` (T | R), not `Dynamic`.
#[test]
fn bt2824_solo_if_nil_returns_branch_union() {
    let source = r#"
typed Object subclass: Repro
  field: count :: Integer | Nil = nil

  go =>
    self.count ifNil: ["none"]
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    assert!(
        find_union_in_type_map(checker.type_map(), &["Integer", "String"]).is_some(),
        "expected `Integer | String` in type_map; got entries: {:?}",
        checker
            .type_map()
            .iter()
            .map(|(_, ty)| ty.display_name())
            .collect::<Vec<_>>()
    );
}

/// Solo `ifNotNil:` on a `T | Nil` receiver infers `R | Nil` — the block's
/// return type (executed on the non-nil branch, with `v` narrowed to `T`)
/// unioned with `Nil` (the branch where `ifNotNil:` returns `self`).
#[test]
fn bt2824_solo_if_not_nil_returns_branch_union() {
    let source = r"
typed Object subclass: Repro
  field: name :: String | Nil = nil

  go =>
    self.name ifNotNil: [:n | n size]
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    assert!(
        find_union_in_type_map(checker.type_map(), &["Integer", "UndefinedObject"]).is_some(),
        "expected `Integer | Nil` in type_map; got entries: {:?}",
        checker
            .type_map()
            .iter()
            .map(|(_, ty)| ty.display_name())
            .collect::<Vec<_>>()
    );
}

/// A non-nullable receiver's solo `ifNotNil:` must NOT gain a spurious `Nil`
/// member — the nil branch is impossible, so the bypass should not fire and
/// the pre-existing (unrelated to this bug) dispatch path is used instead.
#[test]
fn bt2824_solo_if_not_nil_on_non_nullable_receiver_unaffected() {
    let source = r#"
typed Object subclass: Repro
  field: name :: String = ""

  go =>
    self.name ifNotNil: [:n | n size]
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    assert!(
        find_union_in_type_map(checker.type_map(), &["Integer", "UndefinedObject"]).is_none(),
        "a non-nullable `String` receiver must not gain a spurious `Nil` member \
         from solo `ifNotNil:`; got entries: {:?}",
        checker
            .type_map()
            .iter()
            .map(|(_, ty)| ty.display_name())
            .collect::<Vec<_>>()
    );
}

/// A non-nullable receiver's solo `ifNil:` must NOT gain a spurious extra
/// member either — dual of the `ifNotNil:` case above, exercised separately
/// because `ifNil:`'s self-branch goes through `non_nil_type` (a different
/// code path than `ifNotNil:`'s hardcoded `Nil` self-branch).
#[test]
fn bt2824_solo_if_nil_on_non_nullable_receiver_unaffected() {
    let source = r#"
typed Object subclass: Repro
  field: count :: Integer = 0

  go =>
    self.count ifNil: ["none"]
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    assert!(
        find_union_in_type_map(checker.type_map(), &["Integer", "String"]).is_none(),
        "a non-nullable `Integer` receiver must not gain a spurious `String` member \
         from solo `ifNil:`; got entries: {:?}",
        checker
            .type_map()
            .iter()
            .map(|(_, ty)| ty.display_name())
            .collect::<Vec<_>>()
    );
}

/// A receiver union with more than two members (`String | Integer | Nil`)
/// still infers correctly: `ifNil:` widens by the block's return type on top
/// of the full non-nil remainder, not just a single member.
#[test]
fn bt2824_solo_if_nil_on_three_member_union() {
    let source = r"
typed Object subclass: Repro
  field: value :: String | Integer | Nil = nil

  go =>
    self.value ifNil: [true]
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    assert!(
        find_union_in_type_map(checker.type_map(), &["String", "Integer", "Boolean"]).is_some(),
        "expected `String | Integer | Boolean` in type_map; got entries: {:?}",
        checker
            .type_map()
            .iter()
            .map(|(_, ty)| ty.display_name())
            .collect::<Vec<_>>()
    );
}

/// A non-local return (`^`) inside the solo `ifNil:` block exits the method
/// before the block's value is observed — the diverging branch contributes
/// `Never` (skipped by `union_of`), so the expression types as just the
/// non-nil branch, not `T | Integer`.
#[test]
fn bt2824_solo_if_nil_diverging_block_contributes_only_self_branch() {
    let source = r#"
typed Object subclass: Repro
  field: name :: String | Nil = nil

  go -> String =>
    ^self.name ifNil: [^"early"]
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let send_ty = find_send_inferred_ty(&module, checker.type_map(), "ifNil:")
        .expect("no ifNil: send found in type_map");
    assert!(
        matches!(send_ty, InferredType::Known { class_name, .. } if class_name.as_str() == "String"),
        "diverging `ifNil: [^\"early\"]` branch should project to Never — \
         expression should be Known(\"String\"), got: {send_ty:?}",
    );
}

/// Same divergence check for solo `ifNotNil:`: `^` inside the not-nil block
/// contributes `Never`, so the expression types as just the nil self-branch.
#[test]
fn bt2824_solo_if_not_nil_diverging_block_contributes_only_self_branch() {
    let source = r"
typed Object subclass: Repro
  field: name :: String | Nil = nil

  go -> Nil =>
    ^self.name ifNotNil: [:n | ^n size]
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let send_ty = find_send_inferred_ty(&module, checker.type_map(), "ifNotNil:")
        .expect("no ifNotNil: send found in type_map");
    assert!(
        matches!(send_ty, InferredType::Known { class_name, .. } if class_name.as_str() == "UndefinedObject"),
        "diverging `ifNotNil: [:n | ^...]` branch should project to Never — \
         expression should be Known(\"UndefinedObject\") (Nil), got: {send_ty:?}",
    );
}

/// A non-block argument (e.g. a `Dynamic`-typed variable holding a block)
/// isn't a well-formed `Block(...)` type, so `if_nil_solo_union_ret_ty` bails
/// out and the send falls back to the pre-existing generic dispatch — it
/// must not panic or otherwise mishandle the non-literal argument.
#[test]
fn bt2824_solo_if_nil_with_non_block_argument_does_not_panic() {
    let source = r"
typed Object subclass: Repro
  field: name :: String | Nil = nil

  go: aBlock =>
    self.name ifNil: aBlock
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    // No panic is the primary assertion; also sanity-check a type was recorded.
    assert!(
        find_send_inferred_ty(&module, checker.type_map(), "ifNil:").is_some(),
        "expected an inferred type to be recorded for the `ifNil:` send"
    );
}

/// A literal-`nil` receiver (`Known("UndefinedObject")`, not a `Union`) is
/// out of scope for `if_nil_solo_union_ret_ty` — the guard bails since the
/// receiver isn't a `Union`, and the send falls back to the pre-existing
/// generic dispatch path (`infer_args_with_block_context`). That path infers
/// `Known("String")` correctly here: since the receiver is `Known` (not a
/// `Union`), it resolves `UndefinedObject>>ifNil:`'s `Block(R) -> R` param
/// signature and substitutes `R` from the block's actual argument type — the
/// BT-2824 fix (which targets the `T | Nil` union case) doesn't touch or
/// regress this already-correct path.
#[test]
fn bt2824_literal_nil_receiver_if_nil_falls_back_to_generic_dispatch() {
    let source = r#"
typed Object subclass: Repro
  go =>
    nil ifNil: ["a"]
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let send_ty = find_send_inferred_ty(&module, checker.type_map(), "ifNil:")
        .expect("no ifNil: send found in type_map");
    assert!(
        matches!(send_ty, InferredType::Known { class_name, .. } if class_name.as_str() == "String"),
        "literal-nil receiver `ifNil:` falls back to the generic dispatch path, \
         which correctly substitutes the method-local `R` generic from the \
         block's type — expected Known(\"String\"), got: {send_ty:?}",
    );
}
