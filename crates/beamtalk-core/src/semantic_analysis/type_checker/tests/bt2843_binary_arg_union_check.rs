// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Binary-operator sends now get argument-type coverage even when the
//! argument's inferred type is a `Union` (or otherwise doesn't match the
//! `Known`/`Known` pattern `check_binary_operand_types` expects) (BT-2843).
//!
//! Root cause: `infer_message_send_with_receiver_ty` unconditionally skipped
//! `check_argument_types` for `MessageSelector::Binary` sends, on the
//! assumption `check_binary_operand_types` already covers argument
//! validation for binary selectors. But `check_binary_operand_types` only
//! fires when *both* the receiver and the argument destructure as
//! `InferredType::Known` — a `Union` argument (e.g. a `String | Nil` field)
//! fails that match, so neither check ran and the argument went completely
//! unchecked. `"literal" ++ nilableStringField` compiled with zero
//! diagnostics and crashed at runtime with an opaque `badarg`.
//!
//! Fix: `infer_message_send_with_receiver_ty` now tracks whether
//! `check_binary_operand_types` actually ran (the Known/Known case) and only
//! skips the generic `check_argument_types` fallback when it did — avoiding
//! duplicate diagnostics on the existing Known/Known path while giving
//! `Union` (and any other non-Known) arguments the same coverage a
//! keyword/unary send would get.
//!
//! Adversarial-review follow-up: `check_binary_operand_types` only has
//! bespoke logic for arithmetic (`+ - * /`), comparison (`< > <= >=`), and
//! `++` concat on String — *not* every binary selector a class defines
//! (e.g. `String>>,` or `Integer>>**`). The initial version of this fix set
//! `binary_operand_check_ran` from `hierarchy.resolves_selector` succeeding,
//! which is true for `,`/`**` too even though `check_binary_operand_types`
//! silently does nothing for them — re-entrenching the exact "resolved but
//! not actually checked" gap this issue exists to close, just for a
//! different set of operators. `check_binary_operand_types` now returns
//! whether it has bespoke logic for the `(operator, receiver)` shape, and
//! the caller only treats that as "checked" when it says so.

use super::common::*;

fn type_diagnostics(checker: &TypeChecker) -> Vec<&Diagnostic> {
    checker
        .diagnostics()
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect()
}

fn check_source(source: &str) -> Vec<Diagnostic> {
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    run_with_expect(&module, &hierarchy)
}

/// AC (b) — the exact repro from the issue: a `String | Nil` field flows
/// into `++`'s `String`-only parameter. The non-nil member (`String`) is
/// compatible but `Nil` isn't — this is the Nil-unsound case that used to
/// compile with zero diagnostics and crash at runtime.
#[test]
fn bt2843_binary_send_nil_union_member_incompatible_hints() {
    let source = r#"
typed Value subclass: Thing
  field: name :: String | Nil = nil

  greet -> String =>
    "Hello, " ++ self.name
"#;
    let diags = check_source(source);
    let type_diags: Vec<_> = diags
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .filter(|d| d.message.contains("++"))
        .collect();
    assert_eq!(
        type_diags.len(),
        1,
        "`++` with a String | Nil argument should now produce a Type diagnostic: {diags:#?}"
    );
    assert!(type_diags[0].message.contains("String"));
    assert!(
        type_diags[0].message.contains("Nil"),
        "diagnostic should mention the incompatible Nil member: {:?}",
        type_diags[0]
    );
}

/// AC (a): a `Union` argument where *no* member is compatible with the
/// declared parameter type still produces a Type diagnostic on a binary
/// send — mirrors the existing keyword-send coverage (BT-1832) but exercised
/// through the binary-selector inference path this issue fixes.
#[test]
fn bt2843_binary_send_union_all_members_incompatible_warns() {
    let source = r#"
typed Value subclass: Thing
  field: count :: Integer | Boolean = 0

  greet -> String =>
    "Hello, " ++ self.count
"#;
    let diags = check_source(source);
    let type_diags: Vec<_> = diags
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .filter(|d| d.message.contains("++"))
        .collect();
    assert_eq!(
        type_diags.len(),
        1,
        "`++` with an Integer | Boolean argument (neither compatible with String) \
         should produce a Type diagnostic: {diags:#?}"
    );
    assert_eq!(
        type_diags[0].severity,
        crate::source_analysis::Severity::Warning,
        "no union member compatible → Warning severity, not Hint"
    );
}

/// AC (c): a plain `Known`/`Known` binary send with compatible operand types
/// stays diagnostic-free — no regression on the existing
/// `check_binary_operand_types` fast path.
#[test]
fn bt2843_binary_send_known_compatible_no_diagnostic() {
    let source = r#"
typed Value subclass: Thing
  greet -> String =>
    "Hello, " ++ "World"
"#;
    let diags = check_source(source);
    assert!(
        diags
            .iter()
            .all(|d| d.category != Some(DiagnosticCategory::Type)),
        "compatible Known/Known `++` should stay diagnostic-free: {diags:#?}"
    );
}

/// AC (c): a plain `Known`/`Known` binary send with an *incompatible*
/// argument still produces exactly one diagnostic — `check_binary_operand_types`
/// covers it, and the new `check_argument_types` fallback must not fire a
/// duplicate now that `binary_operand_check_ran` is tracked.
#[test]
fn bt2843_binary_send_known_incompatible_single_diagnostic_no_duplicate() {
    let module = make_module(vec![msg_send(
        str_lit("hello"),
        MessageSelector::Binary("++".into()),
        vec![int_lit(42)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let type_diags = type_diagnostics(&checker);
    assert_eq!(
        type_diags.len(),
        1,
        "Known/Known incompatible `++` must still produce exactly one diagnostic \
         (from check_binary_operand_types), not a duplicate from the new fallback: {:?}",
        checker.diagnostics()
    );
    assert!(type_diags[0].message.contains("expects a String argument"));
}

/// Adversarial-review follow-up: a binary selector `check_binary_operand_types`
/// has no bespoke logic for (`String>>,`, declared `other :: String`) must
/// still get generic `check_argument_types` coverage for a Known/Known
/// mismatch — `hierarchy.resolves_selector` succeeding (the operator exists)
/// must not be conflated with "the argument was checked".
#[test]
fn bt2843_binary_send_uncovered_operator_string_comma_known_mismatch_warns() {
    let module = make_module(vec![msg_send(
        str_lit("hello"),
        MessageSelector::Binary(",".into()),
        vec![int_lit(42)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let type_diags = type_diagnostics(&checker);
    assert_eq!(
        type_diags.len(),
        1,
        "`,` on String with an Integer argument (declared `other :: String`) \
         should now produce a Type diagnostic: {:?}",
        checker.diagnostics()
    );
    assert!(type_diags[0].message.contains("String"));
    assert!(type_diags[0].message.contains("Integer"));
}

/// Same follow-up, `Integer>>**` (declared `other :: Number`) — a `String`
/// argument is a Known/Known mismatch `check_binary_operand_types` has no
/// logic for (it's not arithmetic `+-*/`), so it must fall through to
/// `check_argument_types`.
#[test]
fn bt2843_binary_send_uncovered_operator_integer_power_known_mismatch_warns() {
    let module = make_module(vec![msg_send(
        int_lit(2),
        MessageSelector::Binary("**".into()),
        vec![str_lit("x")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let type_diags = type_diagnostics(&checker);
    assert_eq!(
        type_diags.len(),
        1,
        "`**` on Integer with a String argument (declared `other :: Number`) \
         should now produce a Type diagnostic: {:?}",
        checker.diagnostics()
    );
    assert!(type_diags[0].message.contains("**"));
}

/// AC (d): keyword sends are unaffected by this change — the same `Union`
/// argument shape (`String | Nil` field into a `String`-only parameter),
/// but via a keyword selector, still gets the pre-existing BT-1832
/// `check_argument_types` coverage (this path never touched
/// `check_binary_operand_types` and isn't gated on `binary_operand_check_ran`
/// at all — the binary-selector fix only added a fallback for `Binary`
/// sends).
#[test]
fn bt2843_keyword_send_union_argument_unaffected() {
    let source = r"
typed Value subclass: Thing
  field: name :: String | Nil = nil

  take: aString :: String =>
    aString

  greet -> String =>
    self take: self.name
";
    let diags = check_source(source);
    let type_diags: Vec<_> = diags
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .filter(|d| d.message.contains("take:"))
        .collect();
    assert_eq!(
        type_diags.len(),
        1,
        "`take:` with a String | Nil argument should still produce the \
         pre-existing BT-1832 Type diagnostic: {diags:#?}"
    );
    assert_eq!(
        type_diags[0].severity,
        crate::source_analysis::Severity::Hint,
        "one compatible member (String), one incompatible (Nil) → Hint severity"
    );
}
