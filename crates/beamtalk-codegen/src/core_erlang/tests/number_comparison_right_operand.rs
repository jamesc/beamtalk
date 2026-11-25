// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0116 § Addendum (BT-5176), Phase 2b (BT-5210): the right operand of an
//! inlined numeric ordering comparison (`< <= > >=`) is tested with
//! `is_number` unless the compiler *proves* it numeric, and a non-number is
//! sent its reflected hook through
//! `beamtalk_message_dispatch:send_number_comparison/4`. The one "bare and
//! Boolean" predicate (`operators::comparison_is_bare`) decides the lowering,
//! the cell-bracket classification, the BT-4279 fast path and the
//! guard-return rewrite. The generated corpus is in
//! `number_comparison_right_operand_corpus.rs`.

use super::{assert_compiles_through_erlc, codegen, verifier_report};
use crate::core_erlang::fault::Fault;

/// The Core Erlang of the method `selector` of an `Object` / `Value` class:
/// from its function head to the next one.
fn method<'a>(code: &'a str, selector: &str) -> &'a str {
    let head = format!("\n'{selector}'/");
    let start = code
        .match_indices(&head)
        .map(|(i, _)| i + head.len())
        .find(|&i| {
            code[i..]
                .trim_start_matches(|c: char| c.is_ascii_digit())
                .starts_with(" =")
        })
        .unwrap_or_else(|| panic!("no method {selector}:\n{code}"));
    let rest = &code[start..];
    let end = rest.find("\n'").unwrap_or(rest.len());
    &rest[..end]
}

const RIGHT_TEST: &str = "call 'erlang':'is_number'(_BinRight";
const HOOK: &str = "'beamtalk_message_dispatch':'send_number_comparison'(";

#[test]
fn a_proven_numeric_right_operand_emits_no_test() {
    let code = codegen(
        "Object subclass: P\n\
         \x20 lit => 5 < 10\n\
         \x20 flt: x => 5 <= 2.5\n\
         \x20 chr: x => $a > $b\n\
         \x20 neg: x :: Integer => x >= 3\n\
         \x20 rev: x => x < 3\n",
    );
    for selector in ["lit", "flt:", "chr:", "neg:", "rev:"] {
        let body = method(&code, selector);
        assert!(
            !body.contains(RIGHT_TEST) && !body.contains(HOOK),
            "{selector} has a proven right operand and must not test it:\n{body}"
        );
    }
    // Both proven: the bare BIF, nothing else.
    assert!(
        method(&code, "lit").contains("call 'erlang':'<'(5, 10)"),
        "{code}"
    );
}

#[test]
fn self_in_a_numeric_class_is_a_proven_operand() {
    // `self` in `Integer` proves the receiver and, on the right, the operand.
    let code = codegen(
        "Object subclass: Integer\n\
         \x20 below: x => self < x\n\
         \x20 above => 3 < self\n",
    );
    let below = method(&code, "below:");
    assert!(
        below.contains(RIGHT_TEST) && below.contains("'lessThanFromNumber:'"),
        "an unproven right operand is tested:\n{below}"
    );
    assert!(
        !below.contains("is_number'(_BinLeft") && !below.contains("is_binary"),
        "a proven number receiver has no receiver test:\n{below}"
    );
    let above = method(&code, "above");
    assert!(
        !above.contains(RIGHT_TEST) && !above.contains(HOOK),
        "{above}"
    );
}

#[test]
fn class_side_self_is_not_a_proven_number() {
    // In a class method `self` is the class object, never a number: it is
    // neither a proven receiver nor a proven right operand.
    let code = codegen(
        "Object subclass: Integer\n\
         \x20 class below: x => self < x\n\
         \x20 class above => 3 < self\n",
    );
    // A class-side `self` receiver is a class self-send, never the BIF.
    let below = method(&code, "class_below:");
    assert!(
        !below.contains("call 'erlang':'<'"),
        "a class-side `self` receiver must not reach the bare BIF:\n{below}"
    );
    let above = method(&code, "class_above");
    assert!(
        above.contains(RIGHT_TEST) && above.contains(HOOK),
        "a class-side `self` right operand is tested:\n{above}"
    );
}

#[test]
fn a_declared_right_operand_is_tested_not_trusted() {
    let code = codegen(
        "Object subclass: P\n\
         \x20 lit: n :: Integer => 5 < n\n\
         \x20 typed: m :: Integer with: n :: Integer => m <= n\n\
         \x20 flt: f :: Float with: n :: Number => f > n\n",
    );
    for (selector, hook) in [
        ("lit:", "'lessThanFromNumber:', [_BinLeft"),
        ("typed:with:", "'lessOrEqualFromNumber:', [_BinLeft"),
        ("flt:with:", "'greaterThanFromNumber:', [_BinLeft"),
    ] {
        let body = method(&code, selector);
        assert!(
            body.contains(RIGHT_TEST) && body.contains(HOOK) && body.contains(hook),
            "{selector}'s declared right operand must be tested:\n{body}"
        );
    }
    assert_compiles_through_erlc("test", &code);
}

#[test]
fn the_right_operand_is_evaluated_once() {
    let code = codegen(
        "Object subclass: P\n\
         \x20 lit: x => 5 < (x probe)\n\
         \x20 any: y with: x => y >= (x probe)\n",
    );
    for selector in ["lit:", "any:with:"] {
        let body = method(&code, selector);
        assert_eq!(
            body.matches("'probe'").count(),
            1,
            "{selector}: the right operand must be let-bound once:\n{body}"
        );
        // Every use of the right operand after its binding is the variable.
        assert!(body.contains("let _BinRight@"), "{body}");
    }
}

#[test]
fn every_operator_sends_its_own_hook() {
    let code = codegen(
        "Object subclass: P\n\
         \x20 lt: x => 5 < x\n\
         \x20 le: x => 5 <= x\n\
         \x20 gt: x => 5 > x\n\
         \x20 ge: x => 5 >= x\n",
    );
    for (selector, hook, op) in [
        ("lt:", "lessThanFromNumber:", "<"),
        ("le:", "lessOrEqualFromNumber:", "<="),
        ("gt:", "greaterThanFromNumber:", ">"),
        ("ge:", "greaterOrEqualFromNumber:", ">="),
    ] {
        let body = method(&code, selector);
        let call = format!("{HOOK}_BinRight");
        let tail = format!("'{hook}', [_BinLeft");
        let op_atom = format!("], '{op}')");
        assert!(
            body.contains(&call) && body.contains(&tail) && body.contains(&op_atom),
            "{selector}:\n{body}"
        );
    }
    assert_compiles_through_erlc("test", &code);
}

#[test]
fn a_string_receiver_and_a_declared_string_are_unchanged() {
    let code = codegen(
        "Object subclass: P\n\
         \x20 lit: x => \"m\" < x\n\
         \x20 typed: s :: String with: x => s < x\n",
    );
    for selector in ["lit:", "typed:with:"] {
        let body = method(&code, selector);
        assert!(
            !body.contains(RIGHT_TEST) && !body.contains(HOOK),
            "{selector}:\n{body}"
        );
    }
}

/// The `let <R, State> = case <test> of ...` head of a BT-4279 fast-path
/// bracket, or `None`.
fn fast_path_head(out: &str) -> Option<&str> {
    let start = out.find("let <_Synced")?;
    Some(&out[start..out[start..].find("<_> when 'true' -> let _ = ")? + start])
}

#[test]
fn the_fast_path_test_includes_the_right_operand() {
    // Unknown receiver: the number arm's fast test is
    // `is_number(L) and is_number(R)`, the `String` case unchanged.
    let unknown =
        codegen("Actor subclass: Probe\n  state: n = 0\n\n  cmp: a with: b => self.n := a < b\n");
    let head = fast_path_head(&unknown).expect("a fast-path bracket");
    assert!(
        head.contains("call 'erlang':'or'(call 'erlang':'and'(call 'erlang':'is_number'(_BinLeft")
            && head.contains("call 'erlang':'is_number'(_BinRight")
            && head.contains("call 'erlang':'is_binary'(_BinLeft"),
        "{head}"
    );
    assert_compiles_through_erlc("test", &unknown);

    // Proven receiver with a tested right operand: it now dispatches, so it
    // is bracketed, and it offers the fast path `is_number(R)`.
    let proven = codegen("Actor subclass: Probe\n  state: n = 0\n\n  cmp: b => self.n := 0 < b\n");
    let head = fast_path_head(&proven).expect("a fast-path bracket");
    assert!(
        head.contains("case call 'erlang':'is_number'(_BinRight"),
        "{head}"
    );
    assert!(!head.contains("is_number'(_BinLeft"), "{head}");
    assert_compiles_through_erlc("test", &proven);

    // A proven right operand: no right-operand test anywhere.
    let bare = codegen("Actor subclass: Probe\n  state: n = 0\n\n  cmp: a => self.n := a < 3\n");
    let head = fast_path_head(&bare).expect("a fast-path bracket");
    assert!(!head.contains("is_number'(_BinRight"), "{head}");
    assert!(!bare.contains(HOOK), "{bare}");
}

#[test]
fn a_bare_comparison_is_not_bracketed() {
    // Both operands proven: the bare BIF runs no user code, no bracket.
    let out = codegen("Actor subclass: Probe\n  state: n = 0\n\n  cmp => self.n := 0 < 1\n");
    assert!(
        !out.contains("let _Synced") && !out.contains("let <_Synced"),
        "{out}"
    );
}

/// Whether every hook call in `out` runs inside a cell bracket: the actor's
/// `State` stored before the bracketed send, which the hook (user code) may
/// reach back through.
fn hook_calls_are_bracketed(out: &str) -> bool {
    out.match_indices(HOOK).all(|(at, _)| {
        out[..at]
            .rfind("call 'beamtalk_actor_state':'store'(")
            .is_some_and(|store| !out[store..at].contains("\n'"))
    })
}

#[test]
fn a_proven_receiver_with_a_tested_right_operand_is_bracketed() {
    // `0 < b` can send `b` its reflected hook, user code, so it is a
    // `GuardedOperator` (`classify_binary`), bracketed like `a < b`.
    let src = "Actor subclass: Probe\n  state: n = 0\n\n  cmp: b => self.n := 0 < b\n";
    let out = codegen(src);
    assert!(
        out.contains(HOOK) && hook_calls_are_bracketed(&out),
        "{out}"
    );
    assert_eq!(verifier_report(src, None), None);
    // Classifying it by its receiver alone (as before ADR 0116 § Addendum)
    // leaves the hook call outside every bracket.
    let ignoring =
        crate::core_erlang::fault::with(Fault::ComparisonBracketIgnoresRightOperand, || {
            codegen(src)
        });
    assert!(
        ignoring.contains(HOOK) && !hook_calls_are_bracketed(&ignoring),
        "{ignoring}"
    );
}

#[test]
fn a_recorded_comparison_with_a_tested_right_operand_is_reported() {
    // BT-4452 backstop: a literal-receiver comparison the guard-return rewrite
    // took for a Boolean must be lowered bare. With an unproven right operand
    // it is not (`comparison_is_bare`), so a rewrite that trusted it is
    // refused rather than compiled wrong.
    let src = "Object subclass: G\n  one => 1\n  pick: x :: Integer =>\n    0 < x ifTrue: [^self one]\n    2\n";
    assert_eq!(verifier_report(src, None), None);
    let report = verifier_report(src, Some(Fault::LiteralComparisonTrustsRightOperand))
        .expect("the backstop refuses a trusted tested comparison");
    assert!(
        report.contains("comparison") || report.contains("Boolean"),
        "{report}"
    );
    // With a proven right operand the fault changes nothing: it is bare.
    let proven = "Object subclass: G\n  one => 1\n  pick: x :: Integer =>\n    0 < 1 ifTrue: [^self one]\n    x\n";
    assert_eq!(
        verifier_report(proven, Some(Fault::LiteralComparisonTrustsRightOperand)),
        None
    );
}
