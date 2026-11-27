// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0147 (BT-4732): shape-aware keyed operations — the `at:` family,
//! `at:put:`, map patterns and union receivers.

use super::common::*;

fn messages(body: &str) -> Vec<String> {
    let source = format!(
        "Object subclass: Probe\n  takeString: s :: String => s\n  takeInteger: n :: Integer => n\n{body}\n"
    );
    let module = parse_ok(&source);
    crate::semantic_analysis::analyse(&module)
        .diagnostics
        .into_iter()
        .filter(|d| {
            matches!(
                d.category,
                Some(DiagnosticCategory::Type | DiagnosticCategory::Dnu)
            )
        })
        .map(|d| d.message.to_string())
        .collect()
}

fn expects(body: &str, fragment: &str) {
    let msgs = messages(body);
    assert!(
        msgs.iter().any(|m| m.contains(fragment)),
        "expected `{fragment}` in {msgs:#?} for {body}"
    );
}

fn silent(body: &str) {
    let msgs = messages(body);
    assert!(
        msgs.is_empty(),
        "expected no warnings, got {msgs:#?} for {body}"
    );
}

#[test]
fn at_answers_the_key_type() {
    silent("  m: o :: #{#a => Integer} => self takeInteger: (o at: #a)");
    expects(
        "  m: o :: #{#a => Integer} => self takeString: (o at: #a)",
        "got Integer",
    );
    // Optional key: `T`, no diagnostic.
    silent("  m: o :: #{?#a => Integer} => self takeInteger: (o at: #a)");
}

#[test]
fn at_unnamed_key() {
    expects(
        "  m: o :: #{#a => Integer, ...} => self takeInteger: (o at: #zz)",
        "got Object",
    );
    expects(
        "  m: o :: #{#maxFrameSize => Integer} => o at: #maxFramSize",
        "`#maxFramSize` is not a key of",
    );
}

#[test]
fn at_if_absent() {
    expects(
        "  m: o :: #{#a => Integer} => self takeString: (o at: #a ifAbsent: [\"x\"])",
        "got Integer",
    );
    expects(
        "  m: o :: #{?#a => Integer} => self takeString: (o at: #a ifAbsent: [\"x\"])",
        "got Integer | String",
    );
    expects(
        "  m: o :: #{#a => Integer, ...} => self takeInteger: (o at: #q ifAbsent: [\"x\"])",
        "got Object | String",
    );
    // Closed, key absent: the block always runs.
    expects(
        "  m: o :: #{#a => Integer} => self takeInteger: (o at: #q ifAbsent: [\"x\"])",
        "got String",
    );
    expects(
        "  m: o :: #{#a => Integer} => o at: #q ifAbsent: [1]",
        "`#q` is not a key of",
    );
}

#[test]
fn at_if_present_block_gets_the_key_type() {
    expects(
        "  m: o :: #{#a => Integer} => o at: #a ifPresent: [:v | self takeString: v]",
        "got Integer",
    );
    expects(
        "  m: o :: #{?#a => Integer} => o at: #a ifPresent: [:v | self takeString: v] ifAbsent: [nil]",
        "got Integer",
    );
    expects(
        "  m: o :: #{#a => Integer, ...} => o at: #q ifPresent: [:v | self takeString: v]",
        "got Object",
    );
    // One warning, not a second from the generic `Dictionary` key check.
    let msgs = messages("  m: o :: #{#a => Integer} => o at: #q ifPresent: [:v | v]");
    assert_eq!(msgs.len(), 1, "{msgs:#?}");
    assert!(msgs[0].contains("`#q` is not a key of"), "{msgs:#?}");
    // The consequence named in the hint depends on the operation.
    let hint = |body: &str| {
        let source = format!("Object subclass: Probe\n{body}\n");
        let (module, _) =
            crate::source_analysis::parse(crate::source_analysis::lex_with_eof(&source));
        crate::semantic_analysis::analyse(&module)
            .diagnostics
            .into_iter()
            .find_map(|d| d.hint.map(|h| h.to_string()))
            .unwrap_or_default()
    };
    assert!(
        hint("  m: o :: #{#a => Integer} => o at: #q ifPresent: [:v | v]").contains("never runs")
    );
    assert!(hint("  m: o :: #{#a => Integer} => o at: #q ifAbsent: [0]").contains("always runs"));
    assert!(hint("  m: o :: #{#a => Integer} => o at: #q").contains("can only fail"));
}

#[test]
fn at_put_adds_or_replaces_a_required_key() {
    expects(
        "  m: o :: #{#a => Integer} => self takeString: (o at: #b put: 3)",
        "got #{#a => Integer, #b => Integer}",
    );
    expects(
        "  m: o :: #{?#a => Integer, ...} => self takeString: (o at: #a put: \"s\")",
        "got #{#a => String, ...}",
    );
    // The result is accepted where the shape is expected.
    silent(
        "  take: o :: #{#a => Integer, #b => Integer} => o\n  \
           m: o :: #{#a => Integer} => self take: (o at: #b put: 3)",
    );
}

#[test]
fn non_singleton_key_uses_the_dictionary_view() {
    silent("  m: o :: #{#a => Integer} k: k :: Symbol => o at: k");
}

#[test]
fn key_local_bound_to_a_singleton() {
    silent("  m: o :: #{#a => Integer} => k := #a. self takeInteger: (o at: k)");
}

#[test]
fn map_patterns_type_their_variables() {
    expects(
        "  m: o :: #{#a => Integer} => o match: [#{#a => v} -> self takeString: v; _ -> 0]",
        "got Integer",
    );
    expects(
        "  m: o :: #{?#a => Integer} => o match: [#{#a => v} -> self takeString: v; _ -> 0]",
        "got Integer",
    );
    expects(
        "  m: o :: #{#a => Integer, ...} => o match: [#{#z => v} -> self takeInteger: v; _ -> 0]",
        "got Object",
    );
    expects(
        "  m: o :: #{#a => Integer} => o match: [#{#z => v} -> v; _ -> 0]",
        "`#z` is not a key of",
    );
}

#[test]
fn union_receivers() {
    // Union of each member's result.
    expects(
        "  m: o :: #{#a => Integer} | #{#a => String} => self takeInteger: (o at: #a)",
        "got Integer | String",
    );
    // Nil members are skipped, as for any `T | Nil` send.
    silent("  m: o :: #{#a => Integer} | Nil => self takeInteger: (o at: #a)");
    // Warns only when every member is a closed shape lacking the key.
    silent("  m: o :: #{#a => Integer} | #{#b => Integer} => o at: #a");
    expects(
        "  m: o :: #{#a => Integer} | #{#b => Integer} => o at: #z",
        "`#z` is not a key of",
    );
    // A non-shape member is typed as today.
    silent(
        "  m: o :: #{#a => Integer} | Dictionary(Symbol, Integer) => self takeInteger: (o at: #a)",
    );
}

#[test]
fn remove_key_and_merge_answer_the_dictionary_view() {
    expects(
        "  m: o :: #{#a => Integer} => self takeString: (o removeKey: #a)",
        "got Dictionary",
    );
    expects(
        "  m: o :: #{#a => Integer} => self takeString: (o merge: o)",
        "got Dictionary",
    );
}

#[test]
fn is_kind_of_dictionary_keeps_shape_members() {
    expects(
        "  m: o :: #{#a => Integer} | Nil => (o isKindOf: Dictionary) ifTrue: [self takeString: (o at: #a)]",
        "got Integer",
    );
}

#[test]
fn protocol_type_pattern_keeps_the_shape() {
    // `Dictionary` is `Printable`, so the arm is reachable and `v` is not `Never`.
    silent("  m: o :: #{#a => Integer} => o match: [p :: Printable -> p printString; _ -> \"\"]");
}

// ── ADR 0155 / BT-5344: optional map-pattern keys ─────────────

#[test]
fn optional_key_without_default_binds_value_or_nil() {
    // A shape key binds `T | Nil` behind `?`, in an arm and a destructure.
    expects(
        "  m: o :: #{?#a => Integer} => o match: [#{?#a => v} -> self takeInteger: v; _ -> 0]",
        "got Integer | Nil",
    );
    expects(
        "  m: o :: #{?#a => Integer} =>\n    #{?#a => v} := o\n    self takeInteger: v",
        "got Integer | Nil",
    );
}

#[test]
fn optional_key_with_fitting_default_binds_value_type() {
    silent(
        "  m: o :: #{?#a => Integer} => o match: [#{?#a => v = 0} -> self takeInteger: v; _ -> 0]",
    );
    silent("  m: o :: #{?#a => Integer} =>\n    #{?#a => v = 0} := o\n    self takeInteger: v");
    expects(
        "  m: o :: #{?#a => Integer} =>\n    #{?#a => v = \"none\"} := o\n    self takeInteger: v",
        "got Integer | String",
    );
}

#[test]
fn optional_key_with_type_test_binds_tested_type() {
    // String keys type through the `x :: T` test: `T | Nil`, or `T` with a
    // fitting default.
    expects(
        "  m: o :: Dictionary => o match: [#{?\"n\" => n :: Integer} -> self takeString: n; _ -> 0]",
        "got Integer | Nil",
    );
    silent(
        "  m: o :: Dictionary => o match: [#{?\"n\" => n :: Integer = 3} -> self takeInteger: n; _ -> 0]",
    );
    silent(
        "  m: o :: Dictionary =>\n    #{?\"n\" => n :: Integer = 3} := o\n    self takeInteger: n",
    );
}

#[test]
fn optional_key_default_must_fit_type_test() {
    expects(
        "  m: o :: Dictionary => o match: [#{?#n => n :: Integer = \"none\"} -> n; _ -> 0]",
        "String",
    );
}

#[test]
fn optional_key_not_in_closed_shape_is_always_absent() {
    expects(
        "  m: o :: #{#a => Integer} =>\n    #{?#b => v} := o\n    v",
        "`#b` is not a key of",
    );
    // Always absent: the binding is exactly the default's type, so the
    // not-a-key warning is the only diagnostic.
    let msgs = messages(
        "  m: o :: #{#a => Integer} => o match: [#{?#b => v = 0} -> self takeInteger: v; _ -> 0]",
    );
    assert_eq!(msgs.len(), 1, "got {msgs:#?}");
    assert!(msgs[0].contains("`#b` is not a key of"), "got {msgs:#?}");
}

#[test]
fn optional_key_over_untyped_dictionary_stays_dynamic() {
    silent(
        "  m: o :: Dictionary =>\n    #{?\"a\" => v, ?#b => w = 1} := o\n    self takeInteger: v\n    self takeString: w",
    );
}
