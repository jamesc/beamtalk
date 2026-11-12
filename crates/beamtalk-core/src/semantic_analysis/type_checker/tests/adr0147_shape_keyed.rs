// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0147 (BT-4732): shape-aware keyed operations — the `at:` family,
//! `at:put:`, map patterns and union receivers.

use super::common::*;

fn messages(body: &str) -> Vec<String> {
    let source = format!(
        "Object subclass: Probe\n  takeString: s :: String => s\n  takeInteger: n :: Integer => n\n{body}\n"
    );
    let tokens = crate::source_analysis::lex_with_eof(&source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
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

fn one(body: &str) -> Vec<String> {
    messages(body)
}

fn expects(body: &str, fragment: &str) {
    let msgs = one(body);
    assert!(
        msgs.iter().any(|m| m.contains(fragment)),
        "expected `{fragment}` in {msgs:#?} for {body}"
    );
}

fn silent(body: &str) {
    let msgs = one(body);
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
    let msgs = one("  m: o :: #{#a => Integer} => o at: #q ifPresent: [:v | v]");
    assert_eq!(msgs.len(), 1, "{msgs:#?}");
    assert!(msgs[0].contains("`#q` is not a key of"), "{msgs:#?}");
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
