// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5301: list patterns (`#()`, `#(a)`, `#(h | t)`, `#(a, b | rest)`,
//! `#(a | #(b | t))`, `#(k :: Integer | _)`) in `match:` arms and in
//! destructuring assignment, in every frame a statement runs in.
//!
//! A `match:` arm's list pattern lowers natively (`generate_pattern`'s
//! `Pattern::List`, a nested `x :: T` hoisted and class-tested after the
//! shape, BT-5294), and a list destructure binds all its variables from one
//! native `case` (`list_destructure_doc`). Neither touches state threading:
//! the arm bodies and the statements after a destructure thread `State` and
//! outer locals through the existing `ThreadedIr` frames.
//!
//! **Invariant** (every scope kind): a list pattern's bindings are in scope
//! for the arm body / the statements after the destructure, and the frame's
//! `State` and outer-local versions are the ones the statement leaves.
//! [`list_pattern_corpus_compiles`] proves it over pattern site x scrutinee x
//! arm write x frame x actor context: every row passes the debug verifier and
//! compiles through `erlc`. [`list_pattern_corpus_compiles_outside_actors`]
//! covers the same rows in a plain class and a value class.

use super::assert_parses;
use super::loop_body_conditional_receiver_corpus::{
    ARMS, LOOPS, assert_rows_pass, passes, phase0_rejects,
};

/// List-producing operands: a literal, a parameter, and a self-send (which
/// advances `State` in an actor).
const SCRUTINEES: &[&str] = &["#(n, 2, 3)", "xs", "(self listOf: n)"];

/// List-pattern sites over a scrutinee `{X}` and an arm write `{B}`: three
/// `match:` shapes (empty/cons, fixed/two-element cons with a wildcard, a
/// nested cons tail and a nested type pattern) and three destructures
/// (cons, nested cons, annotated cons) whose bindings the next statement
/// reads.
const SITES: &[&str] = &[
    "{X} match: [#() -> {B}; #(h | t) -> h + t size]",
    "{X} match: [#(a) -> {B}; #(a, b | rest) -> a + b + rest size; _ -> 0]",
    "{X} match: [#(a | #(b | t)) -> a + b + t size; #(k :: Integer | _) -> {B}; _ -> 0]",
    "#(h | t) := {X}. {B}. r := h + t size",
    "#(p | #(q | rest)) := {X}. {B}. r := p + q + rest size",
    "#(k :: Integer | _) := {X}. {B}. r := k",
];

/// The arm writes of BT-4000's `ARMS`: nothing and a field. (`ARMS[1]`
/// writes an outer local, which a `match:` arm may not, BT-3875.)
const ARM_WRITES: &[usize] = &[0, 2];

const HELPERS: &str = "  listOf: k => #(k, k + 1)\n";

/// Every `(name, method body)` row the Phase 0 check accepts.
fn rows() -> Vec<(String, String)> {
    let mut out = Vec::new();
    for (l, lp) in LOOPS.iter().enumerate() {
        for (x, scrutinee) in SCRUTINEES.iter().enumerate() {
            for (s, site) in SITES.iter().enumerate() {
                for &a in ARM_WRITES {
                    let stmt = site.replace("{X}", scrutinee).replace("{B}", ARMS[a]);
                    let body = lp.replace("{S}", &stmt);
                    if phase0_rejects(&actor_source(&method("", 0, &body))) {
                        continue;
                    }
                    out.push((format!("loop{l} scrut{x} site{s} arm{a}"), body));
                }
            }
        }
    }
    out
}

fn method(prelude: &str, i: usize, body: &str) -> String {
    format!("  probe{i}: xs =>\n{prelude}    n := 3\n    r := 0\n    {body}\n    r + n\n")
}

fn actor_source(methods: &str) -> String {
    format!("Actor subclass: ListPatternProbe\n  state: f = 0\n{HELPERS}{methods}")
}

/// BT-5301: every row passes the debug verifier and compiles through `erlc`
/// in every actor context (one run per context; bisected on failure).
#[test]
fn list_pattern_corpus_compiles() {
    let rows = rows();
    assert!(
        rows.len() >= 300,
        "the corpus has thinned out: {}",
        rows.len()
    );
    assert_rows_pass("list-pattern", &rows, method, actor_source);
}

/// The same rows, minus the field writes, in a plain class and a value
/// class (no `State` to thread, but outer locals still are).
#[test]
fn list_pattern_corpus_compiles_outside_actors() {
    let rows: Vec<(String, String)> = rows()
        .into_iter()
        .filter(|(name, _)| !name.ends_with("arm2"))
        .collect();
    for header in [
        "Object subclass: ListPatternPlain\n",
        "Value subclass: ListPatternValue\n",
    ] {
        let source = |methods: &str| format!("{header}{HELPERS}{methods}");
        for (_, body) in &rows {
            assert_parses(&source(&method("", 0, body)));
        }
        let methods: String = rows
            .iter()
            .enumerate()
            .map(|(i, (_, body))| method("", i, body))
            .collect();
        if passes(&source(&methods)) {
            continue;
        }
        let failures: Vec<String> = rows
            .iter()
            .enumerate()
            .filter_map(|(i, (name, body))| {
                let single = source(&method("", i, body));
                (!passes(&single)).then(|| format!("{name}:\n{single}"))
            })
            .collect();
        panic!(
            "{} list-pattern row(s) fail in `{header}`:\n\n{}",
            failures.len(),
            failures.join("\n\n")
        );
    }
}
