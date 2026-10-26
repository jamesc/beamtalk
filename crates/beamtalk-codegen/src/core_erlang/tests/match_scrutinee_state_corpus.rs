// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4196: a local-threading producer `match:` compiles its scrutinee before
//! it reads the base `State` for the scratch seed, so the arms thread from the
//! `State` the scrutinee left (not the one before it).
//!
//! The invariant, checked over a generated corpus of scrutinee x arm x
//! position shapes in an actor instance method: every scratch seed
//! (`maps:put('__local__x', V, <State>)`) reads the newest `State` version
//! bound ahead of it, so no state step of the scrutinee is skipped.

use crate::core_erlang::{CodegenOptions, generate_module};
use beamtalk_core::source_analysis::{lex_with_eof, parse};

fn compile(src: &str) -> String {
    let (module, diags) = parse(lex_with_eof(src));
    assert!(diags.is_empty(), "{src}: {diags:?}");
    generate_module(&module, CodegenOptions::new("test")).expect("codegen should succeed")
}

/// Scrutinees whose evaluation steps the actor `State` (a self-send that
/// writes a field, alone and as an operand).
const SCRUTINEES: &[&str] = &[
    "self bump",
    "(self bump + 1)",
    "self bump + self bump",
    "(self bump max: 0)",
    "(self bump) negated",
];

/// `match:` arms writing outer locals `t` and/or `u`.
const ARMS: &[&str] = &[
    "[1 -> (t := 11); _ -> (t := 20)]",
    "[_ -> (t := t + 1)]",
    "[1 -> (t := 11); _ -> (u := 20)]",
    "[1 -> ([t := 11. u := 5. 1] value); _ -> 0]",
    "[n :: Integer -> (t := t + n); _ -> 0]",
];

/// Positions of the producer: an assignment's value, a statement, the last
/// statement.
const POSITIONS: &[&str] = &[
    "t := {m}\n    {self.n, t, u}",
    "{m}\n    {self.n, t, u}",
    "r := {m}\n    {self.n, t, u, r}",
    "{m}",
];

/// The `State` versions of `out` bound by a `let State.. =` (or `StateAcc`)
/// before byte offset `at`, newest last.
fn newest_state_before(out: &str, at: usize) -> Option<String> {
    let head = &out[..at];
    let mut newest = None;
    let mut rest = head;
    while let Some(i) = rest.find("let State") {
        let tail = &rest[i + "let ".len()..];
        let end = tail.find(" = ")?;
        let name = &tail[..end];
        if name.chars().all(char::is_alphanumeric) {
            newest = Some(name.to_string());
        }
        rest = &rest[i + "let State".len()..];
    }
    newest
}

fn check(src: &str) -> usize {
    let out = compile(src);
    let mut seeds = 0;
    let mut from = 0;
    while let Some(i) = out[from..].find("'maps':'put'('__local__") {
        let at = from + i;
        from = at + 1;
        let call = &out[at..];
        let Some(close) = call.find(") in") else {
            continue;
        };
        let args = &call[..close];
        let base = args
            .rsplit(", ")
            .next()
            .unwrap_or_default()
            .trim_end_matches(')')
            .trim();
        // Only the seeds (`let _SeededState.. = maps:put(.., State..)`).
        let line_start = out[..at].rfind("let ").unwrap_or(0);
        if !out[line_start..at].starts_with("let _SeededState") {
            continue;
        }
        seeds += 1;
        let newest = newest_state_before(&out, line_start).unwrap_or_else(|| "State".into());
        assert_eq!(
            base, newest,
            "the seed reads {base}, but the scrutinee left {newest}\n{src}\n{out}"
        );
    }
    seeds
}

#[test]
fn a_producer_match_seeds_its_arms_from_the_state_after_the_scrutinee() {
    let mut seeded = 0;
    for scrutinee in SCRUTINEES {
        for arms in ARMS {
            for position in POSITIONS {
                let m = format!("{scrutinee} match: {arms}");
                let body = position.replace("{m}", &m);
                let src = format!(
                    "Actor subclass: Ctr\n  state: n = 0\n\n  bump =>\n    self.n := self.n + 1\n    self.n\n\n  run =>\n    t := 0\n    u := 0\n    {body}\n"
                );
                seeded += check(&src);
            }
        }
    }
    assert!(seeded > 20, "only {seeded} seeds compared");
}
