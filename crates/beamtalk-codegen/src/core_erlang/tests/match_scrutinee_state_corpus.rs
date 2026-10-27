// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4196: a local-threading producer `match:` compiles its scrutinee before
//! it reads the base `State` for the scratch seed, so the arms thread from the
//! `State` the scrutinee left (not the one before it).
//!
//! **Invariant** (every scope kind): a producer's scratch seed reads the newest
//! `State` version bound ahead of it, so no state step of its operands is
//! skipped. In an actor instance method the scrutinee's prelude is spliced into
//! the producer's frame (the splice path), and the construct tuple records the
//! `State` version its seed read (`scratch_base`), so the verifier checks it
//! (`VerifyError::StaleConstructSeed`, BT-4206). Everywhere else the prelude is
//! rendered inside the tuple, ahead of the seed (the inline path).
//!
//! [`a_producer_match_seeds_its_arms_from_the_state_after_the_scrutinee`]
//! checks the generated output over scrutinee x arm x position in an actor
//! instance method; [`the_verifier_reports_a_seed_read_before_the_scrutinee`]
//! shows the verifier catches the pre-fix ordering
//! ([`LocalRebindFault::SeedBeforeScrutinee`]) on every row; and
//! [`inline_path_producer_matches_compile`] /
//! [`repl_producer_matches_verify`] cover the inline path: class methods,
//! value-type and plain-object methods, and the REPL.

use super::while_condition_binding_corpus::repl_report;
use super::{assert_compiles_through_erlc, codegen, verifier_report};
use crate::core_erlang::threaded_expr::fault::LocalRebindFault;

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

const ACTOR_HEADER: &str =
    "Actor subclass: Ctr\n  state: n = 0\n\n  bump =>\n    self.n := self.n + 1\n    self.n\n\n";

/// The actor-instance corpus: every scrutinee x arm x position.
fn actor_programs() -> Vec<String> {
    let mut out = Vec::new();
    for scrutinee in SCRUTINEES {
        for arms in ARMS {
            for position in POSITIONS {
                let m = format!("{scrutinee} match: {arms}");
                let body = position.replace("{m}", &m);
                out.push(format!(
                    "{ACTOR_HEADER}  run =>\n    t := 0\n    u := 0\n    {body}\n"
                ));
            }
        }
    }
    out
}

/// One lexical scope of the Core Erlang being scanned by [`newest_state_before`].
#[derive(Clone, Copy, PartialEq, Eq)]
enum Scope {
    /// `(`/`[`/`{` up to its closer.
    Bracket,
    /// `case` up to its `end`.
    Case,
    /// One clause of the innermost `case`, from its `->` to the next clause.
    Clause,
}

/// The start of the top-level function definition (`'name'/N = fun ...`)
/// enclosing byte offset `at` of `out`.
fn function_start(out: &str, at: usize) -> usize {
    let mut start = 0;
    let mut offset = 0;
    for line in out[..at].split_inclusive('\n') {
        if line.starts_with('\'') && line.contains(" = fun") {
            start = offset;
        }
        offset += line.len();
    }
    start
}

/// The open scopes during [`newest_state_before`]'s scan, each with a unique
/// id a binding records.
#[derive(Default)]
struct Scopes {
    open: Vec<(Scope, usize)>,
    minted: usize,
}

impl Scopes {
    fn push(&mut self, kind: Scope) {
        self.minted += 1;
        self.open.push((kind, self.minted));
    }

    /// Closes scopes up to and including the innermost `kind`.
    fn close_through(&mut self, kind: Scope) {
        while let Some((top, _)) = self.open.pop() {
            if top == kind {
                break;
            }
        }
    }

    /// The id of the innermost open scope (0: the function body).
    fn innermost(&self) -> usize {
        self.open.last().map_or(0, |&(_, id)| id)
    }

    fn is_open(&self, id: usize) -> bool {
        id == 0 || self.open.iter().any(|&(_, open)| open == id)
    }
}

/// The newest `State` version (`State..`/`StateAcc..`) bound by a `let` that is
/// in scope at byte offset `at` of `out`: bound inside the function enclosing
/// `at`, and not inside a bracket, `case` or `case` clause that closed before
/// `at`. Quoted atoms and strings are skipped.
fn newest_state_before(out: &str, at: usize) -> Option<String> {
    let text = &out[function_start(out, at)..at];
    let bytes = text.as_bytes();
    let mut scopes = Scopes::default();
    // Each binding with the id of the scope it was bound in.
    let mut bindings: Vec<(&str, usize)> = Vec::new();
    let mut i = 0;
    while i < bytes.len() {
        match bytes[i] {
            quote @ (b'\'' | b'"') => {
                i += 1;
                while i < bytes.len() && bytes[i] != quote {
                    i += if bytes[i] == b'\\' { 2 } else { 1 };
                }
            }
            b'(' | b'[' | b'{' => scopes.push(Scope::Bracket),
            b')' | b']' | b'}' => scopes.close_through(Scope::Bracket),
            b'-' if bytes.get(i + 1) == Some(&b'>') => {
                // A clause body of the innermost `case` starts; the previous
                // clause's bindings go out of scope.
                if matches!(scopes.open.last(), Some((Scope::Clause, _))) {
                    scopes.open.pop();
                }
                if matches!(scopes.open.last(), Some((Scope::Case, _))) {
                    scopes.push(Scope::Clause);
                }
                i += 1;
            }
            c if c.is_ascii_alphanumeric() || c == b'_' => {
                let end = text[i..]
                    .find(|c: char| !(c.is_ascii_alphanumeric() || c == '_'))
                    .map_or(text.len(), |n| i + n);
                match &text[i..end] {
                    "case" => scopes.push(Scope::Case),
                    "end" => scopes.close_through(Scope::Case),
                    "let" => {
                        let rest = text[end..].trim_start();
                        let name_len = rest
                            .find(|c: char| !c.is_ascii_alphanumeric())
                            .unwrap_or(rest.len());
                        let name = &rest[..name_len];
                        if name.starts_with("State") && rest[name_len..].starts_with(" = ") {
                            bindings.push((name, scopes.innermost()));
                        }
                    }
                    _ => {}
                }
                i = end;
                continue;
            }
            _ => {}
        }
        i += 1;
    }
    bindings
        .into_iter()
        .rev()
        .find(|&(_, scope)| scopes.is_open(scope))
        .map(|(name, _)| name.to_string())
}

/// Checks every scratch seed of `out`
/// (`let _SeededState.. = maps:put(.., <base>)`) whose base is a `State`
/// version against
/// [`newest_state_before`], answering how many it compared.
fn check_seeds(src: &str, out: &str) -> usize {
    let mut seeds = 0;
    let mut from = 0;
    while let Some(i) = out[from..].find("let _SeededState") {
        let at = from + i;
        from = at + 1;
        let line = &out[at..];
        let Some(close) = line.find(" in ") else {
            continue;
        };
        // The innermost `maps:put`'s map argument is the seed's base.
        let base = line[..close]
            .rsplit(", ")
            .next()
            .unwrap_or_default()
            .trim_end_matches(')')
            .trim();
        if !base.starts_with("State") {
            continue;
        }
        seeds += 1;
        let newest = newest_state_before(out, at).unwrap_or_else(|| "State".into());
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
    for src in actor_programs() {
        let out = codegen(&src);
        seeded += check_seeds(&src, &out);
    }
    assert!(seeded > 20, "only {seeded} seeds compared");
}

/// The oracle sees only bindings in scope: a `State` bound in another
/// function, in a closed `case` or in a sibling clause is not one a seed can
/// read.
#[test]
fn newest_state_before_is_scoped_to_the_enclosing_function_and_open_clauses() {
    let out = "'a'/1 = fun (State) ->\n    let State9 = x in State9\n\n\
               'b'/1 = fun (State) ->\n    let State1 = y in \
               let Z = case X of <1> when 'true' -> let State2 = z in {1, State2} \
               <_> when 'true' -> let State3 = 'let State7 = q' in {2, State3} end in \
               case Y of <1> when 'true' -> let State4 = w in State4 \
               <_> when 'true' -> SEED";
    let at = out.find("SEED").expect("marker");
    assert_eq!(newest_state_before(out, at).as_deref(), Some("State1"));
    let in_clause = out.find("{1, State2}").expect("marker");
    assert_eq!(
        newest_state_before(out, in_clause).as_deref(),
        Some("State2")
    );
    let first_fn = out.find(" State9\n").expect("marker");
    assert_eq!(
        newest_state_before(out, first_fn).as_deref(),
        Some("State9")
    );
}

/// BT-4206: with the scratch seed read before the scrutinee compiles (the
/// pre-BT-4196 ordering, [`LocalRebindFault::SeedBeforeScrutinee`]) every
/// actor row reports `StaleConstructSeed`: the verifier, not the output scan,
/// catches a seed that skips the scrutinee's `State` step.
#[cfg(debug_assertions)]
#[test]
fn the_verifier_reports_a_seed_read_before_the_scrutinee() {
    let programs = actor_programs();
    for src in &programs {
        assert_eq!(verifier_report(src, None), None, "control:\n{src}");
    }
    let missed: Vec<String> = programs
        .iter()
        .filter_map(|src| {
            let report = verifier_report(src, Some(LocalRebindFault::SeedBeforeScrutinee));
            (!report
                .as_deref()
                .is_some_and(|r| r.contains("StaleConstructSeed")))
            .then(|| format!("{src}\n=> {report:?}"))
        })
        .collect();
    assert!(
        missed.is_empty(),
        "{} row(s) where the verifier missed the stale seed:\n{}",
        missed.len(),
        missed.join("\n\n")
    );
}

/// Class headers whose `run` is lowered on the inline path: no actor
/// instance `State` to splice into.
const INLINE_HEADERS: &[&str] = &[
    // Class methods: the scrutinee's `self bump` is a class-side self-send
    // that writes a class variable.
    "Object subclass: Ctr\n  classState: c = 0\n\n  class bump =>\n    self.c := self.c + 1\n    self.c\n\n  class run =>\n",
    "Actor subclass: Ctr\n  classState: c = 0\n\n  class bump =>\n    self.c := self.c + 1\n    self.c\n\n  class run =>\n",
    // A value-type method and a plain-object method.
    "Value subclass: Ctr\n  bump => 1\n\n  run =>\n",
    "Object subclass: Ctr\n  bump => 1\n\n  run =>\n",
];

/// BT-4206: the inline path (the scrutinee's prelude rendered inside the
/// tuple, ahead of the seed) in every non-actor-instance scope: each scrutinee
/// x arm x position compiles with a silent verifier and through `erlc`, and
/// any seed that continues a `State` reads the newest one.
#[test]
fn inline_path_producer_matches_compile() {
    for header in INLINE_HEADERS {
        for scrutinee in SCRUTINEES {
            for arms in ARMS {
                for position in POSITIONS {
                    let m = format!("{scrutinee} match: {arms}");
                    // No instance fields outside an actor instance method.
                    let body = position.replace("{m}", &m).replace("self.n, ", "");
                    let src = format!("{header}    t := 0\n    u := 0\n    {body}\n");
                    let out = codegen(&src);
                    check_seeds(&src, &out);
                    assert_compiles_through_erlc("test", &out);
                }
            }
        }
    }
}

/// BT-4206: the REPL (the inline path over the bindings map): a producer
/// `match:` whose scrutinee sends and computes lowers with a silent verifier.
#[test]
fn repl_producer_matches_verify() {
    const REPL_SCRUTINEES: &[&str] = &["c bump", "(c bump + 1)", "(c bump max: 0)", "t + u"];
    let failures: Vec<String> = REPL_SCRUTINEES
        .iter()
        .flat_map(|scrutinee| {
            ARMS.iter()
                .map(move |arms| format!("r := {scrutinee} match: {arms}"))
        })
        .filter_map(|row| repl_report(&row, None).map(|r| format!("{row}: {r}")))
        .collect();
    assert!(
        failures.is_empty(),
        "{} REPL producer match row(s) fail:\n{}",
        failures.len(),
        failures.join("\n")
    );
}
