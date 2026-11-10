// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4015: a `whileTrue:`/`whileFalse:` condition's outer-local (in the
//! REPL, binding) writes are carried out of the condition closure.
//!
//! **Invariant** (every scope kind): a loop carries the union of the families
//! its condition and its body produce. A local the condition writes rides the
//! `StateAcc` map, so the condition closure hands its map back and the loop's
//! `ConditionalLoop` records the hand-back as a typed
//! `ThreadedStmt::ConditionHandBack`; `condition_families` names every local
//! the condition writes, and `VerifyError::ConditionFamilyNotCarried` fails a
//! loop whose condition writes one without handing its map back. The
//! condition's statements are lowered as its own frame's `ThreadedStmt`s
//! (`generate_stateful_while_condition`), so a value-position write in them is
//! spliced into the frame rather than rendered closed
//! (`StateEffectEscapesExpression`).
//!
//! At the REPL a binding-writing condition was not a carried effect: it was a
//! plain closure answering its boolean, so the write was lost (the loop never
//! ended) or a body that read it read the closure's own variable (`erlc`:
//! unbound variable).
//!
//! [`repl_condition_binding_rows_verify`] runs the REPL rows (a condition that
//! writes a binding x a body that threads one x loop) through the REPL
//! lowering with the debug verifier on;
//! [`repl_condition_binding_rows_report_the_pre_fix_lowering`] shows the
//! verifier catches the pre-fix lowering
//! ([`LocalRebindFault::ConditionWritesNotCarried`]) on every row.
//! `beamtalk-repl`'s `repl_while_condition_binding.rs` compiles the REPL
//! corpus through `erlc`. [`compiled_condition_value_position_writes_compile`]
//! covers the issue's question for compiled methods: a state-effecting
//! condition with a value-position local write, in actor, value-type,
//! `TestCase` and plain-object methods, compiles with a silent verifier and
//! through `erlc`.
//!
//! BT-4602 widens the compiled-method side to the rule the Phase 0 check now
//! reads (`block_facts::while_condition_threads`): a condition with no state
//! effect that writes a local, in every compiled method context (class methods
//! included) and with any literal body, the unary form's empty one included.
//! [`compiled_pure_condition_writes_are_carried`] lowers those rows, as a
//! method statement and nested in an arm, a `do:`, an outer loop, an
//! assignment and every other block role Phase 0 accepts the loop in, with a
//! silent verifier and through `erlc`;
//! [`compiled_pure_condition_rows_report_the_pre_fix_lowering`] shows the
//! verifier catches the dropped write on every row, the plain lowering's
//! included.

use super::{assert_compiles_through_erlc, codegen};
use crate::core_erlang::threaded_expr::fault::{self, LocalRebindFault};
use crate::core_erlang::{CodeGenContext, CodeGenError, CoreErlangGenerator};
use beamtalk_core::source_analysis::{lex_with_eof, parse};
use std::panic::{AssertUnwindSafe, catch_unwind};

/// REPL conditions that write a binding (`CMP` is the loop's comparison).
const REPL_CONDITIONS: &[&str] = &[
    "cw := cw + 1. cw CMP 3",
    "x := (w := cw + 10). cw := cw + 1. cw CMP 3",
    "(cw := cw + 1) CMP 3",
    "cw := cw + 1. (w := cw) CMP 3",
    "(cw > 0) ifTrue: [w := cw]. cw := cw + 1. cw CMP 3",
    "cw := cw + 1. (cw CMP 3) and: [w := cw. true]",
    "cw := (cw + 1) max: 0. cw CMP 3",
    // BT-4584: a destructuring statement writes each binding it binds.
    "cw := cw + 1. #(w, b) := #(w + cw, 0). cw CMP 3",
    "#(cw, w) := #(cw + 1, cw). cw CMP 3",
    "cw := cw + 1. {b, w} := {0, w + cw}. cw CMP 3",
];

/// REPL bodies that write a binding, so the loop is a `ConditionalLoop` the
/// verifier sees (a body that threads nothing is lowered as a plain loop when
/// the condition is not carried).
const REPL_BODIES: &[&str] = &[
    "ck := ck + cw",
    "ck := ck + 1",
    "(ck >= 0) ifTrue: [ck := ck + 1]",
    "x := (ck := ck + cw)",
];

const LOOPS: &[(&str, &str)] = &[("whileTrue:", "<"), ("whileFalse:", ">=")];

fn repl_rows() -> Vec<String> {
    let mut out = Vec::new();
    for (selector, cmp) in LOOPS {
        for cond in REPL_CONDITIONS {
            for body in REPL_BODIES {
                out.push(format!(
                    "[{}] {selector} [{body}]",
                    cond.replace("CMP", cmp)
                ));
            }
        }
    }
    out
}

/// Lowers the REPL input `src` the way the REPL assembler lowers each
/// statement (`local_producer_agreement_repl.rs`), answering the debug-build
/// panic message (a verifier finding) or a codegen error, or `None`.
pub(super) fn repl_report(src: &str, fault_on: Option<LocalRebindFault>) -> Option<String> {
    let (parsed, diags) = parse(lex_with_eof(src));
    assert!(diags.is_empty(), "malformed row:\n{src}\n{diags:?}");
    let statements: Vec<_> = parsed
        .expressions
        .into_iter()
        .map(|s| s.expression)
        .collect();
    let compile = || {
        catch_unwind(AssertUnwindSafe(|| {
            let mut generator = CoreErlangGenerator::new("bt4015_repl");
            generator.context = CodeGenContext::Repl;
            generator.set_is_repl_mode(true);
            generator.push_scope();
            generator.bind_var("__bindings__", "Bindings");
            for stmt in &statements {
                if generator
                    .repl_statement_with_producers(stmt, stmt)?
                    .is_none()
                {
                    generator.expression_doc_with_repl_mutation_tracking(stmt)?;
                }
            }
            Ok::<(), CodeGenError>(())
        }))
    };
    let result = match fault_on {
        Some(f) => fault::with(f, compile),
        None => compile(),
    };
    match result {
        Ok(Ok(())) => None,
        Ok(Err(e)) => Some(format!("codegen: {e:?}")),
        Err(payload) => Some(
            payload
                .downcast_ref::<String>()
                .cloned()
                .or_else(|| payload.downcast_ref::<&str>().map(|s| (*s).to_string()))
                .unwrap_or_default(),
        ),
    }
}

/// BT-4015: every REPL row lowers with a silent verifier.
#[test]
fn repl_condition_binding_rows_verify() {
    let rows = repl_rows();
    assert!(rows.len() >= 50, "the corpus has thinned out");
    let failures: Vec<String> = rows
        .iter()
        .filter_map(|row| repl_report(row, None).map(|r| format!("{row}: {r}")))
        .collect();
    assert!(
        failures.is_empty(),
        "{} REPL while-condition binding row(s) fail:\n{}",
        failures.len(),
        failures.join("\n")
    );
}

/// BT-4015: with the pre-fix gate (a REPL condition's binding write is not a
/// carried effect, [`LocalRebindFault::ConditionWritesNotCarried`]) every
/// row reports `ConditionFamilyNotCarried`: the verifier, not the repro, is
/// what catches a condition write the loop drops.
#[cfg(debug_assertions)]
#[test]
fn repl_condition_binding_rows_report_the_pre_fix_lowering() {
    let missed: Vec<String> = repl_rows()
        .into_iter()
        .filter_map(|row| {
            let report = repl_report(&row, Some(LocalRebindFault::ConditionWritesNotCarried));
            (!report
                .as_deref()
                .is_some_and(|r| r.contains("ConditionFamilyNotCarried")))
            .then(|| format!("{row}: {report:?}"))
        })
        .collect();
    assert!(
        missed.is_empty(),
        "{} row(s) where the verifier missed the dropped condition write:\n{}",
        missed.len(),
        missed.join("\n")
    );
}

/// BT-4584: with the pre-fix lowering (a destructuring statement of a carried
/// condition binds its names with plain `let`s,
/// [`LocalRebindFault::ConditionDestructurePlain`]) every destructuring row
/// reports `ConditionWriteNotInMap`: the condition hands its map back, but the
/// map lacks the write.
#[cfg(debug_assertions)]
#[test]
fn repl_condition_destructure_rows_report_the_pre_fix_lowering() {
    let rows: Vec<String> = repl_rows()
        .into_iter()
        .filter(|row| row.contains(":= #(") || row.contains(":= {"))
        .collect();
    assert!(rows.len() >= 20, "the destructuring rows have thinned out");
    let missed: Vec<String> = rows
        .into_iter()
        .filter_map(|row| {
            let report = repl_report(&row, Some(LocalRebindFault::ConditionDestructurePlain));
            (!report
                .as_deref()
                .is_some_and(|r| r.contains("ConditionWriteNotInMap")))
            .then(|| format!("{row}: {report:?}"))
        })
        .collect();
    assert!(
        missed.is_empty(),
        "{} row(s) where the verifier missed the dropped destructuring write:\n{}",
        missed.len(),
        missed.join("\n")
    );
}

/// State-effecting conditions (a field write or a self-send) with a
/// value-position local write ahead of or in the boolean, in a compiled method
/// (`F` rows write the field and need a class that has one).
const COMPILED_CONDITIONS: &[(&str, bool)] = &[
    ("x := (w := t + 1). self.f := 1. s < 2", true),
    ("x := (w := t + 1). self done. s < 2", false),
    ("self done. (w := t + 1) > s", false),
    ("x := (w := self take: t). s < 2", false),
    ("self.f := (w := t + 1). s < 2", true),
    ("w := t + 1. (self isAlive: s) and: [(x := w) > 0]", false),
    // A construct statement in the condition: a conditional arm, a fold, a
    // counted loop and an `on:do:` that write a local, now lowered at the
    // condition frame's producer sites.
    ("(t > 0) ifTrue: [w := t]. self done. s < 2", false),
    ("#(1, 2) do: [:e | w := w + e]. self done. s < 2", false),
    ("1 to: 2 do: [:i | w := w + i]. self done. s < 2", false),
    (
        "[w := t + 1] on: Error do: [:err | 0]. self done. s < 2",
        false,
    ),
    // BT-4584: a destructuring statement writing an outer local, with the
    // condition's own block local (`d`) bound alongside it.
    ("#(w, d) := #(t + 1, 0). self.f := 1. s < 2", true),
    ("#(w, d) := #(t + 1, 0). self done. s < 2", false),
    ("self done. {d, w} := {s, t + 1}. s < 2", false),
    ("#(w, x) := #(t + 1, w). self done. s < 2", false),
    // A non-local return from the condition after a local write.
    (
        "x := (w := t + 1). (s > 5) ifTrue: [^ w]. self done. s < 2",
        false,
    ),
];

const COMPILED_BODIES: &[&str] = &["s := s + 1", "s := s + 1. t := t + w", "nil"];

/// `(name, header, has field f, actor instance)`.
const COMPILED_CONTEXTS: &[(&str, &str, bool)] = &[
    (
        "actor",
        "Actor subclass: WhileBindingProbe\n  state: f = 0\n",
        true,
    ),
    (
        "value",
        "Value subclass: WhileBindingProbe\n  field: f = 0\n",
        true,
    ),
    (
        "test_case",
        "TestCase subclass: WhileBindingProbe\n  field: f = 0\n",
        true,
    ),
    ("object", "Object subclass: WhileBindingProbe\n", false),
];

const HELPERS: &str = "  done => false
  take: v => v
  isAlive: k => k > 0
";

/// BT-4015: the issue's question for compiled methods. A condition that has a
/// state effect (so its writes are carried) and writes a local in value
/// position compiles with a silent verifier and through `erlc` in every
/// method context; a context that cannot carry the effect gives the same
/// structured rejection a body-side write does, never a verifier finding.
#[test]
fn compiled_condition_value_position_writes_compile() {
    let mut failures = Vec::new();
    let mut compiled = 0;
    for (name, header, has_field) in COMPILED_CONTEXTS {
        for (c, (cond, writes_field)) in COMPILED_CONDITIONS.iter().enumerate() {
            if *writes_field && !has_field {
                continue;
            }
            for (b, body) in COMPILED_BODIES.iter().enumerate() {
                for (selector, negate) in [("whileTrue:", ""), ("whileFalse:", " not")] {
                    let src = format!(
                        "{header}{HELPERS}  probe =>\n    s := 0\n    t := 0\n    w := 0\n    x := 0\n    [{cond}{negate}] {selector} [{body}]\n    #(s, t, w, x)\n"
                    );
                    match super::verifier_report(&src, None) {
                        None => {
                            let ok = catch_unwind(AssertUnwindSafe(|| {
                                assert_compiles_through_erlc("test", &codegen(&src));
                            }))
                            .is_ok();
                            if ok {
                                compiled += 1;
                            } else {
                                failures.push(format!(
                                    "{name} cond{c} body{b} {selector}: erlc\n{src}"
                                ));
                            }
                        }
                        Some(r)
                            if r.contains("ThreadedIr verify")
                                || !r.contains("codegen should succeed") =>
                        {
                            failures.push(format!("{name} cond{c} body{b} {selector}: {r}\n{src}"));
                        }
                        Some(_) => {}
                    }
                }
            }
        }
    }
    assert!(
        failures.is_empty(),
        "{} compiled while-condition row(s) fail:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
    assert!(
        compiled >= 100,
        "only {compiled} rows compiled; the corpus has thinned out"
    );
}

/// BT-4602: conditions with no state effect (no field write, no self-send)
/// that write an outer local, ahead of the boolean, in it, through a nested
/// construct or in an `and:` arm (`CMP` is the loop's comparison).
const PURE_CONDITIONS: &[&str] = &[
    "t := t + 1. t CMP 3",
    "(t := t + 1) CMP 3",
    "t := t + 1. w := t * 2. t CMP 3",
    "x := (w := t + 1). t := w. t CMP 3",
    "(t > 0) ifTrue: [w := t]. t := t + 1. t CMP 3",
    "#(1, 2) do: [:e | w := w + e]. t := t + 1. t CMP 3",
    "1 to: 2 do: [:i | w := w + i]. t := t + 1. t CMP 3",
    "[t := t + 1] on: Error do: [:err | 0]. t CMP 3",
    "t := t + 1. (t CMP 3) and: [w := t. true]",
    "t := t + 1. (t > 5) ifTrue: [^ w]. t CMP 3",
];

/// `None` is the unary form (`[c] whileTrue`, BT-4313), the keyword loop with
/// an empty body once normalised.
const PURE_BODIES: &[Option<&str>] = &[
    None,
    Some(""),
    Some("nil"),
    Some("s := s + 1"),
    Some("s := s + t. u := u + w"),
    Some("s := (self take: s) + 1"),
];

/// `(name, header, method keyword)`: every compiled method context, a class
/// method included (ADR 0130: it threads no family, only locals).
const PURE_CONTEXTS: &[(&str, &str, &str)] = &[
    (
        "actor",
        "Actor subclass: WhileBindingProbe\n  state: f = 0\n",
        "",
    ),
    (
        "value",
        "Value subclass: WhileBindingProbe\n  field: f = 0\n",
        "",
    ),
    (
        "test_case",
        "TestCase subclass: WhileBindingProbe\n  field: f = 0\n",
        "",
    ),
    ("object", "Object subclass: WhileBindingProbe\n", ""),
    ("class", "Object subclass: WhileBindingProbe\n", "class "),
];

/// BT-4602: where the loop sits (`W`): a method statement, and the nested and
/// value positions Phase 0 accepts a condition-writing loop in (a conditional
/// arm, a `do:` body, an outer loop's body, an assignment's value).
const TOP: &[&str] = &["W"];
const NESTED: &[&str] = &[
    "(s >= 0) ifTrue: [W]",
    "#(1, 2) do: [:e | W]",
    "k := 0. [k < 2] whileTrue: [k := k + 1. W]",
    "r := W",
];

/// BT-4602 review: the other block roles a loop can sit in, with a smaller
/// grid. Phase 0 accepts every row in every context, as it does the rows
/// above (checked in [`pure_condition_rows`]).
const MORE_NESTED: &[&str] = &[
    "[W] on: Error do: [:err | nil]",
    "[Error signal] on: Error do: [:err | W]",
    "[W] ensure: [nil]",
    "#(1, 2) inject: 0 into: [:a :e | W. a + e]",
    "#(1, 2) collect: [:e | W. e]",
    "(s >= 0) and: [W. true]",
    "(s < 0) or: [W. true]",
    "2 timesRepeat: [W]",
    "1 to: 2 do: [:i | W]",
    "[W] value",
    "0 match: [0 -> (W); _ -> nil]",
];

/// The first error the semantic checks report for `src`, if any.
fn rejection(src: &str) -> Option<String> {
    let (module, _) = parse(lex_with_eof(src));
    beamtalk_core::semantic_analysis::analyse(&module)
        .diagnostics
        .into_iter()
        .find(|d| d.severity == beamtalk_core::source_analysis::Severity::Error)
        .map(|d| d.message.to_string())
}

/// The BT-4602 rows: context x position x condition x body x loop, as
/// `(label, source)`.
fn condition_rows(
    positions: &[&str],
    conditions: &[&str],
    bodies: &[Option<&str>],
) -> Vec<(String, String)> {
    let mut out = Vec::new();
    for (name, header, kw) in PURE_CONTEXTS {
        for (p, position) in positions.iter().enumerate() {
            for (c, cond) in conditions.iter().enumerate() {
                for (b, body) in bodies.iter().enumerate() {
                    for (selector, cmp) in LOOPS {
                        let cond = cond.replace("CMP", cmp);
                        let send = match body {
                            None => selector.trim_end_matches(':').to_string(),
                            Some(body) => format!("{selector} [{body}]"),
                        };
                        let stmt = position.replace('W', &format!("[{cond}] {send}"));
                        let src = format!(
                            "{header}  {kw}take: v => v\n  {kw}probe =>\n    r := nil\n    s := 0\n    t := 0\n    u := 0\n    w := 0\n    x := 0\n    {stmt}\n    #(r, s, t, u, w, x)\n"
                        );
                        out.push((format!("{name} pos{p} cond{c} body{b} {send}"), src));
                    }
                }
            }
        }
    }
    out
}

/// Every BT-4602 row: the full condition x body grid as a method statement,
/// and a smaller grid (a direct write, a value-position write, a write in an
/// arm; the unary form, an empty and a writing body) in each nested position.
fn pure_condition_rows() -> Vec<(String, String)> {
    let mut rows = condition_rows(TOP, PURE_CONDITIONS, PURE_BODIES);
    let conditions = [PURE_CONDITIONS[0], PURE_CONDITIONS[1], PURE_CONDITIONS[4]];
    let bodies = [None, Some(""), Some("s := s + 1")];
    rows.extend(condition_rows(NESTED, &conditions, &bodies));
    rows.extend(condition_rows(
        MORE_NESTED,
        &[PURE_CONDITIONS[0], PURE_CONDITIONS[4]],
        &[None, Some("s := s + 1")],
    ));
    // Every row is one Phase 0 accepts: a correct position it starts
    // rejecting fails here rather than silently leaving the corpus.
    let rejected: Vec<String> = rows
        .iter()
        .filter_map(|(label, src)| rejection(src).map(|r| format!("{label}: {r}\n{src}")))
        .collect();
    assert!(
        rejected.is_empty(),
        "Phase 0 rejects {} BT-4602 row(s) it should accept:\n{}",
        rejected.len(),
        rejected.join("\n")
    );
    rows
}

/// BT-4602: a condition that writes an outer local is carried in every
/// compiled method context, in every position Phase 0 accepts the loop in and
/// whatever its literal body, with no state effect: every row lowers with a
/// silent verifier and compiles through `erlc`. The verifier sees both
/// lowerings: a `ConditionalLoop` must hand back every local its condition
/// writes, and a plain loop (the one that builds no node) must have a
/// condition that writes none (both `ConditionFamilyNotCarried`). The Phase 0
/// check accepts each row (`block_facts::while_condition_threads`);
/// `stdlib/test/bt4313unary_while_test.bt` runs representative ones.
#[test]
fn compiled_pure_condition_writes_are_carried() {
    let rows = pure_condition_rows();
    assert!(rows.len() >= 1200, "the corpus has thinned out");
    let failures: Vec<String> = rows
        .iter()
        .filter_map(|(label, src)| match super::verifier_report(src, None) {
            Some(r) => Some(format!("{label}: {r}\n{src}")),
            None => catch_unwind(AssertUnwindSafe(|| {
                assert_compiles_through_erlc("test", &codegen(src));
            }))
            .is_err()
            .then(|| format!("{label}: erlc\n{src}")),
        })
        .collect();
    assert!(
        failures.is_empty(),
        "{} compiled pure while-condition row(s) fail:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

/// BT-4602: with the pre-BT-4015 gate
/// ([`LocalRebindFault::ConditionWritesNotCarried`]) every row fails
/// `verify()`: as `ConditionFamilyNotCarried` (a `ConditionalLoop` that does not
/// hand the write back, or a plain loop whose condition writes a local), or
/// first as `UnthreadedLocalAssignment` for a write in value position. The
/// exceptions are the rows the gate decides nothing for, whose lowering the
/// fault leaves unchanged, both in an actor method where another carrier
/// threads the condition either way: a condition holding a `^` (its NLR frame)
/// and a body that sends to `self` (the synced `State` bracket, ADR 0137).
///
/// This is also the agreement check between the Phase 0 rule and codegen's
/// own recognizer: a condition write codegen's `loop_condition_written_locals`
/// did not see would neither be carried nor reported, so its row would compile
/// unchanged here and fail the test.
#[cfg(debug_assertions)]
#[test]
fn compiled_pure_condition_rows_report_the_pre_fix_lowering() {
    let fault = LocalRebindFault::ConditionWritesNotCarried;
    let mut caught = 0;
    let missed: Vec<String> = pure_condition_rows()
        .into_iter()
        .filter_map(
            |(label, src)| match super::verifier_report(&src, Some(fault)) {
                Some(r) if r.contains("ThreadedIr verify") => {
                    caught += 1;
                    None
                }
                Some(r) => Some(format!("{label}: {r}")),
                None if label.starts_with("actor")
                    && (src.contains('^') || src.contains("self take:")) =>
                {
                    let faulted = fault::with(fault, || codegen(&src));
                    (faulted != codegen(&src))
                        .then(|| format!("{label}: the fault changed the lowering unreported"))
                }
                None => Some(format!(
                    "{label}: the dropped condition write went unreported"
                )),
            },
        )
        .collect();
    assert!(
        missed.is_empty(),
        "{} row(s) where the verifier missed the dropped condition write:\n{}",
        missed.len(),
        missed.join("\n")
    );
    assert!(
        caught >= 1200,
        "only {caught} rows caught; the corpus has thinned out"
    );
}

/// BT-4584: the compiled-method twin of
/// [`repl_condition_destructure_rows_report_the_pre_fix_lowering`]. Every
/// destructuring condition row, in every method context that can carry it,
/// reports `ConditionWriteNotInMap` under the pre-fix lowering. In an actor
/// method the pre-fix lowering was not silent: its generic statement path
/// rejected the destructuring (`UnsupportedFeature`, "destructuring
/// assignment in expression position"), which is the only other outcome
/// accepted there.
#[cfg(debug_assertions)]
#[test]
fn compiled_condition_destructure_rows_report_the_pre_fix_lowering() {
    let mut missed = Vec::new();
    let mut checked = 0;
    for (name, header, has_field) in COMPILED_CONTEXTS {
        for (cond, writes_field) in COMPILED_CONDITIONS {
            if !(cond.contains(":= #(") || cond.contains(":= {")) || (*writes_field && !has_field) {
                continue;
            }
            let src = format!(
                "{header}{HELPERS}  probe =>\n    s := 0\n    t := 0\n    w := 0\n    x := 0\n    [{cond}] whileTrue: [s := s + 1]\n    #(s, t, w, x)\n"
            );
            // A context that cannot carry the condition's effect rejects the
            // write before lowering (no finding to look for).
            if super::verifier_report(&src, None).is_some() {
                continue;
            }
            checked += 1;
            let report =
                super::verifier_report(&src, Some(LocalRebindFault::ConditionDestructurePlain));
            let caught = report.as_deref().is_some_and(|r| {
                r.contains("ConditionWriteNotInMap")
                    || (*name == "actor"
                        && r.contains("destructuring assignment in expression position"))
            });
            if !caught {
                missed.push(format!("{name}: {cond}: {report:?}"));
            }
        }
    }
    assert!(
        checked >= 12,
        "only {checked} rows checked; the corpus has thinned out"
    );
    assert!(
        missed.is_empty(),
        "{} row(s) where the verifier missed the dropped destructuring write:\n{}",
        missed.len(),
        missed.join("\n")
    );
}
