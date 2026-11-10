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
//! [`compiled_pure_condition_writes_are_carried`] lowers those rows with a
//! silent verifier and through `erlc`;
//! [`compiled_pure_condition_rows_report_the_pre_fix_lowering`] shows the
//! verifier catches the dropped write.

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

/// The BT-4602 rows: context x condition x body x loop, each with whether its
/// body writes a local (so the loop is a `ConditionalLoop` the verifier sees
/// whatever the condition's gate answers).
fn pure_condition_rows() -> Vec<(String, String, bool)> {
    let mut out = Vec::new();
    for (name, header, kw) in PURE_CONTEXTS {
        for (c, cond) in PURE_CONDITIONS.iter().enumerate() {
            for (b, body) in PURE_BODIES.iter().enumerate() {
                for (selector, cmp) in LOOPS {
                    let cond = cond.replace("CMP", cmp);
                    let send = match body {
                        None => selector.trim_end_matches(':').to_string(),
                        Some(body) => format!("{selector} [{body}]"),
                    };
                    let src = format!(
                        "{header}  {kw}probe =>\n    s := 0\n    t := 0\n    u := 0\n    w := 0\n    x := 0\n    [{cond}] {send}\n    #(s, t, u, w, x)\n"
                    );
                    let body_writes = body.is_some_and(|b| b.contains(":="));
                    out.push((format!("{name} cond{c} body{b} {send}"), src, body_writes));
                }
            }
        }
    }
    out
}

/// BT-4602: a condition that writes an outer local is carried in every
/// compiled method context whatever its body, with no state effect: every row
/// lowers with a silent verifier (`ConditionFamilyNotCarried` names every local
/// the condition writes) and compiles through `erlc`. The Phase 0 check
/// accepts each (`block_facts::while_condition_writes_thread`);
/// `stdlib/test/bt4313unary_while_test.bt` runs them.
#[test]
fn compiled_pure_condition_writes_are_carried() {
    let rows = pure_condition_rows();
    assert!(rows.len() >= 400, "the corpus has thinned out");
    let failures: Vec<String> = rows
        .iter()
        .filter_map(|(label, src, _)| match super::verifier_report(src, None) {
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
/// ([`LocalRebindFault::ConditionWritesNotCarried`]) every row whose loop the
/// verifier sees (its body writes a local) fails `verify()`, mostly as
/// `ConditionFamilyNotCarried` (a condition write in value position can surface
/// first as `UnthreadedLocalAssignment`), unless the gate decides nothing for
/// it: a row whose lowering the fault leaves unchanged (an actor condition
/// holding a `^` is carried by its NLR frame either way). The verifier, not the
/// `BUnit` rows, is what catches a dropped condition write.
#[cfg(debug_assertions)]
#[test]
fn compiled_pure_condition_rows_report_the_pre_fix_lowering() {
    let fault = LocalRebindFault::ConditionWritesNotCarried;
    let mut caught = 0;
    let missed: Vec<String> = pure_condition_rows()
        .into_iter()
        .filter(|(_, _, body_writes)| *body_writes)
        .filter_map(
            |(label, src, _)| match super::verifier_report(&src, Some(fault)) {
                Some(r) if r.contains("ThreadedIr verify") => {
                    caught += 1;
                    None
                }
                Some(r) => Some(format!("{label}: {r}")),
                None => {
                    let faulted = fault::with(fault, || codegen(&src));
                    (faulted != codegen(&src))
                        .then(|| format!("{label}: the fault changed the lowering unreported"))
                }
            },
        )
        .collect();
    assert!(
        caught >= 150,
        "only {caught} rows caught; the corpus has thinned out"
    );
    assert!(
        missed.is_empty(),
        "{} row(s) where the verifier missed the dropped condition write:\n{}",
        missed.len(),
        missed.join("\n")
    );
}
