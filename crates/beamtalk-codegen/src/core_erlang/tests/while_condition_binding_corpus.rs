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
fn repl_report(src: &str, fault_on: Option<LocalRebindFault>) -> Option<String> {
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
