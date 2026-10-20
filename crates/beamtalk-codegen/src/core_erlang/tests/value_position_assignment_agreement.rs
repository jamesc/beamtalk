// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3910 (ADR 0131 §1, §4): every value-position local assignment is a
//! producer, in class, value-type and actor methods. `dropped_assignment_probe`
//! records the local assignments the `Expression::Assignment` arm of
//! `generate_expression` compiles to their value only (dropping the write,
//! BT-3860), and the arm reports them as
//! `VerifyError::UnthreadedLocalAssignment` (a debug-build panic). The corpus
//! ([`dropped_assignment_probe::corpus`]) places a local assignment in every
//! statement and value position, inside every kind of body that has its own
//! statement lowering, and the proof is that it records zero hits and every
//! program compiles. The REPL half is
//! `beamtalk-repl/tests/value_position_assignment_agreement.rs`.

use crate::core_erlang::dropped_assignment_probe::{self, PRELUDE, corpus};
use crate::core_erlang::{CodegenOptions, generate_module_with_warnings};
use beamtalk_core::ast::{Expression, Module};
use beamtalk_core::semantic_analysis::{AnalysisContext, analyse_full};
use beamtalk_core::source_analysis::{Severity, Span, lex_with_eof, parse};
use std::collections::HashSet;
use std::panic::{AssertUnwindSafe, catch_unwind};

/// `body` as the body of a class, a value-type and an actor method.
fn sources(body: &str) -> [String; 4] {
    let mut indented = String::new();
    for line in format!("{PRELUDE}\n{body}").lines() {
        indented.push_str("    ");
        indented.push_str(line);
        indented.push('\n');
    }
    // The legacy module-level dispatch: a top-level `name := [block]` binding.
    let legacy = format!("foo := [:a |\n{indented}]\n");
    [
        format!("Object subclass: AgreeClass\n  class go =>\n{indented}"),
        format!("Value subclass: AgreeValue\n  go =>\n{indented}"),
        format!("Actor subclass: AgreeActor\n  state: n = 0\n\n  go =>\n{indented}"),
        legacy,
    ]
}

/// The assignments codegen drops compiling `module`, or why the compile fails
/// or panics (a verifier finding is a debug-build panic).
///
/// A method's last statement is its value: when it is a bare local assignment,
/// the method returns before anything can read the local, so the class and
/// value-type lowerings compile only its value. That drops nothing, so it is
/// not counted.
fn dropped(module: &Module) -> Result<HashSet<Span>, String> {
    let (compiled, spans) = dropped_assignment_probe::recording(|| {
        catch_unwind(AssertUnwindSafe(|| {
            generate_module_with_warnings(module, CodegenOptions::new("agree"))
                .map(|_| ())
                .map_err(|e| format!("{e:?}"))
        }))
        .unwrap_or_else(|payload| {
            Err(payload
                .downcast_ref::<String>()
                .cloned()
                .or_else(|| payload.downcast_ref::<&str>().map(|s| (*s).to_string()))
                .unwrap_or_default())
        })
    });
    let dead: HashSet<Span> = module
        .classes
        .iter()
        .flat_map(|c| c.methods.iter().chain(&c.class_methods))
        .filter_map(|m| match m.body.last()?.expression.unwrap_parens() {
            last @ Expression::Assignment { target, .. }
                if matches!(target.as_ref(), Expression::Identifier(_)) =>
            {
                Some(last.span())
            }
            _ => None,
        })
        .chain(legacy_block_values(module))
        .collect();
    compiled.map(|()| spans.into_iter().filter(|s| !dead.contains(s)).collect())
}

/// The last statement of each top-level `name := [block]` of `module` (a legacy
/// workspace block body): its value, which nothing reads a local after.
fn legacy_block_values(module: &Module) -> Vec<Span> {
    module
        .expressions
        .iter()
        .filter_map(|s| match &s.expression {
            Expression::Assignment { value, .. } => match value.as_ref() {
                Expression::Block(block) => match block.body.last()?.expression.unwrap_parens() {
                    last @ Expression::Assignment { target, .. }
                        if matches!(target.as_ref(), Expression::Identifier(_)) =>
                    {
                        Some(last.span())
                    }
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        })
        .collect()
}

/// The method contexts of [`sources`], in its order: one test each below
/// (`per_family_tests!`, BT-3888).
const CONTEXTS: &[&str] = &["class", "value type", "actor", "legacy"];

/// The families of [`CONTEXTS`]: one context each.
const CONTEXT_FAMILIES: &[std::ops::Range<usize>] = &[0..1, 1..2, 2..3, 3..4];

/// The whole corpus in the method contexts named: the whole proof is the
/// per-context tests below.
fn no_value_position_local_assignment_is_dropped(contexts: &[&str]) {
    assert_eq!(sources("").len(), CONTEXTS.len(), "one source per context");
    let mut compiled = 0;
    let mut failures = Vec::new();
    for body in corpus() {
        let all = sources(&body);
        for context in contexts {
            let src = &all[CONTEXTS
                .iter()
                .position(|c| c == context)
                .expect("a context of CONTEXTS")];
            let (module, parse_diags) = parse(lex_with_eof(src));
            assert!(parse_diags.is_empty(), "{src}: {parse_diags:?}");
            let analysis = analyse_full(&module, AnalysisContext::default());
            // A program another check rejects (a `match:` arm's write to an
            // outer local, an assignment in an escaping block) is never
            // compiled.
            if analysis
                .diagnostics
                .iter()
                .any(|d| d.severity == Severity::Error)
            {
                continue;
            }
            match dropped(&module) {
                Ok(dropped) if dropped.is_empty() => compiled += 1,
                Ok(dropped) => failures.push(format!("codegen drops {dropped:?}\n{src}")),
                Err(why) => failures.push(format!("fails to compile: {why}\n{src}")),
            }
        }
    }
    assert!(
        failures.is_empty(),
        "{} disagreements:\n{}",
        failures.len(),
        failures.join("\n---\n")
    );
    // Not vacuous: many compiled programs in every context.
    assert!(
        compiled > 100,
        "only {compiled} programs compiled in {contexts:?}"
    );
}

beamtalk_core::per_family_tests! {
    list = CONTEXTS,
    families = CONTEXT_FAMILIES,
    check = no_value_position_local_assignment_is_dropped,
    partition = the_value_position_assignment_agreement_tests_cover_every_context,
    no_value_position_local_assignment_is_dropped_in_class_methods => 0,
    no_value_position_local_assignment_is_dropped_in_value_type_methods => 1,
    no_value_position_local_assignment_is_dropped_in_actor_methods => 2,
    no_value_position_local_assignment_is_dropped_in_legacy_module_bodies => 3,
}
