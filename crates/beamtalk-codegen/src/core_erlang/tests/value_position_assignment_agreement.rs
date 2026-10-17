// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0131 stopgap (BT-3864): the Phase 0 rejection of a value-position local
//! assignment agrees with codegen, both ways, in class, value-type and actor
//! methods. `beamtalk-core`'s `value_position_local_assignments` names an
//! assignment exactly when codegen compiles it to its value only and drops the
//! write (`dropped_assignment_probe`, the `Expression::Assignment` arm of
//! `generate_expression`), and the Phase 0 check reports every assignment it
//! names. The REPL half is
//! `beamtalk-repl/tests/value_position_assignment_agreement.rs`.
//!
//! The corpus ([`dropped_assignment_probe::corpus`]) places a local assignment
//! in every statement and value position, inside every kind of body that has
//! its own statement lowering. A program the predicate rejects is compiled
//! too (with the check bypassed), so both directions are compared; one that
//! another check rejects is skipped, because it is never compiled.

use crate::core_erlang::dropped_assignment_probe::{self, PRELUDE, corpus};
use crate::core_erlang::{CodegenOptions, generate_module_with_warnings};
use beamtalk_core::ast::{Expression, Module};
use beamtalk_core::semantic_analysis::block_facts::{
    RootMethodKind, value_position_local_assignments,
};
use beamtalk_core::semantic_analysis::{AnalysisContext, analyse_full};
use beamtalk_core::source_analysis::{DiagnosticCategory, Severity, Span, lex_with_eof, parse};
use std::collections::HashSet;
use std::panic::{AssertUnwindSafe, catch_unwind};

/// `body` as the body of a class, a value-type and an actor method.
fn sources(body: &str) -> [String; 3] {
    let mut indented = String::new();
    for line in format!("{PRELUDE}\n{body}").lines() {
        indented.push_str("    ");
        indented.push_str(line);
        indented.push('\n');
    }
    [
        format!("Object subclass: AgreeClass\n  class go =>\n{indented}"),
        format!("Value subclass: AgreeValue\n  go =>\n{indented}"),
        format!("Actor subclass: AgreeActor\n  state: n = 0\n\n  go =>\n{indented}"),
    ]
}

/// The predicate's spans over every method body of `module`.
fn predicate(module: &Module) -> HashSet<Span> {
    let mut out = HashSet::new();
    for class in &module.classes {
        let instance = if class.name.name == "AgreeActor" {
            RootMethodKind::Actor
        } else {
            RootMethodKind::ValueType
        };
        let methods = class.methods.iter().map(|m| (m, instance));
        let class_methods = class
            .class_methods
            .iter()
            .map(|m| (m, RootMethodKind::Class));
        for (method, kind) in methods.chain(class_methods) {
            out.extend(
                value_position_local_assignments(&method.body, Some(kind))
                    .into_iter()
                    .map(|found| found.span),
            );
        }
    }
    out
}

/// The assignments codegen drops compiling `module`, or `None` when the
/// compile fails or panics before it finishes (nothing is emitted, so nothing
/// is silently dropped).
///
/// A method's last statement is its value: when it is a bare local assignment,
/// the method returns before anything can read the local, so the class and
/// value-type lowerings compile only its value. That drops nothing, so it is
/// not counted.
fn dropped(module: &Module) -> Option<HashSet<Span>> {
    let (compiled, spans) = dropped_assignment_probe::recording(|| {
        catch_unwind(AssertUnwindSafe(|| {
            generate_module_with_warnings(module, CodegenOptions::new("agree")).is_ok()
        }))
        .unwrap_or(false)
    });
    let dead: HashSet<Span> = module
        .classes
        .iter()
        .flat_map(|c| c.methods.iter().chain(&c.class_methods))
        .filter_map(|m| match &m.body.last()?.expression {
            last @ Expression::Assignment { target, .. }
                if matches!(target.as_ref(), Expression::Identifier(_)) =>
            {
                Some(last.span())
            }
            _ => None,
        })
        .collect();
    compiled.then(|| spans.into_iter().filter(|s| !dead.contains(s)).collect())
}

#[test]
fn the_rejection_names_exactly_the_local_assignments_codegen_drops() {
    let mut named = 0;
    let mut accepted = 0;
    let mut failures = Vec::new();
    for body in corpus() {
        for src in sources(&body) {
            let (module, parse_diags) = parse(lex_with_eof(&src));
            assert!(parse_diags.is_empty(), "{src}: {parse_diags:?}");
            let expected = predicate(&module);
            let analysis = analyse_full(&module, AnalysisContext::default());
            // The Phase 0 check reports every assignment the predicate names
            // (a `match:` arm's write to an outer local keeps its own error).
            for span in &expected {
                let reported = analysis.diagnostics.iter().any(|d| {
                    d.span == *span
                        && matches!(
                            d.category,
                            Some(
                                DiagnosticCategory::ValuePositionLocalAssignment
                                    | DiagnosticCategory::UnmigratedLocalThreading
                            )
                        )
                });
                if !reported {
                    failures.push(format!("{span:?} is not reported\n{src}"));
                }
            }
            let rejected_otherwise = analysis.diagnostics.iter().any(|d| {
                d.severity == Severity::Error
                    && d.category != Some(DiagnosticCategory::ValuePositionLocalAssignment)
            });
            if rejected_otherwise {
                continue;
            }
            match dropped(&module) {
                Some(dropped) if dropped == expected => {
                    if expected.is_empty() {
                        accepted += 1;
                    } else {
                        named += 1;
                    }
                }
                Some(dropped) => failures.push(format!(
                    "the predicate names {expected:?} but codegen drops {dropped:?}\n{src}"
                )),
                // A compile that fails (BT-3799's shape trips the debug
                // verifier) drops nothing silently; an accepted program must
                // compile.
                None if !expected.is_empty() => {}
                None => failures.push(format!("an accepted program fails to compile\n{src}")),
            }
        }
    }
    assert!(
        failures.is_empty(),
        "{} disagreements:\n{}",
        failures.len(),
        failures.join("\n---\n")
    );
    // Not vacuous: both directions compare many compiled programs.
    assert!(named > 300, "only {named} rejected programs compared");
    assert!(accepted > 100, "only {accepted} accepted programs compared");
}
