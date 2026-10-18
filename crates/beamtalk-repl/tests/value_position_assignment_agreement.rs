// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0131 stopgap (BT-3864): the REPL half of the value-position local
//! assignment agreement (the method half is `beamtalk-codegen`'s
//! `value_position_assignment_agreement.rs`). Over the same corpus, as one
//! multi-statement input and as a single input after bindings made by earlier
//! inputs, through the eval and the trace entry points: `beamtalk-core`'s
//! `value_position_local_assignments` (with no method kind, a top-level
//! expression list) names an assignment exactly when the REPL compiles it to its
//! value only (`dropped_assignment_probe`), and the Phase 0 check reports, in
//! the REPL, every assignment it names.

use beamtalk_codegen::core_erlang::dropped_assignment_probe::{self, PRELUDE, corpus};
use beamtalk_core::ast::{Expression, Module};
use beamtalk_core::semantic_analysis::block_facts::value_position_local_assignments;
use beamtalk_core::semantic_analysis::{AnalysisContext, analyse_full};
use beamtalk_core::source_analysis::{DiagnosticCategory, Severity, Span, lex_with_eof, parse};
use beamtalk_repl::codegen::{generate_repl_expressions, generate_repl_expressions_traced};
use std::collections::{HashMap, HashSet};
use std::panic::{AssertUnwindSafe, catch_unwind};

/// The bindings earlier inputs made, for a corpus body entered on its own.
const KNOWN: &[&str] = &["t", "u", "s", "w", "x"];

/// The assignments the REPL drops compiling `expressions` through `entry`, or
/// `None` when the compile fails or panics (nothing is evaluated, so nothing
/// is silently dropped).
///
/// A single-statement input that is a bare assignment compiles the whole
/// statement as an expression, and the REPL binds the result to the name
/// itself (`generate_repl_expression`), so that hit drops nothing and is not
/// counted.
fn dropped(expressions: &[Expression], entry: impl FnOnce() -> bool) -> Option<HashSet<Span>> {
    let (compiled, spans) = dropped_assignment_probe::recording(|| {
        catch_unwind(AssertUnwindSafe(entry)).unwrap_or(false)
    });
    let bound: HashSet<Span> = expressions
        .iter()
        .filter(|e| {
            matches!(e, Expression::Assignment { target, .. }
                if matches!(target.as_ref(), Expression::Identifier(_)))
        })
        .map(Expression::span)
        .collect();
    compiled.then(|| spans.into_iter().filter(|s| !bound.contains(s)).collect())
}

#[test]
fn the_rejection_names_exactly_the_local_assignments_the_repl_drops() {
    let mut named = 0;
    let mut accepted = 0;
    let mut failures = Vec::new();
    for body in corpus() {
        let whole = format!("{PRELUDE}\n{body}");
        for (src, known) in [(whole.as_str(), &[][..]), (body.as_str(), KNOWN)] {
            let (module, parse_diags): (Module, _) = parse(lex_with_eof(src));
            assert!(parse_diags.is_empty(), "{src}: {parse_diags:?}");
            let expected: HashSet<Span> =
                value_position_local_assignments(&module.expressions, None)
                    .into_iter()
                    .map(|found| found.span)
                    .collect();
            let analysis = analyse_full(&module, AnalysisContext::default().with_known_vars(known));
            for span in &expected {
                let reported = analysis.diagnostics.iter().any(|d| {
                    d.span == *span
                        && matches!(
                            d.category,
                            Some(
                                DiagnosticCategory::ValuePositionLocalAssignment
                                    | DiagnosticCategory::UnmigratedLocalThreading
                                    // ADR 0133 (BT-3902): an assignment in an
                                    // escaping block is that error's alone.
                                    | DiagnosticCategory::Tier2BlockNoReturnChannel
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
            let expressions: Vec<Expression> = module
                .expressions
                .iter()
                .map(|s| s.expression.clone())
                .collect();
            let eval = dropped(&expressions, || {
                generate_repl_expressions(&expressions, "agree").is_ok()
            });
            let trace = dropped(&expressions, || {
                generate_repl_expressions_traced(&expressions, src, "agree", HashMap::new()).is_ok()
            });
            for (entry, dropped) in [("eval", eval), ("trace", trace)] {
                match dropped {
                    Some(dropped) if dropped == expected => {
                        if expected.is_empty() {
                            accepted += 1;
                        } else {
                            named += 1;
                        }
                    }
                    Some(dropped) => failures.push(format!(
                        "{entry}: the predicate names {expected:?} but the REPL drops {dropped:?}\n{src}"
                    )),
                    // BT-3799's shape trips the debug verifier.
                    None if !expected.is_empty() => {}
                    None => failures.push(format!(
                        "{entry}: an accepted input fails to compile\n{src}"
                    )),
                }
            }
        }
    }
    assert!(
        failures.is_empty(),
        "{} disagreements:\n{}",
        failures.len(),
        failures.join("\n---\n")
    );
    // Not vacuous: both directions compare many compiled inputs.
    assert!(named > 300, "only {named} rejected inputs compared");
    assert!(accepted > 100, "only {accepted} accepted inputs compared");
}
