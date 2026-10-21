// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3910 (ADR 0131 §1, §4): the REPL half of the value-position local
//! assignment proof (the method half is `beamtalk-codegen`'s
//! `value_position_assignment_agreement.rs`). Over the same corpus, as one
//! multi-statement input and as a single input after bindings made by earlier
//! inputs, through the eval and the trace entry points: every value-position
//! local assignment is a producer, so the REPL records zero dropped
//! assignments (`dropped_assignment_probe`) and every input compiles.

use beamtalk_codegen::core_erlang::dropped_assignment_probe::{self, PRELUDE, corpus};
use beamtalk_core::ast::{Expression, Module};
use beamtalk_core::semantic_analysis::{AnalysisContext, analyse_full};
use beamtalk_core::source_analysis::{Severity, Span, lex_with_eof, parse};
use beamtalk_repl::codegen::{generate_repl_expressions, generate_repl_expressions_traced};
use std::collections::{HashMap, HashSet};
use std::panic::{AssertUnwindSafe, catch_unwind};

/// The bindings earlier inputs made, for a corpus body entered on its own.
const KNOWN: &[&str] = &["t", "u", "s", "w", "x"];

/// The assignments the REPL drops compiling `expressions` through `entry`, or
/// why the compile fails or panics (a verifier finding is a debug-build panic).
///
/// A single-statement input that is a bare assignment compiles the whole
/// statement as an expression, and the REPL binds the result to the name
/// itself (`generate_repl_expression`), so that hit drops nothing and is not
/// counted.
fn dropped(
    expressions: &[Expression],
    entry: impl FnOnce() -> Result<(), String>,
) -> Result<HashSet<Span>, String> {
    let (compiled, spans) = dropped_assignment_probe::recording(|| {
        catch_unwind(AssertUnwindSafe(entry)).unwrap_or_else(|payload| {
            Err(payload
                .downcast_ref::<String>()
                .cloned()
                .or_else(|| payload.downcast_ref::<&str>().map(|s| (*s).to_string()))
                .unwrap_or_default())
        })
    });
    let bound: HashSet<Span> = expressions
        .iter()
        .filter(|e| {
            matches!(e, Expression::Assignment { target, .. }
                if matches!(target.as_ref(), Expression::Identifier(_)))
        })
        .map(Expression::span)
        .collect();
    compiled.map(|()| spans.into_iter().filter(|s| !bound.contains(s)).collect())
}

#[test]
fn no_value_position_local_assignment_is_dropped_by_the_repl() {
    let mut compiled = 0;
    let mut failures = Vec::new();
    for body in corpus() {
        let whole = format!("{PRELUDE}\n{body}");
        for (src, known) in [(whole.as_str(), &[][..]), (body.as_str(), KNOWN)] {
            let (module, parse_diags): (Module, _) = parse(lex_with_eof(src));
            assert!(parse_diags.is_empty(), "{src}: {parse_diags:?}");
            let analysis = analyse_full(&module, AnalysisContext::default().with_known_vars(known));
            // An input another check rejects is never compiled.
            if analysis
                .diagnostics
                .iter()
                .any(|d| d.severity == Severity::Error)
            {
                continue;
            }
            let expressions: Vec<Expression> = module
                .expressions
                .iter()
                .map(|s| s.expression.clone())
                .collect();
            let eval = dropped(&expressions, || {
                generate_repl_expressions(&expressions, "agree")
                    .map(|_| ())
                    .map_err(|e| format!("{e:?}"))
            });
            let trace = dropped(&expressions, || {
                generate_repl_expressions_traced(&expressions, src, "agree", HashMap::new())
                    .map(|_| ())
                    .map_err(|e| format!("{e:?}"))
            });
            for (entry, dropped) in [("eval", eval), ("trace", trace)] {
                match dropped {
                    Ok(dropped) if dropped.is_empty() => compiled += 1,
                    Ok(dropped) => {
                        failures.push(format!("{entry}: the REPL drops {dropped:?}\n{src}"));
                    }
                    Err(why) => failures.push(format!("{entry}: fails to compile: {why}\n{src}")),
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
    // Not vacuous: many compiled inputs.
    assert!(compiled > 300, "only {compiled} inputs compared");
}
