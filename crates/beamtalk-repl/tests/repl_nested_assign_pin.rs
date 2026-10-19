// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3799 / BT-3910 (ADR 0131): a REPL loop body whose conditional's
//! receiver assigns a binding (`(t := t + 1) > 1 ifTrue: [u := 9]`).
//!
//! A local assignment in value position is a producer (BT-3910, ADR 0131 §1): a
//! `LocalRebind` of the enclosing frame. Before it, the assignment never rebound
//! its local (BT-3860), and in this shape the REPL's `do:` fold body also bound a
//! `State` version tagged with the root frame inside the fold's own frame, which
//! the `ThreadedIr` verifier reported as a `BindFrameMismatch` in a debug
//! compiler (BT-3799). The input is accepted by the analyser and compiles in both
//! entry points without a verifier panic; the runtime answer (`t` = 2, `u` = 9
//! after the loop) is pinned in
//! `tests/repl-protocol/cases/adr0131_nested_assign.btscript`.

use beamtalk_core::semantic_analysis::{AnalysisContext, analyse_full};
use beamtalk_core::source_analysis::{Severity, lex_with_eof, parse};

const INPUT: &str = "#(1, 2) do: [:x | (t := t + 1) > 1 ifTrue: [u := 9]]";

#[test]
fn a_conditional_receiver_assignment_in_a_repl_loop_body_is_accepted() {
    let (module, _) = parse(lex_with_eof(INPUT));
    let diags = analyse_full(
        &module,
        AnalysisContext::default().with_known_vars(&["t", "u"]),
    )
    .diagnostics;
    let errors: Vec<_> = diags
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert!(errors.is_empty(), "{errors:?}");
}

/// The codegen half: neither the eval nor the trace entry point trips the debug
/// verifier on the shape (BT-3823, BT-3910).
#[cfg(debug_assertions)]
#[test]
fn compiles_without_a_verifier_panic() {
    use beamtalk_repl::codegen::{generate_repl_expressions, generate_repl_expressions_traced};
    use std::collections::HashMap;
    use std::panic::{AssertUnwindSafe, catch_unwind};

    let (module, diags) = parse(lex_with_eof(INPUT));
    assert!(diags.is_empty(), "{diags:?}");
    let expressions: Vec<_> = module
        .expressions
        .into_iter()
        .map(|s| s.expression)
        .collect();
    let eval = catch_unwind(AssertUnwindSafe(|| {
        generate_repl_expressions(&expressions, "repl_nested_assign")
    }));
    let trace = catch_unwind(AssertUnwindSafe(|| {
        generate_repl_expressions_traced(&expressions, INPUT, "repl_nested_assign", HashMap::new())
    }));
    assert!(
        matches!(eval, Ok(Ok(_))),
        "eval: {:?}",
        eval.map(Result::err)
    );
    assert!(
        matches!(trace, Ok(Ok(_))),
        "trace: {:?}",
        trace.map(Result::err)
    );
}
