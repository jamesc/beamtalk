// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3799 / BT-3860 / BT-3864 (ADR 0131): a REPL loop body whose conditional's
//! receiver assigns a binding (`(t := t + 1) > 1 ifTrue: [u := 9]`).
//!
//! A local assignment in value position never rebinds its local (BT-3860). In
//! this shape the REPL's `do:` fold body also bound a `State` version tagged
//! with the root frame inside the fold's own frame, which the `ThreadedIr`
//! verifier reported as a `BindFrameMismatch` in a debug compiler. Since
//! BT-3823 the conditional is a producer of the fold body bound in that body's
//! frame, so codegen no longer trips the verifier (the same fix covers BT-3877,
//! pinned at runtime in `adr0131_local_rebind.btscript`); only the BT-3860
//! rejection below remains.
//!
//! Since BT-3864 the Phase 0 check rejects the input with a
//! `ValuePositionLocalAssignment` error, so the compiler port never reaches
//! codegen for it (pinned at runtime in
//! `tests/repl-protocol/cases/adr0131_nested_assign.btscript`).
//!
//! PIN-BUG BT-3860: the correct behaviour is no error and `t` = 2, `u` = 9
//! after the loop. The fix for BT-3860 deletes the rejection and turns these
//! into a test that the input compiles in every entry point.

use beamtalk_core::semantic_analysis::{AnalysisContext, analyse_full};
use beamtalk_core::source_analysis::{DiagnosticCategory, lex_with_eof, parse};

const INPUT: &str = "#(1, 2) do: [:x | (t := t + 1) > 1 ifTrue: [u := 9]]";

#[test]
fn a_conditional_receiver_assignment_in_a_repl_loop_body_is_a_compile_error() {
    let (module, _) = parse(lex_with_eof(INPUT));
    let diags = analyse_full(
        &module,
        AnalysisContext::default().with_known_vars(&["t", "u"]),
    )
    .diagnostics;
    let rejected: Vec<_> = diags
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::ValuePositionLocalAssignment))
        .collect();
    assert_eq!(
        rejected.len(),
        1,
        "BT-3860 is fixed? flip this PIN-BUG: {diags:?}"
    );
    let span = rejected[0].span;
    assert_eq!(
        &INPUT[span.start() as usize..span.end() as usize],
        "t := t + 1"
    );
}
