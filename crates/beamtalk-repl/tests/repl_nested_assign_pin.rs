// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3799 / BT-3860 (ADR 0131): a REPL loop body whose conditional's
//! receiver assigns a binding (`(t := t + 1) > 1 ifTrue: [u := 9]`).
//!
//! The REPL's `do:` fold body used to bind a `State` version tagged with the
//! root frame inside the fold's own frame, which the `ThreadedIr` verifier
//! reported as a `BindFrameMismatch` (a debug compiler panicked). Since BT-3823
//! the conditional is a producer of the fold body whose prelude binds in the
//! frame it is spliced into, so every entry point compiles it. A local
//! assignment in value position still never rebinds its local (BT-3860): the
//! answer is pinned at run time in
//! `tests/repl-protocol/cases/adr0131_nested_assign.btscript`.
//!
//! Debug builds only: a release compiler has no verifier panic to check for.
#![cfg(debug_assertions)]

use beamtalk_core::ast::Expression;
use beamtalk_core::source_analysis::{lex_with_eof, parse};
use beamtalk_repl::codegen::{generate_repl_expressions, generate_repl_expressions_traced};
use std::collections::HashMap;
use std::panic::{AssertUnwindSafe, catch_unwind};

const INPUT: &str = "#(1, 2) do: [:x | (t := t + 1) > 1 ifTrue: [u := 9]]";

fn statements(src: &str) -> Vec<Expression> {
    let (module, diags) = parse(lex_with_eof(src));
    assert!(diags.is_empty(), "{src}: {diags:?}");
    module
        .expressions
        .into_iter()
        .map(|s| s.expression)
        .collect()
}

/// The panic message of `f`, if it panicked.
fn panic_message(f: impl FnOnce()) -> Option<String> {
    let payload = catch_unwind(AssertUnwindSafe(f)).err()?;
    Some(
        payload
            .downcast_ref::<String>()
            .cloned()
            .or_else(|| payload.downcast_ref::<&str>().map(|s| (*s).to_string()))
            .unwrap_or_default(),
    )
}

#[test]
fn a_conditional_receiver_assignment_in_a_repl_loop_body_compiles() {
    let expressions = statements(INPUT);
    let eval = panic_message(|| {
        let _ = generate_repl_expressions(&expressions, "repl_nested_assign");
    });
    let trace = panic_message(|| {
        let _ = generate_repl_expressions_traced(
            &expressions,
            INPUT,
            "repl_nested_assign",
            HashMap::new(),
        );
    });
    for (entry, message) in [("eval", eval), ("trace", trace)] {
        assert!(message.is_none(), "{entry}: {message:?}");
    }
}
