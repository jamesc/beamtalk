// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4059 pin: analysis runs on the tree with redundant send-operand
//! parentheses peeled (`ast::normalize`), so a diagnostic about a
//! parenthesised operand lands on the operand's own span — narrower than the
//! `( … )` it was written in — and reads exactly like the bare form's.

use crate::semantic_analysis::{AnalysisContext, analyse_full};
use crate::source_analysis::Severity;
use crate::test_helpers::test_support::parse_bt;

/// Every error diagnostic of `src`, as (message, the source text it spans).
fn errors(src: &str) -> Vec<(String, String)> {
    let result = analyse_full(&parse_bt(src), AnalysisContext::default());
    result
        .diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .map(|d| {
            let text = &src[d.span.start() as usize..d.span.end() as usize];
            (d.message.to_string(), text.to_string())
        })
        .collect()
}

#[test]
fn diagnostic_on_a_parenthesised_block_argument_spans_the_block() {
    let wrapped = errors("Object subclass: A\n  run => #(1) inject: 0 into: ([:a | a])\n");
    let bare = errors("Object subclass: A\n  run => #(1) inject: 0 into: [:a | a]\n");
    assert_eq!(wrapped.len(), 1, "{wrapped:?}");
    assert_eq!(wrapped[0].1, "[:a | a]", "{wrapped:?}");
    assert_eq!(wrapped, bare);
}
