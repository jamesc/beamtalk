// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4244: nested REPL conditionals whose arms assign (the mutation path)
//! must not double the generated code per nesting level through their
//! non-Boolean fallbacks. At the REPL an arm's assignment writes a workspace
//! binding, so each fallback passes the arm as a raising stub and nothing
//! nested is lowered twice. Pins one fallback clause per level over depths
//! 1..=6. The actor, value-type and class-method shapes, whose fallbacks pass
//! real closures, are `nested_mutation_conditionals_grow_linearly` in
//! `beamtalk-codegen`'s `control_flow/conditionals.rs`.

use beamtalk_core::source_analysis::{lex_with_eof, parse};
use beamtalk_repl::codegen::generate_repl_expressions;

/// `x0 ifTrue: [t0 := 0. x1 ifTrue: [t1 := 0. ... t1]. t0]`, `depth` levels.
fn nested(depth: usize) -> String {
    let mut body = String::from("0");
    for level in (0..depth).rev() {
        body = format!("x{level} ifTrue: [t{level} := 0. {body}. t{level}]");
    }
    body
}

/// `(CondNoMatch count, code bytes)` of `nested(depth)` compiled for the REPL.
fn size(depth: usize) -> (usize, usize) {
    let src = nested(depth);
    let (module, diags) = parse(lex_with_eof(&src));
    assert!(diags.is_empty(), "{diags:?}");
    let expressions: Vec<_> = module
        .expressions
        .into_iter()
        .map(|s| s.expression)
        .collect();
    let code = generate_repl_expressions(&expressions, "repl_nested_cond_size")
        .unwrap_or_else(|e| panic!("depth {depth}: {e:?}"));
    (code.matches("CondNoMatch").count(), code.len())
}

#[test]
fn nested_mutation_conditionals_grow_linearly_in_the_repl() {
    let sizes: Vec<(usize, usize)> = (1..=6).map(size).collect();
    for (depth, &(clauses, _)) in (1..).zip(&sizes) {
        assert_eq!(clauses, depth, "one fallback clause per level: {sizes:?}");
    }
    let steps: Vec<usize> = sizes.windows(2).map(|p| p[1].1 - p[0].1).collect();
    for pair in steps.windows(2) {
        assert!(
            2 * pair[1] <= 3 * pair[0],
            "byte growth per level rises from {} to {} (doubling?): {sizes:?}",
            pair[0],
            pair[1]
        );
    }
}
