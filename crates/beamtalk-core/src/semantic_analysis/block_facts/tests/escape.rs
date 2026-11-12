// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

use super::*;

// -- BT-3806: returned operands followed through their value -------------

fn module_body(src: &str) -> Vec<ExpressionStatement> {
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, diagnostics) = crate::source_analysis::parse(tokens);
    assert!(diagnostics.is_empty(), "{src}: {diagnostics:?}");
    module.expressions
}

fn span_text(src: &str, span: Span) -> String {
    src[span.start() as usize..span.end() as usize].to_string()
}

/// The source text of each block literal `escape_sites` reports as
/// returned, and of each one `escaping_blocks` reports, for `src`.
fn returned_blocks(src: &str) -> (Vec<String>, Vec<String>) {
    let body = module_body(src);
    let sites = escape_sites(&body)
        .into_iter()
        .filter(|&(span, shape)| {
            shape == EscapeShape::Returned && src[span.start() as usize..].starts_with('[')
        })
        .map(|(span, _)| span_text(src, span))
        .collect();
    let blocks = escaping_blocks(&body)
        .into_iter()
        .filter(|(_, shape)| *shape == EscapeShape::Returned)
        .map(|(block, _)| span_text(src, block.span))
        .collect();
    (sites, blocks)
}

/// `escape_sites` (ADR 0131 §6) follows a returned value through a local
/// assignment and the value arms of inlined constructs; `escaping_blocks`
/// (the census and the class-state-abroad lint) keeps taking it as
/// written, so their counts do not move.
#[test]
fn escape_sites_follow_returned_values_and_escaping_blocks_do_not() {
    for (src, followed) in [
        ("b := [1]", "[1]"),
        ("^b := [1]", "[1]"),
        ("^(b := [1])", "[1]"),
        ("c ifTrue: [[1]] ifFalse: [2]", "[1]"),
        ("c ifTrue: [2] ifFalse: [x := [1]]", "[1]"),
        ("c ifTrue: [c ifFalse: [[1]]]", "[1]"),
        ("x ifNil: [[1]] ifNotNil: [:v | v]", "[1]"),
        ("c and: [[1]]", "[1]"),
        ("[[1]] value", "[1]"),
        ("[[1]] on: Error do: [:e | 2]", "[1]"),
        ("[2] on: Error do: [:e | [1]]", "[1]"),
        ("[[1]] ensure: [2]", "[1]"),
        ("#a match: [#a -> [1]; _ -> 2]", "[1]"),
    ] {
        let (sites, blocks) = returned_blocks(src);
        assert_eq!(sites, vec![followed.to_string()], "{src}");
        assert!(blocks.is_empty(), "{src}: {blocks:?}");
    }
    // Not the construct's value: the cleanup of `ensure:`, a non-last arm
    // statement, the block a `value` send evaluates, a non-inlined send,
    // a value stored and not returned.
    for src in [
        "[2] ensure: [[1]]",
        "c ifTrue: [[1]. 2]",
        "b := [1] value",
        "c foo: [[1]]",
        "x := c ifTrue: [[1]]\nnil",
    ] {
        let (sites, blocks) = returned_blocks(src);
        assert!(sites.is_empty(), "{src}: {sites:?}");
        assert!(blocks.is_empty(), "{src}: {blocks:?}");
    }
    // As written, both agree.
    let (sites, blocks) = returned_blocks("^[1]");
    assert_eq!(sites, blocks);
    assert_eq!(sites, vec!["[1]".to_string()]);
}

#[test]
fn value_blocks_are_the_blocks_a_returned_expression_evaluates_to() {
    let src = "c ifTrue: [[1]] ifFalse: [d := [2]]";
    let expr = parse_first_expr(src);
    let found: Vec<String> = value_blocks(&expr)
        .into_iter()
        .map(|b| span_text(src, b.span))
        .collect();
    assert_eq!(found, vec!["[1]", "[2]"]);
    assert!(value_blocks(&parse_first_expr("[1] value")).is_empty());
}
