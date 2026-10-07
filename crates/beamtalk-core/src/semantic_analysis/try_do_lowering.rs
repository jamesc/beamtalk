// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `Result tryDo:` lowering (BT-3718).
//!
//! **DDD Context:** Semantic Analysis
//!
//! `Result tryDo: [body]` is `on:do:` with a `Result` around each outcome:
//!
//! ```beamtalk
//! Result tryDo: [body]
//! // is
//! [Result ok: (body's last expression)] on: Exception do: [:e | Result error: e]
//! ```
//!
//! This pass rewrites a literal zero-argument block argument of
//! `Result tryDo:` into the right-hand form, after analysis and before
//! codegen, so codegen has one construct that owns a protected block and its
//! state threading. Nothing about a protected block's outer-local mutation is
//! decided at a second place: the compiled `on:do:` threads the block's
//! writes, takes the ADR 0130 §4 catch boundary (`snapshot`/`restore`) and
//! passes a `^` through, exactly as a written `on:do:` does.
//!
//! Without it the call is an ordinary send to a native function, and a block
//! that writes an outer local compiles to a `StateAcc` (Tier 2) closure the
//! native `tryDo:` cannot call ("expected a zero-arity block").
//!
//! The rewritten tree is for codegen only: type checking, lints and the
//! language service see the source as written, because this runs from
//! [`lower_module_for_codegen`](crate::semantic_analysis::lower_module_for_codegen).
//!
//! # Spans
//!
//! The block keeps its own span and every statement of it keeps its own, so
//! the analysis facts keyed by span still describe them. Every node the pass
//! creates carries the span of the `tryDo:` send it replaces.

use crate::ast::Expression;
use crate::ast::{
    Block, BlockParameter, ExpressionStatement, Identifier, KeywordPart, MessageSelector, Module,
};
use crate::ast_walker::walk_expression_mut;
use crate::span::Span;

/// The class `tryDo:` is sent to.
const RESULT_CLASS: &str = "Result";
/// The exception class the rewritten `on:do:` catches: every exception.
const CATCH_ALL_CLASS: &str = "Exception";
/// The handler parameter.
const HANDLER_PARAM: &str = "e";

/// Rewrites every `Result tryDo: [literal block]` in `module` to its
/// `on:do:` form (see the module doc). Idempotent: the result contains no
/// `tryDo:`.
pub fn apply_try_do_lowering(module: &mut Module) {
    let lower_seq = |seq: &mut [ExpressionStatement]| {
        for stmt in seq {
            walk_expression_mut(&mut stmt.expression, &mut |expr| lower_try_do(expr));
        }
    };
    lower_seq(&mut module.expressions);
    for class in &mut module.classes {
        for method in class
            .methods
            .iter_mut()
            .chain(class.class_methods.iter_mut())
        {
            lower_seq(&mut method.body);
        }
    }
    for standalone in &mut module.method_definitions {
        lower_seq(&mut standalone.method.body);
    }
}

/// Rewrites `expr` in place when it is `Result tryDo: [literal 0-arg block]`.
fn lower_try_do(expr: &mut Expression) {
    let Expression::MessageSend {
        receiver,
        selector,
        arguments,
        is_cast: false,
        span,
    } = expr
    else {
        return;
    };
    if !is_result_class(receiver) || selector.name() != "tryDo:" {
        return;
    }
    let [Expression::Block(block)] = arguments.as_mut_slice() else {
        return;
    };
    if !block.parameters.is_empty() {
        return;
    }
    let span = *span;
    let mut protected = Block::new(Vec::new(), std::mem::take(&mut block.body), block.span);
    wrap_outcome_in_ok(&mut protected, span);
    let handler = Block::new(
        vec![BlockParameter::new(HANDLER_PARAM, span)],
        vec![ExpressionStatement::bare(result_send(
            "error:",
            Expression::Identifier(Identifier::new(HANDLER_PARAM, span)),
            span,
        ))],
        span,
    );
    *expr = Expression::MessageSend {
        receiver: Box::new(Expression::Block(protected)),
        selector: MessageSelector::Keyword(vec![
            KeywordPart::new("on:", span),
            KeywordPart::new("do:", span),
        ]),
        arguments: vec![
            Expression::ClassReference {
                name: Identifier::new(CATCH_ALL_CLASS, span),
                package: None,
                span,
            },
            Expression::Block(handler),
        ],
        is_cast: false,
        span,
    };
}

/// Whether `receiver` is the stdlib class `Result`, unqualified.
fn is_result_class(receiver: &Expression) -> bool {
    matches!(
        receiver,
        Expression::ClassReference { name, package: None, .. } if name.name == RESULT_CLASS
    )
}

/// Wraps the block's value in `Result ok:`: its last expression becomes
/// `Result ok: (last)`, and an empty block becomes `Result ok: nil`. A last
/// statement that is a `^` is left alone, since it never produces the block's
/// value.
fn wrap_outcome_in_ok(block: &mut Block, span: Span) {
    match block.body.last_mut() {
        Some(last) if matches!(last.expression, Expression::Return { .. }) => {}
        Some(last) => {
            let value = std::mem::replace(&mut last.expression, nil_literal(span));
            let value_span = value.span();
            last.expression = result_send(
                "ok:",
                Expression::Parenthesized {
                    expression: Box::new(value),
                    span: value_span,
                },
                span,
            );
        }
        None => block.body.push(ExpressionStatement::bare(result_send(
            "ok:",
            nil_literal(span),
            span,
        ))),
    }
}

/// `Result <keyword> argument`.
fn result_send(keyword: &str, argument: Expression, span: Span) -> Expression {
    Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new(RESULT_CLASS, span),
            package: None,
            span,
        }),
        selector: MessageSelector::Keyword(vec![KeywordPart::new(keyword, span)]),
        arguments: vec![argument],
        is_cast: false,
        span,
    }
}

fn nil_literal(span: Span) -> Expression {
    Expression::Identifier(Identifier::new("nil", span))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::test_support::parse_bt;
    use crate::unparse::unparse_module;

    fn lowered(src: &str) -> String {
        let mut module = parse_bt(src);
        apply_try_do_lowering(&mut module);
        unparse_module(&module)
    }

    #[test]
    fn rewrites_a_literal_block_to_on_do_with_result_wrapping() {
        let out = lowered("Object subclass: Foo\n  run =>\n    Result tryDo: [1 + 2]\n");
        assert!(out.contains("on: Exception do:"), "{out}");
        assert!(out.contains("Result ok: (1 + 2)"), "{out}");
        assert!(out.contains("Result error: e"), "{out}");
        assert!(!out.contains("tryDo:"), "{out}");
    }

    #[test]
    fn an_empty_block_answers_ok_nil() {
        let out = lowered("Object subclass: Foo\n  run =>\n    Result tryDo: []\n");
        assert!(out.contains("Result ok: nil"), "{out}");
    }

    #[test]
    fn a_trailing_return_is_not_wrapped() {
        let out = lowered("Object subclass: Foo\n  run =>\n    Result tryDo: [^3]\n");
        assert!(out.contains("^3"), "{out}");
        assert!(!out.contains("Result ok: (^3)"), "{out}");
    }

    #[test]
    fn rewrites_nested_and_chained_sends() {
        let out = lowered(
            "Object subclass: Foo\n  run =>\n    (Result tryDo: [Result tryDo: [1]]) valueOr: 0\n",
        );
        assert!(!out.contains("tryDo:"), "{out}");
        assert_eq!(out.matches("on: Exception do:").count(), 2, "{out}");
    }

    #[test]
    fn leaves_a_non_literal_argument_and_other_receivers_alone() {
        let out = lowered(
            "Object subclass: Foo\n  run: b =>\n    Result tryDo: b\n  other =>\n    Other tryDo: [1]\n",
        );
        assert_eq!(out.matches("tryDo:").count(), 2, "{out}");
    }
}
