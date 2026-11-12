// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4003 stale-return check: the two `^` shapes that answer wrong silently
//! ([`report_stale_returns`]).
//!
//! **DDD Context:** Semantic Analysis

use crate::ast::{BinarySegment, Block, Expression, ExpressionStatement};
use crate::semantic_analysis::block_facts::{
    LocalThreadingConstruct, LocalThreadingFamily, analyze_block, classify_in_place_returns,
    local_threading_construct,
};
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};
use ecow::EcoString;

use super::ensure_nlr::block_holds_return;
use super::types::MethodContext;

use super::types::MethodContext::Repl;

/// BT-4003: the two `^` shapes that answer wrong silently.
///
/// - **A `^` block that reads a stale local.** A block literal that is not run
///   in place (a closure: stored, passed to a method, ...) captures the values
///   of the locals it reads when it is created, so a `^` in it answers a
///   local's value at creation, not at the throw (measured on a BEAM in a
///   class, a value type, an actor and the REPL: `t := 0. b := [^t]. t := 5.
///   b value` answers 0). Such a block that holds a `^` is an error when it
///   reads a local that the method assigns after the block literal, or inside
///   the body of a loop or fold that creates the block (a later iteration's
///   write); at the REPL, when it reads any binding (a later input may assign
///   it). A block that reads only its own parameters and locals, or locals
///   written only before it, is measured right.
///
///   Conservative: every block that is not a [`local_threading_construct`]
///   body counts as escaping, consistent with ADR 0133 (a block passed to a
///   user method escapes). So `t := 0. self withFallback: [^t]. t := 5` is
///   rejected even when `withFallback:` runs the block before it returns and
///   the `^` would answer right; at the REPL, so is any such block that reads
///   a workspace binding. This cannot be narrowed with `escape_sites`, which
///   does not count a block passed to a method: `b := self keep: [^t]. t :=
///   5. b value` hands the block back and answers the stale 0 (BT-4275, closed
///   won't-do). Telling `withFallback:` from `keep:` would need a per-callee
///   "does not retain its block" fact.
/// - **A REPL `^` that no REPL boundary catches**
///   ([`classify_in_place_returns`]). BT-4288 / BT-4312: a `^` run in place
///   (at the root, in a conditional arm, an evaluated or protected block, a
///   loop or fold body) ends the input with its value, as in a method, and
///   the bindings written before it keep their values (codegen's REPL eval
///   boundary). Two shapes stay rejected. A loop held in a closure (`b :=
///   [#(1, 2) do: [:x | ^x]]`) may run in a later input, after the eval that
///   created it has ended: its `^` answers its value inside the loop, which
///   ignores it and runs on (BT-4003). And a `^` run in place inside the
///   value of a top-level assignment: in a method `r := xs do: [:x | ^x]`
///   and `r := c ifTrue: [^3]` never assign `r`, but the workspace binds a
///   single-statement assignment's variable to the input's value, so `r`
///   would be assigned the `^`'s value.
pub(super) fn report_stale_returns(
    context: MethodContext,
    params: &[EcoString],
    body: &[ExpressionStatement],
    diagnostics: &mut Vec<Diagnostic>,
) {
    let mut assigned: Vec<(EcoString, Span)> = Vec::new();
    for stmt in body {
        crate::ast_walker::walk_expression(&stmt.expression, &mut |e| match e {
            Expression::Assignment { target, .. } => {
                if let Expression::Identifier(id) = target.as_ref() {
                    assigned.push((id.name.clone(), e.span()));
                }
            }
            Expression::DestructureAssignment { pattern, .. } => {
                let (ids, _) = crate::semantic_analysis::extract_pattern_bindings(pattern);
                assigned.extend(ids.into_iter().map(|id| (id.name, e.span())));
            }
            _ => {}
        });
    }
    if context == Repl {
        report_repl_closure_loop_returns(body, diagnostics);
        report_repl_assigned_returns(body, diagnostics);
    }
    let mut scan = StaleReturnScan {
        context,
        bindings: params,
        assigned,
        loops: Vec::new(),
        diagnostics,
    };
    for stmt in body {
        scan.expr(&stmt.expression);
    }
}

/// BT-4003 / BT-4288: a `^` in a REPL loop or fold body held in a closure
/// ([`InPlaceReturns::in_closure`](crate::semantic_analysis::block_facts::InPlaceReturns)).
/// See [`report_stale_returns`].
fn report_repl_closure_loop_returns(
    body: &[ExpressionStatement],
    diagnostics: &mut Vec<Diagnostic>,
) {
    let returns = classify_in_place_returns(body.iter().map(|s| &s.expression));
    for span in returns.in_closure {
        diagnostics.push(
            Diagnostic::error(
                "a non-local return (`^`) in the body of a loop or collection operation \
                 inside a block that is not run where it is written is not supported in a \
                 top-level expression: the loop would ignore it and run on (BT-4003)",
                span,
            )
            .with_hint(
                "answer the value with `detect:ifNone:` or `inject:into:`, or move the \
                 loop into a method and return from there",
            )
            .with_category(DiagnosticCategory::UnthreadedLocalWrite),
        );
    }
}

/// BT-4288 / BT-4312: a `^` run in place
/// ([`InPlaceReturns::in_place`](crate::semantic_analysis::block_facts::InPlaceReturns))
/// inside the value of a top-level REPL assignment. See
/// [`report_stale_returns`].
fn report_repl_assigned_returns(body: &[ExpressionStatement], diagnostics: &mut Vec<Diagnostic>) {
    for stmt in body {
        let (value, what) = match &stmt.expression {
            Expression::Assignment { target, value, .. } => match target.as_ref() {
                Expression::Identifier(id) => (value, format!("`{}`", id.name)),
                _ => continue,
            },
            Expression::DestructureAssignment { value, .. } => {
                (value, "the destructured bindings".to_string())
            }
            _ => continue,
        };
        for span in classify_in_place_returns(std::iter::once(value.as_ref())).in_place {
            diagnostics.push(
                Diagnostic::error(
                    format!(
                        "a non-local return (`^`) ends the input with its value, so the \
                         assignment to {what} never completes; it is not supported in the \
                         value of a top-level assignment (BT-4312)"
                    ),
                    span,
                )
                .with_hint(
                    "drop the `^` and assign the value (in a loop or collection operation, \
                     answer it with `detect:ifNone:` or `inject:into:`), or leave the \
                     assignment out and let the `^` answer the input",
                )
                .with_category(DiagnosticCategory::UnthreadedLocalWrite),
            );
        }
    }
}

/// The walk of [`report_stale_returns`].
struct StaleReturnScan<'a> {
    context: MethodContext,
    /// At the REPL, the bindings of earlier inputs.
    bindings: &'a [EcoString],
    /// Every local assignment of the body, with its span.
    assigned: Vec<(EcoString, Span)>,
    /// The spans of the enclosing loop and fold bodies run in place.
    loops: Vec<Span>,
    diagnostics: &'a mut Vec<Diagnostic>,
}

impl StaleReturnScan<'_> {
    fn body(&mut self, block: &Block) {
        for stmt in &block.body {
            self.expr(&stmt.expression);
        }
    }

    /// A block literal that is not run in place.
    fn closure(&mut self, block: &Block) {
        if block_holds_return(block) {
            let mut reads: Vec<String> = analyze_block(block).captured_reads.into_iter().collect();
            reads.sort();
            if let Some(name) = reads.into_iter().find(|n| self.is_stale(n, block)) {
                let (what, why) = if self.context == Repl {
                    (
                        "the binding",
                        "a later assignment of it is not seen by the block",
                    )
                } else {
                    (
                        "the local",
                        "the method assigns it after the block is created",
                    )
                };
                self.diagnostics.push(
                    Diagnostic::error(
                        format!(
                            "a block that returns non-locally (`^`) and is not run where it is \
                             written reads {what} `{name}`, but {why}: the block keeps the value \
                             `{name}` had when it was created, so if it runs later its `^` may \
                             answer a stale `{name}`. This is rejected conservatively because \
                             the block escapes, even where it is in fact run before the \
                             assignment (BT-4003)"
                        ),
                        block.span,
                    )
                    .with_hint(format!(
                        "pass the current value in as a block argument (`[:v | ^v] value: \
                         {name}`), or write the block where it is run"
                    ))
                    .with_category(DiagnosticCategory::UnthreadedLocalWrite),
                );
            }
        }
        self.body(block);
    }

    fn is_stale(&self, name: &str, block: &Block) -> bool {
        let inside = |outer: Span, s: Span| outer.start() <= s.start() && s.end() <= outer.end();
        if self.context == Repl {
            return self.bindings.iter().any(|b| b == name)
                || self.assigned.iter().any(|(n, _)| n == name);
        }
        self.assigned.iter().any(|(n, s)| {
            n == name
                && !inside(block.span, *s)
                && (s.start() >= block.span.end() || self.loops.iter().any(|l| inside(*l, *s)))
        })
    }

    #[allow(clippy::too_many_lines)] // one arm per expression kind
    fn expr(&mut self, expr: &Expression) {
        match expr {
            Expression::Return { value, .. } => {
                self.expr(value);
            }
            Expression::MessageSend {
                receiver,
                arguments,
                ..
            } => {
                let construct =
                    local_threading_construct(expr).filter(LocalThreadingConstruct::runs_in_place);
                for operand in std::iter::once(receiver.as_ref()).chain(arguments) {
                    let Expression::Block(b) = operand else {
                        self.expr(operand);
                        continue;
                    };
                    match &construct {
                        Some(c) if c.blocks.iter().any(|blk| std::ptr::eq(*blk, b)) => {
                            if matches!(
                                c.family,
                                LocalThreadingFamily::Loop | LocalThreadingFamily::Fold
                            ) {
                                self.loops.push(b.span);
                                self.body(b);
                                self.loops.pop();
                            } else {
                                self.body(b);
                            }
                        }
                        _ => self.closure(b),
                    }
                }
            }
            Expression::Block(b) => self.closure(b),
            Expression::Parenthesized { expression, .. } => self.expr(expression),
            Expression::Assignment { target, value, .. } => {
                self.expr(target);
                self.expr(value);
            }
            Expression::DestructureAssignment { value, .. } => self.expr(value),
            Expression::Cascade {
                receiver, messages, ..
            } => {
                self.expr(receiver);
                for msg in messages {
                    for arg in &msg.arguments {
                        self.expr(arg);
                    }
                }
            }
            Expression::Match { value, arms, .. } => {
                self.expr(value);
                for arm in arms {
                    if let Some(guard) = &arm.guard {
                        self.expr(guard);
                    }
                    self.expr(&arm.body);
                }
            }
            Expression::FieldAccess { receiver, .. } => self.expr(receiver),
            Expression::MapLiteral { pairs, .. } => {
                for pair in pairs {
                    self.expr(&pair.key);
                    self.expr(&pair.value);
                }
            }
            Expression::ListLiteral { elements, tail, .. } => {
                for e in elements.iter().chain(tail.as_deref()) {
                    self.expr(e);
                }
            }
            Expression::ArrayLiteral { elements, .. }
            | Expression::TupleLiteral { elements, .. } => {
                for e in elements {
                    self.expr(e);
                }
            }
            Expression::StringInterpolation { segments, .. } => {
                for segment in segments {
                    if let crate::ast::StringSegment::Interpolation(e) = segment {
                        self.expr(e);
                    }
                }
            }
            Expression::BinaryConstruction { segments, .. } => {
                for e in segments.iter().flat_map(BinarySegment::sub_expressions) {
                    self.expr(e);
                }
            }
            Expression::Identifier(_)
            | Expression::Literal(..)
            | Expression::ClassReference { .. }
            | Expression::Super(_)
            | Expression::Primitive { .. }
            | Expression::ExpectDirective { .. }
            | Expression::Spread { .. }
            | Expression::Error { .. } => {}
        }
    }
}
