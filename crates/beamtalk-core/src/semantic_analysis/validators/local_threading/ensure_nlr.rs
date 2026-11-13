// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0133 `ensure:` cleanup check: a cleanup that reads a local made stale by
//! a `^` or an error in the protected block ([`report_nlr_out_of_ensure`]).
//!
//! **DDD Context:** Semantic Analysis

use std::collections::BTreeSet;

use crate::ast::{BinarySegment, Block, Expression, ExpressionStatement, MessageSelector};
use crate::semantic_analysis::block_facts::{
    LocalThreadingConstruct, LocalThreadingFamily, analyze_block, local_threading_construct,
    outer_local_writes,
};
use crate::source_analysis::{Diagnostic, DiagnosticCategory};
use ecow::EcoString;

use super::types::MethodContext;

/// BT-3910 / BT-3947 / BT-3993 / BT-4003: an `ensure:` cleanup may not read a
/// local its protected block writes when the protected block can leave
/// abruptly after the write, by a non-local return or (BT-4003) by an error.
/// This is a compile error by design (ADR 0133, Consequences): seeing the new
/// value would need a runtime mechanism that carries the protected block's
/// locals out with the throw, which was rejected (BT-4063). The `^` is a throw; the cleanup runs from the `catch`
/// arm over the entry `StateAcc`, with no `LocalRebind` of the protected body's
/// writes, so the cleanup would read the old value (the correct answer is the
/// new one). It makes no difference whether the write is the returned value
/// (`^t := e`) or a statement before the `^`, whether the `^` sits directly in
/// the protected block or in a block literal nested in it, or whether the write
/// is a statement or a value. Without a `^` in the protected block, or with a
/// cleanup that does not read what it writes, the write is seen (ADR 0131 §1).
/// An `on:do:` protected body has no such position: a `^` passes through its
/// catch untouched and no handler runs.
///
/// BT-3993: the throw can also come from a block value created elsewhere
/// (measured on a BEAM in a class, a value type and an actor: all read the old
/// `t`). The static view cannot tell which value is a block, so it is fully
/// conservative ([`nlr_cause`]): any `value`-family send to a receiver that is
/// not a block literal analysed in place (a call result and a collection
/// element included), and any non-constant local, parameter or field (parentheses
/// unwrapped, cascades included) used as the receiver or an argument of a
/// non-binary send, counts as a possible non-local return. Such a protected
/// block is rejected when its cleanup reads a write of it, even if no block
/// returns. Residuals (BT-3950): a self-send whose method evaluates a stored
/// `^` block, and a block reached as a call result or collection element that
/// is passed as an argument or sent a non-`value` message, are not seen.
///
/// BT-4003: an error is a throw too, and the cleanup that runs on it reads the
/// entry locals (measured on a BEAM in a class, a value type, an actor and the
/// REPL: `[[t := 9. 1 / 0] ensure: [rec record: t]] on: Error do: [...]`
/// records 0). Without a `^` cause, a protected block is rejected when
/// something that may raise ([`raise_after_write`]: any send, a destructuring
/// match, a `match:` or an interpolation) runs after a write the cleanup reads,
/// in evaluation order, a loop or fold body counting twice (its next iteration
/// runs after the write). A write that is the last thing the protected block
/// does, or one only preceded by what may raise, is seen (measured right).
///
/// Every local the cleanup reads counts, except one the cleanup binds itself
/// before reading it (its own first assignment): a cleanup that binds its own
/// `x` (`[x := 5. ^x] ensure: [x := 0. x]`) reads that, not the protected
/// block's.
///
/// The one recognizer of this rule: `beamtalk lint`'s `dead_returned_assignment`
/// skips a `^t := e` inside the span of this error rather than re-deriving it.
pub(super) fn report_nlr_out_of_ensure(
    context: MethodContext,
    params: &[EcoString],
    body: &[ExpressionStatement],
    diagnostics: &mut Vec<Diagnostic>,
) {
    // Which locals only ever hold a literal (never a block): every other
    // binding site (a computed assignment, a block parameter, a destructuring
    // or `match:` pattern variable) makes a name "other".
    let mut literal_bound: BTreeSet<EcoString> = BTreeSet::new();
    let mut other_bound: BTreeSet<EcoString> = BTreeSet::new();
    for stmt in body {
        crate::ast_walker::walk_expression(&stmt.expression, &mut |e| match e {
            Expression::Block(b) => {
                other_bound.extend(b.parameters.iter().map(|p| p.name.clone()));
            }
            Expression::Assignment { target, value, .. } => {
                if let Expression::Identifier(id) = target.as_ref() {
                    if matches!(value.as_ref(), Expression::Literal(..)) {
                        literal_bound.insert(id.name.clone());
                    } else {
                        other_bound.insert(id.name.clone());
                    }
                }
            }
            Expression::DestructureAssignment { pattern, .. } => {
                other_bound.extend(
                    crate::semantic_analysis::extract_pattern_bindings(pattern)
                        .0
                        .into_iter()
                        .map(|i| i.name),
                );
            }
            Expression::Match { arms, .. } => {
                for arm in arms {
                    other_bound.extend(
                        crate::semantic_analysis::extract_match_arm_bindings(&arm.pattern)
                            .0
                            .into_iter()
                            .map(|i| i.name),
                    );
                }
            }
            _ => {}
        });
    }
    let sources = NlrSources {
        constant_locals: literal_bound
            .difference(&other_bound)
            .filter(|n| !params.contains(n))
            .cloned()
            .collect(),
    };
    for stmt in body {
        crate::ast_walker::walk_expression(&stmt.expression, &mut |e| {
            if let Expression::MessageSend {
                receiver,
                selector: MessageSelector::Keyword(parts),
                arguments,
                ..
            } = e
                && parts.len() == 1
                && parts[0].keyword == "ensure:"
                && let Expression::Block(protected) = receiver.as_ref()
                && let [Expression::Block(cleanup)] = arguments.as_slice()
                && let Some(dropped) = nlr_dropped_local(protected, cleanup, &sources)
            {
                let EnsureDroppedLocal { name, cause, exit } = dropped;
                let fix = match exit {
                    AbruptExit::Return => format!(
                        "assign `{name}` and return after the `ensure:`, or keep the \
                             non-local return out of the protected block"
                    ),
                    AbruptExit::Error => format!(
                        "assign `{name}` after the `ensure:`, or move what may raise \
                             before the assignment (the check is conservative: anything after \
                             the write that may raise counts, even inside an `on:do:` that \
                             catches it)"
                    ),
                };
                diagnostics.push(
                    Diagnostic::error(
                        format!(
                            "an `ensure:` protected block that writes `{name}` {cause}, and \
                                 its cleanup reads `{name}`: in {context} the cleanup would not \
                                 see the new `{name}` (ADR 0133)"
                        ),
                        protected.span,
                    )
                    .with_hint(format!(
                        "{fix}; or do not read `{name}` in the cleanup (an `ensure:` \
                             cleanup cannot read a local its protected block writes when the \
                             block can exit early)"
                    ))
                    .with_category(DiagnosticCategory::UnthreadedLocalWrite),
                );
            }
        });
    }
}

/// What the conservative reading of "may return non-locally" needs to know
/// about one method (see [`report_nlr_out_of_ensure`]).
struct NlrSources {
    /// Locals that only ever hold a literal: never a block, so passing one on
    /// cannot hand a block over.
    constant_locals: BTreeSet<EcoString>,
}

pub(super) fn block_holds_return(block: &Block) -> bool {
    let mut found = false;
    for stmt in &block.body {
        crate::ast_walker::walk_expression(&stmt.expression, &mut |e| {
            found |= matches!(e, Expression::Return { .. });
        });
    }
    found
}

/// What in `protected` may throw a non-local return: a `^` written in it (or in
/// a block literal nested in it), or, since the static view cannot tell which
/// block a value is, any evaluation of a block that is not a literal analysed
/// in place: a `value`-family send to anything but a block literal (a local, a
/// parameter, a field, a call result, a collection element, ...), or a
/// non-literal value (a local that is not a constant, a parameter, a field)
/// passed as an argument of a keyword send, which may hand a block over to be
/// evaluated. Binary operands are not block hand-overs. `None` when nothing
/// does.
fn nlr_cause(protected: &Block, sources: &NlrSources) -> Option<String> {
    if block_holds_return(protected) {
        return Some("holds a non-local return (`^`)".to_string());
    }
    // A local, parameter or field (parentheses unwrapped) that may hold a
    // block: anything but `self`/`super`, the constants and a local that only
    // ever holds a literal.
    let blockish = |e: &Expression| -> Option<String> {
        match e.unwrap_parens() {
            Expression::Identifier(id)
                if !matches!(
                    id.name.as_str(),
                    "self" | "super" | "nil" | "true" | "false"
                ) && !sources.constant_locals.contains(&id.name) =>
            {
                Some(id.name.to_string())
            }
            Expression::FieldAccess { field, .. } => Some(field.name.to_string()),
            _ => None,
        }
    };
    let check_send = |receiver: &Expression,
                      selector: &MessageSelector,
                      args: &[Expression]|
     -> Option<String> {
        if selector.is_block_invocation() {
            if matches!(receiver.unwrap_parens(), Expression::Block(_)) {
                return None;
            }
            return Some("may return non-locally (it evaluates a block with `value`)".to_string());
        }
        // Binary operands are not block hand-overs.
        if matches!(selector, MessageSelector::Binary(_)) {
            return None;
        }
        std::iter::once(receiver)
            .chain(args)
            .find_map(&blockish)
            .map(|name| format!("may return non-locally (`{name}` may be a block it runs)"))
    };
    let mut cause = None;
    for stmt in &protected.body {
        crate::ast_walker::walk_expression(&stmt.expression, &mut |e| {
            if cause.is_some() {
                return;
            }
            match e {
                Expression::MessageSend {
                    receiver,
                    selector,
                    arguments,
                    ..
                } => cause = check_send(receiver, selector, arguments),
                Expression::Cascade {
                    receiver, messages, ..
                } => {
                    cause = messages
                        .iter()
                        .find_map(|m| check_send(receiver, &m.selector, &m.arguments));
                }
                _ => {}
            }
        });
    }
    cause
}

/// The names `cleanup` binds itself before it reads them: its parameters, and a
/// name assigned by a top-level statement of the cleanup (executed
/// unconditionally) whose value does not mention it, when no earlier statement
/// mentions it. An assignment inside an arm, a loop body or any nested block
/// literal might not run, so it does not bind.
fn names_bound_by_cleanup_first(cleanup: &Block) -> BTreeSet<EcoString> {
    let mut own: BTreeSet<EcoString> = cleanup.parameters.iter().map(|p| p.name.clone()).collect();
    let mut seen: BTreeSet<EcoString> = BTreeSet::new();
    for stmt in &cleanup.body {
        let mut mentioned: BTreeSet<EcoString> = BTreeSet::new();
        crate::ast_walker::walk_expression(&stmt.expression, &mut |e| {
            if let Expression::Identifier(id) = e {
                mentioned.insert(id.name.clone());
            }
        });
        if let Expression::Assignment { target, value, .. } = &stmt.expression
            && let Expression::Identifier(id) = target.as_ref()
        {
            let mut in_value = false;
            crate::ast_walker::walk_expression(value, &mut |v| {
                in_value |= matches!(v, Expression::Identifier(i) if i.name == id.name);
            });
            if !in_value && !seen.contains(&id.name) {
                own.insert(id.name.clone());
            }
        }
        seen.extend(mentioned);
    }
    own
}

/// How the protected block of an `ensure:` leaves abruptly
/// ([`report_nlr_out_of_ensure`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AbruptExit {
    /// A non-local return (`^`), possibly from a block value.
    Return,
    /// An error raised after the write (BT-4003).
    Error,
}

/// A local an `ensure:` cleanup would read stale ([`nlr_dropped_local`]).
struct EnsureDroppedLocal {
    name: String,
    /// What makes the protected block leave abruptly, as a phrase.
    cause: String,
    exit: AbruptExit,
}

/// The local `protected` writes and `cleanup` reads (but does not bind itself
/// first) when the protected block can leave abruptly after writing it (see
/// [`report_nlr_out_of_ensure`]): with a non-local return cause, the first such
/// local in name order; otherwise the first one written before something that
/// may raise ([`raise_after_write`]).
fn nlr_dropped_local(
    protected: &Block,
    cleanup: &Block,
    sources: &NlrSources,
) -> Option<EnsureDroppedLocal> {
    // The writes that reach the protected block's frame, nested loop, fold and
    // conditional bodies included (ADR 0131 threads those back out, and the `^`
    // drops them too): the one outer-local-writes recognizer, with the locals
    // the cleanup reads, and the enclosing scope binds, as the bound-outside set.
    let read = analyze_block(cleanup).local_reads;
    let own = names_bound_by_cleanup_first(cleanup);
    let written: BTreeSet<EcoString> =
        outer_local_writes(protected, &|n| read.contains(n) && !own.contains(n))
            .into_iter()
            .map(|w| w.name)
            .collect();
    let first = written.iter().next()?;
    if let Some(cause) = nlr_cause(protected, sources) {
        return Some(EnsureDroppedLocal {
            name: first.to_string(),
            cause,
            exit: AbruptExit::Return,
        });
    }
    raise_after_write(protected, &written).map(|name| EnsureDroppedLocal {
        name: name.to_string(),
        cause: "may raise an error after writing it".to_string(),
        exit: AbruptExit::Error,
    })
}

/// BT-4003: the first local of `tracked` that `protected` writes before
/// something that may raise an error runs, in evaluation order, or `None`.
///
/// Conservative: every message send (binary operators included) may raise, and
/// so may a destructuring assignment, a `match:` and a string interpolation. A
/// send's receiver and non-block arguments run before it; the block literals a
/// local-threading construct runs in place ([`local_threading_construct`], the
/// one recognizer) run during it: the bodies of a loop or fold twice (a next
/// iteration runs after a write of this one), each arm of a conditional or
/// lookup from the state before it, and the blocks of the other constructs in
/// order. Any other block literal is a closure, whose body does not run here.
///
/// This over-approximates: anything after the write that may raise counts,
/// even inside an `on:do:` (in the protected block) that catches it, so
/// `[[t := 9. 1 / 0] on: Error do: [:e | 0]] ensure: [log record: t]` is
/// rejected though the cleanup would see 9. Documented in ADR 0133
/// § Consequences and the language reference rather than modelled.
fn raise_after_write(protected: &Block, tracked: &BTreeSet<EcoString>) -> Option<EcoString> {
    let mut scan = RaiseScan {
        tracked,
        shadowed: Vec::new(),
        written: None,
        found: None,
    };
    scan.block(protected);
    scan.found
}

/// The walk of [`raise_after_write`].
struct RaiseScan<'a> {
    tracked: &'a BTreeSet<EcoString>,
    /// The parameters (and pattern variables) of the enclosing in-place blocks
    /// and `match:` arms, which shadow a tracked local of the same name.
    shadowed: Vec<BTreeSet<EcoString>>,
    /// The first tracked local written so far on the path being walked.
    written: Option<EcoString>,
    /// The answer: the local written when something that may raise ran.
    found: Option<EcoString>,
}

impl RaiseScan<'_> {
    fn may_raise(&mut self) {
        if self.found.is_none() {
            self.found.clone_from(&self.written);
        }
    }

    fn write(&mut self, name: &EcoString) {
        if self.written.is_none()
            && self.tracked.contains(name)
            && !self.shadowed.iter().any(|s| s.contains(name))
        {
            self.written = Some(name.clone());
        }
    }

    fn block(&mut self, block: &Block) {
        self.shadowed
            .push(block.parameters.iter().map(|p| p.name.clone()).collect());
        for stmt in &block.body {
            self.expr(&stmt.expression);
        }
        self.shadowed.pop();
    }

    /// Each alternative from the state before them; afterwards a local is
    /// written when some alternative wrote it.
    fn alternatives(&mut self, count: usize, mut each: impl FnMut(&mut Self, usize)) {
        let start = self.written.clone();
        let mut after = start.clone();
        for i in 0..count {
            self.written.clone_from(&start);
            each(self, i);
            if after.is_none() {
                after.clone_from(&self.written);
            }
        }
        self.written = after;
    }

    #[allow(clippy::too_many_lines)] // one arm per expression kind
    fn expr(&mut self, expr: &Expression) {
        match expr {
            Expression::Parenthesized { expression, .. } => self.expr(expression),
            Expression::Assignment { target, value, .. } => {
                self.expr(value);
                match target.as_ref() {
                    Expression::Identifier(id) => self.write(&id.name),
                    Expression::FieldAccess { receiver, .. } => self.expr(receiver),
                    _ => {}
                }
            }
            Expression::DestructureAssignment { pattern, value, .. } => {
                self.expr(value);
                self.may_raise();
                let (ids, _) = crate::semantic_analysis::extract_pattern_bindings(pattern);
                for id in ids {
                    self.write(&id.name);
                }
            }
            Expression::Return { value, .. } => self.expr(value),
            Expression::MessageSend {
                receiver,
                arguments,
                ..
            } => {
                let construct =
                    local_threading_construct(expr).filter(LocalThreadingConstruct::runs_in_place);
                let in_place = |e: &Expression| match (e, &construct) {
                    (Expression::Block(b), Some(c)) => {
                        c.blocks.iter().any(|blk| std::ptr::eq(*blk, b))
                    }
                    _ => false,
                };
                for operand in std::iter::once(receiver.as_ref()).chain(arguments) {
                    if !in_place(operand) {
                        self.expr(operand);
                    }
                }
                self.may_raise();
                let Some(c) = construct else {
                    return;
                };
                match c.family {
                    LocalThreadingFamily::Loop | LocalThreadingFamily::Fold => {
                        for _ in 0..2 {
                            for b in &c.blocks {
                                self.block(b);
                            }
                        }
                    }
                    LocalThreadingFamily::Conditional | LocalThreadingFamily::Lookup => {
                        self.alternatives(c.blocks.len(), |s, i| s.block(c.blocks[i]));
                    }
                    _ => {
                        for b in &c.blocks {
                            self.block(b);
                        }
                    }
                }
            }
            Expression::Cascade {
                receiver, messages, ..
            } => {
                // No cascade message inlines its blocks (`generate_cascade`).
                self.expr(receiver);
                for msg in messages {
                    for arg in &msg.arguments {
                        self.expr(arg);
                    }
                    self.may_raise();
                }
            }
            Expression::Match { value, arms, .. } => {
                self.expr(value);
                self.may_raise();
                self.alternatives(arms.len(), |s, i| {
                    let arm = &arms[i];
                    let (ids, _) =
                        crate::semantic_analysis::extract_match_arm_bindings(&arm.pattern);
                    s.shadowed.push(ids.into_iter().map(|id| id.name).collect());
                    if let Some(guard) = &arm.guard {
                        s.expr(guard);
                    }
                    s.expr(&arm.body);
                    s.shadowed.pop();
                });
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
                self.may_raise();
            }
            // Construction raises `badarg` on a mistyped or mis-sized value
            // (ADR 0143 § Runtime failures).
            Expression::BinaryConstruction { segments, .. } => {
                for e in segments.iter().flat_map(BinarySegment::sub_expressions) {
                    self.expr(e);
                }
                self.may_raise();
            }
            Expression::Primitive { .. } => self.may_raise(),
            // A block literal here is a closure: creating it runs nothing.
            Expression::Block(_)
            | Expression::Identifier(_)
            | Expression::Literal(..)
            | Expression::ClassReference { .. }
            | Expression::Super(_)
            | Expression::ExpectDirective { .. }
            | Expression::Spread { .. }
            | Expression::Error { .. } => {}
        }
    }
}
