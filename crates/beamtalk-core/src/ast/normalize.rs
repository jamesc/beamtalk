// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Send-operand parenthesis normalisation (BT-4059).
//!
//! **DDD Context:** Source Analysis
//!
//! A redundant pair of parentheses around a send operand means nothing: the
//! tree already encodes precedence, so `(e) foo`, `x + (e)` and
//! `x at: (e)` are the same program as `e foo`, `x + e` and `x at: e`. The
//! parser nonetheless keeps the `( … )` as an [`Expression::Parenthesized`]
//! node (the formatter and the LSP need it), and every analysis or codegen
//! check that matched an operand without peeling it sent the parenthesised
//! form down a different, sometimes wrong, path.
//!
//! [`peel_operand_parens`] is the one place that removes those wrappers, so
//! analysis and codegen see a single canonical tree. It strips **every**
//! `Parenthesized` layer from:
//!
//! - a message send's receiver and each of its arguments, and
//! - each cascade message's arguments.
//!
//! A cascade's *shared* receiver is the receiver of its first (folded)
//! message, so it is a send receiver and peeled as such. The cascade's own
//! `receiver` field — that folded first send — is **not** an operand and is
//! left alone: `(a foo); bar` cascades onto the *result* of `a foo`, whereas
//! `a foo; bar` cascades onto `a`, so removing that wrapper would change the
//! program.
//!
//! Two further positions are deliberately untouched:
//!
//! - a `super` receiver: `(super) foo` is an ordinary send to the value of
//!   `super`, not a super send, so a wrapper whose innermost node is `super`
//!   is kept whole;
//! - statements, assignment right-hand sides and every other non-operand
//!   position: the parenthesised-statement rule of BT-3838 reads the
//!   statement-level wrapper (BT-4092 owns that decision).
//!
//! Removing a wrapper never changes a span of the node it wrapped, so
//! diagnostics and type-map entries keyed on the inner node land on the inner
//! span (narrower by the parentheses).
//!
//! # Recording and restoring
//!
//! The parentheses are meaningless to compiled code but not to the person who
//! wrote them: a method's reflected source (`methodSource`, xref, the IDE
//! method editor) is the text as written, which the workspace compares with
//! the file on disk. [`peel_operand_parens_recording`] therefore returns a
//! [`PeeledParens`] — for each peeled operand, keyed by its reflection unit
//! ([`PeelScope`]: the enclosing method, or the module level) and the inner
//! node's span, the wrapper layers removed from it, each with its own span —
//! and [`restore_operand_parens`] puts them back on a clone, so
//! `unparse_method` of the restored method is `unparse_method` of the method
//! as parsed. Peel and restore visit operands through the one shared walk
//! (`visit_operands_mut`), so they agree on every position, the skipped ones
//! included.
//!
//! The scope matters because a flattened module holds protocol provisions
//! spliced in from other files, whose byte offsets can coincide with the
//! class file's: keyed by span alone, a restore could wrap an operand that
//! was never parenthesised. Keyed by (method span, inner span), a collision
//! needs both to coincide across files.
//!
//! The pass runs where analysis and codegen take ownership of a module
//! (`analyse_full` on its own clone; `generate_module_with_warnings` and the
//! REPL codegen entries, recording, on theirs). Every other reader of
//! `semantic_analysis` that starts from a parsed module goes through
//! [`normalised`] (BT-4238): the type checker's module entries
//! (`TypeChecker::check_module`, so `infer_types` and the LSP's hover and
//! completion), `local_threading_diagnostics`, `beamtalk-lint`'s pass runner,
//! the FFI-site query and the class-variable census. The formatter, the LSP's
//! position lookups, the `unnecessary_parens` lint, the raw parse and the
//! module a driver hands to codegen keep the parenthesised tree.
//! It is idempotent.
//!
//! # Unary loops (BT-4313)
//!
//! The same pass gives the unary loops one canonical form: `[c] whileTrue`
//! and `[c] whileFalse`, whose literal block receiver is both condition and
//! body, become `[c] whileTrue: []` and `[c] whileFalse: []` — the keyword loop
//! with an empty body ([`desugar_unary_loops`]). Analysis and codegen then
//! classify, thread and lower one loop shape, not two that must agree. The
//! body block the pass adds has a zero-width span at the end of the send,
//! which no parsed block has (a `[]` literal spans its brackets), so
//! [`restore_operand_parens`] recognises it ([`written_unary_loop`]) and puts
//! the unary send back without a record, and diagnostics name the selector as
//! written. Only a parameterless literal block receiver is rewritten: any
//! other receiver is an ordinary send of `whileTrue`/`whileFalse` (`Block`
//! defines both for those).
//!
//! `repeat` is the same loop with a condition that is always true (BT-4589):
//! `[b] repeat` becomes `[true] whileTrue: [b]`. The condition block the pass
//! adds is zero-width at the start of the send and holds only `true`, which no
//! parsed block is, so [`written_repeat`] recognises it and the restore puts
//! `[b] repeat` back. Before, codegen lowered `repeat` on its own path, which
//! no state-threading recognizer knew: a body that wrote a REPL binding was a
//! stateful block applied with no arguments (`arity_mismatch`).

use std::borrow::Cow;
use std::collections::HashMap;
use std::collections::hash_map::Entry;

use super::{
    Block, Expression, ExpressionStatement, Identifier, KeywordPart, MessageSelector,
    MethodDefinition, Module,
};
use crate::ast_walker::{walk_expression, walk_expression_mut};
use crate::span::Span;

/// The reflection unit a peeled operand belongs to: the method whose body
/// holds it (identified by the method's span), or the module level — module
/// statements (and so `ClassBuilder` block methods outside any method), the
/// REPL's expressions and variable initialisers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PeelScope {
    /// Outside any method.
    Module,
    /// Inside the method with this span.
    Method(Span),
}

impl PeelScope {
    /// The scope of `method`'s body.
    #[must_use]
    pub fn of_method(method: &MethodDefinition) -> Self {
        Self::Method(method.span)
    }
}

/// The operand parentheses a [`peel_operand_parens_recording`] run removed:
/// for each peeled operand, keyed by its [`PeelScope`] and the span of the
/// node left in its place, the `Parenthesized` spans that wrapped it,
/// innermost first.
///
/// [`restore_operand_parens`] re-wraps those operands, so a method's text as
/// written can be recovered from the peeled tree codegen compiles.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct PeeledParens {
    layers: HashMap<(PeelScope, Span), Vec<Span>>,
}

impl PeeledParens {
    /// `true` if no operand wrapper was recorded: restoring changes nothing.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.layers.is_empty()
    }

    /// The number of peeled operands recorded.
    #[must_use]
    pub fn len(&self) -> usize {
        self.layers.len()
    }

    /// Adds the operands `other` recorded (a second peel of more of the same
    /// tree, e.g. provisions flattened in after the first). An entry already
    /// recorded is kept, never replaced (see [`Self::record`]).
    pub fn extend(&mut self, other: PeeledParens) {
        for (key, layers) in other.layers {
            self.record(key, layers);
        }
    }

    /// Records `layers` for `key`, never overwriting an existing entry. Within
    /// one scope two operands share an inner span only when they are the same
    /// node (e.g. a protocol's provision and its same-file flattened copy), so
    /// their layers agree; should they ever differ, the first record wins
    /// rather than one operand's wrappers silently replacing another's.
    fn record(&mut self, key: (PeelScope, Span), layers: Vec<Span>) {
        if let Entry::Vacant(slot) = self.layers.entry(key) {
            slot.insert(layers);
        }
    }

    fn layers_of(&self, scope: PeelScope, inner: Span) -> Option<&Vec<Span>> {
        self.layers.get(&(scope, inner))
    }
}

/// Calls `visit` on every send-operand position of `expr`'s tree, in the walk's
/// pre-order: a node's operands are visited before the walk descends into
/// them. The one definition of "operand position" for peel, restore and the
/// `has_*` scans; `$m` is `mut` for the mutable walk.
macro_rules! visit_operands_with {
    ($walk:path, $expr:expr, $visit:expr $(, $m:tt)?) => {
        $walk($expr, &mut |node| match node {
            Expression::MessageSend {
                receiver,
                arguments,
                ..
            } => {
                $visit(&$($m)? **receiver);
                for argument in arguments {
                    $visit(argument);
                }
            }
            // The cascade's `receiver` is the folded first send, not an
            // operand (its own receiver is reached as that send's operand).
            Expression::Cascade { messages, .. } => {
                for message in messages {
                    for argument in &$($m)? message.arguments {
                        $visit(argument);
                    }
                }
            }
            _ => {}
        })
    };
}

/// The shared-reference operand walk (see [`visit_operands_with`]).
fn visit_operands(expr: &Expression, visit: &mut dyn FnMut(&Expression)) {
    visit_operands_with!(walk_expression, expr, visit);
}

/// The mutable operand walk (see [`visit_operands_with`]) peel and restore
/// share. A visit may replace the operand; the walk then descends into the
/// replacement.
fn visit_operands_mut(expr: &mut Expression, visit: &mut dyn FnMut(&mut Expression)) {
    visit_operands_with!(walk_expression_mut, expr, visit, mut);
}

/// Strips redundant parentheses from every send operand in `module`, and
/// canonicalises its unary loops ([`desugar_unary_loops`]).
///
/// See the module documentation for exactly which positions are peeled.
/// Idempotent: a second run changes nothing.
pub fn peel_operand_parens(module: &mut Module) {
    let _ = peel_operand_parens_recording(module);
}

/// `module` with every redundant send-operand wrapper removed: borrowed as it
/// is when it is already canonical (one [`needs_normalising`] scan),
/// otherwise a peeled clone.
///
/// The one entry for a reader that holds a parsed module by shared reference
/// and must see what analysis sees (BT-4238): `(e) foo` then reads exactly
/// like `e foo`. A node's span is the same in both trees, so a span taken
/// from the result names the same source text in the parsed module.
#[must_use]
pub fn normalised(module: &Module) -> Cow<'_, Module> {
    if needs_normalising(module) {
        let mut owned = module.clone();
        peel_operand_parens(&mut owned);
        Cow::Owned(owned)
    } else {
        Cow::Borrowed(module)
    }
}

/// [`peel_operand_parens`], returning what it removed so
/// [`restore_operand_parens`] can put it back.
pub fn peel_operand_parens_recording(module: &mut Module) -> PeeledParens {
    let mut record = PeeledParens::default();
    for_each_root_mut(module, &mut |scope, root| {
        peel_in_scope(root, scope, &mut record);
    });
    record
}

/// Strips redundant parentheses from every send operand in `expr`'s tree
/// (including nested blocks; a wrapper around `expr` itself is kept), adds
/// what it removed to `record`, and canonicalises every unary loop in the
/// tree, `expr` itself included ([`desugar_unary_loops`]).
///
/// The expression-level entry of [`peel_operand_parens_recording`], for
/// callers that hold module-level expressions rather than a module (the
/// REPL): recorded under [`PeelScope::Module`].
pub fn peel_operand_parens_expr_recording(expr: &mut Expression, record: &mut PeeledParens) {
    peel_in_scope(expr, PeelScope::Module, record);
}

fn peel_in_scope(expr: &mut Expression, scope: PeelScope, record: &mut PeeledParens) {
    visit_operands_mut(expr, &mut |operand| peel_operand(operand, scope, record));
    // After the peel, so `([c]) whileTrue` has its literal receiver.
    desugar_unary_loops(expr);
}

/// The unary loop selectors and the keyword loop each one is
/// (see the module documentation's "Unary loops").
const UNARY_LOOPS: [(&str, &str); 2] = [("whileTrue", "whileTrue:"), ("whileFalse", "whileFalse:")];

/// The unary loop selectors as users write them: `whileTrue`, `whileFalse`
/// and `repeat`. [`desugar_unary_loops`] rewrites each sent to a
/// parameterless literal block as a keyword loop, so no analysis or codegen
/// consumer classifies them (BT-4603); this list is for the user-facing note
/// that names the messages which thread a block's assignments
/// (`state_threading_selectors::assignment_threading_constructs`).
#[must_use]
pub fn written_unary_loop_selectors() -> [&'static str; 3] {
    [UNARY_LOOPS[0].0, UNARY_LOOPS[1].0, "repeat"]
}

/// Whether `node` itself (not its subtree) is a loop send
/// [`desugar_unary_loops`] would rewrite: a unary `whileTrue`/`whileFalse`
/// or `repeat` sent to a parameterless literal block. False on a normalised
/// tree; the classifiers that read only normalised trees assert it (BT-4603).
#[must_use]
pub fn is_uncanonicalised_loop(node: &Expression) -> bool {
    unary_loop_keyword(node).is_some() || is_literal_repeat(node)
}

/// The keyword selector `node` canonicalises to when it is a unary loop send
/// with a literal block receiver (parentheses around it included, which the
/// peel removes first), or `None`.
fn unary_loop_keyword(node: &Expression) -> Option<&'static str> {
    let Expression::MessageSend {
        receiver,
        selector: MessageSelector::Unary(name),
        arguments,
        is_cast: false,
        ..
    } = node
    else {
        return None;
    };
    // A receiver with parameters is no loop condition: it stays an ordinary
    // send, which `Block>>whileTrue` rejects at run time. (Rewriting it would
    // turn that into the keyword loop's compile-time arity error, BT-4617.)
    if !arguments.is_empty()
        || !matches!(receiver.unwrap_parens(), Expression::Block(b) if b.parameters.is_empty())
    {
        return None;
    }
    UNARY_LOOPS
        .iter()
        .find(|(unary, _)| name == unary)
        .map(|&(_, keyword)| keyword)
}

/// Whether `node` is a `repeat` send to a parameterless literal block
/// (parentheses around it included, which the peel removes first): the loop
/// [`desugar_unary_loops`] rewrites as `[true] whileTrue: <block>` (BT-4589).
/// Any other receiver is an ordinary send of `repeat`.
fn is_literal_repeat(node: &Expression) -> bool {
    matches!(
        node,
        Expression::MessageSend {
            receiver,
            selector: MessageSelector::Unary(name),
            arguments,
            is_cast: false,
            ..
        } if name == "repeat"
            && arguments.is_empty()
            && matches!(receiver.unwrap_parens(), Expression::Block(b) if b.parameters.is_empty())
    )
}

/// The span of the empty body block [`desugar_unary_loops`] gives the unary
/// loop send spanning `send`: zero-width, at the send's end.
fn desugared_body_span(send: Span) -> Span {
    Span::new(send.end(), send.end())
}

/// Rewrites every unary loop send in `expr`'s tree (`[c] whileTrue`) as its
/// keyword loop with an empty body (`[c] whileTrue: []`), the body's span
/// marking it as added ([`desugared_body_span`]), and every literal `[b]
/// repeat` as `[true] whileTrue: [b]`, the condition's span marking it as
/// added. Idempotent.
pub fn desugar_unary_loops(expr: &mut Expression) {
    walk_expression_mut(expr, &mut |node| {
        if is_literal_repeat(node) {
            desugar_repeat(node);
            return;
        }
        let Some(keyword) = unary_loop_keyword(node) else {
            return;
        };
        if let Expression::MessageSend {
            selector,
            arguments,
            span,
            ..
        } = node
        {
            let body = desugared_body_span(*span);
            *selector = MessageSelector::Keyword(vec![KeywordPart::new(keyword, body)]);
            arguments.push(Expression::Block(Block::new(Vec::new(), Vec::new(), body)));
        }
    });
}

/// Rewrites the literal `[b] repeat` send `node` ([`is_literal_repeat`]) as
/// `[true] whileTrue: [b]`. The added condition block and its keyword are
/// zero-width at the start of the send ([`written_repeat`] reads that).
///
/// The block and its `true` share that one span. Span-keyed maps hold the
/// block's entry: the type checker records a node after its children, so the
/// block's type replaces its statement's (pinned by
/// `repeat_condition_block_keeps_its_block_type`), and no reader looks up the
/// added `true` itself (codegen lowers it as the literal atom).
fn desugar_repeat(node: &mut Expression) {
    let Expression::MessageSend {
        receiver,
        selector,
        arguments,
        span,
        ..
    } = node
    else {
        return;
    };
    let added = Span::new(span.start(), span.start());
    let condition = Expression::Block(Block::new(
        Vec::new(),
        vec![ExpressionStatement::bare(Expression::Identifier(
            Identifier::new("true", added),
        ))],
        added,
    ));
    let body = std::mem::replace(receiver.as_mut(), condition);
    *selector = MessageSelector::Keyword(vec![KeywordPart::new("whileTrue:", added)]);
    arguments.push(body);
}

/// Whether a send of `selector` to `receiver` with `arguments` is a `repeat`
/// [`desugar_unary_loops`] canonicalised (BT-4589): a `whileTrue:` whose
/// receiver is the zero-width, parameterless `[true]` block the pass adds (no
/// parsed block is zero-width). The one test for "this loop was written
/// `repeat`": the restore below and the diagnostics that name the selector as
/// written read it.
#[must_use]
pub fn written_repeat(
    receiver: &Expression,
    selector: &MessageSelector,
    arguments: &[Expression],
) -> bool {
    let MessageSelector::Keyword(parts) = selector else {
        return false;
    };
    let ([part], [_]) = (parts.as_slice(), arguments) else {
        return false;
    };
    part.keyword == "whileTrue:"
        && matches!(
            receiver,
            Expression::Block(b) if b.span.is_empty()
                && b.parameters.is_empty()
                && matches!(
                    b.body.as_slice(),
                    [stmt] if matches!(&stmt.expression, Expression::Identifier(id) if id.name == "true")
                )
        )
}

/// The selector a send of `selector` to `receiver` with `arguments` was
/// written as: `repeat` for a loop [`written_repeat`] reads, the unary loop
/// for one [`written_unary_loop`] reads, else `selector` itself. The one
/// helper diagnostics use to name a canonicalised loop as written.
#[must_use]
pub fn selector_as_written(
    receiver: &Expression,
    selector: &MessageSelector,
    arguments: &[Expression],
) -> ecow::EcoString {
    if written_repeat(receiver, selector, arguments) {
        return "repeat".into();
    }
    match written_unary_loop(selector, arguments) {
        Some(unary) => unary.into(),
        None => selector.name(),
    }
}

/// The unary selector a send of `selector` with `arguments` was written as,
/// when it is a keyword loop [`desugar_unary_loops`] produced: its one
/// argument is the empty, parameterless, zero-width block the pass adds (no
/// parsed block is zero-width: a `[]` literal spans its brackets). `None` for
/// any other send, a written `[c] whileTrue: []` included.
///
/// The one test for "this loop was written unary": the restore below and the
/// diagnostics that name the selector as written read it.
#[must_use]
pub fn written_unary_loop(
    selector: &MessageSelector,
    arguments: &[Expression],
) -> Option<&'static str> {
    let MessageSelector::Keyword(parts) = selector else {
        return None;
    };
    let [part] = parts.as_slice() else {
        return None;
    };
    let &(unary, _) = UNARY_LOOPS.iter().find(|(_, k)| part.keyword == *k)?;
    matches!(
        arguments,
        [Expression::Block(b)] if b.body.is_empty() && b.parameters.is_empty() && b.span.is_empty()
    )
    .then_some(unary)
}

/// Whether `method`'s body holds a loop [`desugar_unary_loops`] produced
/// ([`written_unary_loop`]): the cheap scan that lets a reflection caller
/// with nothing peeled skip the restoring clone.
#[must_use]
pub fn method_has_desugared_unary_loop(method: &MethodDefinition) -> bool {
    let mut found = false;
    for stmt in &method.body {
        if found {
            break;
        }
        walk_expression(&stmt.expression, &mut |node| {
            if found {
                return;
            }
            if let Expression::MessageSend {
                receiver,
                selector,
                arguments,
                ..
            } = node
            {
                found = found
                    || written_unary_loop(selector, arguments).is_some()
                    || written_repeat(receiver, selector, arguments);
            }
        });
    }
    found
}

/// The inverse of [`desugar_unary_loops`] on `expr`'s tree: a keyword loop
/// whose body is the empty block the pass added ([`written_unary_loop`])
/// becomes the unary send again, and a `whileTrue:` whose condition the pass
/// added ([`written_repeat`]) becomes `<body> repeat`.
fn restore_unary_loops(expr: &mut Expression) {
    walk_expression_mut(expr, &mut |node| {
        let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } = node
        else {
            return;
        };
        if written_repeat(receiver, selector, arguments) {
            if let Some(body) = arguments.pop() {
                **receiver = body;
                *selector = MessageSelector::Unary("repeat".into());
            }
            return;
        }
        if let Some(unary) = written_unary_loop(selector, arguments) {
            arguments.clear();
            *selector = MessageSelector::Unary(unary.into());
        }
    });
}

/// [`restore_unary_loops`] over every root of `module`: the normalised tree
/// with only its operand parentheses peeled, for a reader that indexes sends
/// by the selector as written (the send index, `method_source_walker`).
pub fn restore_unary_loops_in_module(module: &mut Module) {
    for_each_root_mut(module, &mut |_, root| restore_unary_loops(root));
}

/// The span of the first unary loop send in `expr`'s tree that
/// [`desugar_unary_loops`] would rewrite, or `None`.
fn expr_first_unary_loop(expr: &Expression) -> Option<Span> {
    let mut found = None;
    walk_expression(expr, &mut |node| {
        if found.is_none() && is_uncanonicalised_loop(node) {
            found = Some(node.span());
        }
    });
    found
}

/// `true` if the canonicalising pass ([`peel_operand_parens`]) would change
/// `module`: a parenthesised send operand or a unary loop. The cheap scan a
/// caller holding a shared reference runs before cloning.
#[must_use]
pub fn needs_normalising(module: &Module) -> bool {
    let mut found = false;
    for_each_root(module, &mut |_, root| {
        found = found || expr_needs_normalising(root);
    });
    found
}

/// [`needs_normalising`] for one expression tree (the REPL entries).
#[must_use]
pub fn expr_needs_normalising(expr: &Expression) -> bool {
    expr_has_parenthesised_operand(expr) || expr_first_unary_loop(expr).is_some()
}

/// Puts back, in `method`'s body, the operand parentheses `peeled` recorded
/// removing from it (its own [`PeelScope::of_method`] scope): the inverse of
/// the peel on the method's tree, so `unparse_method` of the result is the
/// method's text as written, unary loops included. Idempotent.
pub fn restore_operand_parens(method: &mut MethodDefinition, peeled: &PeeledParens) {
    restore_operand_parens_in_scope(method, peeled, PeelScope::of_method(method));
}

/// [`restore_operand_parens`] from the entries of `scope` rather than the
/// method's own: for a method built from a block (a `ClassBuilder` block
/// method), whose operands were recorded in the scope the block sits in.
pub fn restore_operand_parens_in_scope(
    method: &mut MethodDefinition,
    peeled: &PeeledParens,
    scope: PeelScope,
) {
    // The inverse of the peel, in reverse order: the unary loops (which need
    // no record), then the operand parentheses.
    for stmt in &mut method.body {
        restore_unary_loops(&mut stmt.expression);
    }
    if peeled.is_empty() {
        return;
    }
    for stmt in &mut method.body {
        visit_operands_mut(&mut stmt.expression, &mut |operand| {
            restore_operand(operand, scope, peeled);
        });
    }
}

/// `true` if `module` has a send operand [`peel_operand_parens`] would
/// unwrap. Not the whole pass: it ignores the unary loops the pass also
/// canonicalises. To decide whether to clone, use [`needs_normalising`].
#[must_use]
pub fn has_parenthesised_operand(module: &Module) -> bool {
    first_parenthesised_operand(module).is_some()
}

/// `true` if `expr` has a send operand [`peel_operand_parens_expr_recording`]
/// would unwrap (unary loops not included: see [`expr_needs_normalising`]).
#[must_use]
pub fn expr_has_parenthesised_operand(expr: &Expression) -> bool {
    expr_first_parenthesised_operand(expr).is_some()
}

/// The span of the first send operand in `module` that
/// [`peel_operand_parens`] would peel, in root order then walk order; `None`
/// when `module` is canonical.
#[must_use]
pub fn first_parenthesised_operand(module: &Module) -> Option<Span> {
    let mut found = None;
    for_each_root(module, &mut |_, root| {
        if found.is_none() {
            found = expr_first_parenthesised_operand(root);
        }
    });
    found
}

/// [`first_parenthesised_operand`] for one expression tree.
#[must_use]
pub fn expr_first_parenthesised_operand(expr: &Expression) -> Option<Span> {
    let mut found = None;
    visit_operands(expr, &mut |operand| {
        if found.is_none() && is_peelable(operand) {
            found = Some(operand.span());
        }
    });
    found
}

/// The normalisation invariant (BT-4232), for tests: panics unless every send
/// operand of `module` is canonical — no `Parenthesized` wrapper other than
/// the ones [`peel_operand_parens`] deliberately keeps (`(super)`, and the
/// cascade's folded first send, which is not an operand) — and no unary loop
/// the pass canonicalises is left (`[c] whileTrue`, BT-4313).
///
/// Codegen and analysis match send operands directly, without peeling, on
/// the strength of this invariant. Codegen's module entry and the REPL
/// assembler call it under `#[cfg(test)]` (the gate is the caller's, so the
/// check costs nothing outside test builds), so a driver that reaches the
/// generator without running the pass fails the codegen tests instead of
/// silently compiling `(e) foo` differently from `e foo`.
///
/// # Panics
///
/// When a send operand of `module` is still parenthesised, or a unary loop
/// was not canonicalised; the message names `context` (the entry that broke
/// the invariant) and the node's span.
pub fn assert_operands_normalised(module: &Module, context: &str) {
    let mut unary_loop = None;
    for_each_root(module, &mut |_, root| {
        unary_loop = unary_loop.or_else(|| expr_first_unary_loop(root));
    });
    assert_no_unary_loop(unary_loop, context);
    if let Some(span) = first_parenthesised_operand(module) {
        panic!(
            "{context}: send operand at {}..{} is still `Parenthesized`; \
             run `ast::normalize::peel_operand_parens` before this point (BT-4059)",
            span.start(),
            span.end()
        );
    }
}

/// [`assert_operands_normalised`] for expressions (the REPL entries).
///
/// # Panics
///
/// When a send operand in `expressions` is still parenthesised, or a unary
/// loop was not canonicalised.
pub fn assert_expr_operands_normalised(expressions: &[Expression], context: &str) {
    assert_no_unary_loop(expressions.iter().find_map(expr_first_unary_loop), context);
    if let Some(span) = expressions
        .iter()
        .find_map(expr_first_parenthesised_operand)
    {
        panic!(
            "{context}: send operand at {}..{} is still `Parenthesized`; \
             run `ast::normalize::peel_operand_parens_expr` before this point (BT-4059)",
            span.start(),
            span.end()
        );
    }
}

/// Panics naming `context` when `unary_loop` (a unary loop send's span) is set.
fn assert_no_unary_loop(unary_loop: Option<Span>, context: &str) {
    if let Some(span) = unary_loop {
        panic!(
            "{context}: unary loop send at {}..{} was not canonicalised; \
             run `ast::normalize::peel_operand_parens` before this point (BT-4313)",
            span.start(),
            span.end()
        );
    }
}

/// `true` if `operand` is a `Parenthesized` wrapper the pass removes: any
/// wrapper whose innermost node is not `super`.
fn is_peelable(operand: &Expression) -> bool {
    matches!(operand, Expression::Parenthesized { .. })
        && !matches!(operand.unwrap_parens(), Expression::Super(_))
}

/// The node the normalised tree holds in place of `operand`, a send operand
/// (receiver or argument) of the parsed module: its innermost
/// non-`Parenthesized` node when the pass peels it, `operand` itself
/// otherwise (`(super)`, or no wrapper at all).
///
/// For a reader that walks the parsed tree (the language service's position
/// lookups, BT-4237) but matches an operand's shape the way analysis does,
/// so `(self) foo` resolves like `self foo` while `(super) foo` stays an
/// ordinary send.
#[must_use]
pub fn canonical_operand(operand: &Expression) -> &Expression {
    if is_peelable(operand) {
        operand.unwrap_parens()
    } else {
        operand
    }
}

/// A throwaway node to swap into a slot while its content is moved.
fn placeholder() -> Expression {
    Expression::Error {
        message: "".into(),
        span: Span::default(),
    }
}

/// Replaces `operand` by its innermost non-`Parenthesized` node, when
/// [`is_peelable`], recording the removed wrappers' spans in `record` under
/// `scope`.
fn peel_operand(operand: &mut Expression, scope: PeelScope, record: &mut PeeledParens) {
    if !is_peelable(operand) {
        return;
    }
    let mut layers = Vec::new();
    while let Expression::Parenthesized { expression, span } = operand {
        layers.push(*span);
        let inner = std::mem::replace(expression.as_mut(), placeholder());
        *operand = inner;
    }
    // Peeled outermost first; restored innermost first.
    layers.reverse();
    record.record((scope, operand.span()), layers);
}

/// Re-wraps `operand` in the layers recorded for its span in `scope`. A
/// `Parenthesized` operand was never peeled (a recorded node is the innermost
/// one), so it is left alone, which also makes a second restore a no-op.
fn restore_operand(operand: &mut Expression, scope: PeelScope, peeled: &PeeledParens) {
    if matches!(operand, Expression::Parenthesized { .. }) {
        return;
    }
    let Some(layers) = peeled.layers_of(scope, operand.span()) else {
        return;
    };
    for &span in layers {
        let inner = std::mem::replace(operand, placeholder());
        *operand = Expression::Parenthesized {
            expression: Box::new(inner),
            span,
        };
    }
}

/// Calls `f` on every root expression of `module`, with its [`PeelScope`]:
/// module-level statements and every instance- and class-variable initialiser
/// ([`PeelScope::Module`]), and every method body statement (class,
/// class-side, standalone and protocol provisions) under its method.
fn for_each_root(module: &Module, f: &mut dyn FnMut(PeelScope, &Expression)) {
    let method_body = |method: &MethodDefinition, f: &mut dyn FnMut(PeelScope, &Expression)| {
        let scope = PeelScope::of_method(method);
        for stmt in &method.body {
            f(scope, &stmt.expression);
        }
    };
    for stmt in &module.expressions {
        f(PeelScope::Module, &stmt.expression);
    }
    for class in &module.classes {
        for decl in class.state.iter().chain(&class.class_variables) {
            if let Some(default) = &decl.default_value {
                f(PeelScope::Module, default);
            }
        }
        for method in class.methods.iter().chain(&class.class_methods) {
            method_body(method, f);
        }
    }
    for standalone in &module.method_definitions {
        method_body(&standalone.method, f);
    }
    for protocol in &module.protocols {
        for method in &protocol.provided_methods {
            method_body(method, f);
        }
    }
}

/// The mutable counterpart of [`for_each_root`]; the two enumerate the same
/// roots with the same scopes, which
/// `roots_agree_between_shared_and_mutable_walks` asserts.
fn for_each_root_mut(module: &mut Module, f: &mut dyn FnMut(PeelScope, &mut Expression)) {
    let method_body = |method: &mut MethodDefinition,
                       f: &mut dyn FnMut(PeelScope, &mut Expression)| {
        let scope = PeelScope::of_method(method);
        for stmt in &mut method.body {
            f(scope, &mut stmt.expression);
        }
    };
    for stmt in &mut module.expressions {
        f(PeelScope::Module, &mut stmt.expression);
    }
    for class in &mut module.classes {
        for decl in class.state.iter_mut().chain(&mut class.class_variables) {
            if let Some(default) = &mut decl.default_value {
                f(PeelScope::Module, default);
            }
        }
        for method in class.methods.iter_mut().chain(&mut class.class_methods) {
            method_body(method, f);
        }
    }
    for standalone in &mut module.method_definitions {
        method_body(&mut standalone.method, f);
    }
    for protocol in &mut module.protocols {
        for method in &mut protocol.provided_methods {
            method_body(method, f);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::test_support::parse_bt;
    use crate::unparse::unparse_module;

    fn peeled(source: &str) -> Module {
        let mut module = parse_bt(source);
        peel_operand_parens(&mut module);
        module
    }

    fn first_expr(module: &Module) -> &Expression {
        &module.expressions[0].expression
    }

    #[test]
    fn peels_receivers_and_arguments_of_every_selector_shape() {
        for (src, expected) in [
            ("(self) foo", "self foo"),
            ("((C)) + 1", "C + 1"),
            ("(C) add: (3) to: ((4))", "C add: 3 to: 4"),
            ("(x) + (y)", "x + y"),
            ("#(1, 2) do: ([:k | k])", "#(1, 2) do: [:k | k]"),
        ] {
            let module = peeled(src);
            assert!(!has_parenthesised_operand(&module), "{src}");
            assert_eq!(unparse_module(&module).trim(), expected, "{src}");
        }
    }

    #[test]
    fn canonical_operand_is_the_node_the_peel_leaves() {
        for (src, keeps_wrapper) in [
            ("(self) foo", false),
            ("((x)) foo", false),
            ("x foo", false),
            ("(super) foo", true),
            ("((super)) foo", true),
        ] {
            let module = parse_bt(src);
            let Expression::MessageSend { receiver, .. } = first_expr(&module) else {
                panic!("expected a send: {src}");
            };
            let canonical = canonical_operand(receiver);
            assert_eq!(
                matches!(canonical, Expression::Parenthesized { .. }),
                keeps_wrapper,
                "{src}"
            );
            let peeled = peeled(src);
            let Expression::MessageSend {
                receiver: peeled_receiver,
                ..
            } = first_expr(&peeled)
            else {
                panic!("expected a send: {src}");
            };
            assert_eq!(canonical, peeled_receiver.as_ref(), "{src}");
        }
    }

    #[test]
    fn normalised_borrows_a_canonical_module_and_peels_a_clone_otherwise() {
        let canonical = parse_bt("x foo. (y bar)");
        assert!(matches!(normalised(&canonical), Cow::Borrowed(_)));

        let raw = parse_bt("(x) foo: (1)");
        let module = normalised(&raw);
        assert!(matches!(module, Cow::Owned(_)));
        assert!(!has_parenthesised_operand(&module));
        assert_eq!(unparse_module(&module).trim(), "x foo: 1");
        // The input is untouched.
        assert!(has_parenthesised_operand(&raw));
    }

    #[test]
    fn peels_cascade_target_and_message_arguments_but_not_the_folded_first_send() {
        let module = peeled("(x) foo: (1); bar: (2)");
        assert_eq!(unparse_module(&module).trim(), "x foo: 1; bar: 2");

        // `(a foo); bar` cascades onto the result of `a foo`, so the wrapper
        // around the folded first send stays.
        let module = peeled("(a foo); bar");
        let Expression::Cascade { receiver, .. } = first_expr(&module) else {
            panic!("expected a cascade");
        };
        assert!(matches!(
            receiver.as_ref(),
            Expression::Parenthesized { .. }
        ));
    }

    #[test]
    fn keeps_a_parenthesised_super_receiver() {
        let module = peeled("Object subclass: A\n  foo => ((super)) foo\n");
        let body = &module.classes[0].methods[0].body;
        let Expression::MessageSend { receiver, .. } = &body[0].expression else {
            panic!("expected a send");
        };
        assert!(matches!(
            receiver.as_ref(),
            Expression::Parenthesized { .. }
        ));
        assert!(!has_parenthesised_operand(&module));
    }

    #[test]
    fn leaves_statements_and_assignment_values_untouched() {
        let module = peeled("(x foo)\ny := (3 + (4))\n^ (z)");
        let [s0, s1, s2] = module.expressions.as_slice() else {
            panic!("expected three statements");
        };
        // A parenthesised statement keeps its wrapper; its own send operands
        // are peeled.
        assert!(matches!(s0.expression, Expression::Parenthesized { .. }));
        let Expression::Assignment { value, .. } = &s1.expression else {
            panic!("expected an assignment");
        };
        let Expression::Parenthesized { expression, .. } = value.as_ref() else {
            panic!("assignment value keeps its wrapper");
        };
        let Expression::MessageSend { arguments, .. } = expression.as_ref() else {
            panic!("expected a send");
        };
        assert!(matches!(arguments[0], Expression::Literal(..)));
        let Expression::Return { value, .. } = &s2.expression else {
            panic!("expected a return");
        };
        assert!(matches!(value.as_ref(), Expression::Parenthesized { .. }));
    }

    #[test]
    fn keeps_the_inner_span() {
        let module = peeled("(x) foo");
        let Expression::MessageSend { receiver, .. } = first_expr(&module) else {
            panic!("expected a send");
        };
        assert_eq!(receiver.span().start(), 1);
        assert_eq!(receiver.span().end(), 2);
    }

    #[test]
    fn reaches_blocks_initialisers_match_arms_and_every_method_kind() {
        let source = "Object subclass: A
  state: s = (1) + (2)
  classState: c = (3) max: (4)
  foo => [:b | (b) + 1] value: 2
  bar => 3 match: [n -> (n) + 1]
  class baz => (self) foo

A >> qux => (self) bar

Protocol define: P
  pro => (self) foo
";
        let mut module = parse_bt(source);
        assert!(has_parenthesised_operand(&module));
        peel_operand_parens(&mut module);
        assert!(!has_parenthesised_operand(&module), "{module:#?}");
    }

    #[test]
    fn is_idempotent() {
        let source = "Object subclass: A
  foo => ((self) bar: ((1) + (2))) baz; qux: ((3))
  bar => (super) foo
";
        let once = peeled(source);
        let mut twice = once.clone();
        peel_operand_parens(&mut twice);
        assert_eq!(once, twice);
    }

    #[test]
    fn has_parenthesised_operand_is_false_for_canonical_modules() {
        assert!(!has_parenthesised_operand(&parse_bt(
            "x foo\n(y bar)\nz := (3)"
        )));
        assert!(has_parenthesised_operand(&parse_bt("x foo: (y)")));
    }

    /// The methods of `source`'s first class, peeled with a record, each
    /// restored from it, next to the raw parse's.
    fn restored_and_raw(source: &str) -> Vec<(MethodDefinition, MethodDefinition)> {
        let raw = parse_bt(source);
        let mut peeled = raw.clone();
        let record = peel_operand_parens_recording(&mut peeled);
        assert!(!has_parenthesised_operand(&peeled));
        peeled.classes[0]
            .methods
            .iter()
            .zip(&raw.classes[0].methods)
            .map(|(method, raw_method)| {
                let mut restored = method.clone();
                restore_operand_parens(&mut restored, &record);
                (restored, raw_method.clone())
            })
            .collect()
    }

    const NESTED_SOURCE: &str = "Object subclass: A
  summed: a => (a * 2) + 1
  m => (self) foo
  n => ((x)) foo: (((1))) bar: [:b | (b) + ((2))]
  c => (x) foo: (1); bar: ((2)); baz
  f => (a foo); bar
  s => ((super)) foo: (3)
  st => y := (x)
";

    #[test]
    fn restore_gives_back_the_tree_as_parsed() {
        for (restored, raw) in restored_and_raw(NESTED_SOURCE) {
            assert_eq!(restored, raw);
        }
    }

    #[test]
    fn restore_is_idempotent() {
        let mut peeled = parse_bt(NESTED_SOURCE);
        let record = peel_operand_parens_recording(&mut peeled);
        assert!(!record.is_empty());
        for method in &peeled.classes[0].methods {
            let mut once = method.clone();
            restore_operand_parens(&mut once, &record);
            let mut twice = once.clone();
            restore_operand_parens(&mut twice, &record);
            assert_eq!(once, twice);
        }
    }

    #[test]
    fn restore_with_an_empty_record_changes_nothing() {
        let mut peeled = parse_bt(NESTED_SOURCE);
        peel_operand_parens(&mut peeled);
        for method in &peeled.classes[0].methods {
            let mut restored = method.clone();
            restore_operand_parens(&mut restored, &PeeledParens::default());
            assert_eq!(&restored, method);
        }
    }

    #[test]
    fn records_one_entry_per_peeled_operand_with_every_layer() {
        let mut module = parse_bt("((x)) foo: (1)");
        let record = peel_operand_parens_recording(&mut module);
        assert_eq!(record.len(), 2);
        // `((x))`: the inner node `x` at 2..3 was wrapped by 1..4, then 0..5.
        assert_eq!(
            record.layers_of(PeelScope::Module, Span::new(2, 3)),
            Some(&vec![Span::new(1, 4), Span::new(0, 5)])
        );
    }

    #[test]
    fn first_parenthesised_operand_names_the_wrapper_span() {
        // `x foo: (y)`: the wrapper spans bytes 7..10.
        let span = first_parenthesised_operand(&parse_bt("x foo: (y)")).expect("an operand");
        assert_eq!((span.start(), span.end()), (7, 10));
        assert_eq!(first_parenthesised_operand(&peeled("x foo: (y)")), None);
        // The positions the pass keeps are not reported.
        assert_eq!(
            first_parenthesised_operand(&parse_bt("(a foo); bar\n(x)\nz := (3)")),
            None
        );
    }

    #[test]
    #[should_panic(expected = "still `Parenthesized`")]
    fn the_invariant_check_rejects_an_unnormalised_module() {
        assert_operands_normalised(&parse_bt("(self) foo"), "test");
    }

    #[test]
    #[should_panic(expected = "still `Parenthesized`")]
    fn the_expression_invariant_check_rejects_an_unnormalised_expression() {
        let module = parse_bt("3 + (4)");
        assert_expr_operands_normalised(&[first_expr(&module).clone()], "test");
    }

    #[test]
    fn the_invariant_check_accepts_a_normalised_module() {
        let module = peeled(
            "Object subclass: A\n  foo => ((self) bar: (1)); baz: (2)\n  bar => (super) foo\n",
        );
        assert_operands_normalised(&module, "test");
        assert_expr_operands_normalised(
            &module
                .expressions
                .iter()
                .map(|s| s.expression.clone())
                .collect::<Vec<_>>(),
            "test",
        );
    }

    #[test]
    fn roots_agree_between_shared_and_mutable_walks() {
        let mut module = parse_bt(
            "Object subclass: A\n  state: s = 1\n  classState: c = 2\n  foo => 3\n  class bar => 4\nA >> baz => 5\nProtocol define: P\n  pro => 6\n7",
        );
        let mut shared = Vec::new();
        for_each_root(&module, &mut |scope, e| shared.push((scope, e.clone())));
        let mut mutable = Vec::new();
        for_each_root_mut(&mut module, &mut |scope, e| {
            mutable.push((scope, e.clone()));
        });
        assert_eq!(shared.len(), 7);
        assert_eq!(shared, mutable);
        let module_level = shared
            .iter()
            .filter(|(scope, _)| *scope == PeelScope::Module)
            .count();
        // `s = 1`, `c = 2` and `7`; the four method bodies are scoped to their method.
        assert_eq!(module_level, 3);
    }

    /// A class source and a protocol source (another file) in which the
    /// class method's bare operand `x` and the provision's parenthesised
    /// `(x)` have the same byte span.
    fn colliding_sources() -> (String, String) {
        let protocol = format!(
            "// {}\nProtocol define: P\n  pro: x => (x) + 1\n",
            "=".repeat(60)
        );
        let target = protocol.find("(x)").unwrap() + 1;
        let body = "Object subclass: C\n  foo: x => x + 1\n";
        let base = body.find("=> x").unwrap() + 3;
        let pad = format!("//{}\n", "-".repeat(target - base - 3));
        let class = format!("{pad}{body}");
        assert_eq!(class.find("=> x").unwrap() + 3, target);
        (class, protocol)
    }

    #[test]
    fn restore_is_scoped_to_the_method_it_was_peeled_from() {
        let (class_src, protocol_src) = colliding_sources();
        let mut class = parse_bt(&class_src);
        let mut protocol = parse_bt(&protocol_src);
        // As codegen does for a flattened module: one record for both files.
        let mut record = peel_operand_parens_recording(&mut class);
        record.extend(peel_operand_parens_recording(&mut protocol));
        assert_eq!(record.len(), 1);

        let mut foo = class.classes[0].methods[0].clone();
        restore_operand_parens(&mut foo, &record);
        assert_eq!(crate::unparse::unparse_method(&foo), "foo: x => x + 1");
        let mut pro = protocol.protocols[0].provided_methods[0].clone();
        restore_operand_parens(&mut pro, &record);
        assert_eq!(crate::unparse::unparse_method(&pro), "pro: x => (x) + 1");
    }

    #[test]
    fn recording_never_overwrites_an_entry() {
        let key = (PeelScope::Module, Span::new(5, 6));
        let mut record = PeeledParens::default();
        record.record(key, vec![Span::new(4, 7)]);
        let mut other = PeeledParens::default();
        other.record(key, vec![Span::new(3, 8)]);
        record.extend(other);
        assert_eq!(record.layers_of(key.0, key.1), Some(&vec![Span::new(4, 7)]));
    }

    // --- BT-4313: unary loops ---

    const UNARY_LOOP_SOURCE: &str = "Object subclass: A
  t => [x := x + 1. x < 3] whileTrue
  f => ([x foo]) whileFalse
  n => [[y bar] whileTrue. y < 2] whileFalse. z := [false] whileTrue
  k => [x < 3] whileTrue: []
  o => b whileTrue
  p => [:x | x] whileTrue
";

    #[test]
    fn unary_loops_become_the_keyword_loop_with_an_empty_body() {
        let module = peeled(UNARY_LOOP_SOURCE);
        assert!(!needs_normalising(&module));
        let methods = &module.classes[0].methods;
        let text = |i: usize| crate::unparse::unparse_method(&methods[i]);
        assert!(text(0).contains("whileTrue: []"), "{}", text(0));
        assert!(text(1).contains("[x foo] whileFalse: []"), "{}", text(1));
        assert_eq!(text(2).matches(": []").count(), 3, "{}", text(2));
        // A non-literal receiver is an ordinary send.
        assert!(text(4).trim_end().ends_with("b whileTrue"), "{}", text(4));
        // So is a receiver with parameters: it is no loop condition.
        assert!(
            text(5).trim_end().ends_with("[:x | x] whileTrue"),
            "{}",
            text(5)
        );
        let mut unary = 0;
        walk_expression(&methods[0].body[0].expression, &mut |node| {
            if let Expression::MessageSend {
                arguments, span, ..
            } = node
            {
                if let [Expression::Block(body)] = arguments.as_slice() {
                    assert_eq!(body.span, desugared_body_span(*span));
                    unary += 1;
                }
            }
        });
        assert_eq!(unary, 1);
    }

    #[test]
    fn unary_loops_are_detected_and_the_pass_is_idempotent() {
        assert!(needs_normalising(&parse_bt("[x] whileTrue")));
        assert!(needs_normalising(&parse_bt("[x] whileFalse")));
        assert!(!needs_normalising(&parse_bt(
            "[x] whileTrue: []\nb whileTrue"
        )));
        let once = peeled(UNARY_LOOP_SOURCE);
        let mut twice = once.clone();
        peel_operand_parens(&mut twice);
        assert_eq!(once, twice);
    }

    #[test]
    fn restore_gives_back_unary_loops_as_written() {
        for (restored, raw) in restored_and_raw(UNARY_LOOP_SOURCE) {
            assert_eq!(restored, raw);
        }
    }

    // --- BT-4589: `repeat` ---

    const REPEAT_SOURCE: &str = "Object subclass: A
  r => [x := x + 1. x >= 3 ifTrue: [^x]] repeat
  p => ([x foo]) repeat
  n => [[y bar] repeat] repeat. z := [nil] repeat
  w => [true] whileTrue: [x foo]
  o => b repeat
  q => [:x | x] repeat
";

    #[test]
    fn repeat_becomes_while_true_with_a_true_condition() {
        let module = peeled(REPEAT_SOURCE);
        assert!(!needs_normalising(&module));
        let methods = &module.classes[0].methods;
        let text = |i: usize| crate::unparse::unparse_method(&methods[i]);
        for i in 0..2 {
            assert!(
                text(i).contains("[true]") && text(i).contains("whileTrue:"),
                "{}",
                text(i)
            );
            assert!(!text(i).contains("repeat"), "{}", text(i));
        }
        assert_eq!(text(2).matches("whileTrue:").count(), 3, "{}", text(2));
        // A non-literal receiver, or one with parameters, is an ordinary send.
        assert!(text(4).trim_end().ends_with("b repeat"), "{}", text(4));
        assert!(
            text(5).trim_end().ends_with("[:x | x] repeat"),
            "{}",
            text(5)
        );
        // Only the loops the pass wrote are read back as `repeat`: a written
        // `[true] whileTrue:` is not.
        let written = |i: usize| {
            let mut found = 0;
            walk_expression(&methods[i].body[0].expression, &mut |node| {
                if let Expression::MessageSend {
                    receiver,
                    selector,
                    arguments,
                    ..
                } = node
                {
                    found += usize::from(written_repeat(receiver, selector, arguments));
                }
            });
            found
        };
        assert_eq!(
            (0..4).map(written).collect::<Vec<_>>(),
            vec![1, 1, 2, 0],
            "{module:?}"
        );
    }

    #[test]
    fn repeat_is_detected_and_the_pass_is_idempotent() {
        assert!(needs_normalising(&parse_bt("[x] repeat")));
        assert!(!needs_normalising(&parse_bt("b repeat\n[:x | x] repeat")));
        let once = peeled(REPEAT_SOURCE);
        let mut twice = once.clone();
        peel_operand_parens(&mut twice);
        assert_eq!(once, twice);
    }

    /// The added condition block and its `true` share one zero-width span;
    /// the type map holds the block's type there, the same type a written
    /// `[true]` condition gets.
    #[test]
    fn repeat_condition_block_keeps_its_block_type() {
        use crate::semantic_analysis::ClassHierarchy;
        use crate::semantic_analysis::type_checker::infer_types;
        let condition_type = |source: &str| {
            let module = peeled(source);
            let types = infer_types(&module, &ClassHierarchy::with_builtins(), None);
            let Expression::MessageSend { receiver, .. } = first_expr(&module) else {
                panic!("expected a send: {source}");
            };
            types.get(receiver.span()).cloned()
        };
        let canonical = condition_type("[3 printString] repeat");
        assert!(
            canonical.is_some(),
            "no type recorded for the condition block"
        );
        assert_eq!(
            canonical,
            condition_type("[true] whileTrue: [3 printString]")
        );
    }

    #[test]
    fn restore_gives_back_repeat_as_written() {
        for (restored, raw) in restored_and_raw(REPEAT_SOURCE) {
            assert_eq!(restored, raw);
        }
    }
}
