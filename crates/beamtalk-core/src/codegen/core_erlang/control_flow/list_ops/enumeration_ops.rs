// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-2703: Actor state threading for `eachWithIndex:` and `do:separatedBy:`.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Unlike the foldl list-ops in the sibling modules, these two enumeration
//! selectors are *self-hosted* in `Collection.bt` on top of `inject:into:`. A
//! plain dispatch to those methods cannot thread an actor's `State` back out of the
//! block, so a mutating block silently lost its writes — and, once the selectors
//! were classified as state-threading, the block was emitted as a stateful
//! `{Value, State}`-tuple fun whose arity no longer matched the un-threaded
//! `Collection.bt` method, crashing with `badarity`.
//!
//! Rather than re-derive the full `lists:foldl` threading machinery for two more
//! selectors (and, for `do:separatedBy:`, a *second* block), we desugar each call
//! — whenever a block actually needs mutation threading — into the equivalent
//! `inject:into:` fold and reuse its codegen. The desugar fires in *every* context
//! so the stateful block is always consumed by the fold (never dispatched at the
//! wrong arity). `inject:into:` already threads field/local mutations (including
//! those nested in the separator's conditional) and seeds the index / "first
//! element" accumulator without needing a statement before the loop:
//!
//! ```text
//! coll eachWithIndex: [:x :i | BODY]
//!   ⇒  coll inject: 1 into: [:i :x | BODY. i + 1]
//!
//! coll do: [:x | BODY] separatedBy: [SEP]
//!   ⇒  coll inject: true into: [:first :x | first ifFalse: [SEP]. BODY. false]
//! ```
//!
//! Making the index / "first" accumulator a genuine block *parameter* (rather than a
//! synthetic local) means it correctly shadows any outer local of the same name. A
//! single synthetic `inject:into:` send keeps this to one annotated message send, so
//! the result composes with the enclosing method-body threading without the
//! double-parenthesised Core Erlang a nested `[ … ] value` would produce. In an actor
//! method we additionally re-project the fold's `{Acc, NewState}` tuple to
//! `{'nil', NewState}` so the selectors honour their `-> Nil` contract while keeping
//! the threaded `State` (see [`CoreErlangGenerator::finalize_enumeration_fold`]).
//!
//! Non-mutating calls return `None` here and fall through to the ordinary dispatch
//! to the `Collection.bt` method, preserving existing behaviour.

use super::super::super::document::{Document, leaf};
use super::super::super::{CodeGenContext, CoreErlangGenerator, Result, block_analysis};
use crate::ast::{
    Block, BlockParameter, Expression, ExpressionStatement, Identifier, KeywordPart, Literal,
    MessageSelector,
};
use crate::docvec;
use crate::source_analysis::Span;

// ── Synthetic AST builders ──────────────────────────────────────────────────
// These construct the desugared `inject:into:` tree. Spans are inherited from the
// originating block so diagnostics point back at user source.

fn ident(name: &str, span: Span) -> Expression {
    Expression::Identifier(Identifier::new(name, span))
}

fn int_lit(n: i64, span: Span) -> Expression {
    Expression::Literal(Literal::Integer(n), span)
}

fn binary(receiver: Expression, op: &str, arg: Expression, span: Span) -> Expression {
    Expression::MessageSend {
        receiver: Box::new(receiver),
        selector: MessageSelector::Binary(op.into()),
        arguments: vec![arg],
        is_cast: false,
        span,
    }
}

fn if_false(receiver: Expression, false_block: Expression, span: Span) -> Expression {
    Expression::MessageSend {
        receiver: Box::new(receiver),
        selector: MessageSelector::Keyword(vec![KeywordPart::new("ifFalse:", span)]),
        arguments: vec![false_block],
        is_cast: false,
        span,
    }
}

/// Builds `receiver inject: <initial> into: <inject_block>`.
fn inject_into(
    receiver: Expression,
    initial: Expression,
    inject_block: Expression,
    span: Span,
) -> Expression {
    Expression::MessageSend {
        receiver: Box::new(receiver),
        selector: MessageSelector::Keyword(vec![
            KeywordPart::new("inject:", span),
            KeywordPart::new("into:", span),
        ]),
        arguments: vec![initial, inject_block],
        is_cast: false,
        span,
    }
}

fn block_expr(params: &[&str], body: Vec<Expression>, span: Span) -> Expression {
    Expression::Block(Block::new(
        params
            .iter()
            .map(|p| BlockParameter::new(*p, span))
            .collect(),
        body.into_iter().map(ExpressionStatement::bare).collect(),
        span,
    ))
}

/// Clones a block's statements as bare expressions for inlining into the
/// synthetic `inject:into:` body (comment attachments are irrelevant to codegen).
fn inlined_body(body: &[ExpressionStatement]) -> Vec<Expression> {
    body.iter().map(|s| s.expression.clone()).collect()
}

impl CoreErlangGenerator {
    /// Returns `true` if `block` mutates field/local state in a way that requires
    /// threading (and so cannot be served by a plain dispatch to the `Collection.bt`
    /// method). Mirrors the per-block analysis in `control_flow_has_mutations`.
    fn enumeration_block_needs_threading(&self, block: &Block) -> bool {
        let analysis = block_analysis::analyze_block(block);
        self.needs_mutation_threading(&analysis)
            || self.body_has_list_op_cross_scope_mutations(block)
    }

    /// Returns `true` when the desugared fold threads actor `State`: an actor
    /// (`gen_server`) method body not inside a direct-params counted loop.
    ///
    /// The desugar itself fires in *every* context (so the stateful block is consumed
    /// by the fold and never dispatched as a mismatched-arity fun to the un-threaded
    /// `Collection.bt` method). But only an actor fold yields the `{Acc, NewState}`
    /// reply-tuple shape that [`Self::finalize_enumeration_fold`]'s `{'nil', NewState}`
    /// re-projection and the `get_control_flow_threaded_vars` `__local__` extraction
    /// rely on. Value types, class methods and the REPL thread captured locals through
    /// different fold shapes, and a fold nested in a direct-params loop yields an open
    /// let-chain rather than a tuple, so those keep the plain fold result.
    pub(in crate::codegen::core_erlang) fn enumeration_threads_actor_state(&self) -> bool {
        matches!(self.context, CodeGenContext::Actor) && !self.in_direct_params_loop
    }

    /// Lowers a synthetic `inject:into:` send (already threading state) to the value
    /// the enumeration methods actually return.
    ///
    /// In an actor the fold yields a `{Acc, NewState}` tuple whose element 2 is the
    /// threaded `State` map, so we bind the *annotated* fold (its parentheses make a
    /// safe `let`-value) and re-project `{'nil', NewState}` — honouring the methods'
    /// `-> Nil` contract while keeping the threaded state. The `let`-chain also breaks
    /// up the parentheses so the enclosing `eachWithIndex:`/`do:separatedBy:`
    /// annotation does not produce the invalid `( ( … ) )` two-deep parenthesisation
    /// that `core_parse` rejects.
    ///
    /// Elsewhere the fold is emitted as the *un-annotated* message send (the caller's
    /// own send annotation is then the only one, matching a hand-written
    /// `inject:into:`), and its accumulator value stands as the result.
    fn finalize_enumeration_fold(&mut self, inject_send: &Expression) -> Result<Document<'static>> {
        if self.enumeration_threads_actor_state() {
            let inject_doc = self.expression_doc(inject_send)?;
            let tuple_var = self.fresh_temp_var("EnumFold");
            return Ok(docvec![
                "let ",
                leaf::var(tuple_var.clone()),
                " = ",
                inject_doc,
                " in {'nil', call 'erlang':'element'(2, ",
                leaf::var(tuple_var),
                ")}",
            ]);
        }
        let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } = inject_send
        else {
            unreachable!(
                "finalize_enumeration_fold is only called with a synthetic inject:into: send"
            );
        };
        self.generate_message_send(receiver, selector, arguments)
    }

    /// BT-2703: Desugars `receiver eachWithIndex: [:elem :idx | body]` into a
    /// threaded `inject:into:` fold when the block mutates state. Returns `None`
    /// (so the caller dispatches to the `Collection.bt` method as before) for
    /// non-literal callables, the wrong block arity, or a non-mutating block.
    pub(in crate::codegen::core_erlang) fn try_generate_each_with_index(
        &mut self,
        receiver: &Expression,
        block_arg: &Expression,
    ) -> Result<Option<Document<'static>>> {
        let Expression::Block(user_block) = block_arg else {
            return Ok(None);
        };
        // eachWithIndex: takes a 2-arg block `[:elem :idx | …]`.
        if user_block.parameters.len() != 2 {
            return Ok(None);
        }
        if !self.enumeration_block_needs_threading(user_block) {
            return Ok(None);
        }

        let span = user_block.span;
        let elem_name = user_block.parameters[0].name.to_string();
        let idx_name = user_block.parameters[1].name.to_string();
        // Degenerate `[:x :x | …]` — leave it to the normal path's diagnostics.
        if elem_name == idx_name {
            return Ok(None);
        }

        // The 1-based index *is* the fold accumulator (seeded to 1), so the user's
        // index parameter becomes the inject block's accumulator parameter — a genuine
        // block parameter that correctly shadows any outer local of the same name,
        // rather than a synthetic local that would clobber it. The body runs with the
        // current index, then `idx + 1` is the next accumulator.
        let mut inject_body = inlined_body(&user_block.body);
        inject_body.push(binary(ident(&idx_name, span), "+", int_lit(1, span), span));

        let inject_send = inject_into(
            receiver.clone(),
            int_lit(1, span),
            block_expr(&[idx_name.as_str(), elem_name.as_str()], inject_body, span),
            span,
        );
        Ok(Some(self.finalize_enumeration_fold(&inject_send)?))
    }

    /// BT-2703: Desugars `receiver do: [:elem | body] separatedBy: [sep]` into a
    /// threaded `inject:into:` fold when either block mutates state, threading the
    /// "between elements" flag through the accumulator. Returns `None` (so the
    /// caller dispatches to the `Collection.bt` method as before) for non-literal
    /// callables, the wrong block arities, or two non-mutating blocks.
    pub(in crate::codegen::core_erlang) fn try_generate_do_separated_by(
        &mut self,
        receiver: &Expression,
        element_arg: &Expression,
        separator_arg: &Expression,
    ) -> Result<Option<Document<'static>>> {
        let (Expression::Block(element_block), Expression::Block(separator_block)) =
            (element_arg, separator_arg)
        else {
            return Ok(None);
        };
        // `do:` block is 1-arg `[:elem | …]`, separator is 0-arg `[…]`.
        if element_block.parameters.len() != 1 || !separator_block.parameters.is_empty() {
            return Ok(None);
        }
        if !self.enumeration_block_needs_threading(element_block)
            && !self.enumeration_block_needs_threading(separator_block)
        {
            return Ok(None);
        }

        let span = element_block.span;
        let elem_name = element_block.parameters[0].name.to_string();
        // The accumulator is the "is this the first element?" flag (`true` initially,
        // `false` thereafter). A fresh name keeps it clear of user identifiers.
        let first_name = self.fresh_temp_var("BtSepFirst");

        // inject:into: body: `first ifFalse: [sep]` (separator runs *between* elements,
        // so it is skipped on the first), the user body, then `false` as the new acc.
        let separator = block_expr(
            &[],
            inlined_body(&separator_block.body),
            separator_block.span,
        );
        let mut inject_body = vec![if_false(ident(&first_name, span), separator, span)];
        inject_body.extend(inlined_body(&element_block.body));
        inject_body.push(ident("false", span));

        let inject_send = inject_into(
            receiver.clone(),
            ident("true", span),
            block_expr(
                &[first_name.as_str(), elem_name.as_str()],
                inject_body,
                span,
            ),
            span,
        );
        Ok(Some(self.finalize_enumeration_fold(&inject_send)?))
    }
}
