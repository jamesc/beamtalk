// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Actor state threading for `eachWithIndex:` and `do:separatedBy:`.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Unlike the foldl list-ops in the sibling modules, these two enumeration
//! selectors are *self-hosted* in `collection.bt` on top of `inject:into:`. A
//! plain dispatch to those methods cannot thread an actor's `State` back out of the
//! block, so a mutating block silently lost its writes — and, once the selectors
//! were classified as state-threading, the block was emitted as a stateful
//! `{Value, State}`-tuple fun whose arity no longer matched the un-threaded
//! `collection.bt` method, crashing with `badarity`.
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
//! to the `collection.bt` method, preserving existing behaviour.

use super::super::super::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, leaf};
use beamtalk_core::ast::{
    Block, BlockParameter, Expression, ExpressionStatement, Identifier, KeywordPart, Literal,
    MessageSelector,
};
use beamtalk_core::semantic_analysis::block_facts::producer_body_arm_handler;
use beamtalk_core::source_analysis::Span;

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

/// Rebuilds the user's `eachWithIndex:`/`do:separatedBy:` send from the parts
/// the intrinsic dispatch hands over, so
/// [`CoreErlangGenerator::finalize_enumeration_fold`] asks its producer
/// question of the send the consumer asks it of (the predicates read the
/// selector and the blocks, not the send's span).
fn enumeration_send(
    receiver: &Expression,
    selector: &MessageSelector,
    arguments: &[Expression],
) -> Expression {
    let span = arguments
        .last()
        .map_or(receiver.span(), |last| receiver.span().merge(last.span()));
    Expression::MessageSend {
        receiver: Box::new(receiver.clone()),
        selector: selector.clone(),
        arguments: arguments.to_vec(),
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
    /// Returns `true` when the desugared fold threads actor `State`: an actor
    /// (`gen_server`) method body not inside a direct-params counted loop.
    ///
    /// The desugar itself fires in *every* context (so the stateful block is consumed
    /// by the fold and never dispatched as a mismatched-arity fun to the un-threaded
    /// `collection.bt` method). But only an actor fold yields the `{Acc, NewState}`
    /// reply-tuple shape that [`Self::finalize_enumeration_fold`]'s `{'nil', NewState}`
    /// re-projection and the `threaded_locals_of` `__local__` extraction
    /// rely on. Value types, class methods and the REPL thread captured locals through
    /// different fold shapes, and a fold nested in a direct-params loop yields an open
    /// let-chain rather than a tuple, so those keep the plain fold result.
    pub(in crate::core_erlang) fn enumeration_threads_actor_state(&self) -> bool {
        matches!(self.context, CodeGenContext::Actor) && !self.loop_mode.in_direct_params_loop
    }

    /// Whether the user's `eachWithIndex:`/`do:separatedBy:` send
    /// `original_send` is a local-threading producer's construct tuple in a
    /// class or value-type method (BT-3841): the desugared fold answers
    /// `{Acc, StateAcc}`, whose element 1 is the index / "first" accumulator,
    /// not the `nil` the selectors answer, so it is re-projected like an
    /// actor's. A fold nested in a direct-params loop is an open let-chain
    /// instead and keeps the plain fold result.
    ///
    /// BT-3855: the question is asked of the *original* send, through
    /// [`Self::construct_tuple_locals`], the one the producer lowering
    /// (`local_producer_locals`) asks before it unpacks the construct tuple
    /// (in a class or value-type method it is `expr_yields_vt_threaded_tuple`,
    /// which the value-type statement classifier reads too). So the fold is
    /// re-projected exactly when the site that compiles it unpacks it.
    fn enumeration_yields_construct_tuple(&self, original_send: &Expression) -> bool {
        (self.threads_no_actor_state() || matches!(self.context, CodeGenContext::ValueType))
            && !self.loop_mode.in_direct_params_loop
            && self.construct_tuple_locals(original_send).is_some()
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
    /// A class or value-type method whose `original_send` is a producer's
    /// construct tuple ([`Self::enumeration_yields_construct_tuple`]) re-projects
    /// the same way. Elsewhere the fold is emitted as the *un-annotated* message
    /// send (the caller's own send annotation is then the only one, matching a
    /// hand-written `inject:into:`), and its accumulator value stands as the result.
    fn finalize_enumeration_fold(
        &mut self,
        original_send: &Expression,
        inject_send: &Expression,
        user_selector: &'static str,
    ) -> Result<Document<'static>> {
        // Both callers (`try_generate_each_with_index` / `try_generate_do_separated_by`)
        // build `inject_send` via `inject_into`, which always returns an
        // `Expression::MessageSend`, so this destructure cannot fail.
        let Expression::MessageSend {
            receiver,
            arguments,
            span,
            ..
        } = inject_send
        else {
            unreachable!(
                "finalize_enumeration_fold is only called with a synthetic inject:into: send"
            );
        };
        // The synthetic fold is lowered by the `inject:into:` list-op itself,
        // handed the selector the user sent for its receiver guard (BT-3916),
        // not dispatched as a message.
        let [initial, body] = arguments.as_slice() else {
            unreachable!("a synthetic inject:into: send has two arguments");
        };
        if self.enumeration_threads_actor_state() {
            // The annotated fold (its parentheses make a safe `let`-value).
            let fold_doc = self.generate_list_inject_for(receiver, initial, body, user_selector)?;
            let inject_doc = self.annotate_send_with_line(fold_doc, *span);
            return Ok(self.reproject_enumeration_fold(inject_doc));
        }
        // The synthetic send is generated as a message send, not as an
        // expression: its span is not one of the statement's producer sites, so
        // it must not be taken for a producer itself.
        let reproject = self.enumeration_yields_construct_tuple(original_send);
        let fold_doc = self.generate_list_inject_for(receiver, initial, body, user_selector)?;
        if reproject {
            return Ok(self.reproject_enumeration_fold(fold_doc));
        }
        Ok(fold_doc)
    }

    /// Binds the fold's `{Acc, NewState}` tuple and answers `{'nil', NewState}`.
    fn reproject_enumeration_fold(&mut self, fold_doc: Document<'static>) -> Document<'static> {
        let tuple_var = self.fresh_temp_var("EnumFold");
        docvec![
            "let ",
            leaf::var(tuple_var.clone()),
            " = ",
            fold_doc,
            " in {'nil', call 'erlang':'element'(2, ",
            leaf::var(tuple_var),
            ")}",
        ]
    }

    /// Desugars `receiver eachWithIndex: [:elem :idx | body]` into a
    /// threaded `inject:into:` fold when the block mutates state. Returns `None`
    /// (so the caller dispatches to the `collection.bt` method as before) for
    /// non-literal callables, the wrong block arity, or a non-mutating block.
    pub(in crate::core_erlang) fn try_generate_each_with_index(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        let [Expression::Block(user_block)] = arguments else {
            return Ok(None);
        };
        // eachWithIndex: takes a 2-arg block `[:elem :idx | …]`.
        if user_block.parameters.len() != 2 {
            return Ok(None);
        }
        if !self.loop_body_has_carried_mutations(user_block) {
            // falling through to `collection.bt`'s own self-hosted
            // `eachWithIndex:` (built on `do:`).
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
        let original_send = enumeration_send(receiver, selector, arguments);
        Ok(Some(self.finalize_enumeration_fold(
            &original_send,
            &inject_send,
            "eachWithIndex:",
        )?))
    }

    /// Desugars `receiver do: [:elem | body] separatedBy: [sep]` into a
    /// threaded `inject:into:` fold when either block mutates state, threading the
    /// "between elements" flag through the accumulator. Returns `None` (so the
    /// caller dispatches to the `collection.bt` method as before) for non-literal
    /// callables, the wrong block arities, or two non-mutating blocks.
    pub(in crate::core_erlang) fn try_generate_do_separated_by(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        let [
            Expression::Block(element_block),
            Expression::Block(separator_block),
        ] = arguments
        else {
            return Ok(None);
        };
        // `do:` block is 1-arg `[:elem | …]`, separator is 0-arg `[…]`.
        if element_block.parameters.len() != 1 || !separator_block.parameters.is_empty() {
            return Ok(None);
        }
        // The same gate as `eachWithIndex:`, over both blocks.
        if !self.loop_body_has_carried_mutations(element_block)
            && !self.loop_body_has_carried_mutations(separator_block)
        {
            // falling through to `collection.bt`'s own self-hosted
            // `do:separatedBy:` (built on `inject:into:`).
            return Ok(None);
        }
        let original_send = enumeration_send(receiver, selector, arguments);
        // Always the separator: the selector, arity and block shapes are checked.
        // A miss here must not fall back to `collection.bt`'s un-threaded
        // `do:separatedBy:`: the gate above found carried mutations, and the
        // fallback would drop them (the BT-3834 class of bug).
        let Some(arm_handler) = producer_body_arm_handler(&original_send) else {
            return Err(CodeGenError::Internal(
                "do:separatedBy: with carried mutations: core's producer_body_arm_handler \
                 names no separator, so its writes cannot be threaded"
                    .to_string(),
            ));
        };

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
        // The conditional carries the separator block's span (a node of the user's
        // source, not the element block's), so a producer lowering of it can be
        // told from the construct's own: core's `producer_body_arm_handler` names
        // the separator as the handler the producer runs as a body-frame arm, at
        // its own span (BT-3855), the rule the producer agreement test reads.
        let mut inject_body = vec![if_false(
            ident(&first_name, span),
            separator,
            arm_handler.span,
        )];
        inject_body.extend(inlined_body(&element_block.body));
        inject_body.push(ident("false", span));

        // The fold's block is not the user's element block, whose span it would
        // otherwise share: the block profile lookup (`block_profile_or_analyze`) is
        // keyed by span, and the user's block holds no separator write.
        let fold_block_span = Span::new(span.start(), span.end().saturating_add(1));
        let inject_send = inject_into(
            receiver.clone(),
            ident("true", span),
            block_expr(
                &[first_name.as_str(), elem_name.as_str()],
                inject_body,
                fold_block_span,
            ),
            span,
        );
        Ok(Some(self.finalize_enumeration_fold(
            &original_send,
            &inject_send,
            "do:separatedBy:",
        )?))
    }
}
