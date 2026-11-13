// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tier 2 value-call classification and lowering.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Classifies `value:`-family sends on Tier 2 (stateful) blocks and
//! control-flow expressions that mutate threaded state, and lowers the
//! `{Result, NewState}` tuple-unpacking that such calls need (including the
//! sequential Tier 2 cascade).

use crate::core_erlang::{CodeGenError, CoreErlangGenerator, Result};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, leaf};
use beamtalk_core::ast::{Expression, MessageSelector};

impl CoreErlangGenerator {
    /// Checks if an expression is a `value:` call on a Tier 2 block parameter.
    ///
    /// When true, the expression will generate a `{Result, NewState}` tuple via
    /// `generate_block_value_call_stateful()` and must be unpacked by the caller.
    pub(in crate::core_erlang) fn is_tier2_value_call(&self, expr: &Expression) -> bool {
        // BT-3841 / BT-3982: an evaluated literal block that is a producer here
        // is lowered by the evaluated-block producer (see
        // `control_flow_has_mutations`).
        if self.evaluated_block_is_producer(expr) {
            return false;
        }
        if let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } = expr
        {
            // BT-3881: a block literal of another arity is applied
            // (`block_literal_of_other_arity`), answering a plain value.
            if beamtalk_core::semantic_analysis::block_facts::block_literal_of_other_arity(
                receiver,
                selector,
                arguments.len(),
            )
            .is_some()
            {
                return false;
            }
            if selector.is_block_invocation() {
                // A local variable this method itself assigned a Tier 2
                // block literal to earlier in its own body (tier2_local_vars).
                if let Expression::Identifier(id) = receiver.as_ref()
                    && self.tier2_local_vars.contains(id.name.as_str())
                {
                    return true;
                }
            }
            // Literal-block-with-mutations receivers stay scoped to the
            // positional value/value:/... selectors — generate_block_value_inline_with_mutations
            // binds `arguments` directly to the block's own parameters, which
            // doesn't hold for valueWithArguments: (a single runtime list, not
            // per-parameter positional args). Not a motivating shape here.
            if beamtalk_core::semantic_analysis::block_facts::is_safe_value_family_selector(
                selector,
            ) {
                // An inline block literal with captured or field mutations
                // (e.g. [errors := errors add: #foo] value) yields a tuple
                // outside a value-type method: the one rule the intrinsics
                // inline by (`evaluated_block_yields_state_tuple`). A block
                // that only reads state or calls out is a closure on the cell
                // (ADR 0137).
                if let Expression::Block(block) = receiver.as_ref()
                    && self.evaluated_block_yields_state_tuple(block)
                {
                    return true;
                }
            }
        }
        // `blk value: x; value: y` — a cascade where every message
        // (including the one the parser folds into `receiver` — see
        // `normalize_cascade`) is itself a safe value-family send on a receiver
        // that (by the same rules as the single-send case above) may hold a
        // Tier 2 block. Each message needs the same tuple-unpacking treatment
        // as a single Tier2ValueCall, sequenced through
        // `generate_tier2_cascade_doc`.
        if let Expression::Cascade {
            receiver, messages, ..
        } = expr
        {
            let (underlying_receiver, all_messages) = Self::normalize_cascade(receiver, messages);
            let all_safe_value_sends = !all_messages.is_empty()
                && all_messages.iter().all(|(sel, _)| {
                    beamtalk_core::semantic_analysis::block_facts::is_safe_value_family_selector(
                        sel,
                    )
                });
            if all_safe_value_sends
                && let Expression::Identifier(id) = underlying_receiver
                && self.tier2_local_vars.contains(id.name.as_str())
            {
                return true;
            }
        }
        false
    }

    /// Returns captured mutation variable names for a Tier 2
    /// value-call statement (`expr` already classified/proven
    /// `BodyExprKind::Tier2ValueCall` by `is_tier2_value_call`) whose receiver
    /// mutates outer locals, so the caller can rebind them after the call.
    ///
    /// Handles both:
    /// - An inline block literal receiver (`[block] value`/`value:`/... —
    ///   the original scope), via `captured_mutations_for_block` on
    ///   the literal directly.
    /// - A NAMED `tier2_local_vars` identifier receiver (`blk value:
    ///   x`) whose block literal was assigned earlier in the same method —
    ///   the call site only has the identifier, not the block AST, so this
    ///   looks up the mutations `prescan_tier2_local_vars` already recorded
    ///   for that variable name in `tier2_local_var_captured_mutations`.
    ///
    /// Also handles a `Cascade` expression (`blk value: x; value: x`) by
    /// normalizing to its true underlying receiver first — the same
    /// receiver-shape checks then apply.
    //
    // Widened from private to
    // `pub(in crate::core_erlang)` so `control_flow/conditionals.rs`
    // can rebind captured local-var mutations for a bare `Tier2ValueCall`
    // statement inside a conditional branch, mirroring this file's own
    // `Tier2ValueCall` handling.
    pub(in crate::core_erlang) fn get_inline_block_captured_mutations(
        &self,
        expr: &Expression,
    ) -> Option<Vec<String>> {
        let receiver = match expr {
            Expression::MessageSend { receiver, .. } => receiver.as_ref(),
            Expression::Cascade {
                receiver, messages, ..
            } => Self::normalize_cascade(receiver, messages).0,
            _ => return None,
        };
        if let Expression::Identifier(id) = receiver
            && let Some(mutations) = self
                .tier2_local_var_captured_mutations
                .get(id.name.as_str())
        {
            return Some(mutations.clone());
        }
        Self::inline_block_captured_mutations(expr)
    }

    /// Whether an operand-position Tier 2 call on the local `name` merges
    /// into its bracket (BT-4122): a may-write Tier 2 local outside a
    /// cell closure (no bracket wraps there) and outside the two faults
    /// (test-only; the no-brackets fault wraps nothing, and the legacy
    /// fault compiles the legacy `fun`, whose fields live in the return
    /// the merge would drop — full legacy behavior instead). The single
    /// source `close_tier2_value_subexpr_doc` (tuple through vs legacy
    /// drop) and `synced_call_value` (unpack element 1 + rebind locals vs
    /// plain value) both read, so the two cannot disagree.
    pub(in crate::core_erlang) fn tier2_operand_merges(&self, name: &str) -> bool {
        let faulted = fault!(NoBrackets) || fault!(LegacyTier2);
        !faulted
            && !self.in_cell_closure()
            && self.tier2_local_vars.contains(name)
            && self.tier2_local_var_may_write_state.contains(name)
    }

    /// The receiver name when `expr` is a Tier 2 `value`-family send on a
    /// recorded Tier 2 local (BT-4122): `None` for an inline literal
    /// (inlined, no `fun`), a cascade that normalizes to one, or anything
    /// else. No selector check: promotion proved every use of the var is
    /// a `value`-family send, so the name suffices. The wrap-side half of
    /// [`Self::tier2_operand_merges`].
    pub(in crate::core_erlang) fn tier2_merge_call_name(expr: &Expression) -> Option<&str> {
        let inner = expr.unwrap_parens();
        let receiver = match inner {
            Expression::MessageSend { receiver, .. } => receiver.as_ref(),
            Expression::Cascade {
                receiver, messages, ..
            } => {
                beamtalk_core::semantic_analysis::block_facts::normalize_cascade(receiver, messages)
                    .0
            }
            _ => return None,
        };
        match receiver.unwrap_parens() {
            Expression::Identifier(id) => Some(id.name.as_str()),
            _ => None,
        }
    }

    // BT-4122: no caller-unpack arm — a legacy Tier 2 statement unpack
    // rebinds `State` from a return the `fun` threaded self-consistently
    // from a fresh call-time state, so it is correct, and flagging it
    // would fire only on correct code. The `FieldReadFromStateAcc`
    // fun-body arm is the rule's tripwire instead: it forces the
    // position-robust merged shape (a legacy `fun` drops field writes
    // in positions that discard its return — operand drops, closure
    // scratch maps).

    /// Generates the `Document` for an
    /// expression already classified as `BodyExprKind::Tier2ValueCall` or the
    /// RHS of `BodyExprKind::LocalAssignTier2` — i.e. a `value`/`value:`/etc.
    /// send that `is_tier2_value_call` proved needs Tier 2 tuple-unpacking
    /// treatment.
    ///
    /// A `tier2_local_vars` identifier receiver goes
    /// through `generate_block_value_call_stateful` directly, so the raw
    /// `{Result, NewState}` tuple reaches the caller, which unpacks it; an
    /// arbitrary sub-expression reached via plain `expression_doc` would not
    /// unpack it. Every other `Tier2ValueCall` shape (an inline literal block
    /// with captured/field mutations) falls through to `expression_doc`, which
    /// already handles it correctly.
    ///
    /// Also called from `control_flow/mod.rs`'s
    /// `lower_local_var_assignment_bind` (the `is_tier2_value_call`
    /// branch there) for the same reason: it unpacks a
    /// `{Result, NewState}` tuple.
    ///
    /// `valueWithArguments:` has no compile-time-known-Tier-2
    /// "stateful" fast path the way `value`/`value:` do
    /// (`generate_block_value_call_stateful`) — `is_tier2_value_call` only
    /// ever proves a `valueWithArguments:` send needs Tier 2 handling at
    /// all, never which arity branch statically applies, so every match
    /// (`tier2_local_vars`) routes through the
    /// runtime-discriminated codegen here, unconditionally.
    ///
    /// A block stored in a field is not a `Tier2ValueCall`: a block that
    /// writes a field or a captured local and is stored in a field is a
    /// compile error (ADR 0133, BT-3902/BT-3922), so a field only ever holds
    /// a Tier 1 block (BT-3986).
    pub(in crate::core_erlang) fn generate_tier2_value_call_doc(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        if let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } = expr
        {
            if selector.name() == "valueWithArguments:" {
                // The parser always gives a keyword message at least one
                // argument, so `arguments` is never empty here — but fail
                // loudly instead of silently falling through to the
                // positional-value branch below, which would emit a
                // malformed 0-arity value call for this selector.
                let args_expr = arguments.first().ok_or_else(|| {
                    CodeGenError::Internal(
                        "valueWithArguments: with no argument expression".to_string(),
                    )
                })?;
                // The non-fun fallback is a dynamic site (BT-4596).
                self.report_block_value_fallback(receiver, selector);
                return self.generate_block_value_with_arguments_call_runtime_discriminated(
                    receiver, args_expr,
                );
            }
            // `tier2_local_vars` receivers must
            // also bypass `expression_doc` below — since `try_generate_block_value_unary`/
            // `try_generate_block_value_keyword` now intercept this same
            // receiver shape from the generic sub-expression dispatch and
            // (correctly, for THAT position) close the tuple down to just
            // `Result`. Calling `generate_block_value_call_stateful` directly
            // here, exactly like the `self.field` case above, keeps this
            // TOP-LEVEL statement path getting the raw `{Result, NewState}`
            // tuple its callers (`BodyExprKind::Tier2ValueCall` handling in
            // this file, `conditionals.rs`, `control_flow/mod.rs`) unpack.
            if let Expression::Identifier(id) = receiver.as_ref()
                && self.tier2_local_vars.contains(id.name.as_str())
            {
                return self.generate_block_value_call_stateful(receiver, arguments);
            }
            // BT-3982: a literal block receiver is inlined by
            // `generate_block_value_inline_with_mutations`, whose raw tuple
            // this statement's caller unpacks: the one consumed site. Only a
            // block the actor producer does not claim reaches here in a normal
            // compile (one it claims is classified to the producer), so the
            // registration matters only under the test fault
            // `EvaluatedBlockNotAProducer`, which keeps such a statement on this
            // path and must not report it.
            if let Expression::Block(block) = receiver.as_ref() {
                self.tier2_evaluated_block_sites.insert(block.span);
                let doc = self.expression_doc(expr);
                self.tier2_evaluated_block_sites.remove(&block.span);
                return doc;
            }
        }
        // `blk value: x; value: y` — proved safe by `is_tier2_value_call`'s
        // Cascade branch. Unlike the single-send case, `expression_doc` has no
        // generic Tier 2 cascade handling to fall through to, so this must be
        // generated directly regardless of receiver kind.
        if let Expression::Cascade {
            receiver, messages, ..
        } = expr
        {
            let (underlying_receiver, all_messages) = Self::normalize_cascade(receiver, messages);
            return self.generate_tier2_cascade_doc(underlying_receiver, &all_messages);
        }
        self.expression_doc(expr)
    }

    /// Generates a sequential Tier 2 tuple-unpacking cascade
    /// (`blk value: x; value: y`), reached only for a `Cascade` expression that
    /// `is_tier2_value_call` already proved is entirely safe `value`-family
    /// sends on a receiver that may hold a Tier 2 block.
    ///
    /// The receiver is evaluated once per message rather than hoisted into a
    /// single shared binding — harmless here since a `tier2_local_vars`
    /// receiver is always a pure variable read, never an
    /// expression with side effects. Each message is
    /// generated with the *current* threaded state (via
    /// `generate_block_value_call_stateful`, which reads
    /// `self.current_state_var()` internally), then its
    /// returned `{Result, NewState}` tuple is unpacked and `NewState` becomes
    /// the current state for the next message — mirroring the sequencing the
    /// bare (non-cascade) `Tier2ValueCall`/`LocalAssignTier2` call sites already
    /// use between statements.
    ///
    /// Matching ordinary (non-Tier-2) cascade semantics, the overall value is
    /// the result of the LAST message. Returns a `{Result, NewState}` tuple
    /// with that contract — callers unpack it exactly like a single
    /// `Tier2ValueCall` (see `generate_tier2_value_call_doc`'s own callers).
    fn generate_tier2_cascade_doc(
        &mut self,
        receiver: &Expression,
        all_messages: &[(&MessageSelector, &[Expression])],
    ) -> Result<Document<'static>> {
        let mut parts: Vec<Document<'static>> = Vec::with_capacity(all_messages.len() + 1);
        let mut result_var: Option<String> = None;
        let mut state_var: Option<String> = None;
        for (_, args) in all_messages {
            let call_doc = self.generate_block_value_call_stateful(receiver, args)?;
            let tuple_var = self.fresh_temp_var("CascTuple");
            let this_result = self.fresh_temp_var("CascResult");
            let this_state = self.next_state_var();
            parts.push(docvec![
                "let ",
                leaf::var(tuple_var.clone()),
                " = ",
                call_doc,
                " in let ",
                leaf::var(this_result.clone()),
                " = call 'erlang':'element'(1, ",
                leaf::var(tuple_var.clone()),
                ") in let ",
                leaf::var(this_state.clone()),
                " = call 'erlang':'element'(2, ",
                leaf::var(tuple_var),
                ") in ",
            ]);
            result_var = Some(this_result);
            state_var = Some(this_state);
        }
        // is_tier2_value_call requires a non-empty message list before ever
        // routing here, so both are always populated by the loop above.
        let result_var = result_var.expect("cascade must have at least one message");
        let state_var = state_var.expect("cascade must have at least one message");
        parts.push(docvec![
            "{",
            leaf::var(result_var),
            ", ",
            leaf::var(state_var),
            "}"
        ]);
        Ok(Document::Vec(parts))
    }

    /// Checks if a control flow expression actually threads state through mutations.
    ///
    /// This goes beyond mere selector-based classification by analysing whether
    /// the block argument(s) contain mutations that require state threading.
    ///
    /// Returns `true` only if:
    /// 1. The expression is a `ControlFlow` dispatch (from pre-computed `dispatch_kinds`),
    ///    or — when semantic facts are unavailable — the selector matches a known
    ///    exception/conditional selector as a fallback.
    /// 2. The relevant block argument(s) need state threading in the current context
    ///    (checked via `needs_mutation_threading` on pre-computed `block_profiles`).
    ///
    /// Using pre-computed `dispatch_kinds` and `block_profiles` avoids the repeated
    /// selector-based re-classification and `analyze_block` calls that the original
    /// implementation performed.
    pub(in crate::core_erlang) fn control_flow_has_mutations(&self, expr: &Expression) -> bool {
        // See through parentheses so `_r := (1 to: 5 do: [...])` is still
        // classified as control flow with mutations (and thus unpacked + threaded)
        // rather than falling through to a plain pure local assignment.
        let expr = expr.unwrap_parens();

        // BT-3841 / BT-3982: an evaluated literal block that is a producer here
        // (a local-threading one, or an actor one that threads `State`) is
        // control flow with mutations: its arm's writes come back through the
        // producer's prelude, never through the legacy Tier 2 inline-block
        // tuple ([`Self::is_tier2_value_call`] leaves it to this classifier).
        if self.evaluated_block_is_producer(expr) {
            return true;
        }

        // `match:` is a dedicated `Expression::Match` node, not a
        // `MessageSend`, so it's otherwise invisible to this classifier — a
        // `match:` arm body that is a state-mutating `[...] value` block would
        // fall through to `BodyExprKind::Pure` and leak its raw `{Result,
        // NewState}` tuple as the match's value. `generate_match` threads state
        // through every arm (see `match_needs_state_threading`) whenever any
        // arm needs it, so route it through the same tuple-unwrap machinery as
        // `ifTrue:`/`ifFalse:`.
        if let Expression::Match { arms, .. } = expr {
            return self.match_needs_state_threading(arms);
        }

        let Expression::MessageSend {
            receiver,
            arguments,
            selector: beamtalk_core::ast::MessageSelector::Keyword(parts),
            span,
            ..
        } = expr
        else {
            return false;
        };

        // ADR 0137: a loop, fold or conditional whose block operands are not
        // literal blocks is not lowered in place (nothing can be threaded
        // through a block the compiler cannot see): it is a call that may run
        // user code, bracketed as one, never a construct tuple.
        if self.synced_calls_active()
            && self.classify_expr(expr) == crate::core_erlang::synced_call::UserCodeCall::Yes
        {
            return false;
        }

        // Use pre-computed dispatch classification instead of re-deriving it.
        // When semantic_facts is empty (e.g. in unit tests constructed via
        // `CoreErlangGenerator::new`), `dispatch_kind` returns `Unknown`.
        // In that case fall back to local selector-based classification so the
        // function still returns the correct result for known control-flow
        // selectors rather than silently returning `false` for all of them.
        let dispatch_kind = self.semantic_facts.dispatch_kind(span);
        let sel_str: String = parts.iter().map(|p| p.keyword.as_str()).collect();
        let is_control_flow = match dispatch_kind {
            beamtalk_core::semantic_analysis::DispatchKind::ControlFlow => true,
            beamtalk_core::semantic_analysis::DispatchKind::Unknown => {
                beamtalk_core::state_threading_selectors::is_exception_selector(sel_str.as_str())
                    || beamtalk_core::state_threading_selectors::is_conditional_selector(
                        sel_str.as_str(),
                    )
            }
            _ => false,
        };
        if !is_control_flow {
            return false;
        }

        // ADR 0118 phase 3: `whileTrue:`/`whileFalse:`'s RECEIVER
        // is the condition block — like `ensure:`/`on:do:`'s try-body
        // receiver just below, and the conditional-selector receiver
        // further down, it may itself have state effects (a self-send, or
        // an `and:`/`or:` that carries one) even when the BODY argument has
        // none, and neither `is_exception_selector` nor
        // `is_conditional_selector` cover it — the "standard check" below
        // only ever walks `arguments` (the body), never `receiver`. Shares
        // `while_loops.rs`'s own gate (`condition_has_state_effects`)
        // rather than re-deriving it, so the two decisions — "does this
        // statement need `ControlFlowWithMutations` classification" here,
        // "does this loop's own condition need threading" there — cannot
        // disagree.
        if matches!(sel_str.as_str(), "whileTrue:" | "whileFalse:")
            && super::super::control_flow::condition_has_state_effects(
                self,
                receiver,
                arguments
                    .first()
                    .and_then(beamtalk_core::semantic_analysis::block_facts::while_body_block),
            )
        {
            return true;
        }

        // For on:do: and ensure:, the receiver (try body) is also
        // a block that may contain field mutations.
        if beamtalk_core::state_threading_selectors::is_exception_selector(sel_str.as_str())
            && let Expression::Block(block) = receiver.as_ref()
        {
            // Also covers a nested list-op/counted-loop inside the
            // try body mutating an outer local even when the try body's own
            // top-level analysis sees no direct mutation (`analyze_block`
            // does not propagate writes out of a nested, non-conditional
            // block — same gap already closed elsewhere for
            // conditionals and ordinary block arguments below). Without
            // this, `[nested-loop] ensure: [...]`/`on:do:` would be
            // classified as pure here even though the nested loop's own
            // cross-scope collector (this call site's sibling,
            // `loop_threaded_locals`) correctly detects the
            // mutation — the same "two decision points disagree" shape as
            // the do:/collect: self-classification gap this issue fixes.
            if self.block_arg_needs_threading(block) {
                return true;
            }
        }

        // For Boolean conditionals, any block argument may contain mutations.
        // IfNotNil: also needs per-block mutation detection.
        if beamtalk_core::state_threading_selectors::is_conditional_selector(sel_str.as_str()) {
            // The conditional's own RECEIVER may be an actor
            // self-send (`(self recordOnce: x) ifTrue:ifFalse:`) whose own
            // state mutation must be threaded, even when neither block
            // argument mutates anything itself. Must stay in sync with
            // `intrinsics.rs`'s `try_generate_boolean_protocol`'s matching
            // check — the two are independently-computed decisions that
            // must agree (see this file's own commentary on that class of
            // bug, e.g. the "two decision points disagree" note above).
            // Widened to any self-send needing threading in the
            // receiver's sub-tree (`((self recordOnce: x) and: [y])
            // ifTrue: [...]`) — the same probe `compile_conditional_receiver`
            // threads ahead with, so the two cannot disagree.
            //
            // BT-3910: a receiver holding a value-position local assignment is
            // sequenced ahead by `threaded_expression`
            // (`conditional_receiver_operand`), so its `LocalRebind`
            // reaches the statement sequence instead of a closed tuple document.
            if self.conditional_receiver_needs_threading(receiver)
                && self.contained_local_assignment_targets(receiver).is_empty()
            {
                return true;
            }
            for arg in arguments {
                // `block_arg_needs_threading` also catches a nested list
                // op inside a branch mutating an outer local even when the
                // branch block itself has no direct mutation (`analyze_block`
                // does not propagate writes out of nested blocks) — e.g.
                // `flag ifTrue: [ items do: [:x | sum := sum + x] ]`.
                if let Expression::Block(block) = arg
                    && self.block_arg_needs_threading(block)
                {
                    return true;
                }
            }
            return false;
        }

        // Standard check: analyse argument blocks for mutations.
        // Check ALL block arguments, not just the last one.
        // For selectors like `detect:ifNone:`, the mutation-bearing block is the
        // first argument (predicate), not the last (ifNone handler).
        // `block_arg_needs_threading` also catches nested list ops with
        // cross-scope mutations that `analyze_block` alone can't see.
        //
        // Conditionals have returned above, so what reaches here is a loop or
        // fold (and the handler/cleanup blocks of `on:do:`/`ensure:`). A loop
        // or fold block is read through the loop gate's own predicate
        // (`loop_body_has_carried_mutations`, BT-3834), so a loop that carries
        // nothing — and is therefore lowered as a plain loop — is never
        // unpacked as a `{Value, StateAcc}` tuple.
        let is_exception =
            beamtalk_core::state_threading_selectors::is_exception_selector(sel_str.as_str());
        for arg in arguments {
            if let Expression::Block(block) = arg {
                let threads = if is_exception {
                    self.block_arg_needs_threading(block)
                } else {
                    self.loop_body_has_carried_mutations(block)
                };
                if threads {
                    return true;
                }
            }
        }

        false
    }
}
