// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Type inference for cascade expressions.
//!
//! **DDD Context:** Semantic Analysis
//!
//! Covers `Expression::Cascade` (`receiver msg1; msg2; msg3`) — all messages
//! dispatch to the receiver of the cascade's first send, not to its return
//! value, so this walks the class-side / `Meta` / `Known` / `Union` receiver
//! shapes to validate each continuation message the same way
//! `infer_message_send_with_receiver_ty` validates the first.

use crate::ast::{CascadeMessage, Expression, MessageSelector, WellKnownSelector};
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::type_checker::{InferredType, TypeChecker, TypeEnv};
use crate::source_analysis::is_equality_operator;

impl TypeChecker {
    /// Infer a cascade `receiver msg1; msg2; msg3` — the `Expression::Cascade`
    /// arm of `infer_expr`'s dispatch.
    ///
    /// All messages dispatch to the *underlying* receiver of the first send,
    /// not to its return value: the parser bundles `obj msg1; msg2; msg3` as
    /// `Cascade { receiver: MessageSend(obj, msg1), messages: [msg2, msg3] }`,
    /// so this peeks through the first `MessageSend` to find the actual
    /// receiver and validates each continuation message against it,
    /// mirroring the class-side / `Meta` / `Known` / `Union` dispatch
    /// `infer_message_send_with_receiver_ty` performs for the first send.
    #[allow(clippy::too_many_lines)] // one branch per receiver-knowledge shape — irreducible
    pub(in crate::semantic_analysis::type_checker) fn infer_cascade(
        &mut self,
        receiver: &Expression,
        messages: &[CascadeMessage],
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> InferredType {
        // BT-2035: to avoid walking the inner receiver subtree twice
        // (which would double-emit any DNU / type diagnostics it
        // produces), we infer the inner type once and thread it into
        // the first send's inference via `infer_message_send_with_receiver_ty`.
        let (send_ty, cascade_target, dispatch_ty) = if let Expression::MessageSend {
            receiver: inner,
            selector: inner_sel,
            arguments: inner_args,
            span: inner_span,
            is_cast: false,
            ..
        } = receiver
        {
            let inner_ty = self.infer_expr(inner, hierarchy, env, in_abstract_method);
            let send_ty = self.infer_message_send_with_receiver_ty(
                inner,
                inner_ty.clone(),
                inner_sel,
                inner_args,
                *inner_span,
                hierarchy,
                env,
                in_abstract_method,
            );
            // The cascade's first-send node (the outer `MessageSend`
            // that is `receiver`) bypasses `infer_expr`, so record its
            // type in the LSP type map and run the BT-1914 Dynamic
            // warning for it here — mirroring `infer_expr`'s tail.
            self.post_process_expr_type(receiver, &send_ty);
            (send_ty, inner.as_ref(), inner_ty)
        } else {
            // Non-MessageSend receiver (or a cast send, which short-circuits
            // to Dynamic): fall back to a single infer_expr; the cascade
            // dispatches messages to that same type.
            let send_ty = self.infer_expr(receiver, hierarchy, env, in_abstract_method);
            (send_ty.clone(), receiver, send_ty)
        };
        // ADR 0102 §5 (BT-2744): resolve a `Negation`-typed cascade
        // target through `base`, mirroring the same substitution in
        // `infer_message_send_with_receiver_ty` — without it,
        // cascaded messages after the first would silently skip
        // DNU/argument checking entirely (`dispatch_ty` matches
        // neither the `Known` nor `Union` arms below).
        let dispatch_ty = if let InferredType::Negation { base, .. } = dispatch_ty {
            *base
        } else {
            dispatch_ty
        };
        // BT-2158: normalise the cascade target so parenthesised
        // class references (`(HTTPRouter) build: [...]; ...`) are
        // treated as class-side both for block-param inference and
        // downstream selector validation.
        let unwrapped_target = cascade_target.unwrap_parens();
        let is_class_ref = matches!(unwrapped_target, Expression::ClassReference { .. });
        // ADR 0083 / BT-2879: a Meta-typed cascade target (`someVar ::
        // SomeClass class`) is also class-side for block-param
        // propagation purposes, mirroring `infer_generic_send_args`'s
        // `is_class_side` computation (~line 1241).
        let is_class_side_send = Self::is_class_side_receiver(cascade_target, env)
            || matches!(dispatch_ty, InferredType::Meta { .. });
        for msg in messages {
            let selector_name = msg.selector.name();
            // BT-2845: capture the inferred argument types so every
            // cascaded message (not just the first) can be run
            // through `check_argument_types` below — previously this
            // return value was discarded, so a mistyped argument to a
            // second-or-later cascade message went entirely
            // unchecked, unlike the same send written as its own
            // statement.
            let arg_types = self.infer_args_with_block_context(
                &msg.arguments,
                &dispatch_ty,
                &selector_name,
                hierarchy,
                env,
                in_abstract_method,
                is_class_side_send,
            );
            if is_class_ref {
                if let Expression::ClassReference { name, .. } = unwrapped_target {
                    self.check_argument_types(
                        &name.name,
                        &selector_name,
                        &arg_types,
                        msg.span,
                        hierarchy,
                        true,
                        Some(&msg.arguments),
                        Some(env),
                        &[],
                    );
                    // BT-2850: ADR 0104 Phase 2 (BT-2750) `C spawnWith:
                    // #{...}` literal-map key check, mirroring the
                    // ClassReference branch of
                    // `infer_message_send_with_receiver_ty` — otherwise
                    // a cascade's non-first `spawnWith:` message skips
                    // typo-suggestion checking entirely.
                    self.check_spawn_with_map_keys(
                        &name.name,
                        &selector_name,
                        &msg.arguments,
                        hierarchy,
                    );
                    self.check_class_side_send(
                        &name.name,
                        &selector_name,
                        msg.span,
                        hierarchy,
                        &[], // cascade return type is receiver, not send result
                    );
                }
            } else if let InferredType::Meta {
                class_name: ref meta_class,
                ..
            } = dispatch_ty
            {
                // ADR 0083 / BT-2879: cascade continuation messages on
                // a Meta-typed receiver (`someVar :: SomeClass class`)
                // dispatch class-side exactly like a syntactic class
                // reference, mirroring `infer_message_send_with_receiver_ty`'s
                // `Meta` branch (~line 1541). This branch was entirely
                // missing — BT-2850 fixed the ClassReference and
                // self-in-class-method branches the cascade loop
                // already handled, but the Meta branch never existed
                // here, so these sends silently skipped
                // `check_argument_types` and `check_spawn_with_map_keys`.
                let is_equality = matches!(
                    msg.selector,
                    MessageSelector::Binary(ref op) if is_equality_operator(op)
                );
                if !is_equality && msg.selector.well_known() != Some(WellKnownSelector::Class) {
                    self.check_argument_types(
                        meta_class,
                        &selector_name,
                        &arg_types,
                        msg.span,
                        hierarchy,
                        true,
                        Some(&msg.arguments),
                        Some(env),
                        &[],
                    );
                    self.check_spawn_with_map_keys(
                        meta_class,
                        &selector_name,
                        &msg.arguments,
                        hierarchy,
                    );
                    self.check_class_side_send(
                        meta_class,
                        &selector_name,
                        msg.span,
                        hierarchy,
                        &[], // cascade return type is receiver, not send result
                    );
                }
            } else if let InferredType::Known {
                ref class_name,
                ref type_args,
                ..
            } = dispatch_ty
            {
                if env.in_class_method && Self::is_self_receiver(unwrapped_target) {
                    if !in_abstract_method {
                        self.check_argument_types(
                            class_name,
                            &selector_name,
                            &arg_types,
                            msg.span,
                            hierarchy,
                            true,
                            Some(&msg.arguments),
                            Some(env),
                            &[],
                        );
                        // BT-2850: ADR 0104 Phase 2 (BT-2750)
                        // `spawnWith: #{...}` literal-map key check,
                        // mirroring the Meta-typed-receiver branch of
                        // `infer_message_send_with_receiver_ty` (this
                        // branch is the cascade's equivalent — `self`
                        // dispatch inside a class method).
                        self.check_spawn_with_map_keys(
                            class_name,
                            &selector_name,
                            &msg.arguments,
                            hierarchy,
                        );
                        self.check_class_side_send(
                            class_name,
                            &selector_name,
                            msg.span,
                            hierarchy,
                            &[], // cascade return type is receiver, not send result
                        );
                    }
                } else {
                    // BT-2871: unlike the non-cascade path in
                    // `infer_message_send_with_receiver_ty`,
                    // `check_binary_operand_types` never runs for
                    // cascade continuation messages (it's only called
                    // for the first message of a send/cascade), so
                    // there is no more-specific-wording path to defer
                    // to here. Always fall back to the generic
                    // `check_argument_types` for binary continuation
                    // messages too — this is the "simpler" option
                    // from BT-2871's AC: `check_binary_operand_types`'s
                    // only value-add over `check_argument_types` is
                    // more specific wording for arithmetic/comparison/
                    // concat, not broader coverage, so skipping it
                    // here only loses phrasing, not correctness.
                    self.check_argument_types(
                        class_name,
                        &selector_name,
                        &arg_types,
                        msg.span,
                        hierarchy,
                        false,
                        Some(&msg.arguments),
                        Some(env),
                        type_args,
                    );
                    self.check_instance_selector(class_name, &selector_name, msg.span, hierarchy);
                }
            } else if let InferredType::Union { ref members, .. } = dispatch_ty {
                // Union cascades: validate selector on all members.
                // Argument-type checking against a union *receiver* is
                // out of scope here — `infer_message_send_with_receiver_ty`
                // doesn't perform it for the first cascade message
                // either (see `infer_union_message_send`), so this
                // preserves parity rather than introducing new
                // behaviour beyond BT-2845's scope (unchecked
                // arguments on continuation messages).
                //
                // BT-2868: `&msg.arguments` is this continuation
                // message's own arguments, but `&arg_types` is
                // whatever the *outer* send in this cascade computed
                // — they can be mismatched positionally/by-shape for
                // a continuation message. This is harmless here: the
                // call's only purpose is DNU validation (its return
                // value is discarded — the cascade keeps `send_ty`
                // from the first send), and `if_true_false_solo_boolean_ret_ty`
                // bails safely whenever the first arg type it reads
                // doesn't pattern-match `Block(...)`.
                self.infer_union_message_send(
                    members,
                    &selector_name,
                    &msg.arguments,
                    &arg_types,
                    msg.span,
                    hierarchy,
                );
            }
        }
        send_ty
    }
}
