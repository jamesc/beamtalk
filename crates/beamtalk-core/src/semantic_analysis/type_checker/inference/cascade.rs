// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Type inference for cascade expressions.
//!
//! **DDD Context:** Semantic Analysis
//!
//! Covers `Expression::Cascade` (`receiver msg1; msg2; msg3`) — all messages
//! dispatch to the receiver of the cascade's first send, not to its return
//! value, so every continuation message is validated by calling
//! [`TypeChecker::infer_message_send_with_receiver_ty`] against that shared
//! receiver — the same dispatch entry point the cascade's first send (and
//! every non-cascade `MessageSend`) uses.

use crate::ast::{CascadeMessage, Expression};
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::type_checker::{InferredType, TypeChecker, TypeEnv};

impl TypeChecker {
    /// Infer a cascade `receiver msg1; msg2; msg3` — the `Expression::Cascade`
    /// arm of `infer_expr`'s dispatch.
    ///
    /// All messages dispatch to the *underlying* receiver of the first send,
    /// not to its return value: the parser bundles `obj msg1; msg2; msg3` as
    /// `Cascade { receiver: MessageSend(obj, msg1), messages: [msg2, msg3] }`,
    /// so this peeks through the first `MessageSend` to find the actual
    /// receiver, then validates every message — first and continuations
    /// alike — through the single shared send path,
    /// [`Self::infer_message_send_with_receiver_ty`].
    pub(in crate::semantic_analysis::type_checker) fn infer_cascade(
        &mut self,
        receiver: &Expression,
        messages: &[CascadeMessage],
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> InferredType {
        // To avoid walking the inner receiver subtree twice
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
            // that is `receiver`) bypasses `infer_expr`, so its tail —
            // recording the type in the LSP type map and running the
            // Dynamic warning — is invoked directly here via the
            // same shared helper `infer_expr` itself calls.
            self.post_process_expr_type(receiver, &send_ty);
            (send_ty, inner.as_ref(), inner_ty)
        } else {
            // Non-MessageSend receiver (or a cast send, which short-circuits
            // to Dynamic): fall back to a single infer_expr; the cascade
            // dispatches messages to that same type.
            let send_ty = self.infer_expr(receiver, hierarchy, env, in_abstract_method);
            (send_ty.clone(), receiver, send_ty)
        };
        // `dispatch_ty` is passed to `infer_message_send_with_receiver_ty`
        // as-is (Negation and all) for every continuation message below —
        // that function already resolves a `Negation`-typed receiver
        // through `base` itself (ADR 0102 §5) at its own entry,
        // so there is nothing to unwrap here.
        //
        // Every continuation message dispatches against the same
        // `cascade_target` / `dispatch_ty` pair the first send resolved
        // above, so each one is validated by calling the shared send path
        // directly — exactly as if it had been written as its own
        // statement against that receiver. `infer_message_send_with_receiver_ty`
        // only pattern-matches `cascade_target`'s syntactic shape (it never
        // calls `infer_expr` on it), so this doesn't re-walk the receiver
        // subtree and can't double-emit the DNU/type diagnostics the first
        // send already produced.
        for msg in messages {
            self.infer_message_send_with_receiver_ty(
                cascade_target,
                dispatch_ty.clone(),
                &msg.selector,
                &msg.arguments,
                msg.span,
                hierarchy,
                env,
                in_abstract_method,
            );
        }
        send_ty
    }
}
