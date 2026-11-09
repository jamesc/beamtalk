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
//!
//! It also hosts the `cascade-drops-update` warning (ADR 0145 § 3,
//! BT-4512): on a value receiver, a non-final cascade message that answers
//! an updated copy has that copy thrown away, because the next message goes
//! to the original receiver. See [`TypeChecker::check_cascade_drops_update`].

use crate::ast::{CascadeMessage, Expression, MessageSelector};
use crate::semantic_analysis::class_hierarchy::{ClassHierarchy, DeclaredType};
use crate::semantic_analysis::type_checker::{InferredType, TypeChecker, TypeEnv};
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};

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
        if let Expression::MessageSend {
            selector: first_sel,
            span: first_span,
            is_cast: false,
            ..
        } = receiver
        {
            self.check_cascade_drops_update(
                cascade_target,
                &dispatch_ty,
                (first_sel, *first_span),
                messages,
                hierarchy,
            );
        }
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

    /// The `cascade-drops-update` warning (ADR 0145 § 3, BT-4512): one
    /// warning at each **non-final** cascade message that
    /// [`cascade_drops_update`] says answers an updated copy the cascade
    /// then discards. The help suggests the nested form; it switches to the
    /// `|>` pipeline once that operator ships (ADR 0145 § 2).
    fn check_cascade_drops_update(
        &mut self,
        cascade_target: &Expression,
        dispatch_ty: &InferredType,
        first: (&MessageSelector, Span),
        messages: &[CascadeMessage],
        hierarchy: &ClassHierarchy,
    ) {
        let InferredType::Known { class_name, .. } = dispatch_ty else {
            return;
        };
        // Every message but the last one: the first send, then all
        // continuations except the final one.
        let sends = std::iter::once(first)
            .chain(messages.iter().map(|m| (&m.selector, m.span)))
            .collect::<Vec<_>>();
        for pair in sends.windows(2) {
            let [(dropped, span), (next, _)] = pair else {
                continue;
            };
            if !cascade_drops_update(hierarchy, dispatch_ty, &dropped.name()) {
                continue;
            }
            let dropped_name = dropped.name();
            let next_name = next.name();
            let receiver_text = match cascade_target {
                Expression::Identifier(id) => id.name.to_string(),
                Expression::Super(_) => "super".to_string(),
                _ => "…".to_string(),
            };
            let display = InferredType::class_name_for_diagnostic(class_name.as_str());
            self.diagnostics.push(
                Diagnostic::warning(
                    format!("the result of `{dropped_name}` is discarded"),
                    *span,
                )
                .with_note(
                    format!(
                        "`{display}` is a value: `{dropped_name}` answers a new value and \
                         leaves the receiver unchanged, and a cascade sends `{next_name}` \
                         to the receiver, not to that result"
                    ),
                    None,
                )
                .with_hint(format!(
                    "to send `{next_name}` to the result, nest the sends: `({receiver_text} {}) {}`",
                    send_shape(dropped),
                    send_shape(next),
                ))
                .with_category(DiagnosticCategory::CascadeDropsUpdate),
            );
        }
    }
}

/// Whether a non-final cascade message `selector` sent to a receiver of
/// static type `receiver_ty` draws the `cascade-drops-update` warning
/// (ADR 0145 § 3): the receiver is a `Known` instance of a value kind
/// (`Value` or a subclass), and `selector` resolves to a method whose
/// defining class is itself a value kind and whose recorded return type is
/// `Self` or that defining class, type arguments ignored — it answers an
/// updated copy, which the cascade discards.
///
/// The one decision both the type checker's warning and `beamtalk-lint`'s
/// `cascade_candidate` (which must not suggest a cascade this would flag)
/// read. `Dynamic`, union, metatype and non-value receivers answer `false`.
#[must_use]
pub fn cascade_drops_update(
    hierarchy: &ClassHierarchy,
    receiver_ty: &InferredType,
    selector: &str,
) -> bool {
    let InferredType::Known { class_name, .. } = receiver_ty else {
        return false;
    };
    if !hierarchy.is_value_subclass(class_name) {
        return false;
    }
    let Some(method) = hierarchy.find_method(class_name, selector) else {
        return false;
    };
    if !hierarchy.is_value_subclass(&method.defined_in) {
        return false;
    }
    match &method.return_type {
        Some(DeclaredType::SelfType) => true,
        Some(DeclaredType::Simple(name) | DeclaredType::Generic { base: name, .. }) => {
            *name == method.defined_in
        }
        _ => false,
    }
}

/// A selector rendered as a send with its arguments elided (`withX: …`,
/// `+ …`, `negated`), for the nested-form help text.
fn send_shape(selector: &MessageSelector) -> String {
    match selector {
        MessageSelector::Unary(name) => name.to_string(),
        MessageSelector::Binary(op) => format!("{op} …"),
        MessageSelector::Keyword(parts) => parts
            .iter()
            .map(|p| format!("{} …", p.keyword))
            .collect::<Vec<_>>()
            .join(" "),
    }
}
