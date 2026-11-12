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
//! to the original receiver. See [`cascade_drops_update`].

use crate::ast::{CascadeMessage, Expression, Literal, MessageSelector};
use crate::ast_walker::cascade_part_sends;
use crate::semantic_analysis::class_hierarchy::{ClassHierarchy, DeclaredType};
use crate::semantic_analysis::type_checker::{InferredType, TypeChecker, TypeEnv};
use crate::source_analysis::{Diagnostic, DiagnosticCategory};

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
            let inner_ty = self.infer_send_receiver(inner, hierarchy, env, in_abstract_method);
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
            let send_ty = self.infer_send_receiver(receiver, hierarchy, env, in_abstract_method);
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
        if let Expression::MessageSend { is_cast: false, .. } = receiver {
            self.check_cascade_drops_update(
                cascade_target,
                &dispatch_ty,
                receiver,
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
    /// then discards. The help suggests a `|>` pipeline (ADR 0145 § 2), or
    /// the nested form when the next message is binary, since a pipeline
    /// stage cannot start with one.
    fn check_cascade_drops_update(
        &mut self,
        cascade_target: &Expression,
        dispatch_ty: &InferredType,
        receiver: &Expression,
        messages: &[CascadeMessage],
        hierarchy: &ClassHierarchy,
    ) {
        // A map shape receives its messages as its `Dictionary` view.
        let view = dispatch_ty.dictionary_view();
        let dispatch_ty = view.as_ref().unwrap_or(dispatch_ty);
        let InferredType::Known { class_name, .. } = dispatch_ty else {
            return;
        };
        // Every message but the last one: the first send, then all
        // continuations except the final one. Each is reported at its
        // `SendRef::span`, the definition the `setUp` lint also reads.
        let sends = cascade_part_sends(receiver, messages)
            .map(|send| (send.selector, send.span))
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
            let receiver_text = receiver_display(cascade_target);
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
                .with_hint(drops_update_hint(&receiver_text, dropped, next))
                .with_category(DiagnosticCategory::CascadeDropsUpdate),
            );
        }
    }
}

impl TypeChecker {
    /// The `discarded-update` warning (ADR 0145 § 3, BT-4522): a non-final
    /// statement that is a plain send answering an updated copy of a value
    /// receiver — `p withX: 5.` alone — throws the copy away, since the
    /// receiver is unchanged.
    ///
    /// Called by `infer_stmts` right after `expr` was inferred, so the
    /// receiver's type is already in the type map (keyed by its span); no
    /// re-inference, hence no double-emitted diagnostics. Cascades are
    /// `Expression::Cascade` and stay with `cascade-drops-update`; a pipeline
    /// is a nest of sends, so only its last stage (the statement itself) is
    /// examined and it is reported once.
    pub(in crate::semantic_analysis::type_checker) fn check_discarded_update(
        &mut self,
        expr: &Expression,
        hierarchy: &ClassHierarchy,
    ) {
        if self.discarded_update_off {
            return;
        }
        // A parenthesised statement (`(p withX: 5).`) discards just the same.
        let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            is_cast: false,
            span,
        } = expr.unwrap_parens()
        else {
            return;
        };
        // A pure statement (`n + 1.` on plain operands) already draws the
        // effect-free lint; it owns that shape so one problem has one message.
        if crate::semantic_analysis::validators::is_effect_free(expr) {
            return;
        }
        let Some(receiver_ty) = self.type_map.get(receiver.span()).cloned() else {
            return;
        };
        let name = selector.name();
        if !cascade_drops_update(hierarchy, &receiver_ty, &name) {
            return;
        }
        let InferredType::Known { class_name, .. } = &receiver_ty else {
            return;
        };
        let display = InferredType::class_name_for_diagnostic(class_name.as_str());
        let receiver_text = receiver_display(receiver);
        let shape = send_shape(selector);
        // The one guaranteed property is that the updated copy is unused; the
        // receiver and arguments (a block run for its side effects, a
        // `self nextId` argument) may still do work, so the note and help
        // never claim the statement is a no-op or tell the user to delete it.
        let note = format!(
            "`{display}` is a value: `{name}` answers a new value and leaves the \
             receiver unchanged, and that new value is never used"
        );
        let has_block_arg = arguments
            .iter()
            .any(|a| matches!(a.unwrap_parens(), Expression::Block(_)));
        let hint = if has_block_arg {
            "if only the block's side effects matter, iterate with `do:`; \
             otherwise use the result (assign it, return it, or pass it on)"
                .to_string()
        } else if is_assignable_local(receiver) {
            format!(
                "assign the result (`{receiver_text} := {receiver_text} {shape}`) or \
                 chain with `|>`"
            )
        } else {
            "use the result (assign it, return it, or pass it on)".to_string()
        };
        self.diagnostics.push(
            Diagnostic::warning(format!("the result of `{name}` is discarded"), *span)
                .with_note(note, None)
                .with_hint(hint)
                .with_category(DiagnosticCategory::DiscardedUpdate),
        );
    }
}

/// Whether `receiver` is a plain local that `x := x …` can reassign: an
/// identifier other than `self`/`super`.
fn is_assignable_local(receiver: &Expression) -> bool {
    matches!(receiver, Expression::Identifier(id) if id.name != "self" && id.name != "super")
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
    // A map shape receives its messages as its `Dictionary` view.
    let view = receiver_ty.dictionary_view();
    let receiver_ty = view.as_ref().unwrap_or(receiver_ty);
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

/// The `cascade-drops-update` help: send `next` to the result of `dropped`
/// with a pipeline, `p withX: … |> withY: …`. A pipeline stage cannot start
/// with a binary message (ADR 0145 § 2), so a binary `next` gets the nested
/// form, `(p withX: …) + …`, instead.
fn drops_update_hint(
    receiver_text: &str,
    dropped: &MessageSelector,
    next: &MessageSelector,
) -> String {
    let next_name = next.name();
    let dropped_shape = send_shape(dropped);
    let next_shape = send_shape(next);
    if matches!(next, MessageSelector::Binary(_)) {
        format!(
            "to send `{next_name}` to the result, nest the sends: \
             `({receiver_text} {dropped_shape}) {next_shape}`"
        )
    } else {
        format!(
            "to send `{next_name}` to the result, use a pipeline: \
             `{receiver_text} {dropped_shape} |> {next_shape}`"
        )
    }
}

/// The cascade's shared receiver as written, for the help text:
/// an identifier, `super`, a class reference, or a simple literal (integer,
/// symbol, string) renders through the shared unparser; any other receiver
/// expression is elided as `…`.
fn receiver_display(receiver: &Expression) -> String {
    match receiver {
        Expression::Identifier(_)
        | Expression::Super(_)
        | Expression::ClassReference { .. }
        | Expression::Literal(Literal::Integer(_) | Literal::Symbol(_) | Literal::String(_), _) => {
            crate::unparse::unparse_expression(receiver).to_pretty_string()
        }
        _ => "…".to_string(),
    }
}

/// A selector rendered as a send with its arguments elided (`withX: …`,
/// `+ …`, `negated`), for the help text.
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

#[cfg(test)]
mod tests {
    use super::receiver_display;
    use crate::ast::{Expression, Identifier, Literal, MessageSelector};
    use crate::source_analysis::Span;

    fn span() -> Span {
        Span::new(0, 1)
    }

    fn ident(name: &str) -> Identifier {
        Identifier::new(name, span())
    }

    fn lit(literal: Literal) -> Expression {
        Expression::Literal(literal, span())
    }

    #[test]
    fn receiver_display_renders_simple_receivers_as_written() {
        let class_ref = Expression::ClassReference {
            name: ident("Point"),
            package: None,
            span: span(),
        };
        let qualified = Expression::ClassReference {
            name: ident("Parser"),
            package: Some(ident("json")),
            span: span(),
        };
        assert_eq!(receiver_display(&class_ref), "Point");
        assert_eq!(receiver_display(&qualified), "json@Parser");
        assert_eq!(receiver_display(&lit(Literal::Integer(0))), "0");
        assert_eq!(receiver_display(&lit(Literal::Symbol("a".into()))), "#a");
        assert_eq!(
            receiver_display(&lit(Literal::String("ab".into()))),
            "\"ab\""
        );
        assert_eq!(receiver_display(&Expression::Identifier(ident("p"))), "p");
        assert_eq!(receiver_display(&Expression::Super(span())), "super");
    }

    #[test]
    fn receiver_display_elides_other_receivers() {
        let send = Expression::MessageSend {
            receiver: Box::new(Expression::Identifier(ident("p"))),
            selector: MessageSelector::Unary("next".into()),
            arguments: vec![],
            is_cast: false,
            span: span(),
        };
        assert_eq!(receiver_display(&lit(Literal::Float(1.5))), "…");
        assert_eq!(receiver_display(&send), "…");
    }
}
