// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared `try`/`catch` and wrap-then-reraise shapes (BT-4685).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Two shapes recur across codegen and used to be written out by hand at each
//! site, always as string literals:
//!
//! * [`try_catch_doc`] — `try expr of <R> -> R catch <C, E, S> -> handler`,
//!   the "evaluate, return the value, or run a handler on any raise" form.
//! * [`wrap_and_reraise_doc`] — the catch-side prelude that turns a raw
//!   Erlang exception into a Beamtalk Exception object:
//!   `primop 'build_stacktrace'(Raw)` then
//!   `beamtalk_exception_handler:ensure_wrapped(Type, Error, Built)`,
//!   followed by whatever `body` does with the wrapped value (typically a
//!   re-raise).
//!
//! Both are built on the structural constructors in
//! [`beamtalk_cerl_doc::form`], so each site picks a [`Layout`] and owns no
//! indentation of its own.

use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::form::{self, Layout};

/// Variables of a catch clause: the exception class, reason and raw stacktrace.
pub(super) struct CatchVars {
    pub(super) class: Document<'static>,
    pub(super) reason: Document<'static>,
    pub(super) stack: Document<'static>,
}

/// `try expr of <result> -> result catch <class, reason, stack> -> handler`.
///
/// The `of` branch returns the value unchanged; `handler` runs on any raise
/// and may refer to the three [`CatchVars`].
pub(super) fn try_catch_doc(
    expr: Document<'static>,
    result_var: Document<'static>,
    catch: CatchVars,
    handler: Document<'static>,
    layout: Layout,
) -> Document<'static> {
    form::try_of_catch(
        expr,
        [result_var.clone()],
        result_var,
        [catch.class, catch.reason, catch.stack],
        handler,
        layout,
    )
}

/// The wrap-then-reraise prelude:
///
/// ```text
/// let Built = primop 'build_stacktrace'(Stack) in
/// let Wrapped = call 'beamtalk_exception_handler':'ensure_wrapped'(Type, Error, Built[, Context]) in
/// body
/// ```
///
/// `context`, when present, is the optional fourth `ensure_wrapped/4` argument
/// (an empty-context map for "keep an already-wrapped exception's innermost
/// stacktrace"). `body` follows the two lets and normally re-raises `wrapped`.
pub(super) fn wrap_and_reraise_doc(
    catch: &CatchVars,
    built: Document<'static>,
    wrapped: Document<'static>,
    context: Option<Document<'static>>,
    body: Document<'static>,
    layout: Layout,
) -> Document<'static> {
    let mut args = vec![catch.class.clone(), catch.reason.clone(), built.clone()];
    args.extend(context);
    form::let_in(
        built,
        form::primop("build_stacktrace", [catch.stack.clone()]),
        form::let_in(
            wrapped,
            form::call("beamtalk_exception_handler", "ensure_wrapped", args),
            body,
            layout,
        ),
        layout,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use beamtalk_cerl_doc::leaf;

    fn vars() -> CatchVars {
        CatchVars {
            class: leaf::var("C"),
            reason: leaf::var("E"),
            stack: leaf::var("S"),
        }
    }

    #[test]
    fn wrap_flat_matches_historical_literal() {
        let doc = wrap_and_reraise_doc(
            &vars(),
            leaf::var("B"),
            leaf::var("W"),
            None,
            Document::Str(""),
            Layout::Flat,
        );
        assert_eq!(
            doc.to_pretty_string(),
            "let B = primop 'build_stacktrace'(S) in let W = call 'beamtalk_exception_handler':'ensure_wrapped'(C, E, B) in "
        );
    }

    #[test]
    fn wrap_with_context_adds_fourth_arg() {
        let doc = wrap_and_reraise_doc(
            &vars(),
            leaf::var("B"),
            leaf::var("W"),
            Some(Document::Str("~{}~")),
            leaf::var("W"),
            Layout::Block,
        );
        assert_eq!(
            doc.to_pretty_string(),
            "let B = primop 'build_stacktrace'(S) in\nlet W = call 'beamtalk_exception_handler':'ensure_wrapped'(C, E, B, ~{}~) in\nW"
        );
    }

    #[test]
    fn try_catch_returns_value_or_runs_handler() {
        let doc = try_catch_doc(
            leaf::var("X"),
            leaf::var("R"),
            vars(),
            form::primop(
                "raw_raise",
                [leaf::var("C"), leaf::var("E"), leaf::var("S")],
            ),
            Layout::Flat,
        );
        assert_eq!(
            doc.to_pretty_string(),
            "try X of <R> -> R catch <C, E, S> -> primop 'raw_raise'(C, E, S)"
        );
    }
}
