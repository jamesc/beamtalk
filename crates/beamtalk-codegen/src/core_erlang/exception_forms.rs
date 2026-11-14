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

use beamtalk_cerl_doc::form::{self, Layout};
use beamtalk_cerl_doc::{Document, docvec, leaf};

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

/// Turns the raw stacktrace of a caught-exception triple into a built one:
///
/// ```text
/// case Err of
///   <{Type, Reason, Raw}> when 'true' -> let Built = primop 'build_stacktrace'(Raw) in {Type, Reason, Built}
///   <Other> when 'true' -> Other
/// end
/// ```
///
/// `safe_dispatch` packs a caught exception as `{Type, Reason, Raw}` where `Raw`
/// is the opaque trace the catch clause binds. Only `primop 'build_stacktrace'`
/// can turn it into a list, and the runtime's `capture_stacktrace/1` reads a raw
/// trace as empty, so a consumer that hands the triple to `ensure_wrapped` or
/// `reraise` (the self-send error clause, the `handle_call` error arm) builds it
/// first, in the actor process that caught it (BT-4850). A value that is not a
/// triple (a bare `#beamtalk_error{}`, an NLR tuple) passes through unchanged.
///
/// `prefix` names the clause variables so nested uses stay distinct.
pub(super) fn build_caught_triple_doc(error: Document<'static>, prefix: &str) -> Document<'static> {
    // `format!` derives variable-name *inputs* for `leaf::var` (ADR 0089).
    let type_v = leaf::var(format!("{prefix}Type"));
    let reason_v = leaf::var(format!("{prefix}Reason"));
    let raw_v = leaf::var(format!("{prefix}Raw"));
    let built_v = leaf::var(format!("{prefix}Built"));
    let other_v = leaf::var(format!("{prefix}Other"));
    form::case_of(
        error,
        [
            form::clause(
                [docvec![
                    "{",
                    type_v.clone(),
                    ", ",
                    reason_v.clone(),
                    ", ",
                    raw_v.clone(),
                    "}"
                ]],
                Some(leaf::atom("true")),
                form::let_in(
                    built_v.clone(),
                    form::primop("build_stacktrace", [raw_v]),
                    docvec!["{", type_v, ", ", reason_v, ", ", built_v, "}"],
                    Layout::Flat,
                ),
                Layout::Flat,
            ),
            form::clause(
                [other_v.clone()],
                Some(leaf::atom("true")),
                other_v,
                Layout::Flat,
            ),
        ],
        Layout::Flat,
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
