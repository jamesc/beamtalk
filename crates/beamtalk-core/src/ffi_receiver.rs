// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared recognizer for the `Erlang <module>` FFI receiver pattern.
//!
//! **DDD Context:** shared leaf — depended on by `codegen`, `semantic_analysis`,
//! and `queries`; itself depends only on `ast` (see
//! `docs/development/architecture-principles.md`).
//!
//! There is no dedicated `ErlangCall` AST node: an FFI reference such as
//! `Erlang lists` (or the parenthesized `(Erlang lists)`) parses as an
//! ordinary [`Expression::MessageSend`] whose receiver is the `Erlang` class
//! reference and whose selector is a unary message naming the module. The
//! rule "is this an FFI module lookup, or a class-protocol send?" was
//! historically implemented six times across the compiler (BT-3079) and had
//! already drifted three ways. This module is the single source of truth;
//! callers must use [`erlang_module_of_receiver`] (or the smaller building
//! blocks below) rather than re-deriving the pattern.
//!
//! # The three points of drift this module resolves uniformly
//!
//! 1. **Class-protocol filter.** `Erlang class`, `Erlang new`, etc. dispatch
//!    to the class protocol (`beamtalk_object_class:class_send/3` at
//!    runtime) — `Erlang class` returns the `Erlang` metaclass, not a proxy
//!    for a module literally named `class`. [`is_class_protocol_selector`]
//!    is always consulted, so these selectors are never recognized as FFI
//!    module names.
//! 2. **Package qualification.** `json@Erlang lists` names a package-scoped
//!    class called `Erlang` in package `json` — not the compiler's built-in
//!    FFI bridge. [`is_erlang_class_reference`] requires an unqualified
//!    (`package: None`) reference, so a package-qualified `Erlang` is never
//!    treated as the FFI bridge.
//! 3. **Parenthesized receivers.** `(Erlang lists) reverse: xs` is the
//!    canonical form recommended for FFI calls embedded in larger
//!    expressions. [`erlang_module_of_receiver`] peels `Parenthesized`
//!    wrappers before matching, so parenthesized and bare receivers are
//!    recognized identically.

use crate::ast::{Expression, MessageSelector};

/// Class-protocol selectors that must NOT be intercepted as Erlang FFI
/// module lookups, even though `Erlang <name>` parses identically to a
/// module lookup for these names too.
///
/// These selectors are handled by `beamtalk_object_class:class_send/3` at
/// runtime: `Erlang class` returns the `Erlang` metaclass, `Erlang new`
/// raises the usual "cannot instantiate" class-protocol error, and so on —
/// none of them name a real Erlang module in practice.
const CLASS_PROTOCOL_SELECTORS: &[&str] = &[
    "new",
    "spawn",
    "class",
    "methods",
    "superclass",
    "subclasses",
    "allSubclasses",
    "class_name",
    "module_name",
    "printString",
];

/// True when `selector` is a class-protocol selector that must not be
/// intercepted as an Erlang FFI module name (see [`CLASS_PROTOCOL_SELECTORS`]).
#[must_use]
pub(crate) fn is_class_protocol_selector(selector: &str) -> bool {
    CLASS_PROTOCOL_SELECTORS.contains(&selector)
}

/// Peels any number of `Parenthesized` wrappers, returning the inner
/// expression. `(Erlang lists)` and `Erlang lists` must be recognized
/// identically.
fn peel_parens(expr: &Expression) -> &Expression {
    let mut current = expr;
    while let Expression::Parenthesized { expression, .. } = current {
        current = expression;
    }
    current
}

/// True when `expr` (after peeling any parentheses) is the bare, unqualified
/// `Erlang` class reference — the compiler's built-in FFI bridge entry
/// point. A package-qualified reference (`json@Erlang`) is excluded: it
/// names a different, package-scoped class, not the FFI bridge.
#[must_use]
pub(crate) fn is_erlang_class_reference(expr: &Expression) -> bool {
    matches!(
        peel_parens(expr),
        Expression::ClassReference { name, package, .. }
            if package.is_none() && name.name == "Erlang"
    )
}

/// Recognizes the `Erlang <module>` FFI receiver pattern and extracts the
/// module name.
///
/// Matches, after peeling any `Parenthesized` wrapper, a `MessageSend` whose
/// receiver is the bare unqualified `Erlang` class reference and whose
/// selector is a unary message naming the module — the parsed shape of
/// `Erlang lists` and `(Erlang lists)`. Returns `None` when:
///
/// - `expr` is not that shape at all — note callers pass the *receiver of*
///   the FFI call (e.g. the `Erlang lists` in `Erlang lists reverse: xs`),
///   not the whole outer send;
/// - the `Erlang` reference is package-qualified (`json@Erlang lists`); or
/// - the unary selector is a class-protocol selector (`Erlang class`,
///   `Erlang new`, …) — those dispatch to the class protocol instead of FFI.
#[must_use]
pub(crate) fn erlang_module_of_receiver(expr: &Expression) -> Option<&str> {
    if let Expression::MessageSend {
        receiver,
        selector: MessageSelector::Unary(module_name),
        ..
    } = peel_parens(expr)
    {
        if is_erlang_class_reference(receiver) && !is_class_protocol_selector(module_name) {
            return Some(module_name.as_str());
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Identifier;
    use crate::source_analysis::Span;

    fn erlang_ref(package: Option<&str>) -> Expression {
        Expression::ClassReference {
            name: Identifier::new("Erlang", Span::new(0, 6)),
            package: package.map(|p| Identifier::new(p, Span::new(0, 0))),
            span: Span::new(0, 6),
        }
    }

    fn module_send(receiver: Expression, module: &str) -> Expression {
        Expression::MessageSend {
            receiver: Box::new(receiver),
            selector: MessageSelector::Unary(module.into()),
            arguments: vec![],
            is_cast: false,
            span: Span::new(0, 12),
        }
    }

    fn parenthesize(expr: Expression) -> Expression {
        Expression::Parenthesized {
            expression: Box::new(expr),
            span: Span::new(0, 14),
        }
    }

    #[test]
    fn recognizes_bare_module_lookup() {
        let expr = module_send(erlang_ref(None), "lists");
        assert_eq!(erlang_module_of_receiver(&expr), Some("lists"));
    }

    #[test]
    fn recognizes_parenthesized_module_lookup() {
        let expr = parenthesize(module_send(erlang_ref(None), "lists"));
        assert_eq!(erlang_module_of_receiver(&expr), Some("lists"));
    }

    #[test]
    fn rejects_class_protocol_selectors() {
        for selector in CLASS_PROTOCOL_SELECTORS {
            let expr = module_send(erlang_ref(None), selector);
            assert_eq!(
                erlang_module_of_receiver(&expr),
                None,
                "selector {selector} must not be recognized as an FFI module"
            );
        }
    }

    #[test]
    fn rejects_package_qualified_erlang() {
        let expr = module_send(erlang_ref(Some("json")), "lists");
        assert_eq!(erlang_module_of_receiver(&expr), None);
    }

    #[test]
    fn rejects_non_erlang_class_reference() {
        let other = Expression::ClassReference {
            name: Identifier::new("Foo", Span::new(0, 3)),
            package: None,
            span: Span::new(0, 3),
        };
        let expr = module_send(other, "lists");
        assert_eq!(erlang_module_of_receiver(&expr), None);
    }

    #[test]
    fn rejects_non_message_send() {
        let expr = erlang_ref(None);
        assert_eq!(erlang_module_of_receiver(&expr), None);
    }
}
