// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Narrowing subsystem (BT-2050).
//!
//! Type-test expressions like `x isNil`, `x class = Foo`, `x isKindOf: Bar`,
//! and Result-shape checks produce a [`NarrowingInfo`] describing how the
//! tested variable's type changes in the true / false branches of
//! `ifTrue:` / `ifFalse:` / `ifTrue:ifFalse:`. The detection table lives in
//! [`rules`]; the shared detection entry point is [`detect`].
//!
//! Refinement scope is described by [`refinement::Scope`] — block-local
//! (today's default) or method-remainder (BT-2049 post-guard narrowing).
//!
//! AST visitors used by the narrowing machinery (divergence checks,
//! reassignment scans) live in [`visitors`]. Variable-name extraction
//! (including `self.field` synthetic keys) lives in [`extract`].

pub(crate) mod extract;
pub(crate) mod info;
pub(crate) mod refinement;
pub(crate) mod rules;
pub(crate) mod visitors;

use crate::ast::Expression;

pub(crate) use info::NarrowingInfo;

/// Detect a narrowing from a receiver expression.
///
/// Unwraps parentheses once at the top level, then dispatches through the
/// [`rules::RULES`] table. The first rule to match wins; rules are written to
/// be mutually exclusive by shape (different expression variants or
/// selectors), so order is not semantically significant.
///
/// Returns `None` when no rule matches.
pub(crate) fn detect(receiver: &Expression) -> Option<NarrowingInfo> {
    let receiver = extract::unwrap_parens(receiver);
    rules::RULES.iter().find_map(|rule| (rule.detect)(receiver))
}
