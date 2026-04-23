// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Narrowing rule table (BT-2050).
//!
//! Each rule is a self-contained detector: given the receiver of a
//! `ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:` send, it inspects the expression
//! shape and returns [`NarrowingInfo`] when it recognises a type-test.
//!
//! Adding a new narrowing case = one new file here plus one entry in
//! [`RULES`]. The caller in `inference.rs` walks [`RULES`] in order and takes
//! the first match — rules must be mutually exclusive by shape (we assert
//! this via careful ordering; no two current rules match the same expression).

use crate::ast::Expression;

use super::info::NarrowingInfo;

mod class_eq;
mod is_kind_of;
mod is_nil;
mod is_result;
mod responds_to;

/// A single narrowing pattern.
///
/// `detect` inspects a receiver expression and returns `NarrowingInfo` when
/// the pattern matches. The rule is a plain function pointer so rules can live
/// in their own files without ballooning trait boilerplate.
pub(crate) struct NarrowingRule {
    /// Detect the narrowing from a receiver expression. Returns `None` when
    /// the rule doesn't apply.
    pub(crate) detect: fn(&Expression) -> Option<NarrowingInfo>,
}

/// The rule dispatch table.
///
/// Order is load-bearing only insofar as two rules never match the same
/// shape: `class_eq` must run on a binary `=` / `=:=` send whose inner
/// receiver is a unary `class` send, which none of the other rules inspect.
/// The unary-selector rules (`is_nil`, `is_result`) discriminate on the
/// selector name. Keyword-selector rules (`is_kind_of`, `responds_to`)
/// discriminate on the first keyword. So any order is correct; we list them
/// grouped by shape for readability.
pub(crate) static RULES: &[NarrowingRule] = &[
    is_nil::RULE,
    is_result::IS_OK_OR_OK_RULE,
    is_result::IS_ERROR_RULE,
    responds_to::RULE,
    is_kind_of::RULE,
    class_eq::RULE,
];
