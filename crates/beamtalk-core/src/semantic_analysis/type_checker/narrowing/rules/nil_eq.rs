// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `x =:= nil` / `x == nil` / `x =/= nil` / `x /= nil` (either operand order)
//! narrowing (BT-4712).
//!
//! `nil` is the only instance of `UndefinedObject`, so an equality test
//! against it is exactly `x isNil`, and an inequality test is `x notNil`. The
//! rule therefore reuses the [`is_nil`](super::is_nil) detection: `x =:= nil`
//! yields the same [`NarrowingInfo`] as `x isNil`.
//!
//! For an inequality, the branches swap. The non-nil type depends on the
//! variable's live type, which `detect` cannot see, so the rule records
//! [`NarrowingInfo::nil_test_negated`] and `refine_nil_negation` in `refine.rs`
//! resolves the swapped branch types.

use crate::ast::{Expression, MessageSelector};
use crate::semantic_analysis::type_checker::EnvKey;
use crate::source_analysis::equality_operator_is_negated;

use super::super::extract::extract_variable_name;
use super::super::info::NarrowingInfo;
use super::NarrowingRule;
use super::is_nil::nil_check_info;

pub(super) const RULE: NarrowingRule = NarrowingRule { detect };

fn detect(receiver: &Expression) -> Option<NarrowingInfo> {
    let Expression::MessageSend {
        receiver: lhs,
        selector: MessageSelector::Binary(op),
        arguments,
        ..
    } = receiver
    else {
        return None;
    };
    // `equality_operator_is_negated` is the single definition of the
    // equality-operator set. `==` vs `=:=` only diverge on numeric
    // cross-type coercion, which never applies to `nil`.
    let negated = equality_operator_is_negated(op.as_str())?;
    let variable = nil_tested_variable(lhs, arguments.first()?)?;
    let mut info = nil_check_info(variable);
    info.nil_test_negated = negated;
    Some(info)
}

/// The variable compared against `nil` in `lhs <op> rhs`, accepting either
/// operand order. `None` when neither side is the `nil` literal, when both
/// are, or when the other side is not a narrowable variable.
fn nil_tested_variable(lhs: &Expression, rhs: &Expression) -> Option<EnvKey> {
    let var_expr = match (lhs.is_literal_nil(), rhs.is_literal_nil()) {
        (false, true) => lhs,
        (true, false) => rhs,
        _ => return None,
    };
    extract_variable_name(var_expr)
}
