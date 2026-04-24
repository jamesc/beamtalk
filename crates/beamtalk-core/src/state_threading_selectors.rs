// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared selector classification helpers for state-threading control flow.
//!
//! Used by both `semantic_analysis` (for `DispatchKind::ControlFlow` classification)
//! and `codegen` (for block-mutation analysis and state-threading code generation).
//!
//! Predicates in this module classify selectors for block-mutation analysis.
//! Where a selector is also part of [`WellKnownSelector`](crate::ast::WellKnownSelector)
//! — conditionals, `on:do:` — the predicate matches on the enum via
//! [`WellKnownSelector::from_name`] so a typo in either place is a compile
//! error at the call site. Loop and iteration selectors (`whileTrue:`, `do:`,
//! `collect:`, etc.) are not classified as well-known and so remain matched
//! by string; the split is deliberate — `WellKnownSelector` is reserved for
//! selectors the type-checker/codegen *intrinsify*, not every selector the
//! compiler happens to recognise.

use crate::ast::WellKnownSelector;

/// Returns `true` if `sel` is a state-threading keyword selector.
///
/// These selectors receive block arguments that are analysed for field mutations
/// as part of state-threading control flow.
#[must_use]
pub(crate) fn is_state_threading_keyword_selector(sel: &str) -> bool {
    if matches!(
        WellKnownSelector::from_name(sel),
        Some(
            WellKnownSelector::OnDo
                | WellKnownSelector::IfTrue
                | WellKnownSelector::IfFalse
                | WellKnownSelector::IfTrueIfFalse
                | WellKnownSelector::IfNotNil,
        )
    ) {
        return true;
    }
    matches!(
        sel,
        "whileTrue:"
            | "whileFalse:"
            | "timesRepeat:"
            | "to:do:"
            | "to:by:do:"
            | "do:"
            | "collect:"
            | "select:"
            | "reject:"
            | "anySatisfy:"
            | "allSatisfy:"
            | "detect:"
            | "detect:ifNone:"
            | "count:"
            | "flatMap:"
            | "takeWhile:"
            | "dropWhile:"
            | "groupBy:"
            | "partition:"
            | "sort:"
            | "inject:into:"
            | "doWithKey:"
            | "keysAndValuesDo:"
            | "ensure:"
    )
}

/// Returns `true` if `sel` is a state-threading unary selector.
#[must_use]
pub(crate) fn is_state_threading_unary_selector(sel: &str) -> bool {
    matches!(sel, "whileTrue" | "whileFalse" | "timesRepeat")
}

/// Returns `true` if `sel` is an exception-handling selector (`on:do:` or `ensure:`).
///
/// For these selectors the *receiver* (try body) is a block that must also be
/// analysed for field mutations, in addition to the argument blocks.
#[must_use]
pub(crate) fn is_exception_selector(sel: &str) -> bool {
    // `on:do:` is a well-known selector; `ensure:` is not (it's not
    // intrinsified by the type-checker, only by codegen's state-threading).
    matches!(
        WellKnownSelector::from_name(sel),
        Some(WellKnownSelector::OnDo)
    ) || sel == "ensure:"
}

/// Returns `true` if `sel` is a Boolean/optional conditional selector.
///
/// For these selectors every block argument must be analysed independently,
/// because mutations may appear in the first branch but not the second.
#[must_use]
pub(crate) fn is_conditional_selector(sel: &str) -> bool {
    matches!(
        WellKnownSelector::from_name(sel),
        Some(
            WellKnownSelector::IfTrue
                | WellKnownSelector::IfFalse
                | WellKnownSelector::IfTrueIfFalse
                | WellKnownSelector::IfNotNil,
        )
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn keyword_selectors() {
        assert!(is_state_threading_keyword_selector("whileTrue:"));
        assert!(is_state_threading_keyword_selector("do:"));
        assert!(is_state_threading_keyword_selector("on:do:"));
        assert!(is_state_threading_keyword_selector("ifTrue:"));
        assert!(is_state_threading_keyword_selector("doWithKey:"));
        assert!(is_state_threading_keyword_selector("keysAndValuesDo:"));
        // BT-1486: New block-accepting selectors
        assert!(is_state_threading_keyword_selector("detect:"));
        assert!(is_state_threading_keyword_selector("detect:ifNone:"));
        assert!(is_state_threading_keyword_selector("count:"));
        assert!(is_state_threading_keyword_selector("flatMap:"));
        // BT-1487: Medium-risk list selectors
        assert!(is_state_threading_keyword_selector("takeWhile:"));
        assert!(is_state_threading_keyword_selector("dropWhile:"));
        assert!(is_state_threading_keyword_selector("groupBy:"));
        assert!(is_state_threading_keyword_selector("partition:"));
        assert!(is_state_threading_keyword_selector("sort:"));
        assert!(!is_state_threading_keyword_selector("perform:"));
    }

    #[test]
    fn unary_selectors() {
        assert!(is_state_threading_unary_selector("whileTrue"));
        assert!(is_state_threading_unary_selector("whileFalse"));
        assert!(is_state_threading_unary_selector("timesRepeat"));
        assert!(!is_state_threading_unary_selector("do:"));
    }

    #[test]
    fn exception_selectors() {
        assert!(is_exception_selector("on:do:"));
        assert!(is_exception_selector("ensure:"));
        assert!(!is_exception_selector("ifTrue:"));
        assert!(!is_exception_selector("do:"));
    }

    #[test]
    fn conditional_selectors() {
        assert!(is_conditional_selector("ifTrue:"));
        assert!(is_conditional_selector("ifFalse:"));
        assert!(is_conditional_selector("ifTrue:ifFalse:"));
        assert!(is_conditional_selector("ifNotNil:"));
        assert!(!is_conditional_selector("on:do:"));
        assert!(!is_conditional_selector("do:"));
    }
}
