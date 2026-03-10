// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared selector classification helpers for state-threading control flow.
//!
//! Used by both `semantic_analysis` (for `DispatchKind::ControlFlow` classification)
//! and `codegen` (for block-mutation analysis and state-threading code generation).

/// Returns `true` if `sel` is a state-threading keyword selector.
///
/// These selectors receive block arguments that are analysed for field mutations
/// as part of state-threading control flow.
#[must_use]
pub(crate) fn is_state_threading_keyword_selector(sel: &str) -> bool {
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
            | "inject:into:"
            | "on:do:"
            | "ensure:"
            | "ifTrue:"
            | "ifFalse:"
            | "ifTrue:ifFalse:"
            | "ifNotNil:"
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
    matches!(sel, "on:do:" | "ensure:")
}

/// Returns `true` if `sel` is a Boolean/optional conditional selector.
///
/// For these selectors every block argument must be analysed independently,
/// because mutations may appear in the first branch but not the second.
#[must_use]
pub(crate) fn is_conditional_selector(sel: &str) -> bool {
    matches!(
        sel,
        "ifTrue:" | "ifFalse:" | "ifTrue:ifFalse:" | "ifNotNil:"
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
