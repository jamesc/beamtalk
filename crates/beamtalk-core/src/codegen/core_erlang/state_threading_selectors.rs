// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Centralised classification of selectors that require state threading.
//!
//! # Why this module exists
//!
//! Several codegen helpers independently tested whether a message selector
//! belongs to one of the three state-threading groups (loop/exception/conditional).
//! That duplication meant that adding a new selector required touching multiple
//! files, with no compile-time guard against forgetting one of them.
//!
//! This module is the single source of truth.  All other codegen code that
//! needs to classify a selector should call the functions defined here.
//!
//! # Groups
//!
//! | Group         | Selectors                                                        |
//! |---------------|------------------------------------------------------------------|
//! | Loop          | `whileTrue:`, `whileFalse:`, `timesRepeat:`, `to:do:`,          |
//! |               | `to:by:do:`, `do:`, `collect:`, `select:`, `reject:`,           |
//! |               | `inject:into:`                                                  |
//! | Exception     | `on:do:`, `ensure:`                                              |
//! | Conditional   | `ifTrue:`, `ifFalse:`, `ifTrue:ifFalse:`, `ifNotNil:`           |
//! | Unary loop    | `whileTrue`, `whileFalse`, `timesRepeat`                        |

/// Returns `true` if `sel` is an exception-handling selector (`on:do:` or `ensure:`).
///
/// For these selectors the *receiver* (try body) is a block that must also be
/// analysed for field mutations, in addition to the argument blocks.
#[must_use]
pub(super) fn is_exception_selector(sel: &str) -> bool {
    matches!(sel, "on:do:" | "ensure:")
}

/// Returns `true` if `sel` is a Boolean/optional conditional selector.
///
/// For these selectors every block argument must be analysed independently,
/// because mutations may appear in the first branch but not the second.
#[must_use]
pub(super) fn is_conditional_selector(sel: &str) -> bool {
    matches!(
        sel,
        "ifTrue:" | "ifFalse:" | "ifTrue:ifFalse:" | "ifNotNil:"
    )
}

/// Returns `true` if `sel` (a keyword selector string) requires state threading.
///
/// This covers loop, exception, and conditional selectors.
#[must_use]
pub(super) fn is_state_threading_keyword_selector(sel: &str) -> bool {
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

/// Returns `true` if `sel` (a unary selector string) requires state threading.
#[must_use]
pub(super) fn is_state_threading_unary_selector(sel: &str) -> bool {
    matches!(sel, "whileTrue" | "whileFalse" | "timesRepeat")
}

#[cfg(test)]
mod tests {
    use super::*;

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

    #[test]
    fn keyword_threading_selectors_cover_all_groups() {
        // Loop
        for sel in &[
            "whileTrue:",
            "whileFalse:",
            "timesRepeat:",
            "to:do:",
            "to:by:do:",
            "do:",
            "collect:",
            "select:",
            "reject:",
            "inject:into:",
        ] {
            assert!(
                is_state_threading_keyword_selector(sel),
                "{sel} should be a state-threading keyword selector"
            );
        }
        // Exception
        for sel in &["on:do:", "ensure:"] {
            assert!(
                is_state_threading_keyword_selector(sel),
                "{sel} should be a state-threading keyword selector"
            );
        }
        // Conditional
        for sel in &["ifTrue:", "ifFalse:", "ifTrue:ifFalse:", "ifNotNil:"] {
            assert!(
                is_state_threading_keyword_selector(sel),
                "{sel} should be a state-threading keyword selector"
            );
        }
        // Non-threading
        assert!(!is_state_threading_keyword_selector("at:"));
        assert!(!is_state_threading_keyword_selector("at:put:"));
        assert!(!is_state_threading_keyword_selector("printString"));
    }

    #[test]
    fn unary_threading_selectors() {
        assert!(is_state_threading_unary_selector("whileTrue"));
        assert!(is_state_threading_unary_selector("whileFalse"));
        assert!(is_state_threading_unary_selector("timesRepeat"));
        assert!(!is_state_threading_unary_selector("printString"));
        assert!(!is_state_threading_unary_selector("do:"));
    }
}
