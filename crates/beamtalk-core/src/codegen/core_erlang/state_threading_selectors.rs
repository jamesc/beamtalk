// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Selector predicates for control-flow constructs that require per-block semantic
//! analysis during state threading.
//!
//! This module covers two groups that need *special* analysis beyond a simple
//! selector check: exception handlers (where the receiver block must also be
//! analysed for mutations) and Boolean/optional conditionals (where each branch
//! block must be analysed independently).  Loop selectors (`whileTrue:`, `do:`,
//! etc.) and unary loop forms are classified directly in
//! `semantic_analysis::facts` and do not require per-block analysis here.
//!
//! # Groups
//!
//! | Group       | Selectors                                            | Why per-block analysis?                        |
//! |-------------|------------------------------------------------------|------------------------------------------------|
//! | Exception   | `on:do:`, `ensure:`                                  | Receiver (try body) is a block that may mutate |
//! | Conditional | `ifTrue:`, `ifFalse:`, `ifTrue:ifFalse:`, `ifNotNil:` | Each branch may mutate independently           |

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
}
