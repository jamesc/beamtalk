// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Centralised classification of selectors that require state threading.
//!
//! # Groups
//!
//! | Group       | Selectors                                                            |
//! |-------------|----------------------------------------------------------------------|
//! | Exception   | `on:do:`, `ensure:`                                                  |
//! | Conditional | `ifTrue:`, `ifFalse:`, `ifTrue:ifFalse:`, `ifNotNil:`               |

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
