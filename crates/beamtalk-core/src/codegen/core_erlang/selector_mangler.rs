// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Selector mangling for Core Erlang code generation.
//!
//! # DDD: Domain Service
//!
//! `SelectorMangler` is a domain service in the Code Generation bounded context.
//! It encapsulates the domain knowledge of how Beamtalk selectors map to valid
//! Erlang atoms for use in Core Erlang output.
//!
//! # Responsibilities
//!
//! - Convert `MessageSelector` to Erlang atom strings
//! - Handle special characters that need quoting
//! - Ensure generated atoms are valid Core Erlang
//!
//! # Usage
//!
//! The primary method is [`MessageSelector::to_erlang_atom()`], which is defined
//! on the AST type itself. This module provides additional utilities for more
//! complex mangling scenarios.
//!
//! ```
//! use beamtalk_core::ast::MessageSelector;
//! use beamtalk_core::codegen::core_erlang::selector_mangler::format_as_atom;
//!
//! let selector = MessageSelector::Unary("increment".into());
//! let atom = format_as_atom(&selector);
//! assert_eq!(atom, "'increment'");
//! ```

use crate::ast::MessageSelector;

/// Formats a selector as a quoted Core Erlang atom.
///
/// This wraps the selector name in single quotes, which is required for
/// Core Erlang atom syntax.
///
/// # Examples
///
/// ```
/// use beamtalk_core::ast::MessageSelector;
/// use beamtalk_core::codegen::core_erlang::selector_mangler::format_as_atom;
///
/// let selector = MessageSelector::Unary("increment".into());
/// assert_eq!(format_as_atom(&selector), "'increment'");
///
/// let selector = MessageSelector::Binary("+".into());
/// assert_eq!(format_as_atom(&selector), "'+'");
/// ```
#[must_use]
pub fn format_as_atom(selector: &MessageSelector) -> String {
    format!("'{}'", selector.to_erlang_atom())
}

/// Checks if a selector name requires quoting in Erlang.
///
/// Erlang atoms that start with a lowercase letter and contain only
/// alphanumeric characters and underscores don't need quoting. All others do.
///
/// In Core Erlang, we always quote atoms for consistency, but this function
/// is useful for debugging and documentation.
///
/// # Returns
///
/// `true` if the name requires quoting, `false` otherwise.
/// Empty strings always require quoting.
#[must_use]
pub fn requires_quoting(name: &str) -> bool {
    let Some(first) = name.chars().next() else {
        return true;
    };

    // Must start with lowercase letter
    if !first.is_ascii_lowercase() {
        return true;
    }

    // Rest must be alphanumeric or underscore
    for c in name.chars().skip(1) {
        if !c.is_ascii_alphanumeric() && c != '_' {
            return true;
        }
    }

    false
}

/// Escapes special characters in an atom name for Core Erlang.
///
/// This handles characters that have special meaning in Erlang strings/atoms:
/// - Single quotes → `\'`
/// - Backslashes → `\\`
#[must_use]
pub fn escape_atom_chars(name: &str) -> String {
    let mut result = String::with_capacity(name.len());
    for c in name.chars() {
        match c {
            '\'' => result.push_str("\\'"),
            '\\' => result.push_str("\\\\"),
            _ => result.push(c),
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::KeywordPart;
    use crate::parse::Span;

    #[test]
    fn format_unary_selector() {
        let selector = MessageSelector::Unary("increment".into());
        assert_eq!(format_as_atom(&selector), "'increment'");
    }

    #[test]
    fn format_binary_selector() {
        let selector = MessageSelector::Binary("+".into());
        assert_eq!(format_as_atom(&selector), "'+'");
    }

    #[test]
    fn format_keyword_selector() {
        let selector = MessageSelector::Keyword(vec![
            KeywordPart::new("at:", Span::new(0, 3)),
            KeywordPart::new("put:", Span::new(5, 9)),
        ]);
        assert_eq!(format_as_atom(&selector), "'at:put:'");
    }

    #[test]
    fn simple_atom_no_quoting() {
        assert!(!requires_quoting("increment"));
        assert!(!requires_quoting("getValue"));
        assert!(!requires_quoting("some_method"));
    }

    #[test]
    fn special_chars_require_quoting() {
        assert!(requires_quoting("+"));
        assert!(requires_quoting("at:"));
        assert!(requires_quoting("at:put:"));
        assert!(requires_quoting(""));
        assert!(requires_quoting("123"));
        assert!(requires_quoting("Uppercase"));
    }

    #[test]
    fn escape_special_chars() {
        assert_eq!(escape_atom_chars("normal"), "normal");
        assert_eq!(escape_atom_chars("it's"), "it\\'s");
        assert_eq!(escape_atom_chars("back\\slash"), "back\\\\slash");
    }
}
