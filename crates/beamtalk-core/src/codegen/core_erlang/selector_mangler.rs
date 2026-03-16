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

/// Maximum length for an Erlang/Core Erlang atom (hard VM limit).
const MAX_ATOM_LEN: usize = 255;

/// The prefix the runtime prepends to selector atoms to form class method
/// function names (see `beamtalk_class_dispatch:class_method_fun_name/1`).
const CLASS_METHOD_PREFIX: &str = "class_";

/// Returns a safe selector atom for use in `class_send` calls and meta entries.
///
/// Hashes the selector when the corresponding `class_`-prefixed function name
/// would exceed Erlang's 255-char atom limit.  The hash trigger is based on
/// the *function name* length (`"class_" + selector`), not the selector alone,
/// so that [`safe_class_method_fn_name`] and this function stay in lock-step.
///
/// **Invariant:** `safe_class_method_fn_name(sel) == "class_" + safe_class_method_selector(sel)`
///
/// This matches how the runtime constructs function names:
/// `class_method_fun_name(Selector) -> list_to_atom("class_" ++ atom_to_list(Selector))`.
#[must_use]
pub fn safe_class_method_selector(selector: &str) -> String {
    if CLASS_METHOD_PREFIX.len() + selector.len() <= MAX_ATOM_LEN {
        selector.to_string()
    } else {
        let hash = fnv1a_64(selector.as_bytes());
        format!("kw_{hash:016x}")
    }
}

/// Returns a safe function name atom for class method definitions and direct calls.
///
/// Always equal to `"class_" + safe_class_method_selector(selector)`.
#[must_use]
pub fn safe_class_method_fn_name(selector: &str) -> String {
    format!(
        "{CLASS_METHOD_PREFIX}{}",
        safe_class_method_selector(selector)
    )
}

/// Deterministic FNV-1a 64-bit hash (no external dependency).
fn fnv1a_64(data: &[u8]) -> u64 {
    let mut hash: u64 = 0xcbf2_9ce4_8422_2325;
    for &byte in data {
        hash ^= u64::from(byte);
        hash = hash.wrapping_mul(0x0100_0000_01b3);
    }
    hash
}

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
    use crate::source_analysis::Span;

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

    #[test]
    fn safe_class_method_selector_short_unchanged() {
        assert_eq!(safe_class_method_selector("x:y:"), "x:y:");
        assert_eq!(safe_class_method_selector("defaultValue"), "defaultValue");
    }

    #[test]
    fn safe_class_method_fn_name_short() {
        assert_eq!(safe_class_method_fn_name("x:y:"), "class_x:y:");
        assert_eq!(
            safe_class_method_fn_name("defaultValue"),
            "class_defaultValue"
        );
    }

    #[test]
    fn safe_class_method_selector_boundary() {
        // Selector that fits alone but "class_" + selector > 255
        let sel = "a".repeat(250); // "class_" + 250 = 256 > 255
        let safe_sel = safe_class_method_selector(&sel);
        assert!(safe_sel.starts_with("kw_"));
        // Verify fn_name invariant
        assert_eq!(safe_class_method_fn_name(&sel), format!("class_{safe_sel}"));
    }

    #[test]
    fn safe_class_method_selector_exactly_fits() {
        // "class_" is 6 chars; 249 + 6 = 255, exactly at limit
        let sel = "a".repeat(249);
        assert_eq!(safe_class_method_selector(&sel), sel);
        assert_eq!(safe_class_method_fn_name(&sel), format!("class_{sel}"));
    }

    #[test]
    fn safe_class_method_selector_deterministic() {
        let long = "codexInputTokens:codexOutputTokens:codexModelName:codexCachedTokens:codexReasoning:codexReasoningEffort:codexMaxOutputTokens:codexSystemFingerprint:codexServiceTier:operationTokens:operationCost:operationDuration:apiCallCount:errorCount:retryCount:totalLatency:averageLatency:peakMemoryUsage:";
        let r1 = safe_class_method_selector(long);
        let r2 = safe_class_method_selector(long);
        assert_eq!(r1, r2);
        assert!(r1.starts_with("kw_"));
        assert_eq!(r1.len(), 19); // "kw_" + 16 hex digits
    }

    #[test]
    fn safe_class_method_fn_name_invariant() {
        // The invariant: fn_name(sel) == "class_" + selector(sel)
        let long = "codexInputTokens:codexOutputTokens:codexModelName:codexCachedTokens:codexReasoning:codexReasoningEffort:codexMaxOutputTokens:codexSystemFingerprint:codexServiceTier:operationTokens:operationCost:operationDuration:apiCallCount:errorCount:retryCount:totalLatency:averageLatency:peakMemoryUsage:";
        assert_eq!(
            safe_class_method_fn_name(long),
            format!("class_{}", safe_class_method_selector(long))
        );
        // Also for short selectors
        assert_eq!(
            safe_class_method_fn_name("x:y:"),
            format!("class_{}", safe_class_method_selector("x:y:"))
        );
    }
}
