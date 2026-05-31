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
//! Selectors are converted to Erlang atom *strings* by
//! [`MessageSelector::to_erlang_atom()`], defined on the AST type itself. This
//! module provides the atom-length-safe mangling utilities layered on top —
//! [`safe_class_method_selector`] and [`safe_class_method_fn_name`].
//!
//! Quoting an atom string for Core Erlang output is **not** done here: that is
//! the job of the `document::leaf::atom` typed-leaf helper (ADR 0089), which
//! escapes and wraps the name so no raw `'…'` fragment is built by hand.

/// Maximum length for an Erlang/Core Erlang atom (hard VM limit).
const MAX_ATOM_LEN: usize = 255;

/// The prefix the runtime prepends to selector atoms to form class method
/// function names (see `beamtalk_class_dispatch:class_method_fun_name/1`).
const CLASS_METHOD_PREFIX: &str = "class_";

/// Returns an atom-safe version of `name`.
///
/// If `name` is ≤255 bytes it is returned unchanged.  Otherwise it is replaced
/// with a deterministic FNV-1a 64-bit hash: `"kw_<16-hex-digits>"` (19 chars).
///
/// Use this for atoms that don't carry the `class_` prefix (e.g. instance
/// dispatch selectors).  For class method atoms, prefer
/// [`safe_class_method_selector`] instead.
#[must_use]
pub fn safe_atom_name(name: &str) -> String {
    if name.len() <= MAX_ATOM_LEN {
        name.to_string()
    } else {
        let hash = fnv1a_64(name.as_bytes());
        format!("kw_{hash:016x}")
    }
}

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

#[cfg(test)]
mod tests {
    use super::*;

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
