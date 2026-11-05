// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Static export data for common Erlang/OTP modules.
//!
//! **DDD Context:** Language Service
//!
//! Provides completion data for `Erlang <module>` expressions in the editor.
//! Since the LSP has no runtime connection to query `module_info(exports)`,
//! completions come from the curated export table in
//! [`beamtalk_core::ffi_exports`] — the same table the `arity_mismatch` lint
//! reads, so the two cannot disagree (BT-4371).
//!
//! ## Selector Conversion
//!
//! Erlang function/arity pairs are converted to Beamtalk keyword selectors:
//! - `reverse/1` → `reverse:`
//! - `seq/2` → `seq:with:`
//! - `seq/3` → `seq:with:with:`
//! - `node/0` → `node` (nullary)

/// Converts an Erlang function name and arity to a Beamtalk keyword selector.
///
/// # Examples
/// - `("reverse", 1)` → `"reverse:"`
/// - `("seq", 2)` → `"seq:with:"`
/// - `("node", 0)` → `"node"`
#[must_use]
pub(crate) fn export_to_selector(name: &str, arity: u8) -> String {
    match arity {
        0 => name.to_string(),
        1 => format!("{name}:"),
        _ => {
            let mut selector = format!("{name}:");
            for _ in 1..arity {
                selector.push_str("with:");
            }
            selector
        }
    }
}

/// Returns the detail string for an Erlang export completion.
#[must_use]
pub(crate) fn export_detail(module: &str, name: &str, arity: u8) -> String {
    format!("{module}:{name}/{arity}")
}

/// Erlang modules suggested when typing `Erlang `.
///
/// The curated table shared with the `arity_mismatch` lint
/// ([`beamtalk_core::ffi_exports`]) so completions never offer an arity the
/// lint rejects (BT-4371).
pub(crate) use beamtalk_core::ffi_exports::CURATED_MODULES;

/// Look up an Erlang module by name and return its info.
pub(crate) use beamtalk_core::ffi_exports::find_module;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn export_to_selector_unary() {
        assert_eq!(export_to_selector("node", 0), "node");
    }

    #[test]
    fn export_to_selector_one_arg() {
        assert_eq!(export_to_selector("reverse", 1), "reverse:");
    }

    #[test]
    fn export_to_selector_two_args() {
        assert_eq!(export_to_selector("seq", 2), "seq:with:");
    }

    #[test]
    fn export_to_selector_three_args() {
        assert_eq!(export_to_selector("seq", 3), "seq:with:with:");
    }

    #[test]
    fn find_module_exists() {
        assert!(find_module("lists").is_some());
        assert_eq!(find_module("lists").unwrap().name, "lists");
    }

    #[test]
    fn find_module_not_found() {
        assert!(find_module("nonexistent_module").is_none());
    }

    #[test]
    fn export_detail_format() {
        assert_eq!(export_detail("lists", "reverse", 1), "lists:reverse/1");
    }
}
