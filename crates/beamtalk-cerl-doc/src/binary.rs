// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Core Erlang binary-literal byte-segment builders.
//!
//! **DDD Context:** Compilation — Code Generation (shared leaf, ADR 0117 step 4)
//!
//! Moved out of `CoreErlangGenerator` (`codegen::core_erlang::gen_server::spawn`):
//! both functions are static (no `&self`), so they move cleanly alongside the
//! `Document`/`leaf` API that is their only caller outside tests.

use std::fmt::Write as _;

/// Returns a Core Erlang binary string literal for the given string.
///
/// Produces: `#{#<byte1>(8,1,'integer',['unsigned'|['big']]), ...}#`
#[must_use]
pub fn binary_string_literal(s: &str) -> String {
    let mut result = String::from("#{");
    result.push_str(&binary_byte_segments(s));
    result.push_str("}#");
    result
}

/// Returns Core Erlang binary byte segments for a string, without `#{...}#` wrapping.
///
/// Used by `binary_string_literal` and string interpolation codegen.
/// Produces: `#<byte1>(8,1,'integer',['unsigned'|['big']]),#<byte2>(...), ...`
#[must_use]
pub fn binary_byte_segments(s: &str) -> String {
    let mut result = String::new();
    for (i, byte) in s.bytes().enumerate() {
        if i > 0 {
            result.push(',');
        }
        write!(result, "#<{byte}>(8,1,'integer',['unsigned'|['big']])").unwrap();
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn binary_string_literal_wraps_segments() {
        assert_eq!(binary_string_literal(""), "#{}#");
        assert_eq!(
            binary_string_literal("A"),
            "#{#<65>(8,1,'integer',['unsigned'|['big']])}#"
        );
    }

    #[test]
    fn binary_byte_segments_joins_with_comma() {
        assert_eq!(binary_byte_segments(""), "");
        assert_eq!(
            binary_byte_segments("AB"),
            "#<65>(8,1,'integer',['unsigned'|['big']]),#<66>(8,1,'integer',['unsigned'|['big']])"
        );
    }
}
