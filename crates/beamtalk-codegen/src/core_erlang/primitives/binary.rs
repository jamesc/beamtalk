// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Binary primitive implementations (ADR 0086 Phase 1).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Maps `@primitive` selectors on Binary to their Erlang runtime
//! functions in `beamtalk_binary`.

use super::param;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;

/// Binary primitive implementations (ADR 0086).
pub(crate) fn generate_binary_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        // Collection subclass-responsibility: bytes, not graphemes (BT-4170).
        // (The old `byteSize` alias is deleted — `size` is byte-level now.)
        "size" => Some(Document::Str("call 'erlang':'byte_size'(Self)")),
        "do:" => {
            let block = param(params, 0, "_Block");
            Some(docvec![
                "call 'beamtalk_binary':'do'(Self, ",
                leaf::var(block.to_string()),
                ")",
            ])
        }
        "printString" => Some(Document::Str("call 'beamtalk_binary':'print_string'(Self)")),

        // Byte-level access
        "at:" => {
            let index = param(params, 0, "_Index");
            Some(docvec![
                "call 'beamtalk_binary':'at'(Self, ",
                leaf::var(index.to_string()),
                ")",
            ])
        }
        "byteAt:" => {
            let offset = param(params, 0, "_Offset");
            Some(docvec![
                "call 'beamtalk_binary':'byte_at'(Self, ",
                leaf::var(offset.to_string()),
                ")",
            ])
        }
        // Slicing and concatenation
        "part:size:" => {
            let offset = param(params, 0, "_Offset");
            let length = param(params, 1, "_Length");
            Some(docvec![
                "call 'beamtalk_binary':'part'(Self, ",
                leaf::var(offset.to_string()),
                ", ",
                leaf::var(length.to_string()),
                ")",
            ])
        }
        "concat:" => {
            let other = param(params, 0, "_Other");
            Some(docvec![
                "call 'beamtalk_binary':'concat'(Self, ",
                leaf::var(other.to_string()),
                ")",
            ])
        }

        // Byte-level substring search (BT-4190)
        "includesSubstring:" => {
            let needle = param(params, 0, "_Needle");
            Some(docvec![
                "call 'beamtalk_binary':'includes_substring'(Self, ",
                leaf::var(needle.to_string()),
                ")",
            ])
        }
        "indexOf:" => {
            let needle = param(params, 0, "_Needle");
            Some(docvec![
                "call 'beamtalk_binary':'index_of'(Self, ",
                leaf::var(needle.to_string()),
                ")",
            ])
        }

        // Conversion
        "toBytes" => Some(Document::Str("call 'beamtalk_binary':'to_bytes'(Self)")),
        // BT-4170: every `Binary` dispatches here (no sniffing), so
        // .bt-level success is `Result ok:` uniformly.
        "asString" => Some(Document::Str("call 'beamtalk_binary':'as_string'(Self)")),
        "asStringUnchecked" => Some(Document::Str(
            "call 'beamtalk_binary':'as_string_unchecked'(Self)",
        )),

        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::super::assert_bif_table;
    use super::*;

    #[test]
    fn test_byte_size_deleted() {
        // BT-4170: `byteSize` is deleted — `size` is byte-level now.
        assert!(generate_binary_bif("byteSize", &[]).is_none());
    }

    #[test]
    fn test_unknown_selector_returns_none() {
        assert!(generate_binary_bif("size:", &[]).is_none());
        assert!(generate_binary_bif("asInteger", &[]).is_none());
    }

    #[test]
    #[allow(clippy::too_many_lines)] // one contiguous table; splitting hurts readability
    fn generate_binary_bif_table() {
        assert_bif_table(
            generate_binary_bif,
            &[
                ("test_size", "size", &[], "call 'erlang':'byte_size'(Self)"),
                (
                    "test_do",
                    "do:",
                    &["Block"],
                    "call 'beamtalk_binary':'do'(Self, Block)",
                ),
                (
                    "test_print_string",
                    "printString",
                    &[],
                    "call 'beamtalk_binary':'print_string'(Self)",
                ),
                (
                    "test_includes_substring",
                    "includesSubstring:",
                    &["Needle"],
                    "call 'beamtalk_binary':'includes_substring'(Self, Needle)",
                ),
                (
                    "test_index_of",
                    "indexOf:",
                    &["Needle"],
                    "call 'beamtalk_binary':'index_of'(Self, Needle)",
                ),
                (
                    "test_at",
                    "at:",
                    &["Index"],
                    "call 'beamtalk_binary':'at'(Self, Index)",
                ),
                (
                    "test_byte_at",
                    "byteAt:",
                    &["Offset"],
                    "call 'beamtalk_binary':'byte_at'(Self, Offset)",
                ),
                (
                    "test_part_size",
                    "part:size:",
                    &["Offset", "Length"],
                    "call 'beamtalk_binary':'part'(Self, Offset, Length)",
                ),
                (
                    "test_concat",
                    "concat:",
                    &["Other"],
                    "call 'beamtalk_binary':'concat'(Self, Other)",
                ),
                (
                    "test_to_bytes",
                    "toBytes",
                    &[],
                    "call 'beamtalk_binary':'to_bytes'(Self)",
                ),
                (
                    "test_as_string",
                    "asString",
                    &[],
                    "call 'beamtalk_binary':'as_string'(Self)",
                ),
                (
                    "test_as_string_unchecked",
                    "asStringUnchecked",
                    &[],
                    "call 'beamtalk_binary':'as_string_unchecked'(Self)",
                ),
            ],
        );
    }
}
