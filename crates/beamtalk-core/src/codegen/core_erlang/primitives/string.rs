// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! String primitive implementations.
//!
//! **DDD Context:** Compilation — Code Generation

use super::super::document::Document;
use super::{call_self_p0, generate_comparison_bif, param};
use crate::docvec;

/// String primitive implementations.
pub(crate) fn generate_string_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        // Comparison (ADR 0002: Erlang operators)
        "=:=" | "/=" | "=/=" | "<" | ">" | "<=" | ">=" => generate_comparison_bif(selector, params),
        // Concatenation, length, access, case, whitespace, reverse
        "++" | "," | "length" | "at:" | "uppercase" | "lowercase" | "capitalize" | "trim"
        | "trimLeft" | "trimRight" | "reverse" => generate_string_transform_bif(selector, params),
        // Search, splitting, replace, substring, padding
        "includes:" | "startsWith:" | "endsWith:" | "indexOf:" | "split:" | "splitOn:"
        | "repeat:" | "lines" | "words" | "replaceAll:with:" | "replaceFirst:with:" | "take:"
        | "drop:" | "padLeft:" | "padRight:" | "padLeft:with:" | "padRight:with:" => {
            generate_string_search_bif(selector, params)
        }
        // Regex operations (BT-709)
        "matchesRegex:"
        | "matchesRegex:options:"
        | "firstMatch:"
        | "allMatches:"
        | "replaceRegex:with:"
        | "replaceAllRegex:with:"
        | "splitRegex:" => generate_string_regex_bif(selector, params),
        // Testing, conversion, iteration, streaming
        _ => generate_string_misc_bif(selector, params),
    }
}

fn generate_string_transform_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    let p0 = param(params, 0, "_Arg0");
    match selector {
        "++" | "," => Some(docvec![
            "call 'erlang':'iolist_to_binary'([Self, ",
            p0.to_string(),
            "])"
        ]),
        "length" => Some(Document::Str("call 'string':'length'(Self)")),
        "at:" => Some(call_self_p0("beamtalk_string", "at", p0)),
        "uppercase" => Some(Document::Str(
            "call 'unicode':'characters_to_binary'(call 'string':'uppercase'(Self))",
        )),
        "lowercase" => Some(Document::Str(
            "call 'unicode':'characters_to_binary'(call 'string':'lowercase'(Self))",
        )),
        "capitalize" => Some(Document::Str("call 'beamtalk_string':'capitalize'(Self)")),
        "trim" => Some(Document::Str(
            "call 'unicode':'characters_to_binary'(call 'string':'trim'(Self, 'both'))",
        )),
        "trimLeft" => Some(Document::Str(
            "call 'unicode':'characters_to_binary'(call 'string':'trim'(Self, 'leading'))",
        )),
        "trimRight" => Some(Document::Str(
            "call 'unicode':'characters_to_binary'(call 'string':'trim'(Self, 'trailing'))",
        )),
        "reverse" => Some(Document::Str("call 'beamtalk_string':'reverse'(Self)")),
        _ => None,
    }
}

fn generate_string_search_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    let p0 = param(params, 0, "_Arg0");
    match selector {
        "includes:" => Some(call_self_p0("beamtalk_string", "includes", p0)),
        "startsWith:" => Some(call_self_p0("beamtalk_string", "starts_with", p0)),
        "endsWith:" => Some(call_self_p0("beamtalk_string", "ends_with", p0)),
        "indexOf:" => Some(call_self_p0("beamtalk_string", "index_of", p0)),
        "split:" => Some(docvec![
            "call 'binary':'split'(Self, ",
            p0.to_string(),
            ", ['global'])"
        ]),
        "splitOn:" => Some(call_self_p0("beamtalk_string", "split_on", p0)),
        "repeat:" => Some(call_self_p0("beamtalk_string", "repeat", p0)),
        "lines" => Some(Document::Str("call 'beamtalk_string':'lines'(Self)")),
        "words" => Some(Document::Str("call 'beamtalk_string':'words'(Self)")),
        "replaceAll:with:" => {
            let p1 = param(params, 1, "_Arg1");
            Some(docvec![
                "call 'binary':'replace'(Self, ",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ", ['global'])",
            ])
        }
        "replaceFirst:with:" => {
            let p1 = param(params, 1, "_Arg1");
            Some(docvec![
                "call 'binary':'replace'(Self, ",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ", [])"
            ])
        }
        "take:" => Some(call_self_p0("beamtalk_string", "take", p0)),
        "drop:" => Some(call_self_p0("beamtalk_string", "drop", p0)),
        "padLeft:" => Some(docvec![
            "call 'unicode':'characters_to_binary'(call 'string':'pad'(Self, ",
            p0.to_string(),
            ", 'leading'))",
        ]),
        "padRight:" => Some(docvec![
            "call 'unicode':'characters_to_binary'(call 'string':'pad'(Self, ",
            p0.to_string(),
            ", 'trailing'))",
        ]),
        "padLeft:with:" | "padRight:with:" => {
            let p1 = param(params, 1, "_Arg1");
            let dir = if selector == "padLeft:with:" {
                "'leading'"
            } else {
                "'trailing'"
            };
            Some(docvec![
                "call 'unicode':'characters_to_binary'(call 'string':'pad'(Self, ",
                p0.to_string(),
                ", ",
                dir,
                ", ",
                p1.to_string(),
                "))",
            ])
        }
        _ => None,
    }
}

fn generate_string_misc_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    let p0 = param(params, 0, "_Arg0");
    match selector {
        "isBlank" => Some(Document::Str("call 'beamtalk_string':'is_blank'(Self)")),
        "isDigit" => Some(Document::Str("call 'beamtalk_string':'is_digit'(Self)")),
        "isAlpha" => Some(Document::Str("call 'beamtalk_string':'is_alpha'(Self)")),
        "asInteger" => Some(Document::Str("call 'erlang':'binary_to_integer'(Self)")),
        "asFloat" => Some(Document::Str("call 'erlang':'binary_to_float'(Self)")),
        "asAtom" => Some(Document::Str(
            "call 'erlang':'binary_to_atom'(Self, 'utf8')",
        )),
        "asList" => Some(Document::Str("call 'beamtalk_string':'as_list'(Self)")),
        "each:" => Some(call_self_p0("beamtalk_string", "each", p0)),
        "collect:" => Some(call_self_p0("beamtalk_string", "collect", p0)),
        "select:" => Some(call_self_p0("beamtalk_string", "select", p0)),
        "reject:" => Some(call_self_p0("beamtalk_string", "reject", p0)),
        "stream" => Some(Document::Str("call 'beamtalk_stream':'on'(Self)")),
        // Class-side factory: String class withAll: list joins grapheme list into a String
        "withAll:" => Some(docvec![
            "call 'beamtalk_string':'join'(",
            p0.to_string(),
            ")"
        ]),
        // Class-side factory: create a String from a single Unicode code point
        "fromCodePoint:" => Some(docvec![
            "call 'beamtalk_string':'from_code_point'(",
            p0.to_string(),
            ")"
        ]),
        // Class-side factory: create a String from a list of Unicode code points
        "fromCodePoints:" => Some(docvec![
            "call 'beamtalk_string':'from_code_points'(",
            p0.to_string(),
            ")"
        ]),
        // Class-side factory: coerce an Erlang iolist/charlist to a String binary
        "fromIolist:" => Some(docvec![
            "call 'beamtalk_string':'from_iolist'(",
            p0.to_string(),
            ")"
        ]),
        _ => None,
    }
}

/// String regex primitive implementations (BT-709).
///
/// These delegate to `beamtalk_regex` helper functions that accept both
/// String patterns and compiled Regex objects.
fn generate_string_regex_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    let p0 = param(params, 0, "_Arg0");
    match selector {
        "matchesRegex:" => Some(call_self_p0("beamtalk_regex", "matches_regex", p0)),
        "matchesRegex:options:" => {
            let p1 = param(params, 1, "_Arg1");
            Some(docvec![
                "call 'beamtalk_regex':'matches_regex_options'(Self, ",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")",
            ])
        }
        "firstMatch:" => Some(call_self_p0("beamtalk_regex", "first_match", p0)),
        "allMatches:" => Some(call_self_p0("beamtalk_regex", "all_matches", p0)),
        "replaceRegex:with:" => {
            let p1 = param(params, 1, "_Arg1");
            Some(docvec![
                "call 'beamtalk_regex':'replace_regex'(Self, ",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")",
            ])
        }
        "replaceAllRegex:with:" => {
            let p1 = param(params, 1, "_Arg1");
            Some(docvec![
                "call 'beamtalk_regex':'replace_all_regex'(Self, ",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")",
            ])
        }
        "splitRegex:" => Some(call_self_p0("beamtalk_regex", "split_regex", p0)),
        _ => None,
    }
}
