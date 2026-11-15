// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! String primitive implementations.
//!
//! **DDD Context:** Compilation — Code Generation

use super::{call_self_p0, generate_comparison_bif, param, wrap_string_result};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;

/// `call 'beamtalk_primitive':'string_to_binary'(Self)` — unwrap the `String`
/// receiver to the raw bytes the Erlang `string`/`binary`/`unicode` BIFs take
/// (BT-4170: a `String` is the tagged term `{beamtalk_string, Bin}`).
fn unwrap_self() -> Document<'static> {
    Document::Str("call 'beamtalk_primitive':'string_to_binary'(Self)")
}

/// `call 'beamtalk_primitive':'string_to_binary'(<var>)` — unwrap a
/// `String`-typed argument.
fn unwrap_var(var: &str) -> Document<'static> {
    docvec![
        "call 'beamtalk_primitive':'string_to_binary'(",
        leaf::var(var),
        ")"
    ]
}

/// String primitive implementations.
pub(crate) fn generate_string_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        // Comparison (ADR 0002: Erlang operators)
        "=:=" | "/=" | "=/=" | "<" | ">" | "<=" | ">=" => generate_comparison_bif(selector, params),
        // Concatenation, length, access, case, whitespace, reverse
        "++" | "," | "length" | "at:" | "first" | "last" | "uppercase" | "lowercase"
        | "asciiLowercase" | "capitalize" | "trim" | "trimLeft" | "trimRight" | "reverse" => {
            generate_string_transform_bif(selector, params)
        }
        // Search, splitting, replace, substring, padding
        "includes:" | "startsWith:" | "endsWith:" | "indexOf:" | "split:" | "splitOn:"
        | "repeat:" | "lines" | "words" | "replaceAll:with:" | "replaceFirst:with:" | "take:"
        | "drop:" | "padLeft:" | "padRight:" | "padLeft:with:" | "padRight:with:" => {
            generate_string_search_bif(selector, params)
        }
        // Regex operations
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
        "++" | "," => Some(wrap_string_result(docvec![
            "call 'erlang':'iolist_to_binary'([",
            unwrap_self(),
            ", ",
            unwrap_var(p0),
            "])"
        ])),
        "length" => Some(docvec!["call 'string':'length'(", unwrap_self(), ")"]),
        "at:" => Some(call_self_p0("beamtalk_string", "at", p0)),
        // Grapheme-aware element accessors; both raise
        // `empty_collection` on `""`, matching `List first`/`last`.
        "first" => Some(Document::Str("call 'beamtalk_string':'first'(Self)")),
        "last" => Some(Document::Str("call 'beamtalk_string':'last'(Self)")),
        "uppercase" => Some(wrap_string_result(docvec![
            "call 'unicode':'characters_to_binary'(call 'string':'uppercase'(",
            unwrap_self(),
            "))"
        ])),
        "lowercase" => Some(wrap_string_result(docvec![
            "call 'unicode':'characters_to_binary'(call 'string':'lowercase'(",
            unwrap_self(),
            "))"
        ])),
        "asciiLowercase" => Some(Document::Str(
            "call 'beamtalk_string':'ascii_lowercase'(Self)",
        )),
        "capitalize" => Some(Document::Str("call 'beamtalk_string':'capitalize'(Self)")),
        "trim" => Some(wrap_string_result(docvec![
            "call 'unicode':'characters_to_binary'(call 'string':'trim'(",
            unwrap_self(),
            ", 'both'))"
        ])),
        "trimLeft" => Some(wrap_string_result(docvec![
            "call 'unicode':'characters_to_binary'(call 'string':'trim'(",
            unwrap_self(),
            ", 'leading'))"
        ])),
        "trimRight" => Some(wrap_string_result(docvec![
            "call 'unicode':'characters_to_binary'(call 'string':'trim'(",
            unwrap_self(),
            ", 'trailing'))"
        ])),
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
        // BT-4170: routed through `beamtalk_string:split/2`, which unwraps
        // both sides and wraps each part — a bare `binary:split/3` can
        // neither take tagged input nor wrap its list result.
        "split:" => Some(call_self_p0("beamtalk_string", "split", p0)),
        "splitOn:" => Some(call_self_p0("beamtalk_string", "split_on", p0)),
        "repeat:" => Some(call_self_p0("beamtalk_string", "repeat", p0)),
        "lines" => Some(Document::Str("call 'beamtalk_string':'lines'(Self)")),
        "words" => Some(Document::Str("call 'beamtalk_string':'words'(Self)")),
        "replaceAll:with:" => {
            let p1 = param(params, 1, "_Arg1");
            Some(wrap_string_result(docvec![
                "call 'binary':'replace'(",
                unwrap_self(),
                ", ",
                unwrap_var(p0),
                ", ",
                unwrap_var(p1),
                ", ['global'])",
            ]))
        }
        "replaceFirst:with:" => {
            let p1 = param(params, 1, "_Arg1");
            Some(wrap_string_result(docvec![
                "call 'binary':'replace'(",
                unwrap_self(),
                ", ",
                unwrap_var(p0),
                ", ",
                unwrap_var(p1),
                ", [])",
            ]))
        }
        "take:" => Some(call_self_p0("beamtalk_string", "take", p0)),
        "drop:" => Some(call_self_p0("beamtalk_string", "drop", p0)),
        "padLeft:" => Some(wrap_string_result(docvec![
            "call 'unicode':'characters_to_binary'(call 'string':'pad'(",
            unwrap_self(),
            ", ",
            leaf::var(p0),
            ", 'leading'))",
        ])),
        "padRight:" => Some(wrap_string_result(docvec![
            "call 'unicode':'characters_to_binary'(call 'string':'pad'(",
            unwrap_self(),
            ", ",
            leaf::var(p0),
            ", 'trailing'))",
        ])),
        "padLeft:with:" | "padRight:with:" => {
            let p1 = param(params, 1, "_Arg1");
            let dir = if selector == "padLeft:with:" {
                "'leading'"
            } else {
                "'trailing'"
            };
            Some(wrap_string_result(docvec![
                "call 'unicode':'characters_to_binary'(call 'string':'pad'(",
                unwrap_self(),
                ", ",
                leaf::var(p0),
                ", ",
                dir,
                ", ",
                unwrap_var(p1),
                "))",
            ]))
        }
        _ => None,
    }
}

fn generate_string_misc_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    let p0 = param(params, 0, "_Arg0");
    match selector {
        "isEmpty" => Some(Document::Str("call 'beamtalk_string':'is_empty'(Self)")),
        "isNotEmpty" => Some(Document::Str("call 'beamtalk_string':'is_not_empty'(Self)")),
        "isBlank" => Some(Document::Str("call 'beamtalk_string':'is_blank'(Self)")),
        "isDigit" => Some(Document::Str("call 'beamtalk_string':'is_digit'(Self)")),
        "isAlpha" => Some(Document::Str("call 'beamtalk_string':'is_alpha'(Self)")),
        "asInteger" => Some(docvec![
            "call 'erlang':'binary_to_integer'(",
            unwrap_self(),
            ")"
        ]),
        // BT-4544: radix-aware counterpart to `asInteger` (raises like it —
        // use `Integer parse:radix:` for the `Result`-based version).
        "asIntegerRadix:" => {
            let radix = param(params, 0, "_Radix");
            Some(docvec![
                "call 'erlang':'binary_to_integer'(",
                unwrap_self(),
                ", ",
                leaf::var(radix.to_string()),
                ")"
            ])
        }
        "asFloat" => Some(docvec![
            "call 'erlang':'binary_to_float'(",
            unwrap_self(),
            ")"
        ]),
        "asAtom" => Some(docvec![
            "call 'erlang':'binary_to_atom'(",
            unwrap_self(),
            ", 'utf8')"
        ]),
        "asList" => Some(Document::Str("call 'beamtalk_string':'as_list'(Self)")),
        // BT-4380: code points (OTP charlist convention), not UTF-8 bytes.
        "asCharlist" => Some(Document::Str("call 'beamtalk_string':'as_charlist'(Self)")),
        "each:" => Some(call_self_p0("beamtalk_string", "each", p0)),
        "collect:" => Some(call_self_p0("beamtalk_string", "collect", p0)),
        "select:" => Some(call_self_p0("beamtalk_string", "select", p0)),
        "reject:" => Some(call_self_p0("beamtalk_string", "reject", p0)),
        "stream" => Some(Document::Str("call 'beamtalk_stream':'on'(Self)")),
        // Class-side factory: String class withAll: list joins grapheme list into a String
        "withAll:" => Some(docvec![
            "call 'beamtalk_string':'join'(",
            leaf::var(p0),
            ")"
        ]),
        // Class-side factory: create a String from a single Unicode code point
        "fromCodePoint:" => Some(docvec![
            "call 'beamtalk_string':'from_code_point'(",
            leaf::var(p0),
            ")"
        ]),
        // Class-side factory: create a String from a list of Unicode code points
        "fromCodePoints:" => Some(docvec![
            "call 'beamtalk_string':'from_code_points'(",
            leaf::var(p0),
            ")"
        ]),
        // Class-side factory: coerce an Erlang iolist/charlist to a String binary
        "fromIolist:" => Some(docvec![
            "call 'beamtalk_string':'from_iolist'(",
            leaf::var(p0),
            ")"
        ]),
        _ => None,
    }
}

/// String regex primitive implementations.
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
                leaf::var(p0),
                ", ",
                leaf::var(p1),
                ")",
            ])
        }
        "firstMatch:" => Some(call_self_p0("beamtalk_regex", "first_match", p0)),
        "allMatches:" => Some(call_self_p0("beamtalk_regex", "all_matches", p0)),
        "replaceRegex:with:" => {
            let p1 = param(params, 1, "_Arg1");
            Some(docvec![
                "call 'beamtalk_regex':'replace_regex'(Self, ",
                leaf::var(p0),
                ", ",
                leaf::var(p1),
                ")",
            ])
        }
        "replaceAllRegex:with:" => {
            let p1 = param(params, 1, "_Arg1");
            Some(docvec![
                "call 'beamtalk_regex':'replace_all_regex'(Self, ",
                leaf::var(p0),
                ", ",
                leaf::var(p1),
                ")",
            ])
        }
        "splitRegex:" => Some(call_self_p0("beamtalk_regex", "split_regex", p0)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::super::doc_to_string;
    use super::super::{assert_bif_none_table, assert_bif_table};
    use super::*;

    // Misc group

    /// BT-4752 perf regression pin: `String>>isEmpty`/`isNotEmpty` must stay a
    /// direct primitive call. Declaring them as `self bitSize =:= 0` (the
    /// original BT-4752 form) compiled to a dynamic
    /// `beamtalk_message_dispatch:send(Self, 'bitSize', [])` because `bitSize`
    /// is inherited from `Binary` (BT-4766).
    #[test]
    fn test_is_empty_is_direct_primitive_call() {
        let result = doc_to_string(generate_string_bif("isEmpty", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'is_empty'(Self)".to_string())
        );
        let result = doc_to_string(generate_string_bif("isNotEmpty", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'is_not_empty'(Self)".to_string())
        );
    }

    #[test]
    #[allow(clippy::too_many_lines)] // one contiguous table; splitting hurts readability
    fn generate_string_bif_table() {
        assert_bif_table(
            generate_string_bif,
            &[
                // Comparison group
                (
                    "test_eq",
                    "=:=",
                    &["Other"],
                    "call 'erlang':'=:='(Self, Other)",
                ),
                ("test_lt", "<", &["Other"], "call 'erlang':'<'(Self, Other)"),
                // Transform group
                (
                    "test_concat",
                    "++",
                    &["Other"],
                    "call 'beamtalk_primitive':'string_from_binary'(call 'erlang':'iolist_to_binary'([call 'beamtalk_primitive':'string_to_binary'(Self), call 'beamtalk_primitive':'string_to_binary'(Other)]))",
                ),
                (
                    "test_concat_comma",
                    ",",
                    &["Other"],
                    "call 'beamtalk_primitive':'string_from_binary'(call 'erlang':'iolist_to_binary'([call 'beamtalk_primitive':'string_to_binary'(Self), call 'beamtalk_primitive':'string_to_binary'(Other)]))",
                ),
                (
                    "test_length",
                    "length",
                    &[],
                    "call 'string':'length'(call 'beamtalk_primitive':'string_to_binary'(Self))",
                ),
                (
                    "test_at",
                    "at:",
                    &["I"],
                    "call 'beamtalk_string':'at'(Self, I)",
                ),
                (
                    "test_uppercase",
                    "uppercase",
                    &[],
                    "call 'beamtalk_primitive':'string_from_binary'(call 'unicode':'characters_to_binary'(call 'string':'uppercase'(call 'beamtalk_primitive':'string_to_binary'(Self))))",
                ),
                (
                    "test_lowercase",
                    "lowercase",
                    &[],
                    "call 'beamtalk_primitive':'string_from_binary'(call 'unicode':'characters_to_binary'(call 'string':'lowercase'(call 'beamtalk_primitive':'string_to_binary'(Self))))",
                ),
                (
                    "test_ascii_lowercase",
                    "asciiLowercase",
                    &[],
                    "call 'beamtalk_string':'ascii_lowercase'(Self)",
                ),
                (
                    "test_capitalize",
                    "capitalize",
                    &[],
                    "call 'beamtalk_string':'capitalize'(Self)",
                ),
                (
                    "test_trim",
                    "trim",
                    &[],
                    "call 'beamtalk_primitive':'string_from_binary'(call 'unicode':'characters_to_binary'(call 'string':'trim'(call 'beamtalk_primitive':'string_to_binary'(Self), 'both')))",
                ),
                (
                    "test_trim_left",
                    "trimLeft",
                    &[],
                    "call 'beamtalk_primitive':'string_from_binary'(call 'unicode':'characters_to_binary'(call 'string':'trim'(call 'beamtalk_primitive':'string_to_binary'(Self), 'leading')))",
                ),
                (
                    "test_trim_right",
                    "trimRight",
                    &[],
                    "call 'beamtalk_primitive':'string_from_binary'(call 'unicode':'characters_to_binary'(call 'string':'trim'(call 'beamtalk_primitive':'string_to_binary'(Self), 'trailing')))",
                ),
                (
                    "test_reverse",
                    "reverse",
                    &[],
                    "call 'beamtalk_string':'reverse'(Self)",
                ),
                // Search group
                (
                    "test_includes",
                    "includes:",
                    &["Sub"],
                    "call 'beamtalk_string':'includes'(Self, Sub)",
                ),
                (
                    "test_starts_with",
                    "startsWith:",
                    &["P"],
                    "call 'beamtalk_string':'starts_with'(Self, P)",
                ),
                (
                    "test_ends_with",
                    "endsWith:",
                    &["S"],
                    "call 'beamtalk_string':'ends_with'(Self, S)",
                ),
                (
                    "test_index_of",
                    "indexOf:",
                    &["Sub"],
                    "call 'beamtalk_string':'index_of'(Self, Sub)",
                ),
                (
                    "test_split",
                    "split:",
                    &["Sep"],
                    "call 'beamtalk_string':'split'(Self, Sep)",
                ),
                (
                    "test_split_on",
                    "splitOn:",
                    &["Sep"],
                    "call 'beamtalk_string':'split_on'(Self, Sep)",
                ),
                (
                    "test_repeat",
                    "repeat:",
                    &["N"],
                    "call 'beamtalk_string':'repeat'(Self, N)",
                ),
                (
                    "test_lines",
                    "lines",
                    &[],
                    "call 'beamtalk_string':'lines'(Self)",
                ),
                (
                    "test_words",
                    "words",
                    &[],
                    "call 'beamtalk_string':'words'(Self)",
                ),
                (
                    "test_replace_all_with",
                    "replaceAll:with:",
                    &["P", "R"],
                    "call 'beamtalk_primitive':'string_from_binary'(call 'binary':'replace'(call 'beamtalk_primitive':'string_to_binary'(Self), call 'beamtalk_primitive':'string_to_binary'(P), call 'beamtalk_primitive':'string_to_binary'(R), ['global']))",
                ),
                (
                    "test_replace_first_with",
                    "replaceFirst:with:",
                    &["P", "R"],
                    "call 'beamtalk_primitive':'string_from_binary'(call 'binary':'replace'(call 'beamtalk_primitive':'string_to_binary'(Self), call 'beamtalk_primitive':'string_to_binary'(P), call 'beamtalk_primitive':'string_to_binary'(R), []))",
                ),
                (
                    "test_take",
                    "take:",
                    &["N"],
                    "call 'beamtalk_string':'take'(Self, N)",
                ),
                (
                    "test_drop",
                    "drop:",
                    &["N"],
                    "call 'beamtalk_string':'drop'(Self, N)",
                ),
                (
                    "test_pad_left",
                    "padLeft:",
                    &["N"],
                    "call 'beamtalk_primitive':'string_from_binary'(call 'unicode':'characters_to_binary'(call 'string':'pad'(call 'beamtalk_primitive':'string_to_binary'(Self), N, 'leading')))",
                ),
                (
                    "test_pad_right",
                    "padRight:",
                    &["N"],
                    "call 'beamtalk_primitive':'string_from_binary'(call 'unicode':'characters_to_binary'(call 'string':'pad'(call 'beamtalk_primitive':'string_to_binary'(Self), N, 'trailing')))",
                ),
                (
                    "test_pad_left_with",
                    "padLeft:with:",
                    &["N", "Ch"],
                    "call 'beamtalk_primitive':'string_from_binary'(call 'unicode':'characters_to_binary'(call 'string':'pad'(call 'beamtalk_primitive':'string_to_binary'(Self), N, 'leading', call 'beamtalk_primitive':'string_to_binary'(Ch))))",
                ),
                (
                    "test_pad_right_with",
                    "padRight:with:",
                    &["N", "Ch"],
                    "call 'beamtalk_primitive':'string_from_binary'(call 'unicode':'characters_to_binary'(call 'string':'pad'(call 'beamtalk_primitive':'string_to_binary'(Self), N, 'trailing', call 'beamtalk_primitive':'string_to_binary'(Ch))))",
                ),
                // Regex group
                (
                    "test_matches_regex",
                    "matchesRegex:",
                    &["P"],
                    "call 'beamtalk_regex':'matches_regex'(Self, P)",
                ),
                (
                    "test_matches_regex_options",
                    "matchesRegex:options:",
                    &["P", "O"],
                    "call 'beamtalk_regex':'matches_regex_options'(Self, P, O)",
                ),
                (
                    "test_first_match",
                    "firstMatch:",
                    &["P"],
                    "call 'beamtalk_regex':'first_match'(Self, P)",
                ),
                (
                    "test_all_matches",
                    "allMatches:",
                    &["P"],
                    "call 'beamtalk_regex':'all_matches'(Self, P)",
                ),
                (
                    "test_replace_regex_with",
                    "replaceRegex:with:",
                    &["P", "R"],
                    "call 'beamtalk_regex':'replace_regex'(Self, P, R)",
                ),
                (
                    "test_replace_all_regex_with",
                    "replaceAllRegex:with:",
                    &["P", "R"],
                    "call 'beamtalk_regex':'replace_all_regex'(Self, P, R)",
                ),
                (
                    "test_split_regex",
                    "splitRegex:",
                    &["P"],
                    "call 'beamtalk_regex':'split_regex'(Self, P)",
                ),
                (
                    "test_is_blank",
                    "isBlank",
                    &[],
                    "call 'beamtalk_string':'is_blank'(Self)",
                ),
                (
                    "test_is_digit",
                    "isDigit",
                    &[],
                    "call 'beamtalk_string':'is_digit'(Self)",
                ),
                (
                    "test_is_alpha",
                    "isAlpha",
                    &[],
                    "call 'beamtalk_string':'is_alpha'(Self)",
                ),
                (
                    "test_as_integer",
                    "asInteger",
                    &[],
                    "call 'erlang':'binary_to_integer'(call 'beamtalk_primitive':'string_to_binary'(Self))",
                ),
                (
                    "test_as_float",
                    "asFloat",
                    &[],
                    "call 'erlang':'binary_to_float'(call 'beamtalk_primitive':'string_to_binary'(Self))",
                ),
                (
                    "test_as_integer_radix",
                    "asIntegerRadix:",
                    &["Radix"],
                    "call 'erlang':'binary_to_integer'(call 'beamtalk_primitive':'string_to_binary'(Self), Radix)",
                ),
                (
                    "test_as_atom",
                    "asAtom",
                    &[],
                    "call 'erlang':'binary_to_atom'(call 'beamtalk_primitive':'string_to_binary'(Self), 'utf8')",
                ),
                (
                    "test_as_list",
                    "asList",
                    &[],
                    "call 'beamtalk_string':'as_list'(Self)",
                ),
                (
                    "test_string_as_charlist",
                    "asCharlist",
                    &[],
                    "call 'beamtalk_string':'as_charlist'(Self)",
                ),
                (
                    "test_each",
                    "each:",
                    &["Block"],
                    "call 'beamtalk_string':'each'(Self, Block)",
                ),
                (
                    "test_collect",
                    "collect:",
                    &["Block"],
                    "call 'beamtalk_string':'collect'(Self, Block)",
                ),
                (
                    "test_select",
                    "select:",
                    &["Block"],
                    "call 'beamtalk_string':'select'(Self, Block)",
                ),
                (
                    "test_reject",
                    "reject:",
                    &["Block"],
                    "call 'beamtalk_string':'reject'(Self, Block)",
                ),
                (
                    "test_stream",
                    "stream",
                    &[],
                    "call 'beamtalk_stream':'on'(Self)",
                ),
                (
                    "test_with_all_factory",
                    "withAll:",
                    &["L"],
                    "call 'beamtalk_string':'join'(L)",
                ),
                (
                    "test_from_code_point_factory",
                    "fromCodePoint:",
                    &["CP"],
                    "call 'beamtalk_string':'from_code_point'(CP)",
                ),
                (
                    "test_from_code_points_factory",
                    "fromCodePoints:",
                    &["CPs"],
                    "call 'beamtalk_string':'from_code_points'(CPs)",
                ),
                (
                    "test_from_iolist_factory",
                    "fromIolist:",
                    &["IO"],
                    "call 'beamtalk_string':'from_iolist'(IO)",
                ),
            ],
        );
    }

    #[test]
    fn generate_string_bif_none_table() {
        assert_bif_none_table(
            generate_string_bif,
            &[
                // Edge cases
                ("test_unknown_selector_returns_none", "notAMethod", &[]),
            ],
        );
    }
}
