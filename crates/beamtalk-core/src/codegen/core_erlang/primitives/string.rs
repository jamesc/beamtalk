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

#[cfg(test)]
mod tests {
    use super::super::doc_to_string;
    use super::*;

    // Comparison group

    #[test]
    fn test_eq() {
        let result = doc_to_string(generate_string_bif("=:=", &["Other".to_string()]));
        assert_eq!(result, Some("call 'erlang':'=:='(Self, Other)".to_string()));
    }

    #[test]
    fn test_lt() {
        let result = doc_to_string(generate_string_bif("<", &["Other".to_string()]));
        assert_eq!(result, Some("call 'erlang':'<'(Self, Other)".to_string()));
    }

    // Transform group

    #[test]
    fn test_concat() {
        let result = doc_to_string(generate_string_bif("++", &["Other".to_string()]));
        assert_eq!(
            result,
            Some("call 'erlang':'iolist_to_binary'([Self, Other])".to_string())
        );
    }

    #[test]
    fn test_concat_comma() {
        let result = doc_to_string(generate_string_bif(",", &["Other".to_string()]));
        assert_eq!(
            result,
            Some("call 'erlang':'iolist_to_binary'([Self, Other])".to_string())
        );
    }

    #[test]
    fn test_length() {
        let result = doc_to_string(generate_string_bif("length", &[]));
        assert_eq!(result, Some("call 'string':'length'(Self)".to_string()));
    }

    #[test]
    fn test_at() {
        let result = doc_to_string(generate_string_bif("at:", &["I".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'at'(Self, I)".to_string())
        );
    }

    #[test]
    fn test_uppercase() {
        let result = doc_to_string(generate_string_bif("uppercase", &[]));
        assert_eq!(
            result,
            Some(
                "call 'unicode':'characters_to_binary'(call 'string':'uppercase'(Self))"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_lowercase() {
        let result = doc_to_string(generate_string_bif("lowercase", &[]));
        assert_eq!(
            result,
            Some(
                "call 'unicode':'characters_to_binary'(call 'string':'lowercase'(Self))"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_capitalize() {
        let result = doc_to_string(generate_string_bif("capitalize", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'capitalize'(Self)".to_string())
        );
    }

    #[test]
    fn test_trim() {
        let result = doc_to_string(generate_string_bif("trim", &[]));
        assert_eq!(
            result,
            Some(
                "call 'unicode':'characters_to_binary'(call 'string':'trim'(Self, 'both'))"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_trim_left() {
        let result = doc_to_string(generate_string_bif("trimLeft", &[]));
        assert_eq!(
            result,
            Some(
                "call 'unicode':'characters_to_binary'(call 'string':'trim'(Self, 'leading'))"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_trim_right() {
        let result = doc_to_string(generate_string_bif("trimRight", &[]));
        assert_eq!(
            result,
            Some(
                "call 'unicode':'characters_to_binary'(call 'string':'trim'(Self, 'trailing'))"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_reverse() {
        let result = doc_to_string(generate_string_bif("reverse", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'reverse'(Self)".to_string())
        );
    }

    // Search group

    #[test]
    fn test_includes() {
        let result = doc_to_string(generate_string_bif("includes:", &["Sub".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'includes'(Self, Sub)".to_string())
        );
    }

    #[test]
    fn test_starts_with() {
        let result = doc_to_string(generate_string_bif("startsWith:", &["P".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'starts_with'(Self, P)".to_string())
        );
    }

    #[test]
    fn test_ends_with() {
        let result = doc_to_string(generate_string_bif("endsWith:", &["S".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'ends_with'(Self, S)".to_string())
        );
    }

    #[test]
    fn test_index_of() {
        let result = doc_to_string(generate_string_bif("indexOf:", &["Sub".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'index_of'(Self, Sub)".to_string())
        );
    }

    #[test]
    fn test_split() {
        let result = doc_to_string(generate_string_bif("split:", &["Sep".to_string()]));
        assert_eq!(
            result,
            Some("call 'binary':'split'(Self, Sep, ['global'])".to_string())
        );
    }

    #[test]
    fn test_split_on() {
        let result = doc_to_string(generate_string_bif("splitOn:", &["Sep".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'split_on'(Self, Sep)".to_string())
        );
    }

    #[test]
    fn test_repeat() {
        let result = doc_to_string(generate_string_bif("repeat:", &["N".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'repeat'(Self, N)".to_string())
        );
    }

    #[test]
    fn test_lines() {
        let result = doc_to_string(generate_string_bif("lines", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'lines'(Self)".to_string())
        );
    }

    #[test]
    fn test_words() {
        let result = doc_to_string(generate_string_bif("words", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'words'(Self)".to_string())
        );
    }

    #[test]
    fn test_replace_all_with() {
        let result = doc_to_string(generate_string_bif(
            "replaceAll:with:",
            &["P".to_string(), "R".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'binary':'replace'(Self, P, R, ['global'])".to_string())
        );
    }

    #[test]
    fn test_replace_first_with() {
        let result = doc_to_string(generate_string_bif(
            "replaceFirst:with:",
            &["P".to_string(), "R".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'binary':'replace'(Self, P, R, [])".to_string())
        );
    }

    #[test]
    fn test_take() {
        let result = doc_to_string(generate_string_bif("take:", &["N".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'take'(Self, N)".to_string())
        );
    }

    #[test]
    fn test_drop() {
        let result = doc_to_string(generate_string_bif("drop:", &["N".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'drop'(Self, N)".to_string())
        );
    }

    #[test]
    fn test_pad_left() {
        let result = doc_to_string(generate_string_bif("padLeft:", &["N".to_string()]));
        assert_eq!(
            result,
            Some(
                "call 'unicode':'characters_to_binary'(call 'string':'pad'(Self, N, 'leading'))"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_pad_right() {
        let result = doc_to_string(generate_string_bif("padRight:", &["N".to_string()]));
        assert_eq!(
            result,
            Some(
                "call 'unicode':'characters_to_binary'(call 'string':'pad'(Self, N, 'trailing'))"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_pad_left_with() {
        let result = doc_to_string(generate_string_bif(
            "padLeft:with:",
            &["N".to_string(), "Ch".to_string()],
        ));
        assert_eq!(
            result,
            Some(
                "call 'unicode':'characters_to_binary'(call 'string':'pad'(Self, N, 'leading', Ch))"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_pad_right_with() {
        let result = doc_to_string(generate_string_bif(
            "padRight:with:",
            &["N".to_string(), "Ch".to_string()],
        ));
        assert_eq!(
            result,
            Some(
                "call 'unicode':'characters_to_binary'(call 'string':'pad'(Self, N, 'trailing', Ch))"
                    .to_string()
            )
        );
    }

    // Regex group (BT-709)

    #[test]
    fn test_matches_regex() {
        let result = doc_to_string(generate_string_bif("matchesRegex:", &["P".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_regex':'matches_regex'(Self, P)".to_string())
        );
    }

    #[test]
    fn test_matches_regex_options() {
        let result = doc_to_string(generate_string_bif(
            "matchesRegex:options:",
            &["P".to_string(), "O".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_regex':'matches_regex_options'(Self, P, O)".to_string())
        );
    }

    #[test]
    fn test_first_match() {
        let result = doc_to_string(generate_string_bif("firstMatch:", &["P".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_regex':'first_match'(Self, P)".to_string())
        );
    }

    #[test]
    fn test_all_matches() {
        let result = doc_to_string(generate_string_bif("allMatches:", &["P".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_regex':'all_matches'(Self, P)".to_string())
        );
    }

    #[test]
    fn test_replace_regex_with() {
        let result = doc_to_string(generate_string_bif(
            "replaceRegex:with:",
            &["P".to_string(), "R".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_regex':'replace_regex'(Self, P, R)".to_string())
        );
    }

    #[test]
    fn test_replace_all_regex_with() {
        let result = doc_to_string(generate_string_bif(
            "replaceAllRegex:with:",
            &["P".to_string(), "R".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_regex':'replace_all_regex'(Self, P, R)".to_string())
        );
    }

    #[test]
    fn test_split_regex() {
        let result = doc_to_string(generate_string_bif("splitRegex:", &["P".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_regex':'split_regex'(Self, P)".to_string())
        );
    }

    // Misc group

    #[test]
    fn test_is_blank() {
        let result = doc_to_string(generate_string_bif("isBlank", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'is_blank'(Self)".to_string())
        );
    }

    #[test]
    fn test_is_digit() {
        let result = doc_to_string(generate_string_bif("isDigit", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'is_digit'(Self)".to_string())
        );
    }

    #[test]
    fn test_is_alpha() {
        let result = doc_to_string(generate_string_bif("isAlpha", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'is_alpha'(Self)".to_string())
        );
    }

    #[test]
    fn test_as_integer() {
        let result = doc_to_string(generate_string_bif("asInteger", &[]));
        assert_eq!(
            result,
            Some("call 'erlang':'binary_to_integer'(Self)".to_string())
        );
    }

    #[test]
    fn test_as_float() {
        let result = doc_to_string(generate_string_bif("asFloat", &[]));
        assert_eq!(
            result,
            Some("call 'erlang':'binary_to_float'(Self)".to_string())
        );
    }

    #[test]
    fn test_as_atom() {
        let result = doc_to_string(generate_string_bif("asAtom", &[]));
        assert_eq!(
            result,
            Some("call 'erlang':'binary_to_atom'(Self, 'utf8')".to_string())
        );
    }

    #[test]
    fn test_as_list() {
        let result = doc_to_string(generate_string_bif("asList", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'as_list'(Self)".to_string())
        );
    }

    #[test]
    fn test_each() {
        let result = doc_to_string(generate_string_bif("each:", &["Block".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'each'(Self, Block)".to_string())
        );
    }

    #[test]
    fn test_collect() {
        let result = doc_to_string(generate_string_bif("collect:", &["Block".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'collect'(Self, Block)".to_string())
        );
    }

    #[test]
    fn test_select() {
        let result = doc_to_string(generate_string_bif("select:", &["Block".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'select'(Self, Block)".to_string())
        );
    }

    #[test]
    fn test_reject() {
        let result = doc_to_string(generate_string_bif("reject:", &["Block".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'reject'(Self, Block)".to_string())
        );
    }

    #[test]
    fn test_stream() {
        let result = doc_to_string(generate_string_bif("stream", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_stream':'on'(Self)".to_string())
        );
    }

    #[test]
    fn test_with_all_factory() {
        let result = doc_to_string(generate_string_bif("withAll:", &["L".to_string()]));
        assert_eq!(result, Some("call 'beamtalk_string':'join'(L)".to_string()));
    }

    #[test]
    fn test_from_code_point_factory() {
        let result = doc_to_string(generate_string_bif("fromCodePoint:", &["CP".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'from_code_point'(CP)".to_string())
        );
    }

    #[test]
    fn test_from_code_points_factory() {
        let result = doc_to_string(generate_string_bif("fromCodePoints:", &["CPs".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'from_code_points'(CPs)".to_string())
        );
    }

    #[test]
    fn test_from_iolist_factory() {
        let result = doc_to_string(generate_string_bif("fromIolist:", &["IO".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'from_iolist'(IO)".to_string())
        );
    }

    // Edge cases

    #[test]
    fn test_unknown_selector_returns_none() {
        assert_eq!(doc_to_string(generate_string_bif("notAMethod", &[])), None);
    }
}
