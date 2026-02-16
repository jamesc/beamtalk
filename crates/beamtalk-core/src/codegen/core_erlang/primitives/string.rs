// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! String primitive implementations.
//!
//! **DDD Context:** Compilation — Code Generation

use super::super::document::Document;
use super::generate_comparison_bif;
use crate::docvec;

/// String primitive implementations.
pub(crate) fn generate_string_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        // Comparison (ADR 0002: Erlang operators)
        "=:=" | "/=" | "=/=" | "<" | ">" | "<=" | ">=" => generate_comparison_bif(selector, params),
        // Concatenation, length, access, case, whitespace, reverse
        "++" | "length" | "at:" | "uppercase" | "lowercase" | "capitalize" | "trim"
        | "trimLeft" | "trimRight" | "reverse" => generate_string_transform_bif(selector, params),
        // Search, splitting, replace, substring, padding
        "includes:" | "startsWith:" | "endsWith:" | "indexOf:" | "split:" | "splitOn:"
        | "repeat:" | "lines" | "words" | "replaceAll:with:" | "replaceFirst:with:" | "take:"
        | "drop:" | "padLeft:" | "padRight:" | "padLeft:with:" => {
            generate_string_search_bif(selector, params)
        }
        // Testing, conversion, iteration, streaming
        _ => generate_string_misc_bif(selector, params),
    }
}

fn generate_string_transform_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    let p0 = params.first().map_or("_Arg0", String::as_str);
    match selector {
        "++" => Some(docvec![
            "call 'erlang':'iolist_to_binary'([Self, ",
            p0.to_string(),
            "])"
        ]),
        "length" => Some(Document::Str("call 'string':'length'(Self)")),
        "at:" => Some(docvec![
            "call 'beamtalk_string_ops':'at'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "uppercase" => Some(Document::Str(
            "call 'unicode':'characters_to_binary'(call 'string':'uppercase'(Self))",
        )),
        "lowercase" => Some(Document::Str(
            "call 'unicode':'characters_to_binary'(call 'string':'lowercase'(Self))",
        )),
        "capitalize" => Some(Document::Str(
            "call 'beamtalk_string_ops':'capitalize'(Self)",
        )),
        "trim" => Some(Document::Str(
            "call 'unicode':'characters_to_binary'(call 'string':'trim'(Self, 'both'))",
        )),
        "trimLeft" => Some(Document::Str(
            "call 'unicode':'characters_to_binary'(call 'string':'trim'(Self, 'leading'))",
        )),
        "trimRight" => Some(Document::Str(
            "call 'unicode':'characters_to_binary'(call 'string':'trim'(Self, 'trailing'))",
        )),
        "reverse" => Some(Document::Str("call 'beamtalk_string_ops':'reverse'(Self)")),
        _ => None,
    }
}

fn generate_string_search_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    let p0 = params.first().map_or("_Arg0", String::as_str);
    match selector {
        "includes:" => Some(docvec![
            "call 'beamtalk_string_ops':'includes'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "startsWith:" => Some(docvec![
            "call 'beamtalk_string_ops':'starts_with'(Self, ",
            p0.to_string(),
            ")",
        ]),
        "endsWith:" => Some(docvec![
            "call 'beamtalk_string_ops':'ends_with'(Self, ",
            p0.to_string(),
            ")",
        ]),
        "indexOf:" => Some(docvec![
            "call 'beamtalk_string_ops':'index_of'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "split:" => Some(docvec![
            "call 'binary':'split'(Self, ",
            p0.to_string(),
            ", ['global'])"
        ]),
        "splitOn:" => Some(docvec![
            "call 'beamtalk_string_ops':'split_on'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "repeat:" => Some(docvec![
            "call 'beamtalk_string_ops':'repeat'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "lines" => Some(Document::Str("call 'beamtalk_string_ops':'lines'(Self)")),
        "words" => Some(Document::Str("call 'beamtalk_string_ops':'words'(Self)")),
        "replaceAll:with:" => {
            let p1 = params.get(1).map_or("_Arg1", String::as_str);
            Some(docvec![
                "call 'binary':'replace'(Self, ",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ", ['global'])",
            ])
        }
        "replaceFirst:with:" => {
            let p1 = params.get(1).map_or("_Arg1", String::as_str);
            Some(docvec![
                "call 'binary':'replace'(Self, ",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ", [])"
            ])
        }
        "take:" => Some(docvec![
            "call 'beamtalk_string_ops':'take'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "drop:" => Some(docvec![
            "call 'beamtalk_string_ops':'drop'(Self, ",
            p0.to_string(),
            ")"
        ]),
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
        "padLeft:with:" => {
            let p1 = params.get(1).map_or("_Arg1", String::as_str);
            Some(docvec![
                "call 'unicode':'characters_to_binary'(call 'string':'pad'(Self, ",
                p0.to_string(),
                ", 'leading', ",
                p1.to_string(),
                "))",
            ])
        }
        _ => None,
    }
}

fn generate_string_misc_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    let p0 = params.first().map_or("_Arg0", String::as_str);
    match selector {
        "isBlank" => Some(Document::Str("call 'beamtalk_string_ops':'is_blank'(Self)")),
        "isDigit" => Some(Document::Str("call 'beamtalk_string_ops':'is_digit'(Self)")),
        "isAlpha" => Some(Document::Str("call 'beamtalk_string_ops':'is_alpha'(Self)")),
        "asInteger" => Some(Document::Str("call 'erlang':'binary_to_integer'(Self)")),
        "asFloat" => Some(Document::Str("call 'erlang':'binary_to_float'(Self)")),
        "asAtom" => Some(Document::Str(
            "call 'erlang':'binary_to_existing_atom'(Self, 'utf8')",
        )),
        "asList" => Some(Document::Str("call 'beamtalk_string_ops':'as_list'(Self)")),
        "each:" => Some(docvec![
            "call 'beamtalk_string_ops':'each'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "collect:" => Some(docvec![
            "call 'beamtalk_string_ops':'collect'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "select:" => Some(docvec![
            "call 'beamtalk_string_ops':'select'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "stream" => Some(Document::Str("call 'beamtalk_stream':'on'(Self)")),
        _ => None,
    }
}
