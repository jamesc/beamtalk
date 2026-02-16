// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Character primitive implementations (BT-339).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Characters are integers (Unicode codepoints) at the BEAM level.
//! Simple operations use direct BIFs; predicates delegate to `beamtalk_character`.

use super::super::document::Document;
use super::generate_comparison_bif;
use crate::docvec;

/// Character primitive implementations (BT-339).
pub(crate) fn generate_character_bif(
    selector: &str,
    params: &[String],
) -> Option<Document<'static>> {
    let p0 = params.first().map_or("_Arg0", String::as_str);
    match selector {
        // Comparison — direct integer comparison (ADR 0002: Erlang operators)
        "=:=" | "/=" | "=/=" | "<" | ">" | "<=" | ">=" => {
            generate_comparison_bif(selector, params)
        }
        // Conversion
        "asInteger" => Some(Document::Str("Self")),
        "asString" => Some(Document::Str("call 'beamtalk_character':'as_string'(Self)")),
        // Display
        "printString" => Some(Document::Str(
            "call 'beamtalk_character':'print_string'(Self)",
        )),
        // Identity
        "hash" => Some(Document::Str("call 'erlang':'phash2'(Self)")),
        // Predicates — delegate to runtime module
        "isLetter" => Some(Document::Str("call 'beamtalk_character':'is_letter'(Self)")),
        "isDigit" => Some(Document::Str("call 'beamtalk_character':'is_digit'(Self)")),
        "isUppercase" => Some(Document::Str(
            "call 'beamtalk_character':'is_uppercase'(Self)",
        )),
        "isLowercase" => Some(Document::Str(
            "call 'beamtalk_character':'is_lowercase'(Self)",
        )),
        "isWhitespace" => Some(Document::Str(
            "call 'beamtalk_character':'is_whitespace'(Self)",
        )),
        // Case conversion
        "uppercase" => Some(Document::Str(
            "call 'beamtalk_character':'to_uppercase'(Self)",
        )),
        "lowercase" => Some(Document::Str(
            "call 'beamtalk_character':'to_lowercase'(Self)",
        )),
        // Factory class method
        "value:" => Some(docvec![
            "call 'beamtalk_character':'value'(",
            p0.to_string(),
            ")"
        ]),
        _ => None,
    }
}
