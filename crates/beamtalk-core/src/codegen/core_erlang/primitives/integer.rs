// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Integer primitive implementations.
//!
//! **DDD Context:** Compilation — Code Generation

use super::super::document::Document;
use super::{binary_bif, generate_comparison_bif, power_bif};
use crate::docvec;

/// Integer primitive implementations.
pub(crate) fn generate_integer_bif(
    selector: &str,
    params: &[String],
) -> Option<Document<'static>> {
    match selector {
        // Arithmetic — direct Erlang BIF
        "+" => binary_bif("+", params),
        "-" => binary_bif("-", params),
        "*" => binary_bif("*", params),
        "/" => binary_bif("/", params),
        "%" => binary_bif("rem", params),
        "**" => power_bif(params),
        // Comparison (ADR 0002: Erlang operators)
        "=:=" | "/=" | "=/=" | "<" | ">" | "<=" | ">=" => {
            generate_comparison_bif(selector, params)
        }
        // Conversion
        "asString" | "printString" => {
            Some(Document::Str("call 'erlang':'integer_to_binary'(Self)"))
        }
        "asFloat" => Some(Document::Str("call 'erlang':'float'(Self)")),
        // Bitwise operations
        "bitAnd:" => binary_bif("band", params),
        "bitOr:" => binary_bif("bor", params),
        "bitXor:" => binary_bif("bxor", params),
        "bitShift:" => {
            // Positive N shifts left, negative shifts right
            let p0 = params.first()?.clone();
            Some(docvec![
                "case call 'erlang':'>='(",
                p0.clone(),
                ", 0) of \
                 'true' when 'true' -> call 'erlang':'bsl'(Self, ",
                p0.clone(),
                ") \
                 'false' when 'true' -> call 'erlang':'bsr'(Self, call 'erlang':'-'(0, ",
                p0,
                ")) end",
            ])
        }
        "bitNot" => Some(Document::Str("call 'erlang':'bnot'(Self)")),
        // Character predicates — integers are Unicode codepoints (BT-339)
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
        _ => None,
    }
}
