// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Float primitive implementations.
//!
//! **DDD Context:** Compilation — Code Generation

use super::super::document::Document;
use super::{binary_bif, generate_comparison_bif};

/// Float primitive implementations.
pub(crate) fn generate_float_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        // Arithmetic — direct Erlang BIF
        "+" => binary_bif("+", params),
        "-" => binary_bif("-", params),
        "*" => binary_bif("*", params),
        "/" => binary_bif("/", params),
        // Comparison (ADR 0002: Erlang operators)
        "=:=" | "/=" | "=/=" | "<" | ">" | "<=" | ">=" => generate_comparison_bif(selector, params),
        // Rounding
        "rounded" => Some(Document::Str("call 'erlang':'round'(Self)")),
        "ceiling" => Some(Document::Str("call 'erlang':'ceil'(Self)")),
        "floor" => Some(Document::Str("call 'erlang':'floor'(Self)")),
        "truncated" | "asInteger" => Some(Document::Str("call 'erlang':'trunc'(Self)")),
        // Conversion
        "asString" | "printString" => Some(Document::Str(
            "call 'erlang':'float_to_binary'(Self, ['short'])",
        )),
        _ => None,
    }
}
