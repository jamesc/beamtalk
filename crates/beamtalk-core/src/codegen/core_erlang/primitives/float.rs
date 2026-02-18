// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Float primitive implementations.
//!
//! **DDD Context:** Compilation — Code Generation

use super::super::document::Document;
use super::{binary_bif, generate_comparison_bif};
use crate::docvec;

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
        // Trigonometric — Erlang math module
        "sin" => Some(Document::Str("call 'math':'sin'(Self)")),
        "cos" => Some(Document::Str("call 'math':'cos'(Self)")),
        "tan" => Some(Document::Str("call 'math':'tan'(Self)")),
        "asin" => Some(Document::Str("call 'math':'asin'(Self)")),
        "acos" => Some(Document::Str("call 'math':'acos'(Self)")),
        "atan" => Some(Document::Str("call 'math':'atan'(Self)")),
        "atan2:" => {
            let p0 = params.first()?;
            Some(docvec![
                "call 'math':'atan2'(Self, call 'erlang':'float'(",
                p0.clone(),
                "))",
            ])
        }
        // Exponential / logarithmic — Erlang math module
        "sqrt" => Some(Document::Str("call 'math':'sqrt'(Self)")),
        "log" | "ln" => Some(Document::Str("call 'math':'log'(Self)")),
        "log2" => Some(Document::Str("call 'math':'log2'(Self)")),
        "log10" => Some(Document::Str("call 'math':'log10'(Self)")),
        "exp" => Some(Document::Str("call 'math':'exp'(Self)")),
        "raisedTo:" => {
            let p0 = params.first()?;
            Some(docvec![
                "call 'math':'pow'(Self, call 'erlang':'float'(",
                p0.clone(),
                "))",
            ])
        }
        // Class-side constants
        "pi" => Some(Document::Str("call 'math':'pi'()")),
        "e" => Some(Document::Str("call 'math':'exp'(1.0)")),
        _ => None,
    }
}
