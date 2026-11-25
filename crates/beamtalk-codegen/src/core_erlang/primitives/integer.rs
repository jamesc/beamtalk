// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Integer primitive implementations.
//!
//! **DDD Context:** Compilation — Code Generation

use super::{binary_bif, generate_ordering_comparison_bif, param, power_bif, wrap_string_result};
use crate::core_erlang::operators::OrderingKind;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;

/// Integer primitive implementations.
pub(crate) fn generate_integer_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        // Arithmetic — direct Erlang BIF
        "+" => binary_bif("+", params),
        "-" => binary_bif("-", params),
        "*" => binary_bif("*", params),
        "/" => binary_bif("/", params),
        "div:" => binary_bif("div", params),
        "%" => binary_bif("rem", params),
        "**" => power_bif(params),
        // Comparison (ADR 0002: Erlang operators)
        "=:=" | "/=" | "=/=" | "<" | ">" | "<=" | ">=" => {
            generate_ordering_comparison_bif(OrderingKind::Number, selector, params)
        }
        // Conversion
        "asString" | "printString" => Some(wrap_string_result(Document::Str(
            "call 'erlang':'integer_to_binary'(Self)",
        ))),
        "asFloat" => Some(Document::Str("call 'erlang':'float'(Self)")),
        // BT-4544: class-side fallible parse (the `Result`-based counterpart
        // to `String>>asInteger`/`asIntegerRadix:`); accepts a `String` or a
        // bare `Binary`. Cross-module like `String>>stream` is to
        // `beamtalk_stream`. The Erlang helper keeps a plain atom name:
        // colon-named callees have no precedent in `@primitive` codegen
        // (only the `self delegate` path uses them, e.g. `parse:format:`).
        "parse:radix:" => {
            let text = param(params, 0, "_Text");
            let base = param(params, 1, "_Base");
            Some(docvec![
                "call 'beamtalk_string':'parse_integer'(",
                leaf::var(text.to_string()),
                ", ",
                leaf::var(base.to_string()),
                ")"
            ])
        }
        // Bitwise operations
        "bitAnd:" => binary_bif("band", params),
        "bitOr:" => binary_bif("bor", params),
        "bitXor:" => binary_bif("bxor", params),
        "bitShift:" => {
            // Positive N shifts left, negative shifts right
            let p0 = params.first()?.clone();
            Some(docvec![
                "case call 'erlang':'>='(",
                leaf::var(p0.clone()),
                ", 0) of \
                 'true' when 'true' -> call 'erlang':'bsl'(Self, ",
                leaf::var(p0.clone()),
                ") \
                 'false' when 'true' -> call 'erlang':'bsr'(Self, call 'erlang':'-'(0, ",
                leaf::var(p0),
                ")) end",
            ])
        }
        "bitNot" => Some(Document::Str("call 'erlang':'bnot'(Self)")),
        // Character predicates — integers are Unicode codepoints
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
        // Exponential / logarithmic — convert to float, call math module
        "sqrt" => Some(Document::Str(
            "call 'math':'sqrt'(call 'erlang':'float'(Self))",
        )),
        "log" | "ln" => Some(Document::Str(
            "call 'math':'log'(call 'erlang':'float'(Self))",
        )),
        "log2" => Some(Document::Str(
            "call 'math':'log2'(call 'erlang':'float'(Self))",
        )),
        "log10" => Some(Document::Str(
            "call 'math':'log10'(call 'erlang':'float'(Self))",
        )),
        "exp" => Some(Document::Str(
            "call 'math':'exp'(call 'erlang':'float'(Self))",
        )),
        "raisedTo:" => {
            let p0 = params.first()?;
            // Return Integer when exponent is a non-negative integer, Float otherwise.
            Some(docvec![
                "case call 'erlang':'is_integer'(",
                leaf::var(p0.clone()),
                ") of \
                 'true' when 'true' -> \
                   case call 'erlang':'>='(",
                leaf::var(p0.clone()),
                ", 0) of \
                     'true' when 'true' -> \
                       call 'erlang':'round'(call 'math':'pow'(call 'erlang':'float'(Self), call 'erlang':'float'(",
                leaf::var(p0.clone()),
                "))) \
                     'false' when 'true' -> \
                       call 'math':'pow'(call 'erlang':'float'(Self), call 'erlang':'float'(",
                leaf::var(p0.clone()),
                ")) \
                   end \
                 'false' when 'true' -> \
                   call 'math':'pow'(call 'erlang':'float'(Self), call 'erlang':'float'(",
                leaf::var(p0.clone()),
                ")) \
               end",
            ])
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::super::{assert_bif_none_table, assert_bif_table};
    use super::*;

    #[test]
    #[allow(clippy::too_many_lines)] // one contiguous table; splitting hurts readability
    fn generate_integer_bif_table() {
        assert_bif_table(
            generate_integer_bif,
            &[
                // Arithmetic
                (
                    "test_plus",
                    "+",
                    &["Other"],
                    "call 'erlang':'+'(Self, Other)",
                ),
                (
                    "test_minus",
                    "-",
                    &["Other"],
                    "call 'erlang':'-'(Self, Other)",
                ),
                (
                    "test_multiply",
                    "*",
                    &["Other"],
                    "call 'erlang':'*'(Self, Other)",
                ),
                (
                    "test_divide",
                    "/",
                    &["Other"],
                    "call 'erlang':'/'(Self, Other)",
                ),
                (
                    "test_div",
                    "div:",
                    &["Other"],
                    "call 'erlang':'div'(Self, Other)",
                ),
                (
                    "test_modulo",
                    "%",
                    &["Other"],
                    "call 'erlang':'rem'(Self, Other)",
                ),
                (
                    "test_power",
                    "**",
                    &["Other"],
                    "call 'erlang':'round'(call 'math':'pow'(\
                 call 'erlang':'float'(Self), call 'erlang':'float'(Other)))",
                ),
                // Comparisons
                (
                    "test_eq",
                    "=:=",
                    &["Other"],
                    "call 'erlang':'=:='(Self, Other)",
                ),
                (
                    "test_neq",
                    "/=",
                    &["Other"],
                    "call 'erlang':'/='(Self, Other)",
                ),
                (
                    "test_strict_neq",
                    "=/=",
                    &["Other"],
                    "call 'erlang':'=/='(Self, Other)",
                ),
                // `< > <= >=` are guarded (BT-5211):
                // `mod.rs`'s `numeric_ordering_primitives_test_the_argument`.
                // Conversion
                (
                    "test_as_string",
                    "asString",
                    &[],
                    "call 'beamtalk_primitive':'string_from_binary'(call 'erlang':'integer_to_binary'(Self))",
                ),
                (
                    "test_print_string",
                    "printString",
                    &[],
                    "call 'beamtalk_primitive':'string_from_binary'(call 'erlang':'integer_to_binary'(Self))",
                ),
                (
                    "test_as_float",
                    "asFloat",
                    &[],
                    "call 'erlang':'float'(Self)",
                ),
                (
                    "test_parse_radix",
                    "parse:radix:",
                    &["Text", "Base"],
                    "call 'beamtalk_string':'parse_integer'(Text, Base)",
                ),
                // Bitwise operations
                (
                    "test_bit_and",
                    "bitAnd:",
                    &["Other"],
                    "call 'erlang':'band'(Self, Other)",
                ),
                (
                    "test_bit_or",
                    "bitOr:",
                    &["Other"],
                    "call 'erlang':'bor'(Self, Other)",
                ),
                (
                    "test_bit_xor",
                    "bitXor:",
                    &["Other"],
                    "call 'erlang':'bxor'(Self, Other)",
                ),
                ("test_bit_not", "bitNot", &[], "call 'erlang':'bnot'(Self)"),
                (
                    "test_bit_shift_with_param",
                    "bitShift:",
                    &["N"],
                    "case call 'erlang':'>='(N, 0) of \
                 'true' when 'true' -> call 'erlang':'bsl'(Self, N) \
                 'false' when 'true' -> call 'erlang':'bsr'(Self, call 'erlang':'-'(0, N)) end",
                ),
                // Character predicates
                (
                    "test_is_letter",
                    "isLetter",
                    &[],
                    "call 'beamtalk_character':'is_letter'(Self)",
                ),
                (
                    "test_is_digit",
                    "isDigit",
                    &[],
                    "call 'beamtalk_character':'is_digit'(Self)",
                ),
                (
                    "test_is_uppercase",
                    "isUppercase",
                    &[],
                    "call 'beamtalk_character':'is_uppercase'(Self)",
                ),
                (
                    "test_is_lowercase",
                    "isLowercase",
                    &[],
                    "call 'beamtalk_character':'is_lowercase'(Self)",
                ),
                (
                    "test_is_whitespace",
                    "isWhitespace",
                    &[],
                    "call 'beamtalk_character':'is_whitespace'(Self)",
                ),
                // Math functions
                (
                    "test_sqrt",
                    "sqrt",
                    &[],
                    "call 'math':'sqrt'(call 'erlang':'float'(Self))",
                ),
                (
                    "test_log",
                    "log",
                    &[],
                    "call 'math':'log'(call 'erlang':'float'(Self))",
                ),
                (
                    "test_ln",
                    "ln",
                    &[],
                    "call 'math':'log'(call 'erlang':'float'(Self))",
                ),
                (
                    "test_log2",
                    "log2",
                    &[],
                    "call 'math':'log2'(call 'erlang':'float'(Self))",
                ),
                (
                    "test_log10",
                    "log10",
                    &[],
                    "call 'math':'log10'(call 'erlang':'float'(Self))",
                ),
                (
                    "test_exp",
                    "exp",
                    &[],
                    "call 'math':'exp'(call 'erlang':'float'(Self))",
                ),
                (
                    "test_raised_to_with_param",
                    "raisedTo:",
                    &["Exp"],
                    "case call 'erlang':'is_integer'(Exp) of \
                 'true' when 'true' -> \
                   case call 'erlang':'>='(Exp, 0) of \
                     'true' when 'true' -> \
                       call 'erlang':'round'(call 'math':'pow'(call 'erlang':'float'(Self), call 'erlang':'float'(Exp))) \
                     'false' when 'true' -> \
                       call 'math':'pow'(call 'erlang':'float'(Self), call 'erlang':'float'(Exp)) \
                   end \
                 'false' when 'true' -> \
                   call 'math':'pow'(call 'erlang':'float'(Self), call 'erlang':'float'(Exp)) \
               end",
                ),
            ],
        );
    }

    #[test]
    fn generate_integer_bif_none_table() {
        assert_bif_none_table(
            generate_integer_bif,
            &[
                (
                    "test_bit_shift_missing_param_returns_none",
                    "bitShift:",
                    &[],
                ),
                (
                    "test_raised_to_missing_param_returns_none",
                    "raisedTo:",
                    &[],
                ),
                // Edge cases
                ("test_plus_missing_param_returns_none", "+", &[]),
                ("test_power_missing_param_returns_none", "**", &[]),
                ("test_unknown_selector_returns_none", "notAMethod", &[]),
            ],
        );
    }
}
