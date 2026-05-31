// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Integer primitive implementations.
//!
//! **DDD Context:** Compilation — Code Generation

use super::super::document::Document;
use super::super::document::leaf;
use super::{binary_bif, generate_comparison_bif, power_bif};
use crate::docvec;

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
        "=:=" | "/=" | "=/=" | "<" | ">" | "<=" | ">=" => generate_comparison_bif(selector, params),
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
    use super::super::doc_to_string;
    use super::*;

    // Arithmetic

    #[test]
    fn test_plus() {
        let result = doc_to_string(generate_integer_bif("+", &["Other".to_string()]));
        assert_eq!(result, Some("call 'erlang':'+'(Self, Other)".to_string()));
    }

    #[test]
    fn test_minus() {
        let result = doc_to_string(generate_integer_bif("-", &["Other".to_string()]));
        assert_eq!(result, Some("call 'erlang':'-'(Self, Other)".to_string()));
    }

    #[test]
    fn test_multiply() {
        let result = doc_to_string(generate_integer_bif("*", &["Other".to_string()]));
        assert_eq!(result, Some("call 'erlang':'*'(Self, Other)".to_string()));
    }

    #[test]
    fn test_divide() {
        let result = doc_to_string(generate_integer_bif("/", &["Other".to_string()]));
        assert_eq!(result, Some("call 'erlang':'/'(Self, Other)".to_string()));
    }

    #[test]
    fn test_div() {
        let result = doc_to_string(generate_integer_bif("div:", &["Other".to_string()]));
        assert_eq!(result, Some("call 'erlang':'div'(Self, Other)".to_string()));
    }

    #[test]
    fn test_modulo() {
        let result = doc_to_string(generate_integer_bif("%", &["Other".to_string()]));
        assert_eq!(result, Some("call 'erlang':'rem'(Self, Other)".to_string()));
    }

    #[test]
    fn test_power() {
        let result = doc_to_string(generate_integer_bif("**", &["Other".to_string()]));
        assert_eq!(
            result,
            Some(
                "call 'erlang':'round'(call 'math':'pow'(\
                 call 'erlang':'float'(Self), call 'erlang':'float'(Other)))"
                    .to_string()
            )
        );
    }

    // Comparisons

    #[test]
    fn test_eq() {
        let result = doc_to_string(generate_integer_bif("=:=", &["Other".to_string()]));
        assert_eq!(result, Some("call 'erlang':'=:='(Self, Other)".to_string()));
    }

    #[test]
    fn test_neq() {
        let result = doc_to_string(generate_integer_bif("/=", &["Other".to_string()]));
        assert_eq!(result, Some("call 'erlang':'/='(Self, Other)".to_string()));
    }

    #[test]
    fn test_strict_neq() {
        let result = doc_to_string(generate_integer_bif("=/=", &["Other".to_string()]));
        assert_eq!(result, Some("call 'erlang':'=/='(Self, Other)".to_string()));
    }

    #[test]
    fn test_lt() {
        let result = doc_to_string(generate_integer_bif("<", &["Other".to_string()]));
        assert_eq!(result, Some("call 'erlang':'<'(Self, Other)".to_string()));
    }

    #[test]
    fn test_gt() {
        let result = doc_to_string(generate_integer_bif(">", &["Other".to_string()]));
        assert_eq!(result, Some("call 'erlang':'>'(Self, Other)".to_string()));
    }

    #[test]
    fn test_le() {
        let result = doc_to_string(generate_integer_bif("<=", &["Other".to_string()]));
        assert_eq!(result, Some("call 'erlang':'=<'(Self, Other)".to_string()));
    }

    #[test]
    fn test_ge() {
        let result = doc_to_string(generate_integer_bif(">=", &["Other".to_string()]));
        assert_eq!(result, Some("call 'erlang':'>='(Self, Other)".to_string()));
    }

    // Conversion

    #[test]
    fn test_as_string() {
        let result = doc_to_string(generate_integer_bif("asString", &[]));
        assert_eq!(
            result,
            Some("call 'erlang':'integer_to_binary'(Self)".to_string())
        );
    }

    #[test]
    fn test_print_string() {
        let result = doc_to_string(generate_integer_bif("printString", &[]));
        assert_eq!(
            result,
            Some("call 'erlang':'integer_to_binary'(Self)".to_string())
        );
    }

    #[test]
    fn test_as_float() {
        let result = doc_to_string(generate_integer_bif("asFloat", &[]));
        assert_eq!(result, Some("call 'erlang':'float'(Self)".to_string()));
    }

    // Bitwise operations

    #[test]
    fn test_bit_and() {
        let result = doc_to_string(generate_integer_bif("bitAnd:", &["Other".to_string()]));
        assert_eq!(
            result,
            Some("call 'erlang':'band'(Self, Other)".to_string())
        );
    }

    #[test]
    fn test_bit_or() {
        let result = doc_to_string(generate_integer_bif("bitOr:", &["Other".to_string()]));
        assert_eq!(result, Some("call 'erlang':'bor'(Self, Other)".to_string()));
    }

    #[test]
    fn test_bit_xor() {
        let result = doc_to_string(generate_integer_bif("bitXor:", &["Other".to_string()]));
        assert_eq!(
            result,
            Some("call 'erlang':'bxor'(Self, Other)".to_string())
        );
    }

    #[test]
    fn test_bit_not() {
        let result = doc_to_string(generate_integer_bif("bitNot", &[]));
        assert_eq!(result, Some("call 'erlang':'bnot'(Self)".to_string()));
    }

    #[test]
    fn test_bit_shift_with_param() {
        let result = doc_to_string(generate_integer_bif("bitShift:", &["N".to_string()]));
        assert_eq!(
            result,
            Some(
                "case call 'erlang':'>='(N, 0) of \
                 'true' when 'true' -> call 'erlang':'bsl'(Self, N) \
                 'false' when 'true' -> call 'erlang':'bsr'(Self, call 'erlang':'-'(0, N)) end"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_bit_shift_missing_param_returns_none() {
        assert_eq!(doc_to_string(generate_integer_bif("bitShift:", &[])), None);
    }

    // Character predicates (BT-339)

    #[test]
    fn test_is_letter() {
        let result = doc_to_string(generate_integer_bif("isLetter", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'is_letter'(Self)".to_string())
        );
    }

    #[test]
    fn test_is_digit() {
        let result = doc_to_string(generate_integer_bif("isDigit", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'is_digit'(Self)".to_string())
        );
    }

    #[test]
    fn test_is_uppercase() {
        let result = doc_to_string(generate_integer_bif("isUppercase", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'is_uppercase'(Self)".to_string())
        );
    }

    #[test]
    fn test_is_lowercase() {
        let result = doc_to_string(generate_integer_bif("isLowercase", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'is_lowercase'(Self)".to_string())
        );
    }

    #[test]
    fn test_is_whitespace() {
        let result = doc_to_string(generate_integer_bif("isWhitespace", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'is_whitespace'(Self)".to_string())
        );
    }

    // Math functions

    #[test]
    fn test_sqrt() {
        let result = doc_to_string(generate_integer_bif("sqrt", &[]));
        assert_eq!(
            result,
            Some("call 'math':'sqrt'(call 'erlang':'float'(Self))".to_string())
        );
    }

    #[test]
    fn test_log() {
        let result = doc_to_string(generate_integer_bif("log", &[]));
        assert_eq!(
            result,
            Some("call 'math':'log'(call 'erlang':'float'(Self))".to_string())
        );
    }

    #[test]
    fn test_ln() {
        let result = doc_to_string(generate_integer_bif("ln", &[]));
        assert_eq!(
            result,
            Some("call 'math':'log'(call 'erlang':'float'(Self))".to_string())
        );
    }

    #[test]
    fn test_log2() {
        let result = doc_to_string(generate_integer_bif("log2", &[]));
        assert_eq!(
            result,
            Some("call 'math':'log2'(call 'erlang':'float'(Self))".to_string())
        );
    }

    #[test]
    fn test_log10() {
        let result = doc_to_string(generate_integer_bif("log10", &[]));
        assert_eq!(
            result,
            Some("call 'math':'log10'(call 'erlang':'float'(Self))".to_string())
        );
    }

    #[test]
    fn test_exp() {
        let result = doc_to_string(generate_integer_bif("exp", &[]));
        assert_eq!(
            result,
            Some("call 'math':'exp'(call 'erlang':'float'(Self))".to_string())
        );
    }

    #[test]
    fn test_raised_to_with_param() {
        let result = doc_to_string(generate_integer_bif("raisedTo:", &["Exp".to_string()]));
        assert_eq!(
            result,
            Some(
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
               end"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_raised_to_missing_param_returns_none() {
        assert_eq!(doc_to_string(generate_integer_bif("raisedTo:", &[])), None);
    }

    // Edge cases

    #[test]
    fn test_plus_missing_param_returns_none() {
        assert_eq!(doc_to_string(generate_integer_bif("+", &[])), None);
    }

    #[test]
    fn test_power_missing_param_returns_none() {
        assert_eq!(doc_to_string(generate_integer_bif("**", &[])), None);
    }

    #[test]
    fn test_unknown_selector_returns_none() {
        assert_eq!(doc_to_string(generate_integer_bif("notAMethod", &[])), None);
    }
}
