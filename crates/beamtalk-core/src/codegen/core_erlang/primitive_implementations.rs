// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Primitive implementation mapping for stdlib codegen.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Maps `@primitive 'selector'` annotations to direct Erlang BIF calls.
//! When a stdlib `.bt` file declares `+ other => @primitive '+'`, this module
//! provides the actual Core Erlang expression to emit instead of delegating
//! through a hand-written dispatch module.
//!
//! This is part of BT-340: making compiled stdlib modules self-sufficient
//! so hand-written Erlang dispatch modules can be deleted.

use super::document::Document;
use crate::docvec;

/// Generates Core Erlang for a selector-based primitive implementation.
///
/// Returns `Some(code)` if a direct implementation was emitted, `None` if
/// the selector has no known BIF mapping (e.g., unimplemented methods).
///
/// When `None` is returned, the caller should fall back to generating a
/// `does_not_understand` error call.
///
/// # Arguments
///
/// * `class_name` - The class context (e.g., "Integer", "String")
/// * `selector` - The primitive selector (e.g., "+", "length")
/// * `params` - The method parameters (excluding Self)
pub fn generate_primitive_bif(
    class_name: &str,
    selector: &str,
    params: &[String],
) -> Option<Document<'static>> {
    match class_name {
        "Integer" => generate_integer_bif(selector, params),
        "Float" => generate_float_bif(selector, params),
        "String" => generate_string_bif(selector, params),
        "Block" => generate_block_bif(selector, params),
        "File" => generate_file_bif(selector, params),
        "Exception" => generate_exception_bif(selector, params),
        "Symbol" => generate_symbol_bif(selector, params),
        "Tuple" => generate_tuple_bif(selector, params),
        "List" => generate_list_bif(selector, params),
        "Dictionary" => generate_dictionary_bif(selector, params),
        "Object" => generate_object_bif(selector, params),
        "Association" => generate_association_bif(selector, params),
        "Set" => generate_set_bif(selector, params),
        "CompiledMethod" => generate_compiled_method_bif(selector, params),
        "Character" => generate_character_bif(selector, params),
        "TestCase" => generate_test_case_bif(selector, params),
        "Stream" => generate_stream_bif(selector, params),
        "Collection" => generate_collection_bif(selector, params),
        "StackFrame" => generate_stack_frame_bif(selector, params),
        _ => None,
    }
}

/// Integer primitive implementations.
fn generate_integer_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        // Arithmetic — direct Erlang BIF
        "+" => binary_bif("+", params),
        "-" => binary_bif("-", params),
        "*" => binary_bif("*", params),
        "/" => binary_bif("/", params),
        "%" => binary_bif("rem", params),
        "**" => power_bif(params),
        // Comparison (ADR 0002: Erlang operators)
        "=:=" => binary_bif("=:=", params),
        "/=" => binary_bif("/=", params),
        "=/=" => binary_bif("=/=", params),
        "<" => binary_bif("<", params),
        ">" => binary_bif(">", params),
        "<=" => binary_bif("=<", params),
        ">=" => binary_bif(">=", params),
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

/// Float primitive implementations.
fn generate_float_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        // Arithmetic — direct Erlang BIF
        "+" => binary_bif("+", params),
        "-" => binary_bif("-", params),
        "*" => binary_bif("*", params),
        "/" => binary_bif("/", params),
        // Comparison (ADR 0002: Erlang operators)
        "=:=" => binary_bif("=:=", params),
        "/=" => binary_bif("/=", params),
        "=/=" => binary_bif("=/=", params),
        "<" => binary_bif("<", params),
        ">" => binary_bif(">", params),
        "<=" => binary_bif("=<", params),
        ">=" => binary_bif(">=", params),
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

/// String primitive implementations.
fn generate_string_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        // Comparison (ADR 0002: Erlang operators)
        "=:=" | "/=" | "=/=" | "<" | ">" | "<=" | ">=" => {
            generate_string_comparison_bif(selector, params)
        }
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

fn generate_string_comparison_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        "=:=" => binary_bif("=:=", params),
        "/=" => binary_bif("/=", params),
        "=/=" => binary_bif("=/=", params),
        "<" => binary_bif("<", params),
        ">" => binary_bif(">", params),
        "<=" => binary_bif("=<", params),
        ">=" => binary_bif(">=", params),
        _ => None,
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

/// Block primitive implementations.
fn generate_block_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        "arity" => {
            // erlang:fun_info(Self, arity) returns {arity, N}
            Some(Document::Str(
                "let <ArityTuple> = call 'erlang':'fun_info'(Self, 'arity') in \
                 call 'erlang':'element'(2, ArityTuple)",
            ))
        }
        "valueWithArguments:" => {
            let p0 = params.first().map_or("_Args", String::as_str);
            Some(docvec!["call 'erlang':'apply'(Self, ", p0.to_string(), ")"])
        }
        // on:do: and ensure: are structural intrinsics handled at the call site
        // (see control_flow/exception_handling.rs), not here.
        _ => None,
    }
}

/// File primitive implementations (BT-336, BT-513).
///
/// File class methods delegate directly to `beamtalk_file` runtime module.
/// These are class-level methods (no Self parameter needed).
fn generate_file_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    let p0 = params.first().map_or("_Arg0", String::as_str);
    match selector {
        "exists:" => Some(docvec![
            "call 'beamtalk_file':'exists:'(",
            p0.to_string(),
            ")"
        ]),
        "readAll:" => Some(docvec![
            "call 'beamtalk_file':'readAll:'(",
            p0.to_string(),
            ")"
        ]),
        "writeAll:contents:" => {
            let p1 = params.get(1).map_or("_Arg1", String::as_str);
            Some(docvec![
                "call 'beamtalk_file':'writeAll:contents:'(",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")",
            ])
        }
        "lines:" => Some(docvec![
            "call 'beamtalk_file':'lines:'(",
            p0.to_string(),
            ")"
        ]),
        "open:do:" => {
            let p1 = params.get(1).map_or("_Arg1", String::as_str);
            Some(docvec![
                "call 'beamtalk_file':'open:do:'(",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")"
            ])
        }
        _ => None,
    }
}

/// Exception primitive implementations (BT-338).
///
/// Exception field access delegates to `beamtalk_exception_handler` runtime module.
/// This avoids naming conflict: compiled Exception.bt produces `beamtalk_exception`,
/// while the handler module provides the actual implementation.
fn generate_exception_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        "message" => Some(Document::Str(
            "call 'beamtalk_exception_handler':'dispatch'('message', [], Self)",
        )),
        "hint" => Some(Document::Str(
            "call 'beamtalk_exception_handler':'dispatch'('hint', [], Self)",
        )),
        "kind" => Some(Document::Str(
            "call 'beamtalk_exception_handler':'dispatch'('kind', [], Self)",
        )),
        "selector" => Some(Document::Str(
            "call 'beamtalk_exception_handler':'dispatch'('selector', [], Self)",
        )),
        "errorClass" => Some(Document::Str(
            "call 'beamtalk_exception_handler':'dispatch'('errorClass', [], Self)",
        )),
        "printString" => Some(Document::Str(
            "call 'beamtalk_exception_handler':'dispatch'('printString', [], Self)",
        )),
        "signal" => Some(Document::Str(
            "call 'beamtalk_exception_handler':'dispatch'('signal', [], Self)",
        )),
        "signal:" => {
            let p0 = params.first().map_or("_Msg", String::as_str);
            Some(docvec![
                "call 'beamtalk_exception_handler':'signal_message'(",
                p0.to_string(),
                ")",
            ])
        }
        "stackTrace" => Some(Document::Str(
            "call 'beamtalk_exception_handler':'dispatch'('stackTrace', [], Self)",
        )),
        _ => None,
    }
}

/// `StackFrame` primitive implementations (BT-107).
///
/// `StackFrame` field access delegates to `beamtalk_stack_frame` runtime module.
fn generate_stack_frame_bif(selector: &str, _params: &[String]) -> Option<Document<'static>> {
    match selector {
        "method" | "receiverClass" | "arguments" | "sourceLocation" | "moduleName" | "line"
        | "file" | "printString" => Some(Document::String(format!(
            "call 'beamtalk_stack_frame':'dispatch'('{selector}', [], Self)"
        ))),
        _ => None,
    }
}

/// Symbol primitive implementations (BT-273).
///
/// Symbols are Erlang atoms — interned, immutable identifiers.
fn generate_symbol_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        // Comparison (ADR 0002: Erlang operators)
        "=:=" => binary_bif("=:=", params),
        "/=" => binary_bif("/=", params),
        "=/=" => binary_bif("=/=", params),
        // Conversion
        "asString" => Some(Document::Str(
            "call 'erlang':'atom_to_binary'(Self, 'utf8')",
        )),
        "asAtom" => {
            // Identity — symbols are already atoms
            Some(Document::Str("Self"))
        }
        // Display — delegate to runtime primitive for consistent formatting
        "printString" => Some(Document::Str(
            "call 'beamtalk_primitive':'print_string'(Self)",
        )),
        // Identity
        "hash" => Some(Document::Str("call 'erlang':'phash2'(Self)")),
        _ => None,
    }
}

/// Character primitive implementations (BT-339).
///
/// Characters are integers (Unicode codepoints) at the BEAM level.
/// Simple operations use direct BIFs; predicates delegate to `beamtalk_character`.
fn generate_character_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    let p0 = params.first().map_or("_Arg0", String::as_str);
    match selector {
        // Comparison — direct integer comparison (ADR 0002: Erlang operators)
        "=:=" => binary_bif("=:=", params),
        "/=" => binary_bif("/=", params),
        "=/=" => binary_bif("=/=", params),
        "<" => binary_bif("<", params),
        ">" => binary_bif(">", params),
        "<=" => binary_bif("=<", params),
        ">=" => binary_bif(">=", params),
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

/// Tuple primitive implementations (BT-417).
///
/// Tuples are Erlang tuples — immutable fixed-size collections, particularly
/// useful for Erlang interop with {ok, Value} and {error, Reason} patterns.
fn generate_tuple_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        "size" => Some(Document::Str("call 'erlang':'tuple_size'(Self)")),
        "at:" => {
            let p0 = params.first()?;
            Some(docvec![
                "call 'beamtalk_tuple_ops':'at'(Self, ",
                p0.clone(),
                ")"
            ])
        }
        "isOk" => Some(Document::Str(
            "case Self of <{'ok', _Value}> when 'true' -> 'true' <_> when 'true' -> 'false' end",
        )),
        "isError" => Some(Document::Str(
            "case Self of <{'error', _Reason}> when 'true' -> 'true' <_> when 'true' -> 'false' end",
        )),
        "unwrap" => Some(Document::Str("call 'beamtalk_tuple_ops':'unwrap'(Self)")),
        "unwrapOr:" => {
            let p0 = params.first()?;
            Some(docvec![
                "call 'beamtalk_tuple_ops':'unwrap_or'(Self, ",
                p0.clone(),
                ")",
            ])
        }
        "unwrapOrElse:" => {
            let p0 = params.first()?;
            Some(docvec![
                "call 'beamtalk_tuple_ops':'unwrap_or_else'(Self, ",
                p0.clone(),
                ")",
            ])
        }
        "asString" => Some(Document::Str("call 'beamtalk_tuple_ops':'as_string'(Self)")),
        "do:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(docvec![
                "call 'beamtalk_tuple_ops':'do'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        _ => None,
    }
}

/// List primitive implementations (BT-419).
///
/// Lists are Erlang linked lists — fast prepend, sequential access.
/// Complex operations delegate to `beamtalk_list_ops` helper module.
fn generate_list_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        // Access and ordering
        "size" | "isEmpty" | "first" | "rest" | "last" | "at:" | "includes:" | "sort" | "sort:"
        | "reversed" | "unique" => generate_list_access_bif(selector, params),
        // Search, iteration, functional
        "detect:" | "detect:ifNone:" | "do:" | "collect:" | "select:" | "reject:"
        | "inject:into:" | "take:" | "drop:" | "flatten" | "flatMap:" | "count:"
        | "anySatisfy:" | "allSatisfy:" => generate_list_iteration_bif(selector, params),
        // Advanced, mutation, concatenation, subsequence, misc
        _ => generate_list_misc_bif(selector, params),
    }
}

fn generate_list_access_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        "size" => Some(Document::Str("call 'erlang':'length'(Self)")),
        "isEmpty" => Some(Document::Str("call 'erlang':'=:='(Self, [])")),
        "first" => {
            let hint = core_erlang_binary_string("Cannot get first element of empty list");
            Some(docvec![
                "case Self of \
                 <[H|_T]> when 'true' -> H \
                 <[]> when 'true' -> \
                   let Error0 = call 'beamtalk_error':'new'('does_not_understand', 'List') in \
                   let Error1 = call 'beamtalk_error':'with_selector'(Error0, 'first') in \
                   let Error2 = call 'beamtalk_error':'with_hint'(Error1, ",
                hint,
                ") in \
                   call 'beamtalk_error':'raise'(Error2) \
                 end",
            ])
        }
        "rest" => Some(Document::Str(
            "case Self of \
             <[_H|T]> when 'true' -> T \
             <[]> when 'true' -> [] \
             end",
        )),
        "last" => {
            let hint = core_erlang_binary_string("Cannot get last element of empty list");
            Some(docvec![
                "case Self of \
                 <[]> when 'true' -> \
                   let Error0 = call 'beamtalk_error':'new'('does_not_understand', 'List') in \
                   let Error1 = call 'beamtalk_error':'with_selector'(Error0, 'last') in \
                   let Error2 = call 'beamtalk_error':'with_hint'(Error1, ",
                hint,
                ") in \
                   call 'beamtalk_error':'raise'(Error2) \
                 <_> when 'true' -> \
                   call 'lists':'last'(Self) \
                 end",
            ])
        }
        "at:" => {
            let p0 = params.first().map_or("_N", String::as_str);
            Some(docvec![
                "call 'beamtalk_list_ops':'at'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "includes:" => {
            let p0 = params.first().map_or("_Item", String::as_str);
            Some(docvec!["call 'lists':'member'(", p0.to_string(), ", Self)"])
        }
        "sort" => Some(Document::Str("call 'lists':'sort'(Self)")),
        "sort:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(docvec![
                "call 'beamtalk_list_ops':'sort_with'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "reversed" => Some(Document::Str("call 'lists':'reverse'(Self)")),
        "unique" => Some(Document::Str("call 'lists':'usort'(Self)")),
        _ => None,
    }
}

fn generate_list_iteration_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    let p0 = params.first().map_or("_Block", String::as_str);
    match selector {
        "detect:" => Some(docvec![
            "call 'beamtalk_list_ops':'detect'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "detect:ifNone:" => {
            let p1 = params.get(1).map_or("_Default", String::as_str);
            Some(docvec![
                "call 'beamtalk_list_ops':'detect_if_none'(Self, ",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")",
            ])
        }
        "do:" => Some(docvec![
            "call 'beamtalk_list_ops':'do'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "collect:" => Some(docvec!["call 'lists':'map'(", p0.to_string(), ", Self)"]),
        "select:" => Some(docvec!["call 'lists':'filter'(", p0.to_string(), ", Self)"]),
        "reject:" => Some(docvec![
            "call 'beamtalk_list_ops':'reject'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "inject:into:" => {
            let p1 = params.get(1).map_or("_Block", String::as_str);
            Some(docvec![
                "call 'lists':'foldl'(",
                p1.to_string(),
                ", ",
                p0.to_string(),
                ", Self)"
            ])
        }
        "take:" => Some(docvec![
            "call 'beamtalk_list_ops':'take'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "drop:" => Some(docvec![
            "call 'beamtalk_list_ops':'drop'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "flatten" => Some(Document::Str("call 'lists':'flatten'(Self)")),
        "flatMap:" => Some(docvec![
            "call 'lists':'flatmap'(",
            p0.to_string(),
            ", Self)"
        ]),
        "count:" => Some(docvec![
            "call 'erlang':'length'(call 'lists':'filter'(",
            p0.to_string(),
            ", Self))",
        ]),
        "anySatisfy:" => Some(docvec!["call 'lists':'any'(", p0.to_string(), ", Self)"]),
        "allSatisfy:" => Some(docvec!["call 'lists':'all'(", p0.to_string(), ", Self)"]),
        _ => None,
    }
}

fn generate_list_misc_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        "zip:" => {
            let p0 = params.first().map_or("_Other", String::as_str);
            Some(docvec![
                "call 'beamtalk_list_ops':'zip'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "groupBy:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(docvec![
                "call 'beamtalk_list_ops':'group_by'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "partition:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(docvec![
                "call 'beamtalk_list_ops':'partition'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "takeWhile:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(docvec![
                "call 'lists':'takewhile'(",
                p0.to_string(),
                ", Self)"
            ])
        }
        "dropWhile:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(docvec![
                "call 'lists':'dropwhile'(",
                p0.to_string(),
                ", Self)"
            ])
        }
        "intersperse:" => {
            let p0 = params.first().map_or("_Sep", String::as_str);
            Some(docvec![
                "call 'beamtalk_list_ops':'intersperse'(Self, ",
                p0.to_string(),
                ")",
            ])
        }
        "add:" => {
            let p0 = params.first().map_or("_Item", String::as_str);
            Some(docvec![
                "call 'erlang':'++'(Self, [",
                p0.to_string(),
                "|[]])"
            ])
        }
        "++" => {
            let p0 = params.first().map_or("_Other", String::as_str);
            Some(docvec!["call 'erlang':'++'(Self, ", p0.to_string(), ")"])
        }
        "from:to:" => {
            let p0 = params.first().map_or("_Start", String::as_str);
            let p1 = params.get(1).map_or("_End", String::as_str);
            Some(docvec![
                "call 'beamtalk_list_ops':'from_to'(Self, ",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")",
            ])
        }
        "indexOf:" => {
            let p0 = params.first().map_or("_Item", String::as_str);
            Some(docvec![
                "call 'beamtalk_list_ops':'index_of'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "eachWithIndex:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(docvec![
                "call 'beamtalk_list_ops':'each_with_index'(Self, ",
                p0.to_string(),
                ")",
            ])
        }
        "describe" => {
            let s = core_erlang_binary_string("a List");
            Some(s)
        }
        "printString" => Some(Document::Str(
            "call 'beamtalk_primitive':'print_string'(Self)",
        )),
        "stream" => Some(Document::Str("call 'beamtalk_stream':'on'(Self)")),
        _ => None,
    }
}

/// Dictionary primitive implementations (BT-418).
///
/// Dictionaries are Erlang maps — immutable key-value collections.
fn generate_dictionary_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        "size" => Some(Document::Str("call 'erlang':'map_size'(Self)")),
        "keys" => Some(Document::Str("call 'maps':'keys'(Self)")),
        "values" => Some(Document::Str("call 'maps':'values'(Self)")),
        "at:" => {
            let p0 = params.first().map_or("_Key", String::as_str);
            Some(docvec!["call 'maps':'get'(", p0.to_string(), ", Self)"])
        }
        "at:ifAbsent:" => {
            let p0 = params.first().map_or("_Key", String::as_str);
            let p1 = params.get(1).map_or("_Block", String::as_str);
            Some(docvec![
                "call 'beamtalk_map_ops':'at_if_absent'(Self, ",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")",
            ])
        }
        "at:put:" => {
            let p0 = params.first().map_or("_Key", String::as_str);
            let p1 = params.get(1).map_or("_Value", String::as_str);
            Some(docvec![
                "call 'maps':'put'(",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ", Self)"
            ])
        }
        "includesKey:" => {
            let p0 = params.first().map_or("_Key", String::as_str);
            Some(docvec!["call 'maps':'is_key'(", p0.to_string(), ", Self)"])
        }
        "removeKey:" => {
            let p0 = params.first().map_or("_Key", String::as_str);
            Some(docvec!["call 'maps':'remove'(", p0.to_string(), ", Self)"])
        }
        "merge:" => {
            let p0 = params.first().map_or("_Other", String::as_str);
            Some(docvec!["call 'maps':'merge'(Self, ", p0.to_string(), ")"])
        }
        "keysAndValuesDo:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(docvec![
                "call 'beamtalk_map_ops':'keys_and_values_do'(Self, ",
                p0.to_string(),
                ")",
            ])
        }
        "do:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(docvec![
                "call 'beamtalk_map_ops':'do'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "includes:" => {
            let p0 = params.first().map_or("_Element", String::as_str);
            Some(docvec![
                "call 'beamtalk_map_ops':'includes'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "printString" => Some(Document::Str(
            "call 'beamtalk_map_ops':'print_string'(Self)",
        )),
        // Streaming (BT-514)
        "stream" => Some(Document::Str("call 'beamtalk_stream':'on'(Self)")),
        _ => None,
    }
}

/// Object primitive implementations (BT-335).
///
/// Object is the root class — methods here are inherited by all objects.
fn generate_object_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        // Association creation: `self -> value` creates an Association tagged map
        "->" => {
            let p0 = params.first()?;
            Some(docvec![
                "~{'$beamtalk_class' => 'Association', 'key' => Self, 'value' => ",
                p0.clone(),
                "}~",
            ])
        }
        _ => None,
    }
}

/// Association primitive implementations (BT-335).
///
/// Associations are key-value pairs represented as tagged maps.
fn generate_association_bif(selector: &str, _params: &[String]) -> Option<Document<'static>> {
    match selector {
        "key" => Some(Document::Str("call 'maps':'get'('key', Self)")),
        "value" => Some(Document::Str("call 'maps':'get'('value', Self)")),
        "asString" => Some(Document::Str(
            "call 'beamtalk_association':'format_string'(Self)",
        )),
        _ => None,
    }
}

/// Set primitive implementations (BT-73).
///
/// Sets are represented as tagged maps: `#{'$beamtalk_class' => 'Set', elements => OrdsetData}`.
/// Operations delegate to `beamtalk_set_ops` helper module which wraps Erlang `ordsets`.
fn generate_set_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        "fromList:" => {
            let p0 = params.first().map_or("_List", String::as_str);
            Some(docvec![
                "call 'beamtalk_set_ops':'from_list'(",
                p0.to_string(),
                ")"
            ])
        }
        "size" => Some(Document::Str("call 'beamtalk_set_ops':'size'(Self)")),
        "isEmpty" => Some(Document::Str("call 'beamtalk_set_ops':'is_empty'(Self)")),
        "includes:" => {
            let p0 = params.first().map_or("_Element", String::as_str);
            Some(docvec![
                "call 'beamtalk_set_ops':'includes'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "add:" => {
            let p0 = params.first().map_or("_Element", String::as_str);
            Some(docvec![
                "call 'beamtalk_set_ops':'add'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "remove:" => {
            let p0 = params.first().map_or("_Element", String::as_str);
            Some(docvec![
                "call 'beamtalk_set_ops':'remove'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "union:" => {
            let p0 = params.first().map_or("_Other", String::as_str);
            Some(docvec![
                "call 'beamtalk_set_ops':'union'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "intersection:" => {
            let p0 = params.first().map_or("_Other", String::as_str);
            Some(docvec![
                "call 'beamtalk_set_ops':'intersection'(Self, ",
                p0.to_string(),
                ")",
            ])
        }
        "difference:" => {
            let p0 = params.first().map_or("_Other", String::as_str);
            Some(docvec![
                "call 'beamtalk_set_ops':'difference'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "isSubsetOf:" => {
            let p0 = params.first().map_or("_Other", String::as_str);
            Some(docvec![
                "call 'beamtalk_set_ops':'is_subset_of'(Self, ",
                p0.to_string(),
                ")",
            ])
        }
        "asList" => Some(Document::Str("call 'beamtalk_set_ops':'as_list'(Self)")),
        "do:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(docvec![
                "call 'beamtalk_set_ops':'do'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "printString" => {
            // BT-477: Delegate to beamtalk_primitive:print_string/1 which
            // formats Sets as "Set(element1, element2, ...)"
            Some(Document::Str(
                "call 'beamtalk_primitive':'print_string'(Self)",
            ))
        }
        // Streaming (BT-514)
        "stream" => Some(Document::Str("call 'beamtalk_stream':'on'(Self)")),
        _ => None,
    }
}

/// Stream primitive implementations (BT-511).
///
/// Class-side constructors delegate to `beamtalk_stream` module.
/// Instance methods delegate to `beamtalk_stream` module with Self as first arg.
fn generate_stream_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    let p0 = params.first().map_or("_Arg0", String::as_str);
    match selector {
        // Class-side constructors
        "from:" => Some(docvec![
            "call 'beamtalk_stream':'from'(",
            p0.to_string(),
            ")"
        ]),
        "from:by:" => {
            let p1 = params.get(1).map_or("_Arg1", String::as_str);
            Some(docvec![
                "call 'beamtalk_stream':'from_by'(",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")"
            ])
        }
        "on:" => Some(docvec!["call 'beamtalk_stream':'on'(", p0.to_string(), ")"]),
        // Lazy operations
        "select:" => Some(docvec![
            "call 'beamtalk_stream':'select'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "collect:" => Some(docvec![
            "call 'beamtalk_stream':'collect'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "reject:" => Some(docvec![
            "call 'beamtalk_stream':'reject'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "drop:" => Some(docvec![
            "call 'beamtalk_stream':'drop'(Self, ",
            p0.to_string(),
            ")"
        ]),
        // Terminal operations
        "take:" => Some(docvec![
            "call 'beamtalk_stream':'take'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "do:" => Some(docvec![
            "call 'beamtalk_stream':'do'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "inject:into:" => {
            let p1 = params.get(1).map_or("_Arg1", String::as_str);
            Some(docvec![
                "call 'beamtalk_stream':'inject_into'(Self, ",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")",
            ])
        }
        "detect:" => Some(docvec![
            "call 'beamtalk_stream':'detect'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "asList" => Some(Document::Str("call 'beamtalk_stream':'as_list'(Self)")),
        "anySatisfy:" => Some(docvec![
            "call 'beamtalk_stream':'any_satisfy'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "allSatisfy:" => Some(docvec![
            "call 'beamtalk_stream':'all_satisfy'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "printString" => Some(Document::Str("call 'beamtalk_stream':'print_string'(Self)")),
        _ => None,
    }
}

// Helper functions for generating common patterns

/// Encodes a string as a Core Erlang binary literal.
///
/// Core Erlang represents binaries as: `#{#<byte>(8,1,'integer',['unsigned'|['big']]), ...}#`
fn core_erlang_binary_string(s: &str) -> Document<'static> {
    if s.is_empty() {
        return Document::Str("#{}#");
    }
    let segments: Vec<String> = s
        .bytes()
        .map(|b| format!("#<{b}>(8,1,'integer',['unsigned'|['big']])"))
        .collect();
    Document::String(format!("#{{{}}}#", segments.join(",")))
}

/// Returns a binary BIF call: `call 'erlang':'op'(Self, Param0)`
fn binary_bif(erlang_op: &'static str, params: &[String]) -> Option<Document<'static>> {
    let p0 = params.first()?;
    Some(docvec![
        "call 'erlang':'",
        erlang_op,
        "'(Self, ",
        p0.clone(),
        ")",
    ])
}

/// Returns power implementation: `call 'math':'pow'(Self, Param0)`
fn power_bif(params: &[String]) -> Option<Document<'static>> {
    let p0 = params.first()?;
    Some(docvec![
        "call 'erlang':'round'(call 'math':'pow'(\
         call 'erlang':'float'(Self), call 'erlang':'float'(",
        p0.clone(),
        ")))",
    ])
}

/// `CompiledMethod` primitive implementations.
///
/// All selectors delegate to `beamtalk_compiled_method_ops:dispatch/3`
/// (the hand-written Erlang runtime helper) to avoid recursion through
/// the compiled stdlib module's own dispatch/3.
fn generate_compiled_method_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        "selector" | "source" | "argumentCount" | "printString" | "asString" => Some(ops_dispatch(
            "beamtalk_compiled_method_ops",
            selector,
            params,
        )),
        _ => None,
    }
}

/// `TestCase` primitive implementations for `BUnit` test framework (ADR 0014).
fn generate_test_case_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        "assert:" => {
            let p0 = params.first().map_or("_Condition", String::as_str);
            Some(docvec![
                "call 'beamtalk_test_case':'assert'(",
                p0.to_string(),
                ")"
            ])
        }
        "assert:equals:" => {
            let p0 = params.first().map_or("_Actual", String::as_str);
            let p1 = params.get(1).map_or("_Expected", String::as_str);
            Some(docvec![
                "call 'beamtalk_test_case':'assert_equals'(",
                p1.to_string(),
                ", ",
                p0.to_string(),
                ")",
            ])
        }
        "deny:" => {
            let p0 = params.first().map_or("_Condition", String::as_str);
            Some(docvec![
                "call 'beamtalk_test_case':'deny'(",
                p0.to_string(),
                ")"
            ])
        }
        "should:raise:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            let p1 = params.get(1).map_or("_ErrorKind", String::as_str);
            Some(docvec![
                "call 'beamtalk_test_case':'should_raise'(",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")",
            ])
        }
        "fail:" => {
            let p0 = params.first().map_or("_Message", String::as_str);
            Some(docvec![
                "call 'beamtalk_test_case':'fail'(",
                p0.to_string(),
                ")"
            ])
        }
        // BT-440: Class-side methods for REPL test execution
        // These are always called from class method context where self = ClassSelf
        // Pass class name by extracting from ClassSelf tag ('ClassName class')
        "runAll" => Some(Document::Str(
            "let <_RunTag> = call 'erlang':'element'(2, ClassSelf) in \
                 let <_RunTagStr> = call 'erlang':'atom_to_list'(_RunTag) in \
                 let <_RunLen> = call 'erlang':'length'(_RunTagStr) in \
                 let <_RunNameLen> = call 'erlang':'-'(_RunLen, 6) in \
                 let <_RunNameStr> = call 'lists':'sublist'(_RunTagStr, _RunNameLen) in \
                 let <_RunClassName> = call 'erlang':'list_to_atom'(_RunNameStr) in \
                 call 'beamtalk_test_case':'run_all'(_RunClassName)",
        )),
        "run:" => {
            let p0 = params.first().map_or("_TestName", String::as_str);
            Some(docvec![
                "let <_RunTag2> = call 'erlang':'element'(2, ClassSelf) in \
                 let <_RunTagStr2> = call 'erlang':'atom_to_list'(_RunTag2) in \
                 let <_RunLen2> = call 'erlang':'length'(_RunTagStr2) in \
                 let <_RunNameLen2> = call 'erlang':'-'(_RunLen2, 6) in \
                 let <_RunNameStr2> = call 'lists':'sublist'(_RunTagStr2, _RunNameLen2) in \
                 let <_RunClassName2> = call 'erlang':'list_to_atom'(_RunNameStr2) in \
                 call 'beamtalk_test_case':'run_single'(_RunClassName2, ",
                p0.to_string(),
                ")",
            ])
        }
        _ => None,
    }
}

/// Collection primitive implementations.
///
/// Default implementations for abstract Collection protocol methods.
/// These dispatch `do:` on the actual Self value, which routes to
/// the concrete subclass (Set, Dictionary, Tuple).
fn generate_collection_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        "includes:" => {
            let p0 = params.first().map_or("_Element", String::as_str);
            Some(docvec![
                "call 'beamtalk_collection_ops':'includes'(Self, ",
                p0.to_string(),
                ")",
            ])
        }
        "inject:into:" => {
            let p0 = params.first().map_or("_Initial", String::as_str);
            let p1 = params.get(1).map_or("_Block", String::as_str);
            Some(docvec![
                "call 'beamtalk_collection_ops':'inject_into'(Self, ",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")",
            ])
        }
        "collect:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(docvec![
                "call 'beamtalk_collection_ops':'collect'(Self, ",
                p0.to_string(),
                ")",
            ])
        }
        "select:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(docvec![
                "call 'beamtalk_collection_ops':'select'(Self, ",
                p0.to_string(),
                ")",
            ])
        }
        "reject:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(docvec![
                "call 'beamtalk_collection_ops':'reject'(Self, ",
                p0.to_string(),
                ")",
            ])
        }
        "detect:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(docvec![
                "call 'beamtalk_collection_ops':'detect'(Self, ",
                p0.to_string(),
                ")",
            ])
        }
        "detect:ifNone:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            let p1 = params.get(1).map_or("_NoneBlock", String::as_str);
            Some(docvec![
                "call 'beamtalk_collection_ops':'detect_if_none'(Self, ",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")",
            ])
        }
        "anySatisfy:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(docvec![
                "call 'beamtalk_collection_ops':'any_satisfy'(Self, ",
                p0.to_string(),
                ")",
            ])
        }
        "allSatisfy:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(docvec![
                "call 'beamtalk_collection_ops':'all_satisfy'(Self, ",
                p0.to_string(),
                ")",
            ])
        }
        _ => None,
    }
}

/// Generates a call to a `_ops` Erlang module's dispatch/3 function.
fn ops_dispatch(module: &str, selector: &str, params: &[String]) -> Document<'static> {
    let mut parts: Vec<Document<'static>> = vec![Document::String(format!(
        "call '{module}':'dispatch'('{selector}', ["
    ))];
    for (i, param) in params.iter().enumerate() {
        if i > 0 {
            parts.push(Document::Str(", "));
        }
        parts.push(Document::String(param.clone()));
    }
    parts.push(Document::Str("], Self)"));
    Document::Vec(parts)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn doc_to_string(doc: Option<Document<'_>>) -> Option<String> {
        doc.map(|d| d.to_pretty_string())
    }

    #[test]
    fn test_integer_plus() {
        let result = doc_to_string(generate_primitive_bif(
            "Integer",
            "+",
            &["Other".to_string()],
        ));
        assert_eq!(result, Some("call 'erlang':'+'(Self, Other)".to_string()));
    }

    #[test]
    fn test_integer_modulo() {
        let result = doc_to_string(generate_primitive_bif(
            "Integer",
            "%",
            &["Other".to_string()],
        ));
        assert_eq!(result, Some("call 'erlang':'rem'(Self, Other)".to_string()));
    }

    #[test]
    fn test_integer_as_string() {
        let result = doc_to_string(generate_primitive_bif("Integer", "asString", &[]));
        assert_eq!(
            result,
            Some("call 'erlang':'integer_to_binary'(Self)".to_string())
        );
    }

    #[test]
    fn test_string_length() {
        let result = doc_to_string(generate_primitive_bif("String", "length", &[]));
        assert_eq!(result, Some("call 'string':'length'(Self)".to_string()));
    }

    #[test]
    fn test_string_concat() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "++",
            &["Other".to_string()],
        ));
        let output = result.unwrap();
        assert!(output.contains("iolist_to_binary"));
    }

    #[test]
    fn test_unknown_class() {
        let result = doc_to_string(generate_primitive_bif(
            "Counter",
            "+",
            &["Other".to_string()],
        ));
        assert!(result.is_none());
    }

    #[test]
    fn test_unknown_selector() {
        let result = doc_to_string(generate_primitive_bif("Integer", "unknownMethod", &[]));
        assert!(result.is_none());
    }

    #[test]
    fn test_float_as_string() {
        let result = doc_to_string(generate_primitive_bif("Float", "asString", &[]));
        assert_eq!(
            result,
            Some("call 'erlang':'float_to_binary'(Self, ['short'])".to_string())
        );
    }

    #[test]
    fn test_float_rounded() {
        let result = doc_to_string(generate_primitive_bif("Float", "rounded", &[]));
        assert_eq!(result, Some("call 'erlang':'round'(Self)".to_string()));
    }

    #[test]
    fn test_float_ceiling() {
        let result = doc_to_string(generate_primitive_bif("Float", "ceiling", &[]));
        assert_eq!(result, Some("call 'erlang':'ceil'(Self)".to_string()));
    }

    #[test]
    fn test_float_floor() {
        let result = doc_to_string(generate_primitive_bif("Float", "floor", &[]));
        assert_eq!(result, Some("call 'erlang':'floor'(Self)".to_string()));
    }

    #[test]
    fn test_float_truncated() {
        let result = doc_to_string(generate_primitive_bif("Float", "truncated", &[]));
        assert_eq!(result, Some("call 'erlang':'trunc'(Self)".to_string()));
    }

    #[test]
    fn test_float_as_integer() {
        let result = doc_to_string(generate_primitive_bif("Float", "asInteger", &[]));
        assert_eq!(result, Some("call 'erlang':'trunc'(Self)".to_string()));
    }

    #[test]
    fn test_file_exists() {
        let result = doc_to_string(generate_primitive_bif(
            "File",
            "exists:",
            &["Path".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_file':'exists:'(Path)".to_string())
        );
    }

    #[test]
    fn test_file_read_all() {
        let result = doc_to_string(generate_primitive_bif(
            "File",
            "readAll:",
            &["Path".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_file':'readAll:'(Path)".to_string())
        );
    }

    #[test]
    fn test_file_write_all_contents() {
        let result = doc_to_string(generate_primitive_bif(
            "File",
            "writeAll:contents:",
            &["Path".to_string(), "Text".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_file':'writeAll:contents:'(Path, Text)".to_string())
        );
    }

    #[test]
    fn test_file_lines() {
        let result = doc_to_string(generate_primitive_bif(
            "File",
            "lines:",
            &["Path".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_file':'lines:'(Path)".to_string())
        );
    }

    #[test]
    fn test_file_open_do() {
        let result = doc_to_string(generate_primitive_bif(
            "File",
            "open:do:",
            &["Path".to_string(), "Block".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_file':'open:do:'(Path, Block)".to_string())
        );
    }

    #[test]
    fn test_symbol_as_string() {
        let result = doc_to_string(generate_primitive_bif("Symbol", "asString", &[]));
        assert_eq!(
            result,
            Some("call 'erlang':'atom_to_binary'(Self, 'utf8')".to_string())
        );
    }

    #[test]
    fn test_symbol_as_atom() {
        let result = doc_to_string(generate_primitive_bif("Symbol", "asAtom", &[]));
        assert_eq!(result, Some("Self".to_string()));
    }

    #[test]
    fn test_symbol_equality() {
        let result = doc_to_string(generate_primitive_bif(
            "Symbol",
            "=:=",
            &["Other".to_string()],
        ));
        assert_eq!(result, Some("call 'erlang':'=:='(Self, Other)".to_string()));
    }

    #[test]
    fn test_symbol_hash() {
        let result = doc_to_string(generate_primitive_bif("Symbol", "hash", &[]));
        assert_eq!(result, Some("call 'erlang':'phash2'(Self)".to_string()));
    }

    #[test]
    fn test_symbol_print_string() {
        let result = doc_to_string(generate_primitive_bif("Symbol", "printString", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_primitive':'print_string'(Self)".to_string())
        );
    }

    #[test]
    fn test_list_concat() {
        let result = doc_to_string(generate_primitive_bif("List", "++", &["Other".to_string()]));
        assert_eq!(result, Some("call 'erlang':'++'(Self, Other)".to_string()));
    }

    #[test]
    fn test_list_from_to() {
        let result = doc_to_string(generate_primitive_bif(
            "List",
            "from:to:",
            &["Start".to_string(), "End".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_list_ops':'from_to'(Self, Start, End)".to_string())
        );
    }

    #[test]
    fn test_list_index_of() {
        let result = doc_to_string(generate_primitive_bif(
            "List",
            "indexOf:",
            &["Item".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_list_ops':'index_of'(Self, Item)".to_string())
        );
    }

    #[test]
    fn test_list_each_with_index() {
        let result = doc_to_string(generate_primitive_bif(
            "List",
            "eachWithIndex:",
            &["Block".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_list_ops':'each_with_index'(Self, Block)".to_string())
        );
    }

    // Character primitive tests (BT-339)

    #[test]
    fn test_character_as_integer() {
        let result = doc_to_string(generate_primitive_bif("Character", "asInteger", &[]));
        assert_eq!(result, Some("Self".to_string()));
    }

    #[test]
    fn test_character_as_string() {
        let result = doc_to_string(generate_primitive_bif("Character", "asString", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'as_string'(Self)".to_string())
        );
    }

    #[test]
    fn test_character_print_string() {
        let result = doc_to_string(generate_primitive_bif("Character", "printString", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'print_string'(Self)".to_string())
        );
    }

    #[test]
    fn test_character_equality() {
        let result = doc_to_string(generate_primitive_bif(
            "Character",
            "=:=",
            &["Other".to_string()],
        ));
        assert_eq!(result, Some("call 'erlang':'=:='(Self, Other)".to_string()));
    }

    #[test]
    fn test_character_is_letter() {
        let result = doc_to_string(generate_primitive_bif("Character", "isLetter", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'is_letter'(Self)".to_string())
        );
    }

    #[test]
    fn test_character_is_digit() {
        let result = doc_to_string(generate_primitive_bif("Character", "isDigit", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'is_digit'(Self)".to_string())
        );
    }

    #[test]
    fn test_character_value_factory() {
        let result = doc_to_string(generate_primitive_bif(
            "Character",
            "value:",
            &["CP".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'value'(CP)".to_string())
        );
    }

    #[test]
    fn test_character_hash() {
        let result = doc_to_string(generate_primitive_bif("Character", "hash", &[]));
        assert_eq!(result, Some("call 'erlang':'phash2'(Self)".to_string()));
    }

    #[test]
    fn test_character_is_uppercase() {
        let result = doc_to_string(generate_primitive_bif("Character", "isUppercase", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'is_uppercase'(Self)".to_string())
        );
    }

    #[test]
    fn test_character_is_lowercase() {
        let result = doc_to_string(generate_primitive_bif("Character", "isLowercase", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'is_lowercase'(Self)".to_string())
        );
    }

    #[test]
    fn test_character_is_whitespace() {
        let result = doc_to_string(generate_primitive_bif("Character", "isWhitespace", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'is_whitespace'(Self)".to_string())
        );
    }

    #[test]
    fn test_character_uppercase() {
        let result = doc_to_string(generate_primitive_bif("Character", "uppercase", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'to_uppercase'(Self)".to_string())
        );
    }

    #[test]
    fn test_character_lowercase() {
        let result = doc_to_string(generate_primitive_bif("Character", "lowercase", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'to_lowercase'(Self)".to_string())
        );
    }

    #[test]
    fn test_character_comparison() {
        let result = doc_to_string(generate_primitive_bif(
            "Character",
            "<",
            &["Other".to_string()],
        ));
        assert_eq!(result, Some("call 'erlang':'<'(Self, Other)".to_string()));
    }

    // Integer character predicate tests (BT-339)

    #[test]
    fn test_integer_is_letter() {
        let result = doc_to_string(generate_primitive_bif("Integer", "isLetter", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'is_letter'(Self)".to_string())
        );
    }

    #[test]
    fn test_integer_is_digit() {
        let result = doc_to_string(generate_primitive_bif("Integer", "isDigit", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'is_digit'(Self)".to_string())
        );
    }
}
