// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Primitive implementation mapping for stdlib codegen.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Maps `@primitive "selector"` annotations to direct Erlang BIF calls.
//! When a stdlib `.bt` file declares `+ other => @primitive "+"`, this module
//! provides the actual Core Erlang expression to emit instead of delegating
//! through a hand-written dispatch module.
//!
//! This is part of BT-340: making compiled stdlib modules self-sufficient
//! so hand-written Erlang dispatch modules can be deleted.

mod array;
mod behaviour;
mod block;
mod character;
mod collection;
mod dictionary;
mod float;
mod integer;
mod list;
mod metaclass;
mod misc;
mod string;

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
        "Array" => array::generate_array_bif(selector, params),
        "Integer" => integer::generate_integer_bif(selector, params),
        "Float" => float::generate_float_bif(selector, params),
        "String" => string::generate_string_bif(selector, params),
        "Block" => block::generate_block_bif(selector, params),
        "File" => misc::generate_file_bif(selector, params),
        "Exception" => misc::generate_exception_bif(selector, params),
        "Symbol" => misc::generate_symbol_bif(selector, params),
        "Tuple" => misc::generate_tuple_bif(selector, params),
        "List" => list::generate_list_bif(selector, params),
        "Dictionary" => dictionary::generate_dictionary_bif(selector, params),
        "Object" => misc::generate_object_bif(selector, params),
        "Association" => misc::generate_association_bif(selector, params),
        "Set" => misc::generate_set_bif(selector, params),
        "CompiledMethod" => misc::generate_compiled_method_bif(selector, params),
        "Character" => character::generate_character_bif(selector, params),
        "TestCase" => misc::generate_test_case_bif(selector, params),
        "TestRunner" => misc::generate_test_runner_bif(selector, params),
        "TestResult" => misc::generate_test_result_bif(selector, params),
        "Stream" => misc::generate_stream_bif(selector, params),
        "JSON" => misc::generate_json_bif(selector, params),
        "Random" => misc::generate_random_bif(selector, params),
        "Regex" => misc::generate_regex_bif(selector, params),
        "System" => misc::generate_system_bif(selector, params),
        "DateTime" => misc::generate_datetime_bif(selector, params),
        "Collection" => collection::generate_collection_bif(selector, params),
        "Behaviour" => behaviour::generate_behaviour_bif(selector, params),
        "Class" => behaviour::generate_class_bif(selector, params),
        "Metaclass" => metaclass::generate_metaclass_bif(selector, params),
        "StackFrame" => misc::generate_stack_frame_bif(selector, params),
        "Pid" => misc::generate_pid_bif(selector, params),
        "Port" => misc::generate_port_bif(selector, params),
        "Reference" => misc::generate_reference_bif(selector, params),
        "Future" => misc::generate_future_bif(selector, params),
        "FileHandle" => misc::generate_file_handle_bif(selector, params),
        _ => None,
    }
}

// Shared helpers used by per-class modules

/// Generates a comparison BIF call for the standard Erlang comparison operators.
///
/// Used by Integer, Float, String, and Character to avoid duplicating the
/// same 7-operator match block (ADR 0002: Erlang operators).
pub(crate) fn generate_comparison_bif(
    selector: &str,
    params: &[String],
) -> Option<Document<'static>> {
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

/// Returns a binary BIF call: `call 'erlang':'op'(Self, Param0)`
pub(crate) fn binary_bif(erlang_op: &'static str, params: &[String]) -> Option<Document<'static>> {
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
pub(crate) fn power_bif(params: &[String]) -> Option<Document<'static>> {
    let p0 = params.first()?;
    Some(docvec![
        "call 'erlang':'round'(call 'math':'pow'(\
         call 'erlang':'float'(Self), call 'erlang':'float'(",
        p0.clone(),
        ")))",
    ])
}

/// Encodes a string as a Core Erlang binary literal.
///
/// Core Erlang represents binaries as: `#{#<byte>(8,1,'integer',['unsigned'|['big']]), ...}#`
pub(crate) fn core_erlang_binary_string(s: &str) -> Document<'static> {
    if s.is_empty() {
        return Document::Str("#{}#");
    }
    let segments: Vec<String> = s
        .bytes()
        .map(|b| format!("#<{b}>(8,1,'integer',['unsigned'|['big']])"))
        .collect();
    Document::String(format!("#{{{}}}#", segments.join(",")))
}

/// Generates a call to a `_ops` Erlang module's dispatch/3 function.
pub(crate) fn ops_dispatch(module: &str, selector: &str, params: &[String]) -> Document<'static> {
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
