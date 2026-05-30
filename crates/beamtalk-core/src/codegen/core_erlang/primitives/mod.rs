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

mod actor_types;
mod array;
mod behaviour;
mod binary;
mod block;
mod character;
mod collection;
mod dictionary;
mod error_handling;
mod float;
mod integer;
mod list;
mod protocol;
mod reflection;
mod string;
mod value_types;

use super::document::Document;
use super::document::leaf;
use crate::docvec;

/// Core Erlang expression for `printString` — delegates to the runtime's
/// `beamtalk_primitive:print_string/1` which formats any value for display.
pub(crate) const PRINT_STRING: Document<'static> =
    Document::Str("call 'beamtalk_primitive':'print_string'(Self)");

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
    let lower = REGISTRY
        .iter()
        .find_map(|(class, lower)| (*class == class_name).then_some(lower))?;
    lower(selector, params)
}

/// Lowering function for one class's `@primitive` selectors: given the selector
/// and the method's parameters, returns the Core Erlang to emit (or `None` if
/// the selector has no inline lowering).
type PrimitiveLowerFn = fn(&str, &[String]) -> Option<Document<'static>>;

/// The single authoritative primitive-lowering registry (BT-2234).
///
/// This is the one place that maps a class to how its primitives lower,
/// replacing the former per-class `match class_name` dispatch. Classes that
/// share a lowering table point at the **same** function: the
/// `Behaviour` / `Class` / `Metaclass` tower all route to
/// [`behaviour::generate_tower_bif`], so a primitive's lowering follows the
/// primitive, not the class that happens to declare it. That closes the gap
/// that let `className` silently fall through to runtime dispatch when it moved
/// up the tower (BT-2232).
const REGISTRY: &[(&str, PrimitiveLowerFn)] = &[
    ("Array", array::generate_array_bif),
    ("Binary", binary::generate_binary_bif),
    ("Integer", integer::generate_integer_bif),
    ("Float", float::generate_float_bif),
    ("String", string::generate_string_bif),
    ("Block", block::generate_block_bif),
    ("Exception", error_handling::generate_exception_bif),
    ("Symbol", reflection::generate_symbol_bif),
    ("Tuple", value_types::generate_tuple_bif),
    ("List", list::generate_list_bif),
    ("Dictionary", dictionary::generate_dictionary_bif),
    ("ProtoObject", value_types::generate_proto_object_bif),
    ("Object", value_types::generate_object_bif),
    ("Set", value_types::generate_set_bif),
    ("CompiledMethod", reflection::generate_compiled_method_bif),
    ("Character", character::generate_character_bif),
    ("Collection", collection::generate_collection_bif),
    // Behaviour / Class / Metaclass tower: one shared table (BT-2234).
    ("Behaviour", behaviour::generate_tower_bif),
    ("Class", behaviour::generate_tower_bif),
    ("Metaclass", behaviour::generate_tower_bif),
    ("Protocol", protocol::generate_protocol_bif),
    ("StackFrame", error_handling::generate_stack_frame_bif),
    ("Pid", actor_types::generate_pid_bif),
    ("Port", actor_types::generate_port_bif),
    ("Reference", actor_types::generate_reference_bif),
    ("Future", actor_types::generate_future_bif),
    ("FileHandle", actor_types::generate_file_handle_bif),
];

/// BT-2233: Quoted `@primitive "selector"` declarations that intentionally
/// route through runtime dispatch instead of an inline BIF, so the fail-loud
/// cross-check (and `generate_primitive`'s hard error in `mod.rs`) must not
/// flag them as unmapped.
///
/// Each entry is `(defining class, quoted selector)`. These are
/// call-site-intercepted reflective / identity / dynamic-dispatch operations:
/// their compiled method body is only a placeholder reached via indirect
/// dispatch (e.g. `perform:`), so the runtime-dispatch fallback is the correct
/// lowering for that body. See `dispatch_codegen.rs`
/// (`beamtalk_object_class:class_send/3`) and `operators.rs` (binary-operator
/// interception).
///
/// Keep this list minimal: a quoted primitive that maps 1:1 to a runtime
/// function should be added to a per-class BIF table instead, not here.
const RUNTIME_DISPATCHED_PRIMITIVES: &[(&str, &str)] = &[
    ("ProtoObject", "=="),
    ("ProtoObject", "/="),
    ("ProtoObject", "class"),
    ("ProtoObject", "perform:withArguments:"),
    ("ProtoObject", "perform:withArguments:timeout:"),
    ("ProtoObject", "performLocally:withArguments:"),
    ("Object", "class"),
];

/// Returns `true` if `(class_name, selector)` is an intentionally
/// runtime-dispatched quoted primitive (see `RUNTIME_DISPATCHED_PRIMITIVES`).
#[must_use]
pub(crate) fn is_runtime_dispatched_primitive(class_name: &str, selector: &str) -> bool {
    RUNTIME_DISPATCHED_PRIMITIVES.contains(&(class_name, selector))
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

/// Extracts the parameter at `index` from `params`, returning `default` if the
/// index is out of bounds.
///
/// The conventional default names follow the `_ArgN` pattern (e.g., `_Arg0`,
/// `_Arg1`), though callers may use descriptive names like `_Block` or `_Key`
/// to clarify the role of a generated Core Erlang variable.
pub(crate) fn param<'a>(params: &'a [String], index: usize, default: &'static str) -> &'a str {
    params.get(index).map_or(default, String::as_str)
}

/// Returns a call document with Self as the first argument and one extra parameter:
/// `call 'module':'function'(Self, p0)`
pub(crate) fn call_self_p0(
    module: &'static str,
    function: &'static str,
    p0: &str,
) -> Document<'static> {
    docvec![
        "call '",
        module,
        "':'",
        function,
        "'(Self, ",
        p0.to_string(),
        ")",
    ]
}

/// Returns a call document with one extra parameter first, Self second:
/// `call 'module':'function'(p0, Self)`
///
/// Used for functional-style Erlang APIs (e.g. `lists:map/2`, `lists:filter/2`)
/// where the function argument precedes the collection.
pub(crate) fn call_p0_self(
    module: &'static str,
    function: &'static str,
    p0: &str,
) -> Document<'static> {
    docvec![
        "call '",
        module,
        "':'",
        function,
        "'(",
        p0.to_string(),
        ", Self)",
    ]
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

/// Builds the 4-step `new → with_selector → with_hint → raise` DNU error Document.
///
/// Produces the Core Erlang let-chain:
/// ```text
/// let Error0 = call 'beamtalk_error':'new'('does_not_understand', '<class>') in
/// let Error1 = call 'beamtalk_error':'with_selector'(Error0, '<selector>') in
/// let Error2 = call 'beamtalk_error':'with_hint'(Error1, <hint_binary>) in
/// call 'beamtalk_error':'raise'(Error2)
/// ```
pub(crate) fn build_dnu_error_doc(
    class: &'static str,
    selector: &'static str,
    hint: &str,
) -> Document<'static> {
    let hint_bin = core_erlang_binary_string(hint);
    docvec![
        "let Error0 = call 'beamtalk_error':'new'('does_not_understand', '",
        class,
        "') in \
         let Error1 = call 'beamtalk_error':'with_selector'(Error0, '",
        selector,
        "') in \
         let Error2 = call 'beamtalk_error':'with_hint'(Error1, ",
        hint_bin,
        ") in \
         call 'beamtalk_error':'raise'(Error2)",
    ]
}

/// Encodes a string as a Core Erlang binary literal.
///
/// Core Erlang represents binaries as: `#{#<byte>(8,1,'integer',['unsigned'|['big']]), ...}#`
pub(crate) fn core_erlang_binary_string(s: &str) -> Document<'static> {
    if s.is_empty() {
        return Document::Str("#{}#");
    }
    let mut parts: Vec<Document<'static>> = vec![Document::Str("#{")];
    for (i, b) in s.bytes().enumerate() {
        if i > 0 {
            parts.push(Document::Str(","));
        }
        parts.push(Document::Str("#<"));
        parts.push(leaf::int_lit(i64::from(b)));
        parts.push(Document::Str(">(8,1,'integer',['unsigned'|['big']])"));
    }
    parts.push(Document::Str("}#"));
    Document::Vec(parts)
}

/// Converts an `Option<Document>` to an `Option<String>` for test assertions.
#[cfg(test)]
pub(crate) fn doc_to_string(doc: Option<Document<'_>>) -> Option<String> {
    doc.map(|d| d.to_pretty_string())
}

#[cfg(test)]
mod tests {
    use super::*;

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
    fn test_proto_object_dnu_raises_error() {
        let result = doc_to_string(generate_primitive_bif(
            "ProtoObject",
            "doesNotUnderstand:args:",
            &["Selector".to_string(), "Args".to_string()],
        ));
        let output = result.expect("ProtoObject DNU should produce code");
        assert!(output.contains("'does_not_understand'"));
        assert!(output.contains("beamtalk_error':'with_selector'"));
        assert!(output.contains("beamtalk_error':'raise'"));
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
            Some("call 'beamtalk_list':'from_to'(Self, Start, End)".to_string())
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

    // Pid primitive tests (BT-1553)

    #[test]
    fn test_pid_is_alive() {
        let result = doc_to_string(generate_primitive_bif("Pid", "isAlive", &[]));
        assert_eq!(
            result,
            Some("call 'erlang':'is_process_alive'(Self)".to_string())
        );
    }

    #[test]
    fn test_pid_kill() {
        let result = doc_to_string(generate_primitive_bif("Pid", "kill", &[]));
        assert_eq!(
            result,
            Some("call 'erlang':'exit'(Self, 'kill')".to_string())
        );
    }

    #[test]
    fn test_pid_exit() {
        let result = doc_to_string(generate_primitive_bif(
            "Pid",
            "exit:",
            &["Reason".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'erlang':'exit'(Self, Reason)".to_string())
        );
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

    #[test]
    fn test_call_self_p0_renders_correctly() {
        let doc = call_self_p0("beamtalk_list", "detect", "Block");
        assert_eq!(
            doc.to_pretty_string(),
            "call 'beamtalk_list':'detect'(Self, Block)"
        );
    }

    #[test]
    fn test_call_p0_self_renders_correctly() {
        let doc = call_p0_self("lists", "map", "Block");
        assert_eq!(doc.to_pretty_string(), "call 'lists':'map'(Block, Self)");
    }

    // Array primitive tests (BT-822)

    #[test]
    fn test_array_size() {
        let result = doc_to_string(generate_primitive_bif("Array", "size", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_array':'size'(Self)".to_string())
        );
    }

    #[test]
    fn test_array_is_empty() {
        let result = doc_to_string(generate_primitive_bif("Array", "isEmpty", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_array':'is_empty'(Self)".to_string())
        );
    }

    #[test]
    fn test_array_do() {
        let result = doc_to_string(generate_primitive_bif(
            "Array",
            "do:",
            &["Block".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_array':'do'(Self, Block)".to_string())
        );
    }

    #[test]
    fn test_array_at() {
        let result = doc_to_string(generate_primitive_bif(
            "Array",
            "at:",
            &["Index".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_array':'at'(Self, Index)".to_string())
        );
    }

    #[test]
    fn test_array_at_put() {
        let result = doc_to_string(generate_primitive_bif(
            "Array",
            "at:put:",
            &["Index".to_string(), "Value".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_array':'at_put'(Self, Index, Value)".to_string())
        );
    }

    #[test]
    fn test_array_with_all() {
        let result = doc_to_string(generate_primitive_bif(
            "Array",
            "withAll:",
            &["List".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_array':'from_list'(List)".to_string())
        );
    }

    #[test]
    fn test_array_collect() {
        let result = doc_to_string(generate_primitive_bif(
            "Array",
            "collect:",
            &["Block".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_array':'collect'(Self, Block)".to_string())
        );
    }

    #[test]
    fn test_array_select() {
        let result = doc_to_string(generate_primitive_bif(
            "Array",
            "select:",
            &["Block".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_array':'select'(Self, Block)".to_string())
        );
    }

    #[test]
    fn test_array_inject_into() {
        let result = doc_to_string(generate_primitive_bif(
            "Array",
            "inject:into:",
            &["Init".to_string(), "Block".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_array':'inject_into'(Self, Init, Block)".to_string())
        );
    }

    #[test]
    fn test_array_includes() {
        let result = doc_to_string(generate_primitive_bif(
            "Array",
            "includes:",
            &["Elem".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_array':'includes'(Self, Elem)".to_string())
        );
    }

    #[test]
    fn test_array_print_string() {
        let result = doc_to_string(generate_primitive_bif("Array", "printString", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_array':'print_string'(Self)".to_string())
        );
    }

    #[test]
    fn test_array_unknown_selector() {
        let result = doc_to_string(generate_primitive_bif("Array", "notAMethod", &[]));
        assert!(result.is_none());
    }

    // Dictionary primitive tests (BT-418)

    #[test]
    fn test_dictionary_size() {
        let result = doc_to_string(generate_primitive_bif("Dictionary", "size", &[]));
        assert_eq!(result, Some("call 'erlang':'map_size'(Self)".to_string()));
    }

    #[test]
    fn test_dictionary_keys() {
        let result = doc_to_string(generate_primitive_bif("Dictionary", "keys", &[]));
        assert_eq!(result, Some("call 'maps':'keys'(Self)".to_string()));
    }

    #[test]
    fn test_dictionary_values() {
        let result = doc_to_string(generate_primitive_bif("Dictionary", "values", &[]));
        assert_eq!(result, Some("call 'maps':'values'(Self)".to_string()));
    }

    #[test]
    fn test_dictionary_at() {
        let result = doc_to_string(generate_primitive_bif(
            "Dictionary",
            "at:",
            &["Key".to_string()],
        ));
        assert_eq!(result, Some("call 'maps':'get'(Key, Self)".to_string()));
    }

    #[test]
    fn test_dictionary_at_if_absent() {
        let result = doc_to_string(generate_primitive_bif(
            "Dictionary",
            "at:ifAbsent:",
            &["Key".to_string(), "Block".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_map':'at_if_absent'(Self, Key, Block)".to_string())
        );
    }

    #[test]
    fn test_dictionary_at_put() {
        let result = doc_to_string(generate_primitive_bif(
            "Dictionary",
            "at:put:",
            &["Key".to_string(), "Value".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'maps':'put'(Key, Value, Self)".to_string())
        );
    }

    #[test]
    fn test_dictionary_includes_key() {
        let result = doc_to_string(generate_primitive_bif(
            "Dictionary",
            "includesKey:",
            &["Key".to_string()],
        ));
        assert_eq!(result, Some("call 'maps':'is_key'(Key, Self)".to_string()));
    }

    #[test]
    fn test_dictionary_remove_key() {
        let result = doc_to_string(generate_primitive_bif(
            "Dictionary",
            "removeKey:",
            &["Key".to_string()],
        ));
        assert_eq!(result, Some("call 'maps':'remove'(Key, Self)".to_string()));
    }

    #[test]
    fn test_dictionary_merge() {
        let result = doc_to_string(generate_primitive_bif(
            "Dictionary",
            "merge:",
            &["Other".to_string()],
        ));
        assert_eq!(result, Some("call 'maps':'merge'(Self, Other)".to_string()));
    }

    #[test]
    fn test_dictionary_do_with_key() {
        let result = doc_to_string(generate_primitive_bif(
            "Dictionary",
            "doWithKey:",
            &["Block".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_map':'do_with_key'(Self, Block)".to_string())
        );
    }

    #[test]
    fn test_dictionary_do() {
        let result = doc_to_string(generate_primitive_bif(
            "Dictionary",
            "do:",
            &["Block".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_map':'do'(Self, Block)".to_string())
        );
    }

    #[test]
    fn test_dictionary_includes() {
        let result = doc_to_string(generate_primitive_bif(
            "Dictionary",
            "includes:",
            &["Elem".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_map':'includes'(Self, Elem)".to_string())
        );
    }

    #[test]
    fn test_dictionary_print_string() {
        let result = doc_to_string(generate_primitive_bif("Dictionary", "printString", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_map':'print_string'(Self)".to_string())
        );
    }

    #[test]
    fn test_dictionary_unknown_selector() {
        let result = doc_to_string(generate_primitive_bif("Dictionary", "notAMethod", &[]));
        assert!(result.is_none());
    }

    // String primitive tests — transform group

    #[test]
    fn test_string_at() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "at:",
            &["Index".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'at'(Self, Index)".to_string())
        );
    }

    #[test]
    fn test_string_uppercase() {
        let result = doc_to_string(generate_primitive_bif("String", "uppercase", &[]));
        assert_eq!(
            result,
            Some(
                "call 'unicode':'characters_to_binary'(call 'string':'uppercase'(Self))"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_string_lowercase() {
        let result = doc_to_string(generate_primitive_bif("String", "lowercase", &[]));
        assert_eq!(
            result,
            Some(
                "call 'unicode':'characters_to_binary'(call 'string':'lowercase'(Self))"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_string_capitalize() {
        let result = doc_to_string(generate_primitive_bif("String", "capitalize", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'capitalize'(Self)".to_string())
        );
    }

    #[test]
    fn test_string_trim() {
        let result = doc_to_string(generate_primitive_bif("String", "trim", &[]));
        assert_eq!(
            result,
            Some(
                "call 'unicode':'characters_to_binary'(call 'string':'trim'(Self, 'both'))"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_string_trim_left() {
        let result = doc_to_string(generate_primitive_bif("String", "trimLeft", &[]));
        assert_eq!(
            result,
            Some(
                "call 'unicode':'characters_to_binary'(call 'string':'trim'(Self, 'leading'))"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_string_trim_right() {
        let result = doc_to_string(generate_primitive_bif("String", "trimRight", &[]));
        assert_eq!(
            result,
            Some(
                "call 'unicode':'characters_to_binary'(call 'string':'trim'(Self, 'trailing'))"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_string_reverse() {
        let result = doc_to_string(generate_primitive_bif("String", "reverse", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'reverse'(Self)".to_string())
        );
    }

    #[test]
    fn test_string_comma_concat() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            ",",
            &["Other".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'erlang':'iolist_to_binary'([Self, Other])".to_string())
        );
    }

    // String primitive tests — search group

    #[test]
    fn test_string_includes() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "includes:",
            &["Sub".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'includes'(Self, Sub)".to_string())
        );
    }

    #[test]
    fn test_string_starts_with() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "startsWith:",
            &["Prefix".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'starts_with'(Self, Prefix)".to_string())
        );
    }

    #[test]
    fn test_string_ends_with() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "endsWith:",
            &["Suffix".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'ends_with'(Self, Suffix)".to_string())
        );
    }

    #[test]
    fn test_string_index_of() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "indexOf:",
            &["Sub".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'index_of'(Self, Sub)".to_string())
        );
    }

    #[test]
    fn test_string_split() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "split:",
            &["Sep".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'binary':'split'(Self, Sep, ['global'])".to_string())
        );
    }

    #[test]
    fn test_string_split_on() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "splitOn:",
            &["Sep".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'split_on'(Self, Sep)".to_string())
        );
    }

    #[test]
    fn test_string_repeat() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "repeat:",
            &["N".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'repeat'(Self, N)".to_string())
        );
    }

    #[test]
    fn test_string_lines() {
        let result = doc_to_string(generate_primitive_bif("String", "lines", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'lines'(Self)".to_string())
        );
    }

    #[test]
    fn test_string_words() {
        let result = doc_to_string(generate_primitive_bif("String", "words", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'words'(Self)".to_string())
        );
    }

    #[test]
    fn test_string_replace_all_with() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "replaceAll:with:",
            &["Pat".to_string(), "Repl".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'binary':'replace'(Self, Pat, Repl, ['global'])".to_string())
        );
    }

    #[test]
    fn test_string_replace_first_with() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "replaceFirst:with:",
            &["Pat".to_string(), "Repl".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'binary':'replace'(Self, Pat, Repl, [])".to_string())
        );
    }

    #[test]
    fn test_string_take() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "take:",
            &["N".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'take'(Self, N)".to_string())
        );
    }

    #[test]
    fn test_string_drop() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "drop:",
            &["N".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'drop'(Self, N)".to_string())
        );
    }

    #[test]
    fn test_string_pad_left() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "padLeft:",
            &["N".to_string()],
        ));
        assert_eq!(
            result,
            Some(
                "call 'unicode':'characters_to_binary'(call 'string':'pad'(Self, N, 'leading'))"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_string_pad_right() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "padRight:",
            &["N".to_string()],
        ));
        assert_eq!(
            result,
            Some(
                "call 'unicode':'characters_to_binary'(call 'string':'pad'(Self, N, 'trailing'))"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_string_pad_left_with() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
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
    fn test_string_pad_right_with() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
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

    // String primitive tests — misc group

    #[test]
    fn test_string_is_blank() {
        let result = doc_to_string(generate_primitive_bif("String", "isBlank", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'is_blank'(Self)".to_string())
        );
    }

    #[test]
    fn test_string_is_digit() {
        let result = doc_to_string(generate_primitive_bif("String", "isDigit", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'is_digit'(Self)".to_string())
        );
    }

    #[test]
    fn test_string_is_alpha() {
        let result = doc_to_string(generate_primitive_bif("String", "isAlpha", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'is_alpha'(Self)".to_string())
        );
    }

    #[test]
    fn test_string_as_integer() {
        let result = doc_to_string(generate_primitive_bif("String", "asInteger", &[]));
        assert_eq!(
            result,
            Some("call 'erlang':'binary_to_integer'(Self)".to_string())
        );
    }

    #[test]
    fn test_string_as_float() {
        let result = doc_to_string(generate_primitive_bif("String", "asFloat", &[]));
        assert_eq!(
            result,
            Some("call 'erlang':'binary_to_float'(Self)".to_string())
        );
    }

    #[test]
    fn test_string_as_atom() {
        let result = doc_to_string(generate_primitive_bif("String", "asAtom", &[]));
        assert_eq!(
            result,
            Some("call 'erlang':'binary_to_atom'(Self, 'utf8')".to_string())
        );
    }

    #[test]
    fn test_string_as_list() {
        let result = doc_to_string(generate_primitive_bif("String", "asList", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'as_list'(Self)".to_string())
        );
    }

    #[test]
    fn test_string_each() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "each:",
            &["Block".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'each'(Self, Block)".to_string())
        );
    }

    #[test]
    fn test_string_collect() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "collect:",
            &["Block".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'collect'(Self, Block)".to_string())
        );
    }

    #[test]
    fn test_string_select() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "select:",
            &["Block".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'select'(Self, Block)".to_string())
        );
    }

    #[test]
    fn test_string_reject() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "reject:",
            &["Block".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'reject'(Self, Block)".to_string())
        );
    }

    #[test]
    fn test_string_stream() {
        let result = doc_to_string(generate_primitive_bif("String", "stream", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_stream':'on'(Self)".to_string())
        );
    }

    #[test]
    fn test_string_with_all_factory() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "withAll:",
            &["List".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'join'(List)".to_string())
        );
    }

    #[test]
    fn test_string_from_code_point_factory() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "fromCodePoint:",
            &["CP".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'from_code_point'(CP)".to_string())
        );
    }

    #[test]
    fn test_string_from_code_points_factory() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "fromCodePoints:",
            &["CPs".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'from_code_points'(CPs)".to_string())
        );
    }

    #[test]
    fn test_string_from_iolist_factory() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "fromIolist:",
            &["IoList".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_string':'from_iolist'(IoList)".to_string())
        );
    }

    #[test]
    fn test_string_unknown_selector() {
        let result = doc_to_string(generate_primitive_bif("String", "notAStringMethod", &[]));
        assert!(result.is_none());
    }

    // String primitive tests — comparison group

    #[test]
    fn test_string_equality() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "=:=",
            &["Other".to_string()],
        ));
        assert_eq!(result, Some("call 'erlang':'=:='(Self, Other)".to_string()));
    }

    #[test]
    fn test_string_not_equal() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "/=",
            &["Other".to_string()],
        ));
        assert_eq!(result, Some("call 'erlang':'/='(Self, Other)".to_string()));
    }

    #[test]
    fn test_string_strict_not_equal() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "=/=",
            &["Other".to_string()],
        ));
        assert_eq!(result, Some("call 'erlang':'=/='(Self, Other)".to_string()));
    }

    #[test]
    fn test_string_less_than() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "<",
            &["Other".to_string()],
        ));
        assert_eq!(result, Some("call 'erlang':'<'(Self, Other)".to_string()));
    }

    #[test]
    fn test_string_greater_than() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            ">",
            &["Other".to_string()],
        ));
        assert_eq!(result, Some("call 'erlang':'>'(Self, Other)".to_string()));
    }

    #[test]
    fn test_string_less_than_or_equal() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "<=",
            &["Other".to_string()],
        ));
        assert_eq!(result, Some("call 'erlang':'=<'(Self, Other)".to_string()));
    }

    #[test]
    fn test_string_greater_than_or_equal() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            ">=",
            &["Other".to_string()],
        ));
        assert_eq!(result, Some("call 'erlang':'>='(Self, Other)".to_string()));
    }

    // String primitive tests — regex group (BT-709)

    #[test]
    fn test_string_matches_regex() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "matchesRegex:",
            &["Pat".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_regex':'matches_regex'(Self, Pat)".to_string())
        );
    }

    #[test]
    fn test_string_matches_regex_options() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "matchesRegex:options:",
            &["Pat".to_string(), "Opts".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_regex':'matches_regex_options'(Self, Pat, Opts)".to_string())
        );
    }

    #[test]
    fn test_string_first_match() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "firstMatch:",
            &["Pat".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_regex':'first_match'(Self, Pat)".to_string())
        );
    }

    #[test]
    fn test_string_all_matches() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "allMatches:",
            &["Pat".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_regex':'all_matches'(Self, Pat)".to_string())
        );
    }

    #[test]
    fn test_string_replace_regex_with() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "replaceRegex:with:",
            &["Pat".to_string(), "Repl".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_regex':'replace_regex'(Self, Pat, Repl)".to_string())
        );
    }

    #[test]
    fn test_string_replace_all_regex_with() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "replaceAllRegex:with:",
            &["Pat".to_string(), "Repl".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_regex':'replace_all_regex'(Self, Pat, Repl)".to_string())
        );
    }

    #[test]
    fn test_string_split_regex() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "splitRegex:",
            &["Pat".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_regex':'split_regex'(Self, Pat)".to_string())
        );
    }

    // CompiledMethod primitive tests

    #[test]
    fn test_compiled_method_selector() {
        let result = doc_to_string(generate_primitive_bif("CompiledMethod", "selector", &[]));
        assert_eq!(
            result,
            Some(
                "call 'beamtalk_compiled_method_ops':'dispatch'('selector', [], Self)".to_string()
            )
        );
    }

    #[test]
    fn test_compiled_method_source() {
        let result = doc_to_string(generate_primitive_bif("CompiledMethod", "source", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_compiled_method_ops':'dispatch'('source', [], Self)".to_string())
        );
    }

    #[test]
    fn test_compiled_method_doc() {
        let result = doc_to_string(generate_primitive_bif("CompiledMethod", "doc", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_compiled_method_ops':'dispatch'('doc', [], Self)".to_string())
        );
    }

    #[test]
    fn test_compiled_method_argument_count() {
        let result = doc_to_string(generate_primitive_bif(
            "CompiledMethod",
            "argumentCount",
            &[],
        ));
        assert_eq!(
            result,
            Some(
                "call 'beamtalk_compiled_method_ops':'dispatch'('argumentCount', [], Self)"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_compiled_method_print_string() {
        let result = doc_to_string(generate_primitive_bif("CompiledMethod", "printString", &[]));
        assert_eq!(
            result,
            Some(
                "call 'beamtalk_compiled_method_ops':'dispatch'('printString', [], Self)"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_compiled_method_as_string() {
        let result = doc_to_string(generate_primitive_bif("CompiledMethod", "asString", &[]));
        assert_eq!(
            result,
            Some(
                "call 'beamtalk_compiled_method_ops':'dispatch'('asString', [], Self)".to_string()
            )
        );
    }

    #[test]
    fn test_compiled_method_unknown_selector() {
        let result = doc_to_string(generate_primitive_bif("CompiledMethod", "notAMethod", &[]));
        assert!(result.is_none());
    }

    // Integer arithmetic — remaining ops (BT-2161)

    #[test]
    fn test_integer_minus() {
        let result = doc_to_string(generate_primitive_bif(
            "Integer",
            "-",
            &["Other".to_string()],
        ));
        assert_eq!(result, Some("call 'erlang':'-'(Self, Other)".to_string()));
    }

    #[test]
    fn test_integer_multiply() {
        let result = doc_to_string(generate_primitive_bif(
            "Integer",
            "*",
            &["Other".to_string()],
        ));
        assert_eq!(result, Some("call 'erlang':'*'(Self, Other)".to_string()));
    }

    #[test]
    fn test_integer_divide() {
        let result = doc_to_string(generate_primitive_bif(
            "Integer",
            "/",
            &["Other".to_string()],
        ));
        assert_eq!(result, Some("call 'erlang':'/'(Self, Other)".to_string()));
    }

    #[test]
    fn test_integer_integer_divide() {
        let result = doc_to_string(generate_primitive_bif(
            "Integer",
            "div:",
            &["Other".to_string()],
        ));
        assert_eq!(result, Some("call 'erlang':'div'(Self, Other)".to_string()));
    }

    #[test]
    fn test_integer_power() {
        let result = doc_to_string(generate_primitive_bif(
            "Integer",
            "**",
            &["Other".to_string()],
        ));
        assert_eq!(
            result,
            Some(
                "call 'erlang':'round'(call 'math':'pow'(\
                 call 'erlang':'float'(Self), call 'erlang':'float'(Other)))"
                    .to_string()
            )
        );
    }

    // Integer comparisons — all 7 operators (BT-2161)

    #[test]
    fn test_integer_eq() {
        let result = doc_to_string(generate_primitive_bif(
            "Integer",
            "=:=",
            &["Other".to_string()],
        ));
        assert_eq!(result, Some("call 'erlang':'=:='(Self, Other)".to_string()));
    }

    #[test]
    fn test_integer_neq() {
        let result = doc_to_string(generate_primitive_bif(
            "Integer",
            "/=",
            &["Other".to_string()],
        ));
        assert_eq!(result, Some("call 'erlang':'/='(Self, Other)".to_string()));
    }

    #[test]
    fn test_integer_strict_neq() {
        let result = doc_to_string(generate_primitive_bif(
            "Integer",
            "=/=",
            &["Other".to_string()],
        ));
        assert_eq!(result, Some("call 'erlang':'=/='(Self, Other)".to_string()));
    }

    #[test]
    fn test_integer_lt() {
        let result = doc_to_string(generate_primitive_bif(
            "Integer",
            "<",
            &["Other".to_string()],
        ));
        assert_eq!(result, Some("call 'erlang':'<'(Self, Other)".to_string()));
    }

    #[test]
    fn test_integer_gt() {
        let result = doc_to_string(generate_primitive_bif(
            "Integer",
            ">",
            &["Other".to_string()],
        ));
        assert_eq!(result, Some("call 'erlang':'>'(Self, Other)".to_string()));
    }

    #[test]
    fn test_integer_le() {
        let result = doc_to_string(generate_primitive_bif(
            "Integer",
            "<=",
            &["Other".to_string()],
        ));
        assert_eq!(result, Some("call 'erlang':'=<'(Self, Other)".to_string()));
    }

    #[test]
    fn test_integer_ge() {
        let result = doc_to_string(generate_primitive_bif(
            "Integer",
            ">=",
            &["Other".to_string()],
        ));
        assert_eq!(result, Some("call 'erlang':'>='(Self, Other)".to_string()));
    }

    // Integer conversions — asFloat and printString (BT-2161)

    #[test]
    fn test_integer_as_float() {
        let result = doc_to_string(generate_primitive_bif("Integer", "asFloat", &[]));
        assert_eq!(result, Some("call 'erlang':'float'(Self)".to_string()));
    }

    #[test]
    fn test_integer_print_string() {
        let result = doc_to_string(generate_primitive_bif("Integer", "printString", &[]));
        assert_eq!(
            result,
            Some("call 'erlang':'integer_to_binary'(Self)".to_string())
        );
    }

    // Integer bitwise operations (BT-2161)

    #[test]
    fn test_integer_bit_and() {
        let result = doc_to_string(generate_primitive_bif(
            "Integer",
            "bitAnd:",
            &["Other".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'erlang':'band'(Self, Other)".to_string())
        );
    }

    #[test]
    fn test_integer_bit_or() {
        let result = doc_to_string(generate_primitive_bif(
            "Integer",
            "bitOr:",
            &["Other".to_string()],
        ));
        assert_eq!(result, Some("call 'erlang':'bor'(Self, Other)".to_string()));
    }

    #[test]
    fn test_integer_bit_xor() {
        let result = doc_to_string(generate_primitive_bif(
            "Integer",
            "bitXor:",
            &["Other".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'erlang':'bxor'(Self, Other)".to_string())
        );
    }

    #[test]
    fn test_integer_bit_not() {
        let result = doc_to_string(generate_primitive_bif("Integer", "bitNot", &[]));
        assert_eq!(result, Some("call 'erlang':'bnot'(Self)".to_string()));
    }

    #[test]
    fn test_integer_bit_shift() {
        let result = doc_to_string(generate_primitive_bif(
            "Integer",
            "bitShift:",
            &["N".to_string()],
        ));
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

    // Integer character predicates — remaining three (BT-2161)

    #[test]
    fn test_integer_is_uppercase() {
        let result = doc_to_string(generate_primitive_bif("Integer", "isUppercase", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'is_uppercase'(Self)".to_string())
        );
    }

    #[test]
    fn test_integer_is_lowercase() {
        let result = doc_to_string(generate_primitive_bif("Integer", "isLowercase", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'is_lowercase'(Self)".to_string())
        );
    }

    #[test]
    fn test_integer_is_whitespace() {
        let result = doc_to_string(generate_primitive_bif("Integer", "isWhitespace", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'is_whitespace'(Self)".to_string())
        );
    }

    // Integer math functions (BT-2161)

    #[test]
    fn test_integer_sqrt() {
        let result = doc_to_string(generate_primitive_bif("Integer", "sqrt", &[]));
        assert_eq!(
            result,
            Some("call 'math':'sqrt'(call 'erlang':'float'(Self))".to_string())
        );
    }

    #[test]
    fn test_integer_log() {
        let result = doc_to_string(generate_primitive_bif("Integer", "log", &[]));
        assert_eq!(
            result,
            Some("call 'math':'log'(call 'erlang':'float'(Self))".to_string())
        );
    }

    #[test]
    fn test_integer_ln() {
        let result = doc_to_string(generate_primitive_bif("Integer", "ln", &[]));
        assert_eq!(
            result,
            Some("call 'math':'log'(call 'erlang':'float'(Self))".to_string())
        );
    }

    #[test]
    fn test_integer_log2() {
        let result = doc_to_string(generate_primitive_bif("Integer", "log2", &[]));
        assert_eq!(
            result,
            Some("call 'math':'log2'(call 'erlang':'float'(Self))".to_string())
        );
    }

    #[test]
    fn test_integer_log10() {
        let result = doc_to_string(generate_primitive_bif("Integer", "log10", &[]));
        assert_eq!(
            result,
            Some("call 'math':'log10'(call 'erlang':'float'(Self))".to_string())
        );
    }

    #[test]
    fn test_integer_exp() {
        let result = doc_to_string(generate_primitive_bif("Integer", "exp", &[]));
        assert_eq!(
            result,
            Some("call 'math':'exp'(call 'erlang':'float'(Self))".to_string())
        );
    }

    #[test]
    fn test_integer_raised_to() {
        let result = doc_to_string(generate_primitive_bif(
            "Integer",
            "raisedTo:",
            &["Exp".to_string()],
        ));
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

    // Integer edge cases (BT-2161)

    #[test]
    fn test_integer_unknown_selector() {
        let result = doc_to_string(generate_primitive_bif("Integer", "notAMethod", &[]));
        assert!(result.is_none());
    }

    #[test]
    fn test_integer_binary_op_missing_param() {
        // binary_bif returns None when params is empty (uses `?` on params.first())
        let result = doc_to_string(generate_primitive_bif("Integer", "+", &[]));
        assert!(result.is_none());
    }

    #[test]
    fn test_integer_power_missing_param() {
        // power_bif returns None when params is empty
        let result = doc_to_string(generate_primitive_bif("Integer", "**", &[]));
        assert!(result.is_none());
    }

    #[test]
    fn test_integer_bit_shift_missing_param() {
        // bitShift: returns None when params is empty
        let result = doc_to_string(generate_primitive_bif("Integer", "bitShift:", &[]));
        assert!(result.is_none());
    }

    #[test]
    fn test_integer_raised_to_missing_param() {
        // raisedTo: returns None when params is empty
        let result = doc_to_string(generate_primitive_bif("Integer", "raisedTo:", &[]));
        assert!(result.is_none());
    }

    #[test]
    fn test_core_erlang_binary_string_empty() {
        let doc = core_erlang_binary_string("");
        assert_eq!(doc.to_pretty_string(), "#{}#");
    }

    #[test]
    fn test_core_erlang_binary_string_single_byte() {
        // 'a' = 97
        let doc = core_erlang_binary_string("a");
        assert_eq!(
            doc.to_pretty_string(),
            "#{#<97>(8,1,'integer',['unsigned'|['big']])}#"
        );
    }

    #[test]
    fn test_is_runtime_dispatched_primitive() {
        // Allowlisted call-site-intercepted operations.
        assert!(is_runtime_dispatched_primitive("ProtoObject", "class"));
        assert!(is_runtime_dispatched_primitive(
            "ProtoObject",
            "perform:withArguments:"
        ));
        assert!(is_runtime_dispatched_primitive("Object", "class"));
        // Not allowlisted: mapped primitives and unrelated pairs.
        assert!(!is_runtime_dispatched_primitive("Integer", "+"));
        assert!(!is_runtime_dispatched_primitive(
            "Behaviour",
            "classFieldNames"
        ));
        assert!(!is_runtime_dispatched_primitive(
            "Object",
            "perform:withArguments:"
        ));
    }

    #[test]
    fn test_core_erlang_binary_string_multi_byte() {
        // "hi" = bytes [104, 105]
        let doc = core_erlang_binary_string("hi");
        assert_eq!(
            doc.to_pretty_string(),
            "#{#<104>(8,1,'integer',['unsigned'|['big']]),#<105>(8,1,'integer',['unsigned'|['big']])}#"
        );
    }

    /// BT-2233: Cross-check that every quoted `@primitive "X"` declared in the
    /// stdlib resolves to a registered inline BIF lowering. An unmapped quoted
    /// primitive in a value-type class would silently fall back to runtime
    /// dispatch and raise `does_not_understand` at runtime (the BT-2232
    /// regression: moving `name` to Behaviour left `className` unmapped). This
    /// fast `cargo test` catches such gaps before the slower stdlib build does.
    ///
    /// Exclusions, to avoid false positives (acceptance criterion 3):
    /// - Unquoted `@primitive` (structural intrinsics) — handled by the compiler
    ///   at the call site, never via `generate_primitive_bif`.
    /// - Actor-backed classes — they legitimately route unmapped quoted
    ///   primitives through their hand-written `beamtalk_X:dispatch/3` module
    ///   (e.g. `Actor`'s `actorPid`, `ReactiveSubprocess`'s `open:args:notify:`).
    #[test]
    fn test_every_quoted_primitive_resolves_to_a_bif() {
        use crate::ast::Expression;
        use std::collections::{HashMap, HashSet};

        let lib_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .join("stdlib/src");

        if !lib_dir.exists() {
            // Skip when stdlib sources aren't present (e.g. partial checkouts).
            return;
        }

        // Parse every stdlib module once. Parse diagnostics are ignored — class
        // and method structure (which is all we need) parses fine per-file, the
        // same approach `primitive_bindings::load_from_directory` relies on.
        let mut modules = Vec::new();
        for entry in std::fs::read_dir(&lib_dir)
            .expect("read stdlib/src")
            .flatten()
        {
            let path = entry.path();
            if path.extension().and_then(|e| e.to_str()) != Some("bt") {
                continue;
            }
            let source = std::fs::read_to_string(&path).expect("read .bt file");
            let tokens = crate::source_analysis::lex_with_eof(&source);
            let (module, _diagnostics) = crate::source_analysis::parse(tokens);
            modules.push(module);
        }

        // Build a class -> superclass map across all modules to classify
        // actor-backed classes (mirrors the spirit of `is_actor_class`).
        let mut superclass: HashMap<String, String> = HashMap::new();
        for module in &modules {
            for class in &module.classes {
                superclass.insert(
                    class.name.name.to_string(),
                    class.superclass_name().to_string(),
                );
            }
        }

        let is_actor_backed = |start: &str| -> bool {
            if start == "Actor" {
                return true;
            }
            let mut current = start.to_string();
            // Bounded walk guards against cycles in malformed hierarchies.
            for _ in 0..64 {
                match superclass.get(&current) {
                    Some(parent) if parent == "Actor" => return true,
                    Some(parent) if parent != "none" => current = parent.clone(),
                    _ => return false,
                }
            }
            false
        };

        let mut unmapped: Vec<String> = Vec::new();
        // Every quoted (class, primitive) pair seen, for the allowlist-rot check.
        let mut all_quoted: HashSet<(String, String)> = HashSet::new();
        for module in &modules {
            for class in &module.classes {
                let class_name = class.name.name.as_str();
                // Mirror `is_actor_class`: actor classes (by chain) and concrete
                // supervisor subclasses compile as actors, so their unmapped
                // quoted primitives route through hand-written dispatch.
                let is_concrete_supervisor = class.supervisor_kind.is_some() && !class.is_abstract;
                let actor_backed = is_concrete_supervisor || is_actor_backed(class_name);
                for method in class.methods.iter().chain(class.class_methods.iter()) {
                    if method.body.len() != 1 {
                        continue;
                    }
                    let Expression::Primitive {
                        name, is_quoted, ..
                    } = &method.body[0].expression
                    else {
                        continue;
                    };
                    if !is_quoted {
                        continue;
                    }
                    all_quoted.insert((class_name.to_string(), name.to_string()));
                    if actor_backed || is_runtime_dispatched_primitive(class_name, name.as_str()) {
                        continue;
                    }
                    // Mirror the real codegen path: `current_method_params` holds
                    // one entry per method parameter (names are irrelevant here).
                    let params: Vec<String> = (0..method.parameters.len())
                        .map(|i| format!("Arg{i}"))
                        .collect();
                    if generate_primitive_bif(class_name, name.as_str(), &params).is_none() {
                        unmapped.push(format!(
                            "{class_name}:{name} (arity {})",
                            method.parameters.len()
                        ));
                    }
                }
            }
        }
        unmapped.sort();

        assert!(
            unmapped.is_empty(),
            "Quoted @primitive selectors with no inline BIF lowering in \
             generate_primitive_bif. These would silently fall back to runtime \
             dispatch and raise does_not_understand at runtime (BT-2232/BT-2233). \
             Add a mapping in the relevant primitives/*.rs table:\n  {}",
            unmapped.join("\n  ")
        );

        // BT-2233: Keep the allowlist honest — a stale entry (for a quoted
        // primitive that no longer exists in the stdlib) would silently mask a
        // future unmapped-primitive bug. Every allowlist entry must still
        // correspond to a real quoted @primitive declaration.
        let stale: Vec<String> = RUNTIME_DISPATCHED_PRIMITIVES
            .iter()
            .filter(|(class, selector)| {
                !all_quoted.contains(&((*class).to_string(), (*selector).to_string()))
            })
            .map(|(class, selector)| format!("{class}:{selector}"))
            .collect();
        assert!(
            stale.is_empty(),
            "RUNTIME_DISPATCHED_PRIMITIVES has stale entries with no matching \
             quoted @primitive in the stdlib (remove them):\n  {}",
            stale.join("\n  ")
        );
    }
}
