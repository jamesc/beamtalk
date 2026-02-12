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
) -> Option<String> {
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
        _ => None,
    }
}

/// Integer primitive implementations.
fn generate_integer_bif(selector: &str, params: &[String]) -> Option<String> {
    match selector {
        // Arithmetic — direct Erlang BIF
        "+" => binary_bif("+", params),
        "-" => binary_bif("-", params),
        "*" => binary_bif("*", params),
        "/" => binary_bif("/", params),
        "%" => binary_bif("rem", params),
        "**" => power_bif(params),
        // Comparison
        "=" => binary_bif("=:=", params),
        "<" => binary_bif("<", params),
        ">" => binary_bif(">", params),
        "<=" => binary_bif("=<", params),
        ">=" => binary_bif(">=", params),
        // Conversion
        "asString" | "printString" => Some("call 'erlang':'integer_to_binary'(Self)".to_string()),
        "asFloat" => Some("call 'erlang':'float'(Self)".to_string()),
        // Bitwise operations
        "bitAnd:" => binary_bif("band", params),
        "bitOr:" => binary_bif("bor", params),
        "bitXor:" => binary_bif("bxor", params),
        "bitShift:" => {
            // Positive N shifts left, negative shifts right
            let p0 = params.first()?;
            Some(format!(
                "case call 'erlang':'>='({p0}, 0) of \
                 'true' when 'true' -> call 'erlang':'bsl'(Self, {p0}) \
                 'false' when 'true' -> call 'erlang':'bsr'(Self, call 'erlang':'-'(0, {p0})) end"
            ))
        }
        "bitNot" => Some("call 'erlang':'bnot'(Self)".to_string()),
        // Character predicates — integers are Unicode codepoints (BT-339)
        "isLetter" => Some("call 'beamtalk_character':'is_letter'(Self)".to_string()),
        "isDigit" => Some("call 'beamtalk_character':'is_digit'(Self)".to_string()),
        "isUppercase" => Some("call 'beamtalk_character':'is_uppercase'(Self)".to_string()),
        "isLowercase" => Some("call 'beamtalk_character':'is_lowercase'(Self)".to_string()),
        "isWhitespace" => Some("call 'beamtalk_character':'is_whitespace'(Self)".to_string()),
        _ => None,
    }
}

/// Float primitive implementations.
fn generate_float_bif(selector: &str, params: &[String]) -> Option<String> {
    match selector {
        // Arithmetic — direct Erlang BIF
        "+" => binary_bif("+", params),
        "-" => binary_bif("-", params),
        "*" => binary_bif("*", params),
        "/" => binary_bif("/", params),
        // Comparison
        "=" => binary_bif("=:=", params),
        "<" => binary_bif("<", params),
        ">" => binary_bif(">", params),
        "<=" => binary_bif("=<", params),
        ">=" => binary_bif(">=", params),
        // Rounding
        "rounded" => Some("call 'erlang':'round'(Self)".to_string()),
        "ceiling" => Some("call 'erlang':'ceil'(Self)".to_string()),
        "floor" => Some("call 'erlang':'floor'(Self)".to_string()),
        "truncated" | "asInteger" => Some("call 'erlang':'trunc'(Self)".to_string()),
        // Conversion
        "asString" | "printString" => {
            Some("call 'erlang':'float_to_binary'(Self, ['short'])".to_string())
        }
        _ => None,
    }
}

/// String primitive implementations.
#[allow(clippy::too_many_lines)]
fn generate_string_bif(selector: &str, params: &[String]) -> Option<String> {
    let p0 = params.first().map_or("_Arg0", String::as_str);
    match selector {
        // Comparison
        "=" => binary_bif("=:=", params),
        "<" => binary_bif("<", params),
        ">" => binary_bif(">", params),
        "<=" => binary_bif("=<", params),
        ">=" => binary_bif(">=", params),
        // Concatenation — direct iolist concat (binaries are valid iolists)
        "++" => Some(format!("call 'erlang':'iolist_to_binary'([Self, {p0}])")),
        // Length
        "length" => Some("call 'string':'length'(Self)".to_string()),
        // Access
        "at:" => {
            // 1-based grapheme access — delegate to runtime helper
            Some(format!("call 'beamtalk_string_ops':'at'(Self, {p0})"))
        }
        // Case transformation
        "uppercase" => Some(
            "call 'unicode':'characters_to_binary'(call 'string':'uppercase'(Self))".to_string(),
        ),
        "lowercase" => Some(
            "call 'unicode':'characters_to_binary'(call 'string':'lowercase'(Self))".to_string(),
        ),
        "capitalize" => Some("call 'beamtalk_string_ops':'capitalize'(Self)".to_string()),
        // Whitespace
        "trim" => Some(
            "call 'unicode':'characters_to_binary'(call 'string':'trim'(Self, 'both'))".to_string(),
        ),
        "trimLeft" => Some(
            "call 'unicode':'characters_to_binary'(call 'string':'trim'(Self, 'leading'))"
                .to_string(),
        ),
        "trimRight" => Some(
            "call 'unicode':'characters_to_binary'(call 'string':'trim'(Self, 'trailing'))"
                .to_string(),
        ),
        // Reverse
        "reverse" => Some("call 'beamtalk_string_ops':'reverse'(Self)".to_string()),
        // Search
        "includes:" => Some(format!("call 'beamtalk_string_ops':'includes'(Self, {p0})")),
        "startsWith:" => Some(format!(
            "call 'beamtalk_string_ops':'starts_with'(Self, {p0})"
        )),
        "endsWith:" => Some(format!(
            "call 'beamtalk_string_ops':'ends_with'(Self, {p0})"
        )),
        "indexOf:" => Some(format!("call 'beamtalk_string_ops':'index_of'(Self, {p0})")),
        // Splitting
        "split:" => Some(format!("call 'binary':'split'(Self, {p0}, ['global'])")),
        "splitOn:" => Some(format!("call 'beamtalk_string_ops':'split_on'(Self, {p0})")),
        "repeat:" => Some(format!("call 'beamtalk_string_ops':'repeat'(Self, {p0})")),
        // Lines and words
        "lines" => Some("call 'beamtalk_string_ops':'lines'(Self)".to_string()),
        "words" => Some("call 'beamtalk_string_ops':'words'(Self)".to_string()),
        // Replace
        "replaceAll:with:" => {
            let p1 = params.get(1).map_or("_Arg1", String::as_str);
            Some(format!(
                "call 'binary':'replace'(Self, {p0}, {p1}, ['global'])"
            ))
        }
        "replaceFirst:with:" => {
            let p1 = params.get(1).map_or("_Arg1", String::as_str);
            Some(format!("call 'binary':'replace'(Self, {p0}, {p1}, [])"))
        }
        // Substring
        "take:" => Some(format!("call 'beamtalk_string_ops':'take'(Self, {p0})")),
        "drop:" => Some(format!("call 'beamtalk_string_ops':'drop'(Self, {p0})")),
        // Padding
        "padLeft:" => Some(format!(
            "call 'unicode':'characters_to_binary'(call 'string':'pad'(Self, {p0}, 'leading'))"
        )),
        "padRight:" => Some(format!(
            "call 'unicode':'characters_to_binary'(call 'string':'pad'(Self, {p0}, 'trailing'))"
        )),
        "padLeft:with:" => {
            let p1 = params.get(1).map_or("_Arg1", String::as_str);
            Some(format!(
                "call 'unicode':'characters_to_binary'(call 'string':'pad'(Self, {p0}, 'leading', {p1}))"
            ))
        }
        // Testing
        "isBlank" => Some("call 'beamtalk_string_ops':'is_blank'(Self)".to_string()),
        "isDigit" => Some("call 'beamtalk_string_ops':'is_digit'(Self)".to_string()),
        "isAlpha" => Some("call 'beamtalk_string_ops':'is_alpha'(Self)".to_string()),
        // Conversion
        "asInteger" => Some("call 'erlang':'binary_to_integer'(Self)".to_string()),
        "asFloat" => Some("call 'erlang':'binary_to_float'(Self)".to_string()),
        "asAtom" => Some("call 'erlang':'binary_to_existing_atom'(Self, 'utf8')".to_string()),
        "asList" => Some("call 'beamtalk_string_ops':'as_list'(Self)".to_string()),
        // Iteration
        "each:" => Some(format!("call 'beamtalk_string_ops':'each'(Self, {p0})")),
        "collect:" => Some(format!("call 'beamtalk_string_ops':'collect'(Self, {p0})")),
        "select:" => Some(format!("call 'beamtalk_string_ops':'select'(Self, {p0})")),
        _ => None,
    }
}

/// Block primitive implementations.
fn generate_block_bif(selector: &str, params: &[String]) -> Option<String> {
    match selector {
        "arity" => {
            // erlang:fun_info(Self, arity) returns {arity, N}
            Some(
                "let <ArityTuple> = call 'erlang':'fun_info'(Self, 'arity') in \
                 call 'erlang':'element'(2, ArityTuple)"
                    .to_string(),
            )
        }
        "valueWithArguments:" => {
            let p0 = params.first().map_or("_Args", String::as_str);
            Some(format!("call 'erlang':'apply'(Self, {p0})"))
        }
        // on:do: and ensure: are structural intrinsics handled at the call site
        // (see control_flow/exception_handling.rs), not here.
        _ => None,
    }
}

/// File primitive implementations (BT-336).
///
/// File class methods delegate directly to `beamtalk_file` runtime module.
/// These are class-level methods (no Self parameter needed).
fn generate_file_bif(selector: &str, params: &[String]) -> Option<String> {
    let p0 = params.first().map_or("_Arg0", String::as_str);
    match selector {
        "exists:" => Some(format!("call 'beamtalk_file':'exists:'({p0})")),
        "readAll:" => Some(format!("call 'beamtalk_file':'readAll:'({p0})")),
        "writeAll:contents:" => {
            let p1 = params.get(1).map_or("_Arg1", String::as_str);
            Some(format!(
                "call 'beamtalk_file':'writeAll:contents:'({p0}, {p1})"
            ))
        }
        _ => None,
    }
}

/// Exception primitive implementations (BT-338).
///
/// Exception field access delegates to `beamtalk_exception_handler` runtime module.
/// This avoids naming conflict: compiled Exception.bt produces `beamtalk_exception`,
/// while the handler module provides the actual implementation.
fn generate_exception_bif(selector: &str, params: &[String]) -> Option<String> {
    match selector {
        "message" => {
            Some("call 'beamtalk_exception_handler':'dispatch'('message', [], Self)".to_string())
        }
        "hint" => {
            Some("call 'beamtalk_exception_handler':'dispatch'('hint', [], Self)".to_string())
        }
        "kind" => {
            Some("call 'beamtalk_exception_handler':'dispatch'('kind', [], Self)".to_string())
        }
        "selector" => {
            Some("call 'beamtalk_exception_handler':'dispatch'('selector', [], Self)".to_string())
        }
        "errorClass" => {
            Some("call 'beamtalk_exception_handler':'dispatch'('errorClass', [], Self)".to_string())
        }
        "printString" => Some(
            "call 'beamtalk_exception_handler':'dispatch'('printString', [], Self)".to_string(),
        ),
        "signal" => {
            Some("call 'beamtalk_exception_handler':'dispatch'('signal', [], Self)".to_string())
        }
        "signal:" => {
            let p0 = params.first().map_or("_Msg", String::as_str);
            Some(format!(
                "call 'beamtalk_exception_handler':'signal_message'({p0})"
            ))
        }
        _ => None,
    }
}

/// Symbol primitive implementations (BT-273).
///
/// Symbols are Erlang atoms — interned, immutable identifiers.
fn generate_symbol_bif(selector: &str, params: &[String]) -> Option<String> {
    match selector {
        // Comparison
        "=" => binary_bif("=:=", params),
        "~=" => binary_bif("=/=", params),
        // Conversion
        "asString" => Some("call 'erlang':'atom_to_binary'(Self, 'utf8')".to_string()),
        "asAtom" => {
            // Identity — symbols are already atoms
            Some("Self".to_string())
        }
        // Display — delegate to runtime primitive for consistent formatting
        "printString" => Some("call 'beamtalk_primitive':'print_string'(Self)".to_string()),
        // Identity
        "hash" => Some("call 'erlang':'phash2'(Self)".to_string()),
        _ => None,
    }
}

/// Character primitive implementations (BT-339).
///
/// Characters are integers (Unicode codepoints) at the BEAM level.
/// Simple operations use direct BIFs; predicates delegate to `beamtalk_character`.
fn generate_character_bif(selector: &str, params: &[String]) -> Option<String> {
    let p0 = params.first().map_or("_Arg0", String::as_str);
    match selector {
        // Comparison — direct integer comparison
        "=" => binary_bif("=:=", params),
        "<" => binary_bif("<", params),
        ">" => binary_bif(">", params),
        "<=" => binary_bif("=<", params),
        ">=" => binary_bif(">=", params),
        // Conversion
        "asInteger" => Some("Self".to_string()),
        "asString" => Some("call 'beamtalk_character':'as_string'(Self)".to_string()),
        // Display
        "printString" => Some("call 'beamtalk_character':'print_string'(Self)".to_string()),
        // Identity
        "hash" => Some("call 'erlang':'phash2'(Self)".to_string()),
        // Predicates — delegate to runtime module
        "isLetter" => Some("call 'beamtalk_character':'is_letter'(Self)".to_string()),
        "isDigit" => Some("call 'beamtalk_character':'is_digit'(Self)".to_string()),
        "isUppercase" => Some("call 'beamtalk_character':'is_uppercase'(Self)".to_string()),
        "isLowercase" => Some("call 'beamtalk_character':'is_lowercase'(Self)".to_string()),
        "isWhitespace" => Some("call 'beamtalk_character':'is_whitespace'(Self)".to_string()),
        // Case conversion
        "uppercase" => Some("call 'beamtalk_character':'to_uppercase'(Self)".to_string()),
        "lowercase" => Some("call 'beamtalk_character':'to_lowercase'(Self)".to_string()),
        // Factory class method
        "value:" => Some(format!("call 'beamtalk_character':'value'({p0})")),
        _ => None,
    }
}

/// Tuple primitive implementations (BT-417).
///
/// Tuples are Erlang tuples — immutable fixed-size collections, particularly
/// useful for Erlang interop with {ok, Value} and {error, Reason} patterns.
fn generate_tuple_bif(selector: &str, params: &[String]) -> Option<String> {
    match selector {
        "size" => Some("call 'erlang':'tuple_size'(Self)".to_string()),
        "at:" => {
            let p0 = params.first()?;
            Some(format!("call 'beamtalk_tuple_ops':'at'(Self, {p0})"))
        }
        "isOk" => Some(
            "case Self of <{'ok', _Value}> when 'true' -> 'true' <_> when 'true' -> 'false' end"
                .to_string(),
        ),
        "isError" => Some(
            "case Self of <{'error', _Reason}> when 'true' -> 'true' <_> when 'true' -> 'false' end"
                .to_string(),
        ),
        "unwrap" => {
            Some("call 'beamtalk_tuple_ops':'unwrap'(Self)".to_string())
        }
        "unwrapOr:" => {
            let p0 = params.first()?;
            Some(format!(
                "call 'beamtalk_tuple_ops':'unwrap_or'(Self, {p0})"
            ))
        }
        "unwrapOrElse:" => {
            let p0 = params.first()?;
            Some(format!(
                "call 'beamtalk_tuple_ops':'unwrap_or_else'(Self, {p0})"
            ))
        }
        "asString" => {
            Some("call 'beamtalk_tuple_ops':'as_string'(Self)".to_string())
        }
        _ => None,
    }
}

/// List primitive implementations (BT-419).
///
/// Lists are Erlang linked lists — fast prepend, sequential access.
/// Complex operations delegate to `beamtalk_list_ops` helper module.
#[allow(clippy::too_many_lines)]
fn generate_list_bif(selector: &str, params: &[String]) -> Option<String> {
    match selector {
        // Access
        "size" => Some("call 'erlang':'length'(Self)".to_string()),
        "isEmpty" => Some("call 'erlang':'=:='(Self, [])".to_string()),
        "first" => {
            let hint = core_erlang_binary_string("Cannot get first element of empty list");
            Some(format!(
                "case Self of \
                 <[H|_T]> when 'true' -> H \
                 <[]> when 'true' -> \
                   let Error0 = call 'beamtalk_error':'new'('does_not_understand', 'List') in \
                   let Error1 = call 'beamtalk_error':'with_selector'(Error0, 'first') in \
                   let Error2 = call 'beamtalk_error':'with_hint'(Error1, {hint}) in \
                   call 'beamtalk_error':'raise'(Error2) \
                 end"
            ))
        }
        "rest" => Some(
            "case Self of \
             <[_H|T]> when 'true' -> T \
             <[]> when 'true' -> [] \
             end"
            .to_string(),
        ),
        "last" => {
            let hint = core_erlang_binary_string("Cannot get last element of empty list");
            Some(format!(
                "case Self of \
                 <[]> when 'true' -> \
                   let Error0 = call 'beamtalk_error':'new'('does_not_understand', 'List') in \
                   let Error1 = call 'beamtalk_error':'with_selector'(Error0, 'last') in \
                   let Error2 = call 'beamtalk_error':'with_hint'(Error1, {hint}) in \
                   call 'beamtalk_error':'raise'(Error2) \
                 <_> when 'true' -> \
                   call 'lists':'last'(Self) \
                 end"
            ))
        }
        "at:" => {
            let p0 = params.first().map_or("_N", String::as_str);
            Some(format!("call 'beamtalk_list_ops':'at'(Self, {p0})"))
        }
        "includes:" => {
            let p0 = params.first().map_or("_Item", String::as_str);
            Some(format!("call 'lists':'member'({p0}, Self)"))
        }
        // Ordering
        "sort" => Some("call 'lists':'sort'(Self)".to_string()),
        "sort:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(format!("call 'beamtalk_list_ops':'sort_with'(Self, {p0})"))
        }
        "reversed" => Some("call 'lists':'reverse'(Self)".to_string()),
        "unique" => Some("call 'lists':'usort'(Self)".to_string()),
        // Search
        "detect:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(format!("call 'beamtalk_list_ops':'detect'(Self, {p0})"))
        }
        "detect:ifNone:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            let p1 = params.get(1).map_or("_Default", String::as_str);
            Some(format!(
                "call 'beamtalk_list_ops':'detect_if_none'(Self, {p0}, {p1})"
            ))
        }
        // Iteration
        "do:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(format!("call 'beamtalk_list_ops':'do'(Self, {p0})"))
        }
        "collect:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(format!("call 'lists':'map'({p0}, Self)"))
        }
        "select:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(format!("call 'lists':'filter'({p0}, Self)"))
        }
        "reject:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(format!("call 'beamtalk_list_ops':'reject'(Self, {p0})"))
        }
        "inject:into:" => {
            let p0 = params.first().map_or("_Initial", String::as_str);
            let p1 = params.get(1).map_or("_Block", String::as_str);
            Some(format!("call 'lists':'foldl'({p1}, {p0}, Self)"))
        }
        // Functional
        "take:" => {
            let p0 = params.first().map_or("_N", String::as_str);
            Some(format!("call 'beamtalk_list_ops':'take'(Self, {p0})"))
        }
        "drop:" => {
            let p0 = params.first().map_or("_N", String::as_str);
            Some(format!("call 'beamtalk_list_ops':'drop'(Self, {p0})"))
        }
        "flatten" => Some("call 'lists':'flatten'(Self)".to_string()),
        "flatMap:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(format!("call 'lists':'flatmap'({p0}, Self)"))
        }
        "count:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(format!(
                "call 'erlang':'length'(call 'lists':'filter'({p0}, Self))"
            ))
        }
        "anySatisfy:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(format!("call 'lists':'any'({p0}, Self)"))
        }
        "allSatisfy:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(format!("call 'lists':'all'({p0}, Self)"))
        }
        // Advanced
        "zip:" => {
            let p0 = params.first().map_or("_Other", String::as_str);
            Some(format!("call 'beamtalk_list_ops':'zip'(Self, {p0})"))
        }
        "groupBy:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(format!("call 'beamtalk_list_ops':'group_by'(Self, {p0})"))
        }
        "partition:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(format!("call 'beamtalk_list_ops':'partition'(Self, {p0})"))
        }
        "takeWhile:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(format!("call 'lists':'takewhile'({p0}, Self)"))
        }
        "dropWhile:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(format!("call 'lists':'dropwhile'({p0}, Self)"))
        }
        "intersperse:" => {
            let p0 = params.first().map_or("_Sep", String::as_str);
            Some(format!(
                "call 'beamtalk_list_ops':'intersperse'(Self, {p0})"
            ))
        }
        "add:" => {
            let p0 = params.first().map_or("_Item", String::as_str);
            Some(format!("call 'erlang':'++'(Self, [{p0}|[]])"))
        }
        // Concatenation
        "++" => {
            let p0 = params.first().map_or("_Other", String::as_str);
            Some(format!("call 'erlang':'++'(Self, {p0})"))
        }
        // Subsequence / Search
        "from:to:" => {
            let p0 = params.first().map_or("_Start", String::as_str);
            let p1 = params.get(1).map_or("_End", String::as_str);
            Some(format!(
                "call 'beamtalk_list_ops':'from_to'(Self, {p0}, {p1})"
            ))
        }
        "indexOf:" => {
            let p0 = params.first().map_or("_Item", String::as_str);
            Some(format!("call 'beamtalk_list_ops':'index_of'(Self, {p0})"))
        }
        // Iteration with index
        "eachWithIndex:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(format!(
                "call 'beamtalk_list_ops':'each_with_index'(Self, {p0})"
            ))
        }
        // Reflection
        "describe" => {
            let s = core_erlang_binary_string("a List");
            Some(s)
        }
        // Display
        "printString" => Some("call 'beamtalk_primitive':'print_string'(Self)".to_string()),
        _ => None,
    }
}

/// Dictionary primitive implementations (BT-418).
///
/// Dictionaries are Erlang maps — immutable key-value collections.
fn generate_dictionary_bif(selector: &str, params: &[String]) -> Option<String> {
    match selector {
        "size" => Some("call 'erlang':'map_size'(Self)".to_string()),
        "keys" => Some("call 'maps':'keys'(Self)".to_string()),
        "values" => Some("call 'maps':'values'(Self)".to_string()),
        "at:" => {
            let p0 = params.first().map_or("_Key", String::as_str);
            Some(format!("call 'maps':'get'({p0}, Self)"))
        }
        "at:ifAbsent:" => {
            let p0 = params.first().map_or("_Key", String::as_str);
            let p1 = params.get(1).map_or("_Block", String::as_str);
            Some(format!(
                "call 'beamtalk_map_ops':'at_if_absent'(Self, {p0}, {p1})"
            ))
        }
        "at:put:" => {
            let p0 = params.first().map_or("_Key", String::as_str);
            let p1 = params.get(1).map_or("_Value", String::as_str);
            Some(format!("call 'maps':'put'({p0}, {p1}, Self)"))
        }
        "includesKey:" => {
            let p0 = params.first().map_or("_Key", String::as_str);
            Some(format!("call 'maps':'is_key'({p0}, Self)"))
        }
        "removeKey:" => {
            let p0 = params.first().map_or("_Key", String::as_str);
            Some(format!("call 'maps':'remove'({p0}, Self)"))
        }
        "merge:" => {
            let p0 = params.first().map_or("_Other", String::as_str);
            Some(format!("call 'maps':'merge'(Self, {p0})"))
        }
        "keysAndValuesDo:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(format!(
                "call 'beamtalk_map_ops':'keys_and_values_do'(Self, {p0})"
            ))
        }
        _ => None,
    }
}

/// Object primitive implementations (BT-335).
///
/// Object is the root class — methods here are inherited by all objects.
fn generate_object_bif(selector: &str, params: &[String]) -> Option<String> {
    match selector {
        // Association creation: `self -> value` creates an Association tagged map
        "->" => {
            let p0 = params.first()?;
            Some(format!(
                "~{{'$beamtalk_class' => 'Association', 'key' => Self, 'value' => {p0}}}~"
            ))
        }
        _ => None,
    }
}

/// Association primitive implementations (BT-335).
///
/// Associations are key-value pairs represented as tagged maps.
fn generate_association_bif(selector: &str, _params: &[String]) -> Option<String> {
    match selector {
        "key" => Some("call 'maps':'get'('key', Self)".to_string()),
        "value" => Some("call 'maps':'get'('value', Self)".to_string()),
        "asString" => Some("call 'beamtalk_association':'format_string'(Self)".to_string()),
        _ => None,
    }
}

/// Set primitive implementations (BT-73).
///
/// Sets are represented as tagged maps: `#{'$beamtalk_class' => 'Set', elements => OrdsetData}`.
/// Operations delegate to `beamtalk_set_ops` helper module which wraps Erlang `ordsets`.
fn generate_set_bif(selector: &str, params: &[String]) -> Option<String> {
    match selector {
        "fromList:" => {
            let p0 = params.first().map_or("_List", String::as_str);
            Some(format!("call 'beamtalk_set_ops':'from_list'({p0})"))
        }
        "size" => Some("call 'beamtalk_set_ops':'size'(Self)".to_string()),
        "isEmpty" => Some("call 'beamtalk_set_ops':'is_empty'(Self)".to_string()),
        "includes:" => {
            let p0 = params.first().map_or("_Element", String::as_str);
            Some(format!("call 'beamtalk_set_ops':'includes'(Self, {p0})"))
        }
        "add:" => {
            let p0 = params.first().map_or("_Element", String::as_str);
            Some(format!("call 'beamtalk_set_ops':'add'(Self, {p0})"))
        }
        "remove:" => {
            let p0 = params.first().map_or("_Element", String::as_str);
            Some(format!("call 'beamtalk_set_ops':'remove'(Self, {p0})"))
        }
        "union:" => {
            let p0 = params.first().map_or("_Other", String::as_str);
            Some(format!("call 'beamtalk_set_ops':'union'(Self, {p0})"))
        }
        "intersection:" => {
            let p0 = params.first().map_or("_Other", String::as_str);
            Some(format!(
                "call 'beamtalk_set_ops':'intersection'(Self, {p0})"
            ))
        }
        "difference:" => {
            let p0 = params.first().map_or("_Other", String::as_str);
            Some(format!("call 'beamtalk_set_ops':'difference'(Self, {p0})"))
        }
        "isSubsetOf:" => {
            let p0 = params.first().map_or("_Other", String::as_str);
            Some(format!(
                "call 'beamtalk_set_ops':'is_subset_of'(Self, {p0})"
            ))
        }
        "asList" => Some("call 'beamtalk_set_ops':'as_list'(Self)".to_string()),
        "do:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(format!("call 'beamtalk_set_ops':'do'(Self, {p0})"))
        }
        "printString" => {
            // BT-477: Delegate to beamtalk_primitive:print_string/1 which
            // formats Sets as "Set(element1, element2, ...)"
            Some("call 'beamtalk_primitive':'print_string'(Self)".to_string())
        }
        _ => None,
    }
}

// Helper functions for generating common patterns

/// Encodes a string as a Core Erlang binary literal.
///
/// Core Erlang represents binaries as: `#{#<byte>(8,1,'integer',['unsigned'|['big']]), ...}#`
fn core_erlang_binary_string(s: &str) -> String {
    if s.is_empty() {
        return "#{}#".to_string();
    }
    let segments: Vec<String> = s
        .bytes()
        .map(|b| format!("#<{b}>(8,1,'integer',['unsigned'|['big']])"))
        .collect();
    format!("#{{{}}}#", segments.join(","))
}

/// Returns a binary BIF call: `call 'erlang':'op'(Self, Param0)`
fn binary_bif(erlang_op: &str, params: &[String]) -> Option<String> {
    let p0 = params.first()?;
    Some(format!("call 'erlang':'{erlang_op}'(Self, {p0})"))
}

/// Returns power implementation: `call 'math':'pow'(Self, Param0)`
fn power_bif(params: &[String]) -> Option<String> {
    let p0 = params.first()?;
    Some(format!(
        "call 'erlang':'round'(call 'math':'pow'(\
         call 'erlang':'float'(Self), call 'erlang':'float'({p0})))"
    ))
}

/// `CompiledMethod` primitive implementations.
///
/// All selectors delegate to `beamtalk_compiled_method_ops:dispatch/3`
/// (the hand-written Erlang runtime helper) to avoid recursion through
/// the compiled stdlib module's own dispatch/3.
fn generate_compiled_method_bif(selector: &str, params: &[String]) -> Option<String> {
    match selector {
        "selector" | "source" | "argumentCount" | "printString" | "asString" => {
            ops_dispatch("beamtalk_compiled_method_ops", selector, params)
        }
        _ => None,
    }
}

/// Generates a call to a `_ops` Erlang module's dispatch/3 function.
#[allow(clippy::unnecessary_wraps)]
fn ops_dispatch(module: &str, selector: &str, params: &[String]) -> Option<String> {
    let mut result = format!("call '{module}':'dispatch'('{selector}', [");
    for (i, param) in params.iter().enumerate() {
        if i > 0 {
            result.push_str(", ");
        }
        result.push_str(param);
    }
    result.push_str("], Self)");
    Some(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_integer_plus() {
        let result = generate_primitive_bif("Integer", "+", &["Other".to_string()]);
        assert_eq!(result, Some("call 'erlang':'+'(Self, Other)".to_string()));
    }

    #[test]
    fn test_integer_modulo() {
        let result = generate_primitive_bif("Integer", "%", &["Other".to_string()]);
        assert_eq!(result, Some("call 'erlang':'rem'(Self, Other)".to_string()));
    }

    #[test]
    fn test_integer_as_string() {
        let result = generate_primitive_bif("Integer", "asString", &[]);
        assert_eq!(
            result,
            Some("call 'erlang':'integer_to_binary'(Self)".to_string())
        );
    }

    #[test]
    fn test_string_length() {
        let result = generate_primitive_bif("String", "length", &[]);
        assert_eq!(result, Some("call 'string':'length'(Self)".to_string()));
    }

    #[test]
    fn test_string_concat() {
        let result = generate_primitive_bif("String", "++", &["Other".to_string()]);
        let output = result.unwrap();
        assert!(output.contains("iolist_to_binary"));
    }

    #[test]
    fn test_unknown_class() {
        let result = generate_primitive_bif("Counter", "+", &["Other".to_string()]);
        assert!(result.is_none());
    }

    #[test]
    fn test_unknown_selector() {
        let result = generate_primitive_bif("Integer", "unknownMethod", &[]);
        assert!(result.is_none());
    }

    #[test]
    fn test_float_as_string() {
        let result = generate_primitive_bif("Float", "asString", &[]);
        assert_eq!(
            result,
            Some("call 'erlang':'float_to_binary'(Self, ['short'])".to_string())
        );
    }

    #[test]
    fn test_float_rounded() {
        let result = generate_primitive_bif("Float", "rounded", &[]);
        assert_eq!(result, Some("call 'erlang':'round'(Self)".to_string()));
    }

    #[test]
    fn test_float_ceiling() {
        let result = generate_primitive_bif("Float", "ceiling", &[]);
        assert_eq!(result, Some("call 'erlang':'ceil'(Self)".to_string()));
    }

    #[test]
    fn test_float_floor() {
        let result = generate_primitive_bif("Float", "floor", &[]);
        assert_eq!(result, Some("call 'erlang':'floor'(Self)".to_string()));
    }

    #[test]
    fn test_float_truncated() {
        let result = generate_primitive_bif("Float", "truncated", &[]);
        assert_eq!(result, Some("call 'erlang':'trunc'(Self)".to_string()));
    }

    #[test]
    fn test_float_as_integer() {
        let result = generate_primitive_bif("Float", "asInteger", &[]);
        assert_eq!(result, Some("call 'erlang':'trunc'(Self)".to_string()));
    }

    #[test]
    fn test_file_exists() {
        let result = generate_primitive_bif("File", "exists:", &["Path".to_string()]);
        assert_eq!(
            result,
            Some("call 'beamtalk_file':'exists:'(Path)".to_string())
        );
    }

    #[test]
    fn test_file_read_all() {
        let result = generate_primitive_bif("File", "readAll:", &["Path".to_string()]);
        assert_eq!(
            result,
            Some("call 'beamtalk_file':'readAll:'(Path)".to_string())
        );
    }

    #[test]
    fn test_file_write_all_contents() {
        let result = generate_primitive_bif(
            "File",
            "writeAll:contents:",
            &["Path".to_string(), "Text".to_string()],
        );
        assert_eq!(
            result,
            Some("call 'beamtalk_file':'writeAll:contents:'(Path, Text)".to_string())
        );
    }

    #[test]
    fn test_symbol_as_string() {
        let result = generate_primitive_bif("Symbol", "asString", &[]);
        assert_eq!(
            result,
            Some("call 'erlang':'atom_to_binary'(Self, 'utf8')".to_string())
        );
    }

    #[test]
    fn test_symbol_as_atom() {
        let result = generate_primitive_bif("Symbol", "asAtom", &[]);
        assert_eq!(result, Some("Self".to_string()));
    }

    #[test]
    fn test_symbol_equality() {
        let result = generate_primitive_bif("Symbol", "=", &["Other".to_string()]);
        assert_eq!(result, Some("call 'erlang':'=:='(Self, Other)".to_string()));
    }

    #[test]
    fn test_symbol_hash() {
        let result = generate_primitive_bif("Symbol", "hash", &[]);
        assert_eq!(result, Some("call 'erlang':'phash2'(Self)".to_string()));
    }

    #[test]
    fn test_symbol_print_string() {
        let result = generate_primitive_bif("Symbol", "printString", &[]);
        assert_eq!(
            result,
            Some("call 'beamtalk_primitive':'print_string'(Self)".to_string())
        );
    }

    #[test]
    fn test_list_concat() {
        let result = generate_primitive_bif("List", "++", &["Other".to_string()]);
        assert_eq!(result, Some("call 'erlang':'++'(Self, Other)".to_string()));
    }

    #[test]
    fn test_list_from_to() {
        let result = generate_primitive_bif(
            "List",
            "from:to:",
            &["Start".to_string(), "End".to_string()],
        );
        assert_eq!(
            result,
            Some("call 'beamtalk_list_ops':'from_to'(Self, Start, End)".to_string())
        );
    }

    #[test]
    fn test_list_index_of() {
        let result = generate_primitive_bif("List", "indexOf:", &["Item".to_string()]);
        assert_eq!(
            result,
            Some("call 'beamtalk_list_ops':'index_of'(Self, Item)".to_string())
        );
    }

    #[test]
    fn test_list_each_with_index() {
        let result = generate_primitive_bif("List", "eachWithIndex:", &["Block".to_string()]);
        assert_eq!(
            result,
            Some("call 'beamtalk_list_ops':'each_with_index'(Self, Block)".to_string())
        );
    }

    // Character primitive tests (BT-339)

    #[test]
    fn test_character_as_integer() {
        let result = generate_primitive_bif("Character", "asInteger", &[]);
        assert_eq!(result, Some("Self".to_string()));
    }

    #[test]
    fn test_character_as_string() {
        let result = generate_primitive_bif("Character", "asString", &[]);
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'as_string'(Self)".to_string())
        );
    }

    #[test]
    fn test_character_print_string() {
        let result = generate_primitive_bif("Character", "printString", &[]);
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'print_string'(Self)".to_string())
        );
    }

    #[test]
    fn test_character_equality() {
        let result = generate_primitive_bif("Character", "=", &["Other".to_string()]);
        assert_eq!(result, Some("call 'erlang':'=:='(Self, Other)".to_string()));
    }

    #[test]
    fn test_character_is_letter() {
        let result = generate_primitive_bif("Character", "isLetter", &[]);
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'is_letter'(Self)".to_string())
        );
    }

    #[test]
    fn test_character_is_digit() {
        let result = generate_primitive_bif("Character", "isDigit", &[]);
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'is_digit'(Self)".to_string())
        );
    }

    #[test]
    fn test_character_value_factory() {
        let result = generate_primitive_bif("Character", "value:", &["CP".to_string()]);
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'value'(CP)".to_string())
        );
    }

    #[test]
    fn test_character_hash() {
        let result = generate_primitive_bif("Character", "hash", &[]);
        assert_eq!(result, Some("call 'erlang':'phash2'(Self)".to_string()));
    }

    #[test]
    fn test_character_is_uppercase() {
        let result = generate_primitive_bif("Character", "isUppercase", &[]);
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'is_uppercase'(Self)".to_string())
        );
    }

    #[test]
    fn test_character_is_lowercase() {
        let result = generate_primitive_bif("Character", "isLowercase", &[]);
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'is_lowercase'(Self)".to_string())
        );
    }

    #[test]
    fn test_character_is_whitespace() {
        let result = generate_primitive_bif("Character", "isWhitespace", &[]);
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'is_whitespace'(Self)".to_string())
        );
    }

    #[test]
    fn test_character_uppercase() {
        let result = generate_primitive_bif("Character", "uppercase", &[]);
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'to_uppercase'(Self)".to_string())
        );
    }

    #[test]
    fn test_character_lowercase() {
        let result = generate_primitive_bif("Character", "lowercase", &[]);
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'to_lowercase'(Self)".to_string())
        );
    }

    #[test]
    fn test_character_comparison() {
        let result = generate_primitive_bif("Character", "<", &["Other".to_string()]);
        assert_eq!(result, Some("call 'erlang':'<'(Self, Other)".to_string()));
    }

    // Integer character predicate tests (BT-339)

    #[test]
    fn test_integer_is_letter() {
        let result = generate_primitive_bif("Integer", "isLetter", &[]);
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'is_letter'(Self)".to_string())
        );
    }

    #[test]
    fn test_integer_is_digit() {
        let result = generate_primitive_bif("Integer", "isDigit", &[]);
        assert_eq!(
            result,
            Some("call 'beamtalk_character':'is_digit'(Self)".to_string())
        );
    }
}
