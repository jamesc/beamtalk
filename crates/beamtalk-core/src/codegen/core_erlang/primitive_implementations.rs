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

use std::fmt::Write;

/// Generates Core Erlang for a selector-based primitive implementation.
///
/// Returns `Some(())` if a direct implementation was emitted, `None` if
/// the selector has no known BIF mapping (e.g., unimplemented methods).
///
/// When `None` is returned, the caller should fall back to generating a
/// `does_not_understand` error call.
///
/// # Arguments
///
/// * `output` - The output buffer to write Core Erlang into
/// * `class_name` - The class context (e.g., "Integer", "String")
/// * `selector` - The primitive selector (e.g., "+", "length")
/// * `params` - The method parameters (excluding Self)
pub fn generate_primitive_bif(
    output: &mut String,
    class_name: &str,
    selector: &str,
    params: &[String],
) -> Option<()> {
    match class_name {
        "Integer" => generate_integer_bif(output, selector, params),
        "Float" => generate_float_bif(output, selector, params),
        "String" => generate_string_bif(output, selector, params),
        "Block" => generate_block_bif(output, selector, params),
        "File" => generate_file_bif(output, selector, params),
        "Exception" => generate_exception_bif(output, selector, params),
        "Symbol" => generate_symbol_bif(output, selector, params),
        "Tuple" => generate_tuple_bif(output, selector, params),
        "List" => generate_list_bif(output, selector, params),
        "Dictionary" => generate_dictionary_bif(output, selector, params),
        "Object" => generate_object_bif(output, selector, params),
        "Association" => generate_association_bif(output, selector, params),
        "Set" => generate_set_bif(output, selector, params),
        _ => None,
    }
}

/// Integer primitive implementations.
fn generate_integer_bif(output: &mut String, selector: &str, params: &[String]) -> Option<()> {
    match selector {
        // Arithmetic — direct Erlang BIF
        "+" => write_binary_bif(output, "+", params),
        "-" => write_binary_bif(output, "-", params),
        "*" => write_binary_bif(output, "*", params),
        "/" => write_binary_bif(output, "/", params),
        "%" => write_binary_bif(output, "rem", params),
        "**" => write_power_bif(output, params),
        // Comparison
        "=" => write_binary_bif(output, "=:=", params),
        "<" => write_binary_bif(output, "<", params),
        ">" => write_binary_bif(output, ">", params),
        "<=" => write_binary_bif(output, "=<", params),
        ">=" => write_binary_bif(output, ">=", params),
        // Conversion
        "asString" | "printString" => {
            write!(output, "call 'erlang':'integer_to_binary'(Self)").ok()?;
            Some(())
        }
        "asFloat" => {
            write!(output, "call 'erlang':'float'(Self)").ok()?;
            Some(())
        }
        // Bitwise operations
        "bitAnd:" => write_binary_bif(output, "band", params),
        "bitOr:" => write_binary_bif(output, "bor", params),
        "bitXor:" => write_binary_bif(output, "bxor", params),
        "bitShift:" => {
            // Positive N shifts left, negative shifts right
            let p0 = params.first()?;
            write!(
                output,
                "case call 'erlang':'>='({p0}, 0) of \
                 'true' when 'true' -> call 'erlang':'bsl'(Self, {p0}) \
                 'false' when 'true' -> call 'erlang':'bsr'(Self, call 'erlang':'-'(0, {p0})) end"
            )
            .ok()?;
            Some(())
        }
        "bitNot" => {
            write!(output, "call 'erlang':'bnot'(Self)").ok()?;
            Some(())
        }
        _ => None,
    }
}

/// Float primitive implementations.
fn generate_float_bif(output: &mut String, selector: &str, params: &[String]) -> Option<()> {
    match selector {
        // Arithmetic — direct Erlang BIF
        "+" => write_binary_bif(output, "+", params),
        "-" => write_binary_bif(output, "-", params),
        "*" => write_binary_bif(output, "*", params),
        "/" => write_binary_bif(output, "/", params),
        // Comparison
        "=" => write_binary_bif(output, "=:=", params),
        "<" => write_binary_bif(output, "<", params),
        ">" => write_binary_bif(output, ">", params),
        "<=" => write_binary_bif(output, "=<", params),
        ">=" => write_binary_bif(output, ">=", params),
        // Rounding
        "rounded" => {
            write!(output, "call 'erlang':'round'(Self)").ok()?;
            Some(())
        }
        "ceiling" => {
            write!(output, "call 'erlang':'ceil'(Self)").ok()?;
            Some(())
        }
        "floor" => {
            write!(output, "call 'erlang':'floor'(Self)").ok()?;
            Some(())
        }
        "truncated" | "asInteger" => {
            write!(output, "call 'erlang':'trunc'(Self)").ok()?;
            Some(())
        }
        // Conversion
        "asString" | "printString" => {
            write!(output, "call 'erlang':'float_to_binary'(Self, ['short'])").ok()?;
            Some(())
        }
        _ => None,
    }
}

/// String primitive implementations.
#[allow(clippy::too_many_lines)]
fn generate_string_bif(output: &mut String, selector: &str, params: &[String]) -> Option<()> {
    let p0 = params.first().map_or("_Arg0", String::as_str);
    match selector {
        // Comparison
        "=" => write_binary_bif(output, "=:=", params),
        "<" => write_binary_bif(output, "<", params),
        ">" => write_binary_bif(output, ">", params),
        "<=" => write_binary_bif(output, "=<", params),
        ">=" => write_binary_bif(output, ">=", params),
        // Concatenation — direct iolist concat (binaries are valid iolists)
        "++" => {
            write!(output, "call 'erlang':'iolist_to_binary'([Self, {p0}])").ok()?;
            Some(())
        }
        // Length
        "length" => {
            write!(output, "call 'string':'length'(Self)").ok()?;
            Some(())
        }
        // Access
        "at:" => {
            // 1-based grapheme access — delegate to runtime helper
            write!(output, "call 'beamtalk_string_ops':'at'(Self, {p0})").ok()?;
            Some(())
        }
        // Case transformation
        "uppercase" => {
            write!(
                output,
                "call 'unicode':'characters_to_binary'(call 'string':'uppercase'(Self))"
            )
            .ok()?;
            Some(())
        }
        "lowercase" => {
            write!(
                output,
                "call 'unicode':'characters_to_binary'(call 'string':'lowercase'(Self))"
            )
            .ok()?;
            Some(())
        }
        "capitalize" => {
            write!(output, "call 'beamtalk_string_ops':'capitalize'(Self)").ok()?;
            Some(())
        }
        // Whitespace
        "trim" => {
            write!(
                output,
                "call 'unicode':'characters_to_binary'(call 'string':'trim'(Self, 'both'))"
            )
            .ok()?;
            Some(())
        }
        "trimLeft" => {
            write!(
                output,
                "call 'unicode':'characters_to_binary'(call 'string':'trim'(Self, 'leading'))"
            )
            .ok()?;
            Some(())
        }
        "trimRight" => {
            write!(
                output,
                "call 'unicode':'characters_to_binary'(call 'string':'trim'(Self, 'trailing'))"
            )
            .ok()?;
            Some(())
        }
        // Reverse
        "reverse" => {
            write!(output, "call 'beamtalk_string_ops':'reverse'(Self)").ok()?;
            Some(())
        }
        // Search
        "includes:" => {
            write!(output, "call 'beamtalk_string_ops':'includes'(Self, {p0})").ok()?;
            Some(())
        }
        "startsWith:" => {
            write!(
                output,
                "call 'beamtalk_string_ops':'starts_with'(Self, {p0})"
            )
            .ok()?;
            Some(())
        }
        "endsWith:" => {
            write!(output, "call 'beamtalk_string_ops':'ends_with'(Self, {p0})").ok()?;
            Some(())
        }
        "indexOf:" => {
            write!(output, "call 'beamtalk_string_ops':'index_of'(Self, {p0})").ok()?;
            Some(())
        }
        // Splitting
        "split:" => {
            write!(output, "call 'binary':'split'(Self, {p0}, ['global'])").ok()?;
            Some(())
        }
        "splitOn:" => {
            write!(output, "call 'beamtalk_string_ops':'split_on'(Self, {p0})").ok()?;
            Some(())
        }
        "repeat:" => {
            write!(output, "call 'beamtalk_string_ops':'repeat'(Self, {p0})").ok()?;
            Some(())
        }
        // Conversion
        "asInteger" => {
            write!(output, "call 'erlang':'binary_to_integer'(Self)").ok()?;
            Some(())
        }
        "asFloat" => {
            write!(output, "call 'erlang':'binary_to_float'(Self)").ok()?;
            Some(())
        }
        "asAtom" => {
            write!(
                output,
                "call 'erlang':'binary_to_existing_atom'(Self, 'utf8')"
            )
            .ok()?;
            Some(())
        }
        "asList" => {
            write!(output, "call 'beamtalk_string_ops':'as_list'(Self)").ok()?;
            Some(())
        }
        // Iteration
        "each:" => {
            write!(output, "call 'beamtalk_string_ops':'each'(Self, {p0})").ok()?;
            Some(())
        }
        "collect:" => {
            write!(output, "call 'beamtalk_string_ops':'collect'(Self, {p0})").ok()?;
            Some(())
        }
        "select:" => {
            write!(output, "call 'beamtalk_string_ops':'select'(Self, {p0})").ok()?;
            Some(())
        }
        _ => None,
    }
}

/// Block primitive implementations.
fn generate_block_bif(output: &mut String, selector: &str, params: &[String]) -> Option<()> {
    match selector {
        "arity" => {
            // erlang:fun_info(Self, arity) returns {arity, N}
            write!(
                output,
                "let <ArityTuple> = call 'erlang':'fun_info'(Self, 'arity') in \
                 call 'erlang':'element'(2, ArityTuple)"
            )
            .ok()?;
            Some(())
        }
        "valueWithArguments:" => {
            let p0 = params.first().map_or("_Args", String::as_str);
            write!(output, "call 'erlang':'apply'(Self, {p0})").ok()?;
            Some(())
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
fn generate_file_bif(output: &mut String, selector: &str, params: &[String]) -> Option<()> {
    let p0 = params.first().map_or("_Arg0", String::as_str);
    match selector {
        "exists:" => {
            write!(output, "call 'beamtalk_file':'exists:'({p0})").ok()?;
            Some(())
        }
        "readAll:" => {
            write!(output, "call 'beamtalk_file':'readAll:'({p0})").ok()?;
            Some(())
        }
        "writeAll:contents:" => {
            let p1 = params.get(1).map_or("_Arg1", String::as_str);
            write!(
                output,
                "call 'beamtalk_file':'writeAll:contents:'({p0}, {p1})"
            )
            .ok()?;
            Some(())
        }
        _ => None,
    }
}

/// Exception primitive implementations (BT-338).
///
/// Exception field access delegates to `beamtalk_exception_handler` runtime module.
/// This avoids naming conflict: compiled Exception.bt produces `beamtalk_exception`,
/// while the handler module provides the actual implementation.
fn generate_exception_bif(output: &mut String, selector: &str, params: &[String]) -> Option<()> {
    match selector {
        "message" => {
            write!(
                output,
                "call 'beamtalk_exception_handler':'dispatch'('message', [], Self)"
            )
            .ok()?;
            Some(())
        }
        "hint" => {
            write!(
                output,
                "call 'beamtalk_exception_handler':'dispatch'('hint', [], Self)"
            )
            .ok()?;
            Some(())
        }
        "kind" => {
            write!(
                output,
                "call 'beamtalk_exception_handler':'dispatch'('kind', [], Self)"
            )
            .ok()?;
            Some(())
        }
        "selector" => {
            write!(
                output,
                "call 'beamtalk_exception_handler':'dispatch'('selector', [], Self)"
            )
            .ok()?;
            Some(())
        }
        "errorClass" => {
            write!(
                output,
                "call 'beamtalk_exception_handler':'dispatch'('errorClass', [], Self)"
            )
            .ok()?;
            Some(())
        }
        "printString" => {
            write!(
                output,
                "call 'beamtalk_exception_handler':'dispatch'('printString', [], Self)"
            )
            .ok()?;
            Some(())
        }
        "signal" => {
            write!(
                output,
                "call 'beamtalk_exception_handler':'dispatch'('signal', [], Self)"
            )
            .ok()?;
            Some(())
        }
        "signal:" => {
            let p0 = params.first().map_or("_Msg", String::as_str);
            write!(
                output,
                "call 'beamtalk_exception_handler':'signal_message'({p0})"
            )
            .ok()?;
            Some(())
        }
        _ => None,
    }
}

/// Symbol primitive implementations (BT-273).
///
/// Symbols are Erlang atoms — interned, immutable identifiers.
fn generate_symbol_bif(output: &mut String, selector: &str, params: &[String]) -> Option<()> {
    match selector {
        // Comparison
        "=" => write_binary_bif(output, "=:=", params),
        "~=" => write_binary_bif(output, "=/=", params),
        // Conversion
        "asString" => {
            write!(output, "call 'erlang':'atom_to_binary'(Self, 'utf8')").ok()?;
            Some(())
        }
        "asAtom" => {
            // Identity — symbols are already atoms
            write!(output, "Self").ok()?;
            Some(())
        }
        // Display — delegate to runtime primitive for consistent formatting
        "printString" => {
            write!(output, "call 'beamtalk_primitive':'print_string'(Self)").ok()?;
            Some(())
        }
        // Identity
        "hash" => {
            write!(output, "call 'erlang':'phash2'(Self)").ok()?;
            Some(())
        }
        _ => None,
    }
}

/// Tuple primitive implementations (BT-417).
///
/// Tuples are Erlang tuples — immutable fixed-size collections, particularly
/// useful for Erlang interop with {ok, Value} and {error, Reason} patterns.
fn generate_tuple_bif(output: &mut String, selector: &str, params: &[String]) -> Option<()> {
    match selector {
        "size" => {
            write!(output, "call 'erlang':'tuple_size'(Self)").ok()?;
            Some(())
        }
        "at:" => {
            let p0 = params.first()?;
            write!(output, "call 'beamtalk_tuple_ops':'at'(Self, {p0})").ok()?;
            Some(())
        }
        "isOk" => {
            write!(
                output,
                "case Self of <{{'ok', _Value}}> when 'true' -> 'true' <_> when 'true' -> 'false' end"
            )
            .ok()?;
            Some(())
        }
        "isError" => {
            write!(
                output,
                "case Self of <{{'error', _Reason}}> when 'true' -> 'true' <_> when 'true' -> 'false' end"
            )
            .ok()?;
            Some(())
        }
        "unwrap" => {
            write!(output, "call 'beamtalk_tuple_ops':'unwrap'(Self)").ok()?;
            Some(())
        }
        "unwrapOr:" => {
            let p0 = params.first()?;
            write!(output, "call 'beamtalk_tuple_ops':'unwrap_or'(Self, {p0})").ok()?;
            Some(())
        }
        "unwrapOrElse:" => {
            let p0 = params.first()?;
            write!(
                output,
                "call 'beamtalk_tuple_ops':'unwrap_or_else'(Self, {p0})"
            )
            .ok()?;
            Some(())
        }
        "asString" => {
            write!(output, "call 'beamtalk_tuple_ops':'as_string'(Self)").ok()?;
            Some(())
        }
        _ => None,
    }
}

/// List primitive implementations (BT-419).
///
/// Lists are Erlang linked lists — fast prepend, sequential access.
/// Complex operations delegate to `beamtalk_list_ops` helper module.
#[allow(clippy::too_many_lines)]
fn generate_list_bif(output: &mut String, selector: &str, params: &[String]) -> Option<()> {
    match selector {
        // Access
        "size" => {
            write!(output, "call 'erlang':'length'(Self)").ok()?;
            Some(())
        }
        "isEmpty" => {
            write!(output, "call 'erlang':'=:='(Self, [])").ok()?;
            Some(())
        }
        "first" => {
            let hint = core_erlang_binary_string("Cannot get first element of empty list");
            write!(
                output,
                "case Self of \
                 <[H|_T]> when 'true' -> H \
                 <[]> when 'true' -> \
                   let Error0 = call 'beamtalk_error':'new'('does_not_understand', 'List') in \
                   let Error1 = call 'beamtalk_error':'with_selector'(Error0, 'first') in \
                   let Error2 = call 'beamtalk_error':'with_hint'(Error1, {hint}) in \
                   call 'erlang':'error'(Error2) \
                 end"
            )
            .ok()?;
            Some(())
        }
        "rest" => {
            write!(
                output,
                "case Self of \
                 <[_H|T]> when 'true' -> T \
                 <[]> when 'true' -> [] \
                 end"
            )
            .ok()?;
            Some(())
        }
        "last" => {
            let hint = core_erlang_binary_string("Cannot get last element of empty list");
            write!(
                output,
                "case Self of \
                 <[]> when 'true' -> \
                   let Error0 = call 'beamtalk_error':'new'('does_not_understand', 'List') in \
                   let Error1 = call 'beamtalk_error':'with_selector'(Error0, 'last') in \
                   let Error2 = call 'beamtalk_error':'with_hint'(Error1, {hint}) in \
                   call 'erlang':'error'(Error2) \
                 <_> when 'true' -> \
                   call 'lists':'last'(Self) \
                 end"
            )
            .ok()?;
            Some(())
        }
        "at:" => {
            let p0 = params.first().map_or("_N", String::as_str);
            write!(output, "call 'beamtalk_list_ops':'at'(Self, {p0})").ok()?;
            Some(())
        }
        "includes:" => {
            let p0 = params.first().map_or("_Item", String::as_str);
            write!(output, "call 'lists':'member'({p0}, Self)").ok()?;
            Some(())
        }
        // Ordering
        "sort" => {
            write!(output, "call 'lists':'sort'(Self)").ok()?;
            Some(())
        }
        "sort:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            write!(output, "call 'beamtalk_list_ops':'sort_with'(Self, {p0})").ok()?;
            Some(())
        }
        "reversed" => {
            write!(output, "call 'lists':'reverse'(Self)").ok()?;
            Some(())
        }
        "unique" => {
            write!(output, "call 'lists':'usort'(Self)").ok()?;
            Some(())
        }
        // Search
        "detect:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            write!(output, "call 'beamtalk_list_ops':'detect'(Self, {p0})").ok()?;
            Some(())
        }
        "detect:ifNone:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            let p1 = params.get(1).map_or("_Default", String::as_str);
            write!(
                output,
                "call 'beamtalk_list_ops':'detect_if_none'(Self, {p0}, {p1})"
            )
            .ok()?;
            Some(())
        }
        // Iteration
        "do:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            write!(output, "call 'beamtalk_list_ops':'do'(Self, {p0})").ok()?;
            Some(())
        }
        "collect:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            write!(output, "call 'lists':'map'({p0}, Self)").ok()?;
            Some(())
        }
        "select:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            write!(output, "call 'lists':'filter'({p0}, Self)").ok()?;
            Some(())
        }
        "reject:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            write!(output, "call 'beamtalk_list_ops':'reject'(Self, {p0})").ok()?;
            Some(())
        }
        "inject:into:" => {
            let p0 = params.first().map_or("_Initial", String::as_str);
            let p1 = params.get(1).map_or("_Block", String::as_str);
            write!(output, "call 'lists':'foldl'({p1}, {p0}, Self)").ok()?;
            Some(())
        }
        // Functional
        "take:" => {
            let p0 = params.first().map_or("_N", String::as_str);
            write!(output, "call 'beamtalk_list_ops':'take'(Self, {p0})").ok()?;
            Some(())
        }
        "drop:" => {
            let p0 = params.first().map_or("_N", String::as_str);
            write!(output, "call 'beamtalk_list_ops':'drop'(Self, {p0})").ok()?;
            Some(())
        }
        "flatten" => {
            write!(output, "call 'lists':'flatten'(Self)").ok()?;
            Some(())
        }
        "flatMap:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            write!(output, "call 'lists':'flatmap'({p0}, Self)").ok()?;
            Some(())
        }
        "count:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            write!(
                output,
                "call 'erlang':'length'(call 'lists':'filter'({p0}, Self))"
            )
            .ok()?;
            Some(())
        }
        "anySatisfy:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            write!(output, "call 'lists':'any'({p0}, Self)").ok()?;
            Some(())
        }
        "allSatisfy:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            write!(output, "call 'lists':'all'({p0}, Self)").ok()?;
            Some(())
        }
        // Advanced
        "zip:" => {
            let p0 = params.first().map_or("_Other", String::as_str);
            write!(output, "call 'beamtalk_list_ops':'zip'(Self, {p0})").ok()?;
            Some(())
        }
        "groupBy:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            write!(output, "call 'beamtalk_list_ops':'group_by'(Self, {p0})").ok()?;
            Some(())
        }
        "partition:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            write!(output, "call 'beamtalk_list_ops':'partition'(Self, {p0})").ok()?;
            Some(())
        }
        "takeWhile:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            write!(output, "call 'lists':'takewhile'({p0}, Self)").ok()?;
            Some(())
        }
        "dropWhile:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            write!(output, "call 'lists':'dropwhile'({p0}, Self)").ok()?;
            Some(())
        }
        "intersperse:" => {
            let p0 = params.first().map_or("_Sep", String::as_str);
            write!(output, "call 'beamtalk_list_ops':'intersperse'(Self, {p0})").ok()?;
            Some(())
        }
        "add:" => {
            let p0 = params.first().map_or("_Item", String::as_str);
            write!(output, "call 'erlang':'++'(Self, [{p0}|[]])").ok()?;
            Some(())
        }
        // Concatenation
        "++" => {
            let p0 = params.first().map_or("_Other", String::as_str);
            write!(output, "call 'erlang':'++'(Self, {p0})").ok()?;
            Some(())
        }
        // Subsequence / Search
        "from:to:" => {
            let p0 = params.first().map_or("_Start", String::as_str);
            let p1 = params.get(1).map_or("_End", String::as_str);
            write!(output, "call 'beamtalk_list_ops':'from_to'(Self, {p0}, {p1})").ok()?;
            Some(())
        }
        "indexOf:" => {
            let p0 = params.first().map_or("_Item", String::as_str);
            write!(output, "call 'beamtalk_list_ops':'index_of'(Self, {p0})").ok()?;
            Some(())
        }
        // Iteration with index
        "eachWithIndex:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            write!(output, "call 'beamtalk_list_ops':'each_with_index'(Self, {p0})").ok()?;
            Some(())
        }
        // Reflection
        "describe" => {
            let s = core_erlang_binary_string("a List");
            write!(output, "{s}").ok()?;
            Some(())
        }
        // Display
        "printString" => {
            write!(output, "call 'beamtalk_primitive':'print_string'(Self)").ok()?;
            Some(())
        }
        _ => None,
    }
}

/// Dictionary primitive implementations (BT-418).
///
/// Dictionaries are Erlang maps — immutable key-value collections.
fn generate_dictionary_bif(output: &mut String, selector: &str, params: &[String]) -> Option<()> {
    match selector {
        "size" => {
            write!(output, "call 'erlang':'map_size'(Self)").ok()?;
            Some(())
        }
        "keys" => {
            write!(output, "call 'maps':'keys'(Self)").ok()?;
            Some(())
        }
        "values" => {
            write!(output, "call 'maps':'values'(Self)").ok()?;
            Some(())
        }
        "at:" => {
            let p0 = params.first().map_or("_Key", String::as_str);
            write!(output, "call 'maps':'get'({p0}, Self)").ok()?;
            Some(())
        }
        "at:ifAbsent:" => {
            let p0 = params.first().map_or("_Key", String::as_str);
            let p1 = params.get(1).map_or("_Block", String::as_str);
            write!(
                output,
                "call 'beamtalk_map_ops':'at_if_absent'(Self, {p0}, {p1})"
            )
            .ok()?;
            Some(())
        }
        "at:put:" => {
            let p0 = params.first().map_or("_Key", String::as_str);
            let p1 = params.get(1).map_or("_Value", String::as_str);
            write!(output, "call 'maps':'put'({p0}, {p1}, Self)").ok()?;
            Some(())
        }
        "includesKey:" => {
            let p0 = params.first().map_or("_Key", String::as_str);
            write!(output, "call 'maps':'is_key'({p0}, Self)").ok()?;
            Some(())
        }
        "removeKey:" => {
            let p0 = params.first().map_or("_Key", String::as_str);
            write!(output, "call 'maps':'remove'({p0}, Self)").ok()?;
            Some(())
        }
        "merge:" => {
            let p0 = params.first().map_or("_Other", String::as_str);
            write!(output, "call 'maps':'merge'(Self, {p0})").ok()?;
            Some(())
        }
        "keysAndValuesDo:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            write!(
                output,
                "call 'beamtalk_map_ops':'keys_and_values_do'(Self, {p0})"
            )
            .ok()?;
            Some(())
        }
        _ => None,
    }
}

/// Object primitive implementations (BT-335).
///
/// Object is the root class — methods here are inherited by all objects.
fn generate_object_bif(output: &mut String, selector: &str, params: &[String]) -> Option<()> {
    match selector {
        // Association creation: `self -> value` creates an Association tagged map
        "->" => {
            let p0 = params.first()?;
            write!(
                output,
                "~{{'$beamtalk_class' => 'Association', 'key' => Self, 'value' => {p0}}}~"
            )
            .ok()?;
            Some(())
        }
        _ => None,
    }
}

/// Association primitive implementations (BT-335).
///
/// Associations are key-value pairs represented as tagged maps.
fn generate_association_bif(output: &mut String, selector: &str, _params: &[String]) -> Option<()> {
    match selector {
        "key" => {
            write!(output, "call 'maps':'get'('key', Self)").ok()?;
            Some(())
        }
        "value" => {
            write!(output, "call 'maps':'get'('value', Self)").ok()?;
            Some(())
        }
        "asString" => {
            write!(output, "call 'beamtalk_association':'format_string'(Self)").ok()?;
            Some(())
        }
        _ => None,
    }
}

/// Set primitive implementations (BT-73).
///
/// Sets are represented as tagged maps: `#{'$beamtalk_class' => 'Set', elements => OrdsetData}`.
/// Operations delegate to `beamtalk_set_ops` helper module which wraps Erlang `ordsets`.
fn generate_set_bif(output: &mut String, selector: &str, params: &[String]) -> Option<()> {
    match selector {
        "fromList:" => {
            let p0 = params.first().map_or("_List", String::as_str);
            write!(output, "call 'beamtalk_set_ops':'from_list'({p0})").ok()?;
            Some(())
        }
        "size" => {
            write!(output, "call 'beamtalk_set_ops':'size'(Self)").ok()?;
            Some(())
        }
        "isEmpty" => {
            write!(output, "call 'beamtalk_set_ops':'is_empty'(Self)").ok()?;
            Some(())
        }
        "includes:" => {
            let p0 = params.first().map_or("_Element", String::as_str);
            write!(output, "call 'beamtalk_set_ops':'includes'(Self, {p0})").ok()?;
            Some(())
        }
        "add:" => {
            let p0 = params.first().map_or("_Element", String::as_str);
            write!(output, "call 'beamtalk_set_ops':'add'(Self, {p0})").ok()?;
            Some(())
        }
        "remove:" => {
            let p0 = params.first().map_or("_Element", String::as_str);
            write!(output, "call 'beamtalk_set_ops':'remove'(Self, {p0})").ok()?;
            Some(())
        }
        "union:" => {
            let p0 = params.first().map_or("_Other", String::as_str);
            write!(output, "call 'beamtalk_set_ops':'union'(Self, {p0})").ok()?;
            Some(())
        }
        "intersection:" => {
            let p0 = params.first().map_or("_Other", String::as_str);
            write!(output, "call 'beamtalk_set_ops':'intersection'(Self, {p0})").ok()?;
            Some(())
        }
        "difference:" => {
            let p0 = params.first().map_or("_Other", String::as_str);
            write!(output, "call 'beamtalk_set_ops':'difference'(Self, {p0})").ok()?;
            Some(())
        }
        "isSubsetOf:" => {
            let p0 = params.first().map_or("_Other", String::as_str);
            write!(output, "call 'beamtalk_set_ops':'is_subset_of'(Self, {p0})").ok()?;
            Some(())
        }
        "asList" => {
            write!(output, "call 'beamtalk_set_ops':'as_list'(Self)").ok()?;
            Some(())
        }
        "do:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            write!(output, "call 'beamtalk_set_ops':'do'(Self, {p0})").ok()?;
            Some(())
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

/// Writes a binary BIF call: `call 'erlang':'op'(Self, Param0)`
fn write_binary_bif(output: &mut String, erlang_op: &str, params: &[String]) -> Option<()> {
    let p0 = params.first()?;
    write!(output, "call 'erlang':'{erlang_op}'(Self, {p0})").ok()?;
    Some(())
}

/// Writes power implementation: `call 'math':'pow'(Self, Param0)`
fn write_power_bif(output: &mut String, params: &[String]) -> Option<()> {
    let p0 = params.first()?;
    // math:pow returns float, convert to integer if both args are integers
    write!(
        output,
        "call 'erlang':'round'(call 'math':'pow'(\
         call 'erlang':'float'(Self), call 'erlang':'float'({p0})))"
    )
    .ok()?;
    Some(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_integer_plus() {
        let mut output = String::new();
        let result = generate_primitive_bif(&mut output, "Integer", "+", &["Other".to_string()]);
        assert!(result.is_some());
        assert_eq!(output, "call 'erlang':'+'(Self, Other)");
    }

    #[test]
    fn test_integer_modulo() {
        let mut output = String::new();
        let result = generate_primitive_bif(&mut output, "Integer", "%", &["Other".to_string()]);
        assert!(result.is_some());
        assert_eq!(output, "call 'erlang':'rem'(Self, Other)");
    }

    #[test]
    fn test_integer_as_string() {
        let mut output = String::new();
        let result = generate_primitive_bif(&mut output, "Integer", "asString", &[]);
        assert!(result.is_some());
        assert_eq!(output, "call 'erlang':'integer_to_binary'(Self)");
    }

    #[test]
    fn test_string_length() {
        let mut output = String::new();
        let result = generate_primitive_bif(&mut output, "String", "length", &[]);
        assert!(result.is_some());
        assert_eq!(output, "call 'string':'length'(Self)");
    }

    #[test]
    fn test_string_concat() {
        let mut output = String::new();
        let result = generate_primitive_bif(&mut output, "String", "++", &["Other".to_string()]);
        assert!(result.is_some());
        assert!(output.contains("iolist_to_binary"));
    }

    #[test]
    fn test_unknown_class() {
        let mut output = String::new();
        let result = generate_primitive_bif(&mut output, "Counter", "+", &["Other".to_string()]);
        assert!(result.is_none());
    }

    #[test]
    fn test_unknown_selector() {
        let mut output = String::new();
        let result = generate_primitive_bif(&mut output, "Integer", "unknownMethod", &[]);
        assert!(result.is_none());
    }

    #[test]
    fn test_float_as_string() {
        let mut output = String::new();
        let result = generate_primitive_bif(&mut output, "Float", "asString", &[]);
        assert!(result.is_some());
        assert_eq!(output, "call 'erlang':'float_to_binary'(Self, ['short'])");
    }

    #[test]
    fn test_float_rounded() {
        let mut output = String::new();
        let result = generate_primitive_bif(&mut output, "Float", "rounded", &[]);
        assert!(result.is_some());
        assert_eq!(output, "call 'erlang':'round'(Self)");
    }

    #[test]
    fn test_float_ceiling() {
        let mut output = String::new();
        let result = generate_primitive_bif(&mut output, "Float", "ceiling", &[]);
        assert!(result.is_some());
        assert_eq!(output, "call 'erlang':'ceil'(Self)");
    }

    #[test]
    fn test_float_floor() {
        let mut output = String::new();
        let result = generate_primitive_bif(&mut output, "Float", "floor", &[]);
        assert!(result.is_some());
        assert_eq!(output, "call 'erlang':'floor'(Self)");
    }

    #[test]
    fn test_float_truncated() {
        let mut output = String::new();
        let result = generate_primitive_bif(&mut output, "Float", "truncated", &[]);
        assert!(result.is_some());
        assert_eq!(output, "call 'erlang':'trunc'(Self)");
    }

    #[test]
    fn test_float_as_integer() {
        let mut output = String::new();
        let result = generate_primitive_bif(&mut output, "Float", "asInteger", &[]);
        assert!(result.is_some());
        assert_eq!(output, "call 'erlang':'trunc'(Self)");
    }

    #[test]
    fn test_file_exists() {
        let mut output = String::new();
        let result = generate_primitive_bif(&mut output, "File", "exists:", &["Path".to_string()]);
        assert!(result.is_some());
        assert_eq!(output, "call 'beamtalk_file':'exists:'(Path)");
    }

    #[test]
    fn test_file_read_all() {
        let mut output = String::new();
        let result = generate_primitive_bif(&mut output, "File", "readAll:", &["Path".to_string()]);
        assert!(result.is_some());
        assert_eq!(output, "call 'beamtalk_file':'readAll:'(Path)");
    }

    #[test]
    fn test_file_write_all_contents() {
        let mut output = String::new();
        let result = generate_primitive_bif(
            &mut output,
            "File",
            "writeAll:contents:",
            &["Path".to_string(), "Text".to_string()],
        );
        assert!(result.is_some());
        assert_eq!(
            output,
            "call 'beamtalk_file':'writeAll:contents:'(Path, Text)"
        );
    }

    #[test]
    fn test_symbol_as_string() {
        let mut output = String::new();
        let result = generate_primitive_bif(&mut output, "Symbol", "asString", &[]);
        assert!(result.is_some());
        assert_eq!(output, "call 'erlang':'atom_to_binary'(Self, 'utf8')");
    }

    #[test]
    fn test_symbol_as_atom() {
        let mut output = String::new();
        let result = generate_primitive_bif(&mut output, "Symbol", "asAtom", &[]);
        assert!(result.is_some());
        assert_eq!(output, "Self");
    }

    #[test]
    fn test_symbol_equality() {
        let mut output = String::new();
        let result = generate_primitive_bif(&mut output, "Symbol", "=", &["Other".to_string()]);
        assert!(result.is_some());
        assert_eq!(output, "call 'erlang':'=:='(Self, Other)");
    }

    #[test]
    fn test_symbol_hash() {
        let mut output = String::new();
        let result = generate_primitive_bif(&mut output, "Symbol", "hash", &[]);
        assert!(result.is_some());
        assert_eq!(output, "call 'erlang':'phash2'(Self)");
    }

    #[test]
    fn test_symbol_print_string() {
        let mut output = String::new();
        let result = generate_primitive_bif(&mut output, "Symbol", "printString", &[]);
        assert!(result.is_some());
        assert_eq!(output, "call 'beamtalk_primitive':'print_string'(Self)");
    }
}
