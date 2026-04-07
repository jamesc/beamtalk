// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Erlang→Beamtalk type mapping and spec line parsing (ADR 0075 Phase 1).
//!
//! **DDD Context:** Semantic Analysis
//!
//! Converts the structured output from `beamtalk_build_worker` (the
//! `beamtalk-specs-module:` protocol lines) into Rust-side
//! [`NativeTypeRegistry`] entries.
//!
//! The Erlang side (`beamtalk_spec_reader.erl`) does the heavy lifting of
//! reading `-spec` attributes from `.beam` abstract code and mapping Erlang
//! types to Beamtalk type name strings. This module parses those string
//! representations into [`InferredType`] values and populates the registry.
//!
//! ## Protocol Format
//!
//! Each module's specs arrive as a single line:
//! ```text
//! beamtalk-specs-module:<module_name>:<erlang_term>
//! ```
//!
//! Where `<erlang_term>` is a printed Erlang list of maps:
//! ```erlang
//! [#{name => <<"reverse">>, arity => 1,
//!    params => [#{name => <<"list">>, type => <<"List">>}],
//!    return_type => <<"List">>}, ...]
//! ```

use super::native_type_registry::{FunctionSignature, NativeTypeRegistry, ParamType};
use super::types::{DynamicReason, InferredType, TypeProvenance};
use ecow::EcoString;

/// Prefix for spec module lines in the build worker protocol.
const SPECS_MODULE_PREFIX: &str = "beamtalk-specs-module:";

/// Maps a Beamtalk type name string (as emitted by `beamtalk_spec_reader.erl`)
/// to an [`InferredType`].
///
/// The Erlang side already maps Erlang abstract types to Beamtalk type name
/// strings (e.g., `integer()` → `"Integer"`, `binary()` → `"String"`). This
/// function converts those string names into the Rust type representation.
///
/// ## Type Variable Handling
///
/// - Concrete type names (e.g., `"Integer"`, `"List"`) produce `Known` types
///   with `Extracted` provenance.
/// - `"Dynamic"` produces `InferredType::Dynamic(DynamicReason::DynamicSpec)`.
/// - Union types (e.g., `"Integer | String"`) are split and produce `Union` types.
///
/// ## Examples
///
/// ```ignore
/// map_type_name("Integer")           // => Known { class_name: "Integer", .. }
/// map_type_name("Dynamic")           // => Dynamic
/// map_type_name("Integer | String")  // => Union { members: [Integer, String], .. }
/// ```
#[must_use]
pub fn map_type_name(type_name: &str) -> InferredType {
    let trimmed = type_name.trim();

    if trimmed == "Dynamic" {
        return InferredType::Dynamic(DynamicReason::DynamicSpec);
    }

    // Handle union types: "Integer | String"
    if trimmed.contains(" | ") {
        let members: Vec<InferredType> = trimmed
            .split(" | ")
            .map(|part| map_single_type_name(part.trim()))
            .collect();
        return InferredType::union_of(&members);
    }

    map_single_type_name(trimmed)
}

/// Maps a single (non-union) type name to an [`InferredType`].
fn map_single_type_name(name: &str) -> InferredType {
    if name == "Dynamic" {
        return InferredType::Dynamic(DynamicReason::DynamicSpec);
    }

    InferredType::Known {
        class_name: EcoString::from(name),
        type_args: vec![],
        provenance: TypeProvenance::Extracted,
    }
}

/// Parses a `beamtalk-specs-module:` line and registers the specs in the registry.
///
/// Returns the module name on success, or `None` if the line is not a specs
/// module line or cannot be parsed.
///
/// ## Protocol Format
///
/// ```text
/// beamtalk-specs-module:lists:[#{name => <<"reverse">>, ...}]
/// ```
///
/// The Erlang term is a list of maps with keys: `name` (binary), `arity`
/// (integer), `params` (list of `#{name, type}` maps), `return_type` (binary).
pub fn parse_specs_line(line: &str, registry: &mut NativeTypeRegistry) -> Option<String> {
    let rest = line.strip_prefix(SPECS_MODULE_PREFIX)?;

    // Split on first ':' to get module_name and erlang_term
    let colon_pos = rest.find(':')?;
    let module_name = &rest[..colon_pos];
    let term_str = &rest[colon_pos + 1..];

    if module_name.is_empty() {
        return None;
    }

    let functions = parse_erlang_spec_term(term_str);
    registry.register_module(module_name, functions);

    Some(module_name.to_string())
}

/// Checks whether a line is a specs module protocol line.
#[must_use]
pub fn is_specs_line(line: &str) -> bool {
    line.starts_with(SPECS_MODULE_PREFIX)
}

/// Checks whether a line signals specs result completion.
#[must_use]
pub fn is_specs_result_ok(line: &str) -> bool {
    line == "beamtalk-specs-result-ok"
}

/// Checks whether a line signals specs result failure.
#[must_use]
pub fn is_specs_result_error(line: &str) -> bool {
    line == "beamtalk-specs-result-error"
}

/// Parses a printed Erlang term (list of spec maps) into function signatures.
///
/// The Erlang term format (printed via `~0tp`):
/// ```erlang
/// [#{arity => 1,name => <<"reverse">>,
///    params => [#{name => <<"list">>,type => <<"List">>}],
///    return_type => <<"List">>}]
/// ```
///
/// This is a best-effort parser for the specific format emitted by
/// `beamtalk_spec_reader.erl`. It handles the structural patterns we emit
/// rather than implementing a full Erlang term parser.
fn parse_erlang_spec_term(term: &str) -> Vec<FunctionSignature> {
    let trimmed = term.trim();

    // The term should be a list: [...]
    let inner = match trimmed.strip_prefix('[').and_then(|s| s.strip_suffix(']')) {
        Some(s) => s.trim(),
        None => return vec![],
    };

    if inner.is_empty() {
        return vec![];
    }

    // Split into individual map entries at top-level `#{` boundaries
    let maps = split_top_level_maps(inner);

    maps.iter()
        .filter_map(|map_str| parse_spec_map(map_str))
        .collect()
}

/// Splits a string containing multiple Erlang maps at the top level.
///
/// Maps are delimited by `#{...}` with possible nesting. We split on `,#{`
/// patterns that occur at nesting depth 0.
fn split_top_level_maps(input: &str) -> Vec<&str> {
    let mut result = Vec::new();
    let mut depth = 0;
    let mut start = 0;
    let bytes = input.as_bytes();
    let mut i = 0;

    while i < bytes.len() {
        match bytes[i] {
            b'#' if i + 1 < bytes.len() && bytes[i + 1] == b'{' => {
                depth += 1;
                i += 2;
            }
            b'{' => {
                depth += 1;
                i += 1;
            }
            b'}' => {
                depth -= 1;
                if depth == 0 {
                    // End of a top-level map
                    result.push(&input[start..=i]);
                    // Skip comma and whitespace after the closing brace
                    i += 1;
                    while i < bytes.len()
                        && (bytes[i] == b',' || bytes[i] == b' ' || bytes[i] == b'\n')
                    {
                        i += 1;
                    }
                    start = i;
                    continue;
                }
                i += 1;
            }
            b'<' if i + 1 < bytes.len() && bytes[i + 1] == b'<' => {
                // Skip binary literal: <<"...">>
                i += 2;
                while i < bytes.len() {
                    if bytes[i] == b'>' && i + 1 < bytes.len() && bytes[i + 1] == b'>' {
                        i += 2;
                        break;
                    }
                    i += 1;
                }
            }
            _ => {
                i += 1;
            }
        }
    }

    // If there's remaining content (shouldn't happen for well-formed input)
    if start < bytes.len() && depth == 0 {
        let remainder = input[start..].trim();
        if !remainder.is_empty() {
            result.push(remainder);
        }
    }

    result
}

/// Parses a single Erlang map representing a spec entry.
///
/// Expected format:
/// ```erlang
/// #{arity => 1,name => <<"reverse">>,
///   params => [#{name => <<"list">>,type => <<"List">>}],
///   return_type => <<"List">>}
/// ```
fn parse_spec_map(map_str: &str) -> Option<FunctionSignature> {
    let inner = map_str
        .trim()
        .strip_prefix("#{")
        .and_then(|s| s.strip_suffix('}'))?;

    let raw_name = extract_binary_value(inner, "name")?;
    // Normalize the function name to match selector_to_function/1 in
    // beamtalk_erlang_proxy: strip everything from the first colon onward.
    // E.g., "readAll:" → "readAll", "new:type:" → "new".
    // Unary names without colons pass through unchanged.
    // We normalize here (not in the spec reader) so that Erlang-side
    // consumers like dedupe_keyword_aliases retain the original colon form.
    let name = canonical_function_name(&raw_name);
    let arity = extract_integer_value(inner, "arity")?;
    let return_type_str = extract_binary_value(inner, "return_type")?;
    let params = extract_params_list(inner)?;
    let line = extract_integer_value(inner, "line").and_then(|v| u32::try_from(v).ok());

    Some(FunctionSignature {
        name,
        arity: u8::try_from(arity).ok()?,
        params,
        return_type: map_type_name(&return_type_str),
        provenance: TypeProvenance::Extracted,
        line,
    })
}

/// Normalizes a function name from the spec reader to its canonical bare form.
///
/// Mirrors `selector_to_function/1` in `beamtalk_erlang_proxy`:
/// keyword selectors (`"readAll:"`, `"new:type:"`) → first keyword (`"readAll"`, `"new"`),
/// unary selectors (`"reverse"`) → unchanged.
fn canonical_function_name(name: &str) -> String {
    match name.find(':') {
        Some(pos) => name[..pos].to_string(),
        None => name.to_string(),
    }
}

/// Extracts a binary value (`<<"value">>`) for a given key from a map string.
fn extract_binary_value(map_inner: &str, key: &str) -> Option<String> {
    // Look for pattern: key => <<"value">>
    let key_pattern = format!("{key} => <<\"");
    let start = map_inner.find(&key_pattern)?;
    let value_start = start + key_pattern.len();
    let value_end = map_inner[value_start..].find("\">>")? + value_start;
    Some(map_inner[value_start..value_end].to_string())
}

/// Extracts an integer value for a given key from a map string.
fn extract_integer_value(map_inner: &str, key: &str) -> Option<usize> {
    // Look for pattern: key => N (where N is followed by , or end of string)
    let key_pattern = format!("{key} => ");
    let start = map_inner.find(&key_pattern)?;
    let value_start = start + key_pattern.len();
    let remaining = &map_inner[value_start..];

    // The integer ends at the next comma or end of string
    let end = remaining
        .find(|c: char| !c.is_ascii_digit())
        .unwrap_or(remaining.len());
    remaining[..end].parse().ok()
}

/// Extracts the params list from a map string.
///
/// Params format: `params => [#{name => <<"list">>,type => <<"List">>}]`
fn extract_params_list(map_inner: &str) -> Option<Vec<ParamType>> {
    let key_pattern = "params => [";
    let start = map_inner.find(key_pattern)?;
    let list_start = start + key_pattern.len();

    // Find the matching closing bracket, accounting for nested brackets
    let remaining = &map_inner[list_start..];
    let mut depth = 1;
    let mut end = 0;
    for (i, c) in remaining.char_indices() {
        match c {
            '[' => depth += 1,
            ']' => {
                depth -= 1;
                if depth == 0 {
                    end = i;
                    break;
                }
            }
            _ => {}
        }
    }

    let list_content = &remaining[..end];

    if list_content.trim().is_empty() {
        return Some(vec![]);
    }

    let param_maps = split_top_level_maps(list_content);
    let params = param_maps
        .iter()
        .filter_map(|pm| parse_param_map(pm))
        .collect();
    Some(params)
}

/// Parses a single param map: `#{name => <<"list">>,type => <<"List">>}`
fn parse_param_map(map_str: &str) -> Option<ParamType> {
    let inner = map_str
        .trim()
        .strip_prefix("#{")
        .and_then(|s| s.strip_suffix('}'))?;

    let name = extract_binary_value(inner, "name")?;
    let type_name = extract_binary_value(inner, "type")?;

    Some(ParamType {
        keyword: Some(EcoString::from(name)),
        type_: map_type_name(&type_name),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    // -----------------------------------------------------------------------
    // map_type_name tests
    // -----------------------------------------------------------------------

    #[test]
    fn map_type_name_integer() {
        let ty = map_type_name("Integer");
        assert_eq!(ty, InferredType::known("Integer"));
    }

    #[test]
    fn map_type_name_dynamic() {
        let ty = map_type_name("Dynamic");
        match ty {
            InferredType::Dynamic(reason) => assert_eq!(reason, DynamicReason::DynamicSpec),
            other => panic!("expected Dynamic(DynamicSpec), got {other:?}"),
        }
    }

    #[test]
    fn map_type_name_string() {
        let ty = map_type_name("String");
        assert_eq!(ty, InferredType::known("String"));
    }

    #[test]
    fn map_type_name_boolean() {
        let ty = map_type_name("Boolean");
        assert_eq!(ty, InferredType::known("Boolean"));
    }

    #[test]
    fn map_type_name_symbol() {
        let ty = map_type_name("Symbol");
        assert_eq!(ty, InferredType::known("Symbol"));
    }

    #[test]
    fn map_type_name_list() {
        let ty = map_type_name("List");
        assert_eq!(ty, InferredType::known("List"));
    }

    #[test]
    fn map_type_name_tuple() {
        let ty = map_type_name("Tuple");
        assert_eq!(ty, InferredType::known("Tuple"));
    }

    #[test]
    fn map_type_name_dictionary() {
        let ty = map_type_name("Dictionary");
        assert_eq!(ty, InferredType::known("Dictionary"));
    }

    #[test]
    fn map_type_name_pid() {
        let ty = map_type_name("Pid");
        assert_eq!(ty, InferredType::known("Pid"));
    }

    #[test]
    fn map_type_name_block() {
        let ty = map_type_name("Block");
        assert_eq!(ty, InferredType::known("Block"));
    }

    #[test]
    fn map_type_name_float() {
        let ty = map_type_name("Float");
        assert_eq!(ty, InferredType::known("Float"));
    }

    #[test]
    fn map_type_name_number() {
        let ty = map_type_name("Number");
        assert_eq!(ty, InferredType::known("Number"));
    }

    #[test]
    fn map_type_name_true() {
        let ty = map_type_name("True");
        assert_eq!(ty, InferredType::known("True"));
    }

    #[test]
    fn map_type_name_false() {
        let ty = map_type_name("False");
        assert_eq!(ty, InferredType::known("False"));
    }

    #[test]
    fn map_type_name_nil() {
        let ty = map_type_name("Nil");
        assert_eq!(ty, InferredType::known("Nil"));
    }

    #[test]
    fn map_type_name_union() {
        let ty = map_type_name("Integer | String");
        assert_eq!(ty, InferredType::simple_union(&["Integer", "String"]));
    }

    #[test]
    fn map_type_name_union_with_dynamic() {
        // If any member is Dynamic, union_of returns Dynamic
        let ty = map_type_name("Integer | Dynamic");
        match ty {
            InferredType::Dynamic(reason) => assert_eq!(reason, DynamicReason::DynamicSpec),
            other => panic!("expected Dynamic(DynamicSpec), got {other:?}"),
        }
    }

    #[test]
    fn map_type_name_trimmed() {
        let ty = map_type_name("  Integer  ");
        assert_eq!(ty, InferredType::known("Integer"));
    }

    #[test]
    fn map_type_name_extracted_provenance() {
        let ty = map_type_name("Integer");
        match ty {
            InferredType::Known { provenance, .. } => {
                assert_eq!(provenance, TypeProvenance::Extracted);
            }
            _ => panic!("expected Known type"),
        }
    }

    // -----------------------------------------------------------------------
    // Protocol line detection
    // -----------------------------------------------------------------------

    #[test]
    fn is_specs_line_detects_prefix() {
        assert!(is_specs_line("beamtalk-specs-module:lists:[#{arity => 1}]"));
        assert!(!is_specs_line("beamtalk-compile-module:lists"));
        assert!(!is_specs_line("some random line"));
    }

    #[test]
    fn is_specs_result_ok_detects() {
        assert!(is_specs_result_ok("beamtalk-specs-result-ok"));
        assert!(!is_specs_result_ok("beamtalk-specs-result-error"));
    }

    #[test]
    fn is_specs_result_error_detects() {
        assert!(is_specs_result_error("beamtalk-specs-result-error"));
        assert!(!is_specs_result_error("beamtalk-specs-result-ok"));
    }

    // -----------------------------------------------------------------------
    // parse_specs_line integration tests
    // -----------------------------------------------------------------------

    #[test]
    fn parse_specs_line_single_function() {
        let mut reg = NativeTypeRegistry::new();
        let line = "beamtalk-specs-module:lists:[#{arity => 1,name => <<\"reverse\">>,params => [#{name => <<\"list\">>,type => <<\"List\">>}],return_type => <<\"List\">>}]";

        let result = parse_specs_line(line, &mut reg);
        assert_eq!(result, Some("lists".to_string()));
        assert!(reg.has_module("lists"));

        let sig = reg.lookup("lists", "reverse", 1).unwrap();
        assert_eq!(sig.return_type, InferredType::known("List"));
        assert_eq!(sig.params.len(), 1);
        assert_eq!(sig.params[0].keyword, Some(EcoString::from("list")));
        assert_eq!(sig.params[0].type_, InferredType::known("List"));
    }

    #[test]
    fn parse_specs_line_multiple_functions() {
        let mut reg = NativeTypeRegistry::new();
        let line = "beamtalk-specs-module:lists:[#{arity => 1,name => <<\"reverse\">>,params => [#{name => <<\"list\">>,type => <<\"List\">>}],return_type => <<\"List\">>},#{arity => 1,name => <<\"sort\">>,params => [#{name => <<\"list\">>,type => <<\"List\">>}],return_type => <<\"List\">>}]";

        let result = parse_specs_line(line, &mut reg);
        assert_eq!(result, Some("lists".to_string()));

        let fns = reg.module_functions("lists").unwrap();
        assert_eq!(fns.len(), 2);
        assert!(reg.lookup("lists", "reverse", 1).is_some());
        assert!(reg.lookup("lists", "sort", 1).is_some());
    }

    #[test]
    fn parse_specs_line_multi_param_function() {
        let mut reg = NativeTypeRegistry::new();
        let line = "beamtalk-specs-module:lists:[#{arity => 2,name => <<\"seq\">>,params => [#{name => <<\"from\">>,type => <<\"Integer\">>},#{name => <<\"to\">>,type => <<\"Integer\">>}],return_type => <<\"List\">>}]";

        parse_specs_line(line, &mut reg);

        let sig = reg.lookup("lists", "seq", 2).unwrap();
        assert_eq!(sig.return_type, InferredType::known("List"));
        assert_eq!(sig.params.len(), 2);
        assert_eq!(sig.params[0].keyword, Some(EcoString::from("from")));
        assert_eq!(sig.params[0].type_, InferredType::known("Integer"));
        assert_eq!(sig.params[1].keyword, Some(EcoString::from("to")));
        assert_eq!(sig.params[1].type_, InferredType::known("Integer"));
    }

    #[test]
    fn parse_specs_line_union_return_type() {
        let mut reg = NativeTypeRegistry::new();
        let line = "beamtalk-specs-module:maps:[#{arity => 2,name => <<\"find\">>,params => [#{name => <<\"key\">>,type => <<\"Dynamic\">>},#{name => <<\"map\">>,type => <<\"Dictionary\">>}],return_type => <<\"Tuple | Symbol\">>}]";

        parse_specs_line(line, &mut reg);

        let sig = reg.lookup("maps", "find", 2).unwrap();
        assert_eq!(
            sig.return_type,
            InferredType::simple_union(&["Tuple", "Symbol"])
        );
    }

    #[test]
    fn parse_specs_line_nullary_function() {
        let mut reg = NativeTypeRegistry::new();
        let line = "beamtalk-specs-module:erlang:[#{arity => 0,name => <<\"node\">>,params => [],return_type => <<\"Symbol\">>}]";

        parse_specs_line(line, &mut reg);

        let sig = reg.lookup("erlang", "node", 0).unwrap();
        assert_eq!(sig.return_type, InferredType::known("Symbol"));
        assert!(sig.params.is_empty());
    }

    #[test]
    fn parse_specs_line_empty_list() {
        let mut reg = NativeTypeRegistry::new();
        let line = "beamtalk-specs-module:empty_mod:[]";

        let result = parse_specs_line(line, &mut reg);
        assert_eq!(result, Some("empty_mod".to_string()));
        assert!(reg.has_module("empty_mod"));

        let fns = reg.module_functions("empty_mod").unwrap();
        assert!(fns.is_empty());
    }

    #[test]
    fn parse_specs_line_not_a_specs_line() {
        let mut reg = NativeTypeRegistry::new();
        let result = parse_specs_line("beamtalk-compile-module:lists", &mut reg);
        assert!(result.is_none());
    }

    #[test]
    fn parse_specs_line_empty_module_name() {
        let mut reg = NativeTypeRegistry::new();
        let result = parse_specs_line("beamtalk-specs-module::[]", &mut reg);
        assert!(result.is_none());
    }

    #[test]
    fn parse_specs_line_dynamic_return_type() {
        let mut reg = NativeTypeRegistry::new();
        let line = "beamtalk-specs-module:erlang:[#{arity => 1,name => <<\"apply\">>,params => [#{name => <<\"fun\">>,type => <<\"Dynamic\">>}],return_type => <<\"Dynamic\">>}]";

        parse_specs_line(line, &mut reg);

        let sig = reg.lookup("erlang", "apply", 1).unwrap();
        assert_eq!(
            sig.return_type,
            InferredType::Dynamic(DynamicReason::DynamicSpec)
        );
    }

    #[test]
    fn parse_specs_line_extracted_provenance() {
        let mut reg = NativeTypeRegistry::new();
        let line = "beamtalk-specs-module:lists:[#{arity => 1,name => <<\"reverse\">>,params => [#{name => <<\"list\">>,type => <<\"List\">>}],return_type => <<\"List\">>}]";

        parse_specs_line(line, &mut reg);

        let sig = reg.lookup("lists", "reverse", 1).unwrap();
        assert_eq!(sig.provenance, TypeProvenance::Extracted);
    }

    #[test]
    fn parse_specs_line_with_line_number() {
        let mut reg = NativeTypeRegistry::new();
        let line = "beamtalk-specs-module:lists:[#{arity => 1,line => 156,name => <<\"reverse\">>,params => [#{name => <<\"list\">>,type => <<\"List\">>}],return_type => <<\"List\">>}]";

        parse_specs_line(line, &mut reg);

        let sig = reg.lookup("lists", "reverse", 1).unwrap();
        assert_eq!(sig.line, Some(156));
    }

    #[test]
    fn parse_specs_line_without_line_number() {
        let mut reg = NativeTypeRegistry::new();
        let line = "beamtalk-specs-module:lists:[#{arity => 1,name => <<\"reverse\">>,params => [#{name => <<\"list\">>,type => <<\"List\">>}],return_type => <<\"List\">>}]";

        parse_specs_line(line, &mut reg);

        let sig = reg.lookup("lists", "reverse", 1).unwrap();
        assert_eq!(sig.line, None);
    }

    // -----------------------------------------------------------------------
    // canonical_function_name tests
    // -----------------------------------------------------------------------

    #[test]
    fn canonical_name_unary_unchanged() {
        assert_eq!(canonical_function_name("reverse"), "reverse");
    }

    #[test]
    fn canonical_name_single_keyword_strips_colon() {
        assert_eq!(canonical_function_name("readAll:"), "readAll");
    }

    #[test]
    fn canonical_name_multi_keyword_takes_first() {
        assert_eq!(canonical_function_name("new:type:"), "new");
    }

    #[test]
    fn canonical_name_parsed_spec_has_bare_name() {
        // Verify that a spec with a colon-suffixed name gets normalized
        // when parsed into a FunctionSignature.
        let line = "beamtalk-specs-module:beamtalk_file:[#{arity => 1,name => <<\"readAll:\">>,params => [#{name => <<\"path\">>,type => <<\"String\">>}],return_type => <<\"Result\">>}]";
        let mut reg = NativeTypeRegistry::new();
        parse_specs_line(line, &mut reg);
        // Should find by bare name, not "readAll:"
        assert!(reg.lookup("beamtalk_file", "readAll", 1).is_some());
        assert!(reg.lookup("beamtalk_file", "readAll:", 1).is_none());
    }
}
