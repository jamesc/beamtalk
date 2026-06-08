// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Value type primitive implementations.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Contains BIF generators for immutable value types: `Tuple`, `Set`, `Object`.

use super::super::document::Document;
use super::super::document::leaf;
use super::param;
use crate::docvec;

/// Tuple primitive implementations (BT-417).
///
/// Tuples are Erlang tuples — immutable fixed-size collections, particularly
/// useful for Erlang interop with {ok, Value} and {error, Reason} patterns.
pub(crate) fn generate_tuple_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        "size" => Some(Document::Str("call 'erlang':'tuple_size'(Self)")),
        "at:" => {
            let p0 = params.first()?;
            Some(docvec![
                "call 'beamtalk_tuple':'at'(Self, ",
                leaf::var(p0.clone()),
                ")"
            ])
        }
        "isOk" => Some(Document::Str(
            "case Self of <{'ok', _Value}> when 'true' -> 'true' <_> when 'true' -> 'false' end",
        )),
        "isError" => Some(Document::Str(
            "case Self of <{'error', _Reason}> when 'true' -> 'true' <_> when 'true' -> 'false' end",
        )),
        "unwrap" => Some(Document::Str("call 'beamtalk_tuple':'unwrap'(Self)")),
        "unwrapOr:" => {
            let p0 = params.first()?;
            Some(docvec![
                "call 'beamtalk_tuple':'unwrap_or'(Self, ",
                leaf::var(p0.clone()),
                ")",
            ])
        }
        "unwrapOrElse:" => {
            let p0 = params.first()?;
            Some(docvec![
                "call 'beamtalk_tuple':'unwrap_or_else'(Self, ",
                leaf::var(p0.clone()),
                ")",
            ])
        }
        "asString" => Some(Document::Str("call 'beamtalk_tuple':'as_string'(Self)")),
        "do:" => {
            let p0 = param(params, 0, "_Block");
            Some(docvec![
                "call 'beamtalk_tuple':'do'(Self, ",
                leaf::var(p0.to_string()),
                ")"
            ])
        }
        "atRandom" => Some(Document::Str("call 'beamtalk_random':'atRandom'(Self)")),
        "withAll:" => {
            let p0 = param(params, 0, "_List");
            Some(docvec![
                "call 'erlang':'list_to_tuple'(",
                leaf::var(p0.to_string()),
                ")"
            ])
        }
        _ => None,
    }
}

/// `ProtoObject` primitive implementations (BT-1158).
///
/// `ProtoObject` is the root of the class hierarchy. Its `doesNotUnderstand:args:`
/// method raises a `does_not_understand` error so that messages sent to objects
/// that have no implementation propagate as errors rather than silently returning
/// a falsy value.
pub(crate) fn generate_proto_object_bif(
    selector: &str,
    params: &[String],
) -> Option<Document<'static>> {
    match selector {
        "doesNotUnderstand:args:" => {
            let selector_var = super::param(params, 0, "_Selector");
            let hint = super::core_erlang_binary_string(
                "Check spelling or use 'respondsTo:' to verify method exists",
            );
            Some(docvec![
                "let <DnuClass0> = call 'beamtalk_primitive':'class_of'(Self) in\n",
                "let <DnuErr0> = call 'beamtalk_error':'new'('does_not_understand', DnuClass0) in\n",
                "let <DnuErr1> = call 'beamtalk_error':'with_selector'(DnuErr0, ",
                leaf::var(selector_var.to_string()),
                ") in\n",
                "let <DnuHint> = ",
                hint,
                " in\n",
                "let <DnuErr2> = call 'beamtalk_error':'with_hint'(DnuErr1, DnuHint) in\n",
                "call 'beamtalk_error':'raise'(DnuErr2)",
            ])
        }
        _ => None,
    }
}

/// Object primitive implementations.
///
/// Object is the root class — methods here are inherited by all objects.
pub(crate) fn generate_object_bif(
    _selector: &str,
    _params: &[String],
) -> Option<Document<'static>> {
    None
}

/// `Value` primitive implementations (ADR 0094, Phase 2).
///
/// `Value` is the base class for immutable value types. Its `printString`
/// renders the structural `ClassName(field: value, ...)` form via the
/// canonical renderer `beamtalk_object_printer:structural_from_state/1`,
/// guaranteeing byte-identical output with the runtime fallback paths
/// (Critical Risk #4).
pub(crate) fn generate_value_bif(selector: &str, _params: &[String]) -> Option<Document<'static>> {
    match selector {
        "printString" => Some(Document::Str(
            "call 'beamtalk_object_printer':'structural_from_state'(Self)",
        )),
        _ => None,
    }
}

/// Set primitive implementations (BT-73).
///
/// Sets are represented as tagged maps: `#{'$beamtalk_class' => 'Set', elements => OrdsetData}`.
/// Operations delegate to `beamtalk_set` helper module which wraps Erlang `ordsets`.
pub(crate) fn generate_set_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        "fromList:" | "withAll:" => {
            let p0 = param(params, 0, "_List");
            Some(docvec![
                "call 'beamtalk_set':'from_list'(",
                leaf::var(p0.to_string()),
                ")"
            ])
        }
        "size" => Some(Document::Str("call 'beamtalk_set':'size'(Self)")),
        "isEmpty" => Some(Document::Str("call 'beamtalk_set':'is_empty'(Self)")),
        "includes:" => {
            let p0 = param(params, 0, "_Element");
            Some(docvec![
                "call 'beamtalk_set':'includes'(Self, ",
                leaf::var(p0.to_string()),
                ")"
            ])
        }
        "add:" => {
            let p0 = param(params, 0, "_Element");
            Some(docvec![
                "call 'beamtalk_set':'add'(Self, ",
                leaf::var(p0.to_string()),
                ")"
            ])
        }
        "remove:" => {
            let p0 = param(params, 0, "_Element");
            Some(docvec![
                "call 'beamtalk_set':'remove'(Self, ",
                leaf::var(p0.to_string()),
                ")"
            ])
        }
        "union:" => {
            let p0 = param(params, 0, "_Other");
            Some(docvec![
                "call 'beamtalk_set':'union'(Self, ",
                leaf::var(p0.to_string()),
                ")"
            ])
        }
        "intersection:" => {
            let p0 = param(params, 0, "_Other");
            Some(docvec![
                "call 'beamtalk_set':'intersection'(Self, ",
                leaf::var(p0.to_string()),
                ")",
            ])
        }
        "difference:" => {
            let p0 = param(params, 0, "_Other");
            Some(docvec![
                "call 'beamtalk_set':'difference'(Self, ",
                leaf::var(p0.to_string()),
                ")"
            ])
        }
        "isSubsetOf:" => {
            let p0 = param(params, 0, "_Other");
            Some(docvec![
                "call 'beamtalk_set':'is_subset_of'(Self, ",
                leaf::var(p0.to_string()),
                ")",
            ])
        }
        "asList" => Some(Document::Str("call 'beamtalk_set':'as_list'(Self)")),
        "do:" => {
            let p0 = param(params, 0, "_Block");
            Some(docvec![
                "call 'beamtalk_set':'do'(Self, ",
                leaf::var(p0.to_string()),
                ")"
            ])
        }
        // BT-477: Delegate to beamtalk_primitive:print_string/1 which
        // formats Sets as "Set(element1, element2, ...)"
        "printString" => Some(super::PRINT_STRING),
        // Streaming (BT-514)
        "stream" => Some(Document::Str("call 'beamtalk_stream':'on'(Self)")),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::super::doc_to_string;
    use super::*;

    // generate_tuple_bif tests

    #[test]
    fn test_tuple_size() {
        let result = doc_to_string(generate_tuple_bif("size", &[]));
        assert_eq!(result, Some("call 'erlang':'tuple_size'(Self)".to_string()));
    }

    #[test]
    fn test_tuple_at_with_param() {
        let result = doc_to_string(generate_tuple_bif("at:", &["Idx".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_tuple':'at'(Self, Idx)".to_string())
        );
    }

    #[test]
    fn test_tuple_at_without_params_returns_none() {
        assert_eq!(doc_to_string(generate_tuple_bif("at:", &[])), None);
    }

    #[test]
    fn test_tuple_is_ok() {
        let result = doc_to_string(generate_tuple_bif("isOk", &[]));
        let output = result.expect("isOk should produce code");
        assert!(output.contains("'ok'"));
        assert!(output.contains("'true'"));
        assert!(output.contains("'false'"));
    }

    #[test]
    fn test_tuple_is_error() {
        let result = doc_to_string(generate_tuple_bif("isError", &[]));
        let output = result.expect("isError should produce code");
        assert!(output.contains("'error'"));
        assert!(output.contains("'true'"));
        assert!(output.contains("'false'"));
    }

    #[test]
    fn test_tuple_unwrap() {
        let result = doc_to_string(generate_tuple_bif("unwrap", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_tuple':'unwrap'(Self)".to_string())
        );
    }

    #[test]
    fn test_tuple_unwrap_or_with_param() {
        let result = doc_to_string(generate_tuple_bif("unwrapOr:", &["Default".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_tuple':'unwrap_or'(Self, Default)".to_string())
        );
    }

    #[test]
    fn test_tuple_unwrap_or_without_params_returns_none() {
        assert_eq!(doc_to_string(generate_tuple_bif("unwrapOr:", &[])), None);
    }

    #[test]
    fn test_tuple_unwrap_or_else_with_param() {
        let result = doc_to_string(generate_tuple_bif("unwrapOrElse:", &["Block".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_tuple':'unwrap_or_else'(Self, Block)".to_string())
        );
    }

    #[test]
    fn test_tuple_unwrap_or_else_without_params_returns_none() {
        assert_eq!(
            doc_to_string(generate_tuple_bif("unwrapOrElse:", &[])),
            None
        );
    }

    #[test]
    fn test_tuple_as_string() {
        let result = doc_to_string(generate_tuple_bif("asString", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_tuple':'as_string'(Self)".to_string())
        );
    }

    #[test]
    fn test_tuple_do_with_param() {
        let result = doc_to_string(generate_tuple_bif("do:", &["Block".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_tuple':'do'(Self, Block)".to_string())
        );
    }

    #[test]
    fn test_tuple_do_uses_default_when_no_param() {
        let result = doc_to_string(generate_tuple_bif("do:", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_tuple':'do'(Self, _Block)".to_string())
        );
    }

    #[test]
    fn test_tuple_at_random() {
        let result = doc_to_string(generate_tuple_bif("atRandom", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_random':'atRandom'(Self)".to_string())
        );
    }

    #[test]
    fn test_tuple_with_all_with_param() {
        let result = doc_to_string(generate_tuple_bif("withAll:", &["TheList".to_string()]));
        assert_eq!(
            result,
            Some("call 'erlang':'list_to_tuple'(TheList)".to_string())
        );
    }

    #[test]
    fn test_tuple_with_all_uses_default_when_no_param() {
        let result = doc_to_string(generate_tuple_bif("withAll:", &[]));
        assert_eq!(
            result,
            Some("call 'erlang':'list_to_tuple'(_List)".to_string())
        );
    }

    #[test]
    fn test_tuple_unknown_returns_none() {
        assert_eq!(
            doc_to_string(generate_tuple_bif("unknownSelector", &[])),
            None
        );
    }

    // generate_proto_object_bif tests

    #[test]
    fn test_proto_object_unknown_returns_none() {
        assert_eq!(doc_to_string(generate_proto_object_bif("size", &[])), None);
    }

    // generate_object_bif tests

    #[test]
    fn test_object_always_returns_none() {
        assert_eq!(doc_to_string(generate_object_bif("anything", &[])), None);
        assert_eq!(doc_to_string(generate_object_bif("size", &[])), None);
    }

    // generate_value_bif tests

    #[test]
    fn test_value_print_string_uses_canonical_renderer() {
        // ADR 0094: Value printString routes through the canonical structural
        // renderer for byte-identical output with the runtime fallback.
        let result = doc_to_string(generate_value_bif("printString", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_object_printer':'structural_from_state'(Self)".to_string())
        );
    }

    #[test]
    fn test_value_unknown_selector_returns_none() {
        assert_eq!(doc_to_string(generate_value_bif("size", &[])), None);
        assert_eq!(doc_to_string(generate_value_bif("inspect", &[])), None);
    }

    // generate_set_bif tests

    #[test]
    fn test_set_from_list_with_param() {
        let result = doc_to_string(generate_set_bif("fromList:", &["L".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_set':'from_list'(L)".to_string())
        );
    }

    #[test]
    fn test_set_with_all_aliases_from_list() {
        let result = doc_to_string(generate_set_bif("withAll:", &["L".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_set':'from_list'(L)".to_string())
        );
    }

    #[test]
    fn test_set_size() {
        let result = doc_to_string(generate_set_bif("size", &[]));
        assert_eq!(result, Some("call 'beamtalk_set':'size'(Self)".to_string()));
    }

    #[test]
    fn test_set_is_empty() {
        let result = doc_to_string(generate_set_bif("isEmpty", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_set':'is_empty'(Self)".to_string())
        );
    }

    #[test]
    fn test_set_includes_with_param() {
        let result = doc_to_string(generate_set_bif("includes:", &["Elem".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_set':'includes'(Self, Elem)".to_string())
        );
    }

    #[test]
    fn test_set_add_with_param() {
        let result = doc_to_string(generate_set_bif("add:", &["Elem".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_set':'add'(Self, Elem)".to_string())
        );
    }

    #[test]
    fn test_set_remove_with_param() {
        let result = doc_to_string(generate_set_bif("remove:", &["Elem".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_set':'remove'(Self, Elem)".to_string())
        );
    }

    #[test]
    fn test_set_union_with_param() {
        let result = doc_to_string(generate_set_bif("union:", &["Other".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_set':'union'(Self, Other)".to_string())
        );
    }

    #[test]
    fn test_set_intersection_with_param() {
        let result = doc_to_string(generate_set_bif("intersection:", &["Other".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_set':'intersection'(Self, Other)".to_string())
        );
    }

    #[test]
    fn test_set_difference_with_param() {
        let result = doc_to_string(generate_set_bif("difference:", &["Other".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_set':'difference'(Self, Other)".to_string())
        );
    }

    #[test]
    fn test_set_is_subset_of_with_param() {
        let result = doc_to_string(generate_set_bif("isSubsetOf:", &["Other".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_set':'is_subset_of'(Self, Other)".to_string())
        );
    }

    #[test]
    fn test_set_as_list() {
        let result = doc_to_string(generate_set_bif("asList", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_set':'as_list'(Self)".to_string())
        );
    }

    #[test]
    fn test_set_do_with_param() {
        let result = doc_to_string(generate_set_bif("do:", &["Block".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_set':'do'(Self, Block)".to_string())
        );
    }

    #[test]
    fn test_set_print_string() {
        let result = doc_to_string(generate_set_bif("printString", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_primitive':'print_string'(Self)".to_string())
        );
    }

    #[test]
    fn test_set_stream() {
        let result = doc_to_string(generate_set_bif("stream", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_stream':'on'(Self)".to_string())
        );
    }

    #[test]
    fn test_set_unknown_returns_none() {
        assert_eq!(
            doc_to_string(generate_set_bif("unknownSelector", &[])),
            None
        );
    }
}
