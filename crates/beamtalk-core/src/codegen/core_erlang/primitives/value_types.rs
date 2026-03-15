// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Value type primitive implementations.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Contains BIF generators for immutable value types: `Tuple`, `Set`, `Object`.

use super::super::document::Document;
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
        "unwrap" => Some(Document::Str("call 'beamtalk_tuple':'unwrap'(Self)")),
        "unwrapOr:" => {
            let p0 = params.first()?;
            Some(docvec![
                "call 'beamtalk_tuple':'unwrap_or'(Self, ",
                p0.clone(),
                ")",
            ])
        }
        "unwrapOrElse:" => {
            let p0 = params.first()?;
            Some(docvec![
                "call 'beamtalk_tuple':'unwrap_or_else'(Self, ",
                p0.clone(),
                ")",
            ])
        }
        "asString" => Some(Document::Str("call 'beamtalk_tuple':'as_string'(Self)")),
        "do:" => {
            let p0 = param(params, 0, "_Block");
            Some(docvec![
                "call 'beamtalk_tuple':'do'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "atRandom" => Some(Document::Str("call 'beamtalk_random':'atRandom'(Self)")),
        "withAll:" => {
            let p0 = param(params, 0, "_List");
            Some(docvec![
                "call 'erlang':'list_to_tuple'(",
                p0.to_string(),
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
                selector_var.to_string(),
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
                p0.to_string(),
                ")"
            ])
        }
        "size" => Some(Document::Str("call 'beamtalk_set':'size'(Self)")),
        "isEmpty" => Some(Document::Str("call 'beamtalk_set':'is_empty'(Self)")),
        "includes:" => {
            let p0 = param(params, 0, "_Element");
            Some(docvec![
                "call 'beamtalk_set':'includes'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "add:" => {
            let p0 = param(params, 0, "_Element");
            Some(docvec![
                "call 'beamtalk_set':'add'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "remove:" => {
            let p0 = param(params, 0, "_Element");
            Some(docvec![
                "call 'beamtalk_set':'remove'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "union:" => {
            let p0 = param(params, 0, "_Other");
            Some(docvec![
                "call 'beamtalk_set':'union'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "intersection:" => {
            let p0 = param(params, 0, "_Other");
            Some(docvec![
                "call 'beamtalk_set':'intersection'(Self, ",
                p0.to_string(),
                ")",
            ])
        }
        "difference:" => {
            let p0 = param(params, 0, "_Other");
            Some(docvec![
                "call 'beamtalk_set':'difference'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "isSubsetOf:" => {
            let p0 = param(params, 0, "_Other");
            Some(docvec![
                "call 'beamtalk_set':'is_subset_of'(Self, ",
                p0.to_string(),
                ")",
            ])
        }
        "asList" => Some(Document::Str("call 'beamtalk_set':'as_list'(Self)")),
        "do:" => {
            let p0 = param(params, 0, "_Block");
            Some(docvec![
                "call 'beamtalk_set':'do'(Self, ",
                p0.to_string(),
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
