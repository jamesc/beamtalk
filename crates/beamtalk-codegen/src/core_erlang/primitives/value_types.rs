// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Value type primitive implementations.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Contains BIF generators for immutable value types: `Tuple`, `Set`, `Object`.

use super::{call_self_p0, param, wrap_string_result};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;

/// Tuple primitive implementations.
///
/// Tuples are Erlang tuples — immutable fixed-size collections, particularly
/// useful for Erlang interop with {ok, Value} and {error, Reason} patterns.
pub(crate) fn generate_tuple_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        "size" => Some(Document::Str("call 'erlang':'tuple_size'(Self)")),
        "at:" => {
            // BT-4400: the in-range case reads the element inline, so a typed
            // `t at: i` costs no more than any other direct call (no extra hop
            // into the runtime helper). Every other index (non-integer, out of
            // range) takes the helper, which raises the structured error — so
            // the error behaviour is the helper's, unchanged.
            let p0 = params.first()?;
            let idx = || leaf::var(p0.clone());
            Some(docvec![
                "case ",
                idx(),
                " of <_> when call 'erlang':'and'(call 'erlang':'is_integer'(",
                idx(),
                "), call 'erlang':'and'(call 'erlang':'>='(",
                idx(),
                ", 1), call 'erlang':'=<'(",
                idx(),
                ", call 'erlang':'tuple_size'(Self)))) -> call 'erlang':'element'(",
                idx(),
                ", Self) <_> when 'true' -> call 'beamtalk_tuple':'at'(Self, ",
                idx(),
                ") end"
            ])
        }
        "isOk" => Some(Document::Str(
            "case Self of <{'ok', _Value}> when 'true' -> 'true' <_> when 'true' -> 'false' end",
        )),
        "isError" => Some(Document::Str(
            "case Self of <{'error', _Reason}> when 'true' -> 'true' <_> when 'true' -> 'false' end",
        )),
        "asString" => Some(wrap_string_result(Document::Str(
            "call 'beamtalk_tuple':'as_string'(Self)",
        ))),
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

/// `ProtoObject` primitive implementations.
///
/// `ProtoObject` is the root of the class hierarchy. Its `doesNotUnderstand:args:`
/// method raises a `does_not_understand` error so that messages sent to objects
/// that have no implementation propagate as errors rather than silently returning
/// a falsy value.
///
/// The equality operators and the timeout / local `perform` variants also get
/// real bodies here (BT-3919). A call-site `==` never reaches them
/// (`operators.rs` lowers it straight to the Erlang BIF, ADR 0002), but a
/// dynamic send does — `perform:`, `beamtalk_primitive:send/3`, or an actor's
/// hierarchy walk — since every value type's dispatch chain ends in
/// `bt@stdlib@proto_object:dispatch/3`. Without a body they would compile to
/// the runtime-dispatch placeholder, which re-enters this module's own
/// `dispatch/3` with the same selector and never returns.
pub(crate) fn generate_proto_object_bif(
    selector: &str,
    params: &[String],
) -> Option<Document<'static>> {
    match selector {
        "==" => super::binary_bif("==", params),
        "/=" => super::binary_bif("/=", params),
        "=:=" => super::binary_bif("=:=", params),
        "=/=" => super::binary_bif("=/=", params),
        // The same entry point as the call-site interception: honours the
        // timeout for actors, ignores it for value types.
        "perform:withArguments:timeout:" => {
            let sel = param(params, 0, "_Selector");
            let args = param(params, 1, "_Args");
            let timeout = param(params, 2, "_Timeout");
            Some(crate::core_erlang::dispatch_codegen::runtime_send_doc(
                Document::Str("Self"),
                leaf::var(sel.to_string()),
                leaf::var(args.to_string()),
                Some(leaf::var(timeout.to_string())),
            ))
        }
        // Class objects only: any other receiver gets a structured
        // `type_error` from `local_call/3`.
        "performLocally:withArguments:" => {
            let sel = param(params, 0, "_Selector");
            let args = param(params, 1, "_Args");
            Some(docvec![
                "call 'beamtalk_object_class':'local_call'(Self, ",
                leaf::var(sel.to_string()),
                ", ",
                leaf::var(args.to_string()),
                ")",
            ])
        }
        "doesNotUnderstand:args:" => {
            let selector_var = super::param(params, 0, "_Selector");
            let hint =
                leaf::binary_lit("Check spelling or use 'respondsTo:' to verify method exists");
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
///
/// `isKindOf:` (BT-4410) calls the runtime intrinsic directly: it answers a
/// boolean and raises only `#beamtalk_error{}`, so the foreign-FFI wrapper
/// (`beamtalk_erlang_proxy:direct_call/4` and its result coercion) did
/// nothing for it but cost four calls per send.
pub(crate) fn generate_object_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        "isKindOf:" => Some(call_self_p0(
            "beamtalk_behaviour_intrinsics",
            "objectIsKindOf",
            param(params, 0, "_Class"),
        )),
        _ => None,
    }
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
        "printString" => Some(wrap_string_result(Document::Str(
            "call 'beamtalk_object_printer':'structural_from_state'(Self)",
        ))),
        _ => None,
    }
}

/// Set primitive implementations.
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
        // Delegate to beamtalk_primitive:print_string/1 which
        // formats Sets as "Set(element1, element2, ...)"
        "printString" => Some(super::PRINT_STRING),
        // Streaming
        "stream" => Some(Document::Str("call 'beamtalk_stream':'on'(Self)")),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::super::doc_to_string;
    use super::super::{assert_bif_none_table, assert_bif_table};
    use super::*;

    #[test]
    fn test_tuple_at_with_param() {
        let result = doc_to_string(generate_tuple_bif("at:", &["Idx".to_string()]))
            .expect("at: should produce code");
        // BT-4400: an in-range integer index reads the element inline...
        assert!(
            result.contains(
                "when call 'erlang':'and'(call 'erlang':'is_integer'(Idx), call 'erlang':'and'(call 'erlang':'>='(Idx, 1), call 'erlang':'=<'(Idx, call 'erlang':'tuple_size'(Self)))) -> call 'erlang':'element'(Idx, Self)"
            ),
            "{result}"
        );
        // ...and every other index keeps the helper, which owns the errors.
        assert!(
            result.contains("<_> when 'true' -> call 'beamtalk_tuple':'at'(Self, Idx)"),
            "{result}"
        );
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

    // generate_object_bif tests

    #[test]
    fn test_object_unknown_returns_none() {
        assert_eq!(doc_to_string(generate_object_bif("anything", &[])), None);
        assert_eq!(doc_to_string(generate_object_bif("size", &[])), None);
    }

    #[test]
    fn test_value_unknown_selector_returns_none() {
        assert_eq!(doc_to_string(generate_value_bif("size", &[])), None);
        assert_eq!(doc_to_string(generate_value_bif("inspect", &[])), None);
    }

    #[test]
    fn generate_tuple_bif_table() {
        assert_bif_table(
            generate_tuple_bif,
            &[
                // generate_tuple_bif tests
                (
                    "test_tuple_size",
                    "size",
                    &[],
                    "call 'erlang':'tuple_size'(Self)",
                ),
                (
                    "test_tuple_as_string",
                    "asString",
                    &[],
                    "call 'beamtalk_primitive':'string_from_binary'(call 'beamtalk_tuple':'as_string'(Self))",
                ),
                (
                    "test_tuple_do_with_param",
                    "do:",
                    &["Block"],
                    "call 'beamtalk_tuple':'do'(Self, Block)",
                ),
                (
                    "test_tuple_do_uses_default_when_no_param",
                    "do:",
                    &[],
                    "call 'beamtalk_tuple':'do'(Self, _Block)",
                ),
                (
                    "test_tuple_at_random",
                    "atRandom",
                    &[],
                    "call 'beamtalk_random':'atRandom'(Self)",
                ),
                (
                    "test_tuple_with_all_with_param",
                    "withAll:",
                    &["TheList"],
                    "call 'erlang':'list_to_tuple'(TheList)",
                ),
                (
                    "test_tuple_with_all_uses_default_when_no_param",
                    "withAll:",
                    &[],
                    "call 'erlang':'list_to_tuple'(_List)",
                ),
            ],
        );
    }

    #[test]
    fn generate_tuple_bif_none_table() {
        assert_bif_none_table(
            generate_tuple_bif,
            &[
                ("test_tuple_at_without_params_returns_none", "at:", &[]),
                ("test_tuple_unknown_returns_none", "unknownSelector", &[]),
            ],
        );
    }

    #[test]
    fn generate_proto_object_bif_none_table() {
        assert_bif_none_table(
            generate_proto_object_bif,
            &[
                // generate_proto_object_bif tests
                ("test_proto_object_unknown_returns_none", "size", &[]),
            ],
        );
    }

    #[test]
    fn generate_value_bif_table() {
        assert_bif_table(
            generate_value_bif,
            &[
                // generate_value_bif tests
                // ADR 0094: Value printString routes through the canonical structural
                // renderer for byte-identical output with the runtime fallback.
                (
                    "test_value_print_string_uses_canonical_renderer",
                    "printString",
                    &[],
                    "call 'beamtalk_primitive':'string_from_binary'(call 'beamtalk_object_printer':'structural_from_state'(Self))",
                ),
            ],
        );
    }

    #[test]
    fn generate_set_bif_table() {
        assert_bif_table(
            generate_set_bif,
            &[
                // generate_set_bif tests
                (
                    "test_set_from_list_with_param",
                    "fromList:",
                    &["L"],
                    "call 'beamtalk_set':'from_list'(L)",
                ),
                (
                    "test_set_with_all_aliases_from_list",
                    "withAll:",
                    &["L"],
                    "call 'beamtalk_set':'from_list'(L)",
                ),
                (
                    "test_set_size",
                    "size",
                    &[],
                    "call 'beamtalk_set':'size'(Self)",
                ),
                (
                    "test_set_is_empty",
                    "isEmpty",
                    &[],
                    "call 'beamtalk_set':'is_empty'(Self)",
                ),
                (
                    "test_set_includes_with_param",
                    "includes:",
                    &["Elem"],
                    "call 'beamtalk_set':'includes'(Self, Elem)",
                ),
                (
                    "test_set_add_with_param",
                    "add:",
                    &["Elem"],
                    "call 'beamtalk_set':'add'(Self, Elem)",
                ),
                (
                    "test_set_remove_with_param",
                    "remove:",
                    &["Elem"],
                    "call 'beamtalk_set':'remove'(Self, Elem)",
                ),
                (
                    "test_set_union_with_param",
                    "union:",
                    &["Other"],
                    "call 'beamtalk_set':'union'(Self, Other)",
                ),
                (
                    "test_set_intersection_with_param",
                    "intersection:",
                    &["Other"],
                    "call 'beamtalk_set':'intersection'(Self, Other)",
                ),
                (
                    "test_set_difference_with_param",
                    "difference:",
                    &["Other"],
                    "call 'beamtalk_set':'difference'(Self, Other)",
                ),
                (
                    "test_set_is_subset_of_with_param",
                    "isSubsetOf:",
                    &["Other"],
                    "call 'beamtalk_set':'is_subset_of'(Self, Other)",
                ),
                (
                    "test_set_as_list",
                    "asList",
                    &[],
                    "call 'beamtalk_set':'as_list'(Self)",
                ),
                (
                    "test_set_do_with_param",
                    "do:",
                    &["Block"],
                    "call 'beamtalk_set':'do'(Self, Block)",
                ),
                (
                    "test_set_print_string",
                    "printString",
                    &[],
                    "call 'beamtalk_primitive':'string_from_binary'(call 'beamtalk_primitive':'print_string'(Self))",
                ),
                (
                    "test_set_stream",
                    "stream",
                    &[],
                    "call 'beamtalk_stream':'on'(Self)",
                ),
            ],
        );
    }

    #[test]
    fn generate_set_bif_none_table() {
        assert_bif_none_table(
            generate_set_bif,
            &[("test_set_unknown_returns_none", "unknownSelector", &[])],
        );
    }
}
