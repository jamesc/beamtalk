// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Actor type primitive implementations.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Contains BIF generators for BEAM process and concurrency types:
//! `Pid`, `Port`, `Reference`, `Future`, `FileHandle`.

use super::{binary_bif, param, wrap_string_result};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;

/// Generates BIFs for opaque BEAM types (Pid, Port, Reference).
///
/// These types share identical structure: equality, hash, and `asString`
/// (which delegates to a type-specific function in `beamtalk_opaque_ops`).
/// Extra selectors (e.g. `isAlive` for Pid) are handled via `extra_selector`.
pub(crate) fn generate_opaque_bif(
    selector: &str,
    params: &[String],
    to_string_fn: &'static str,
    extra_selector: fn(&str, &[String]) -> Option<Document<'static>>,
) -> Option<Document<'static>> {
    match selector {
        "=:=" => binary_bif("=:=", params),
        "/=" => binary_bif("/=", params),
        // Strict-inequality completes the comparison set. For opaque
        // identity types (Pid/Port/Reference) `=/=` is equivalent to `/=`.
        "=/=" => binary_bif("=/=", params),
        "asString" => Some(wrap_string_result(Document::Str(to_string_fn))),
        "hash" => Some(super::phash2_doc("Self")),
        _ => extra_selector(selector, params),
    }
}

/// Registry entry point for `Pid` primitives.
pub(crate) fn generate_pid_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    generate_opaque_bif(
        selector,
        params,
        "call 'beamtalk_opaque_ops':'pid_to_string'(Self)",
        pid_extra,
    )
}

/// Registry entry point for `Port` primitives.
pub(crate) fn generate_port_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    generate_opaque_bif(
        selector,
        params,
        "call 'beamtalk_opaque_ops':'port_to_string'(Self)",
        no_extra,
    )
}

/// Registry entry point for `Reference` primitives.
pub(crate) fn generate_reference_bif(
    selector: &str,
    params: &[String],
) -> Option<Document<'static>> {
    generate_opaque_bif(
        selector,
        params,
        "call 'beamtalk_opaque_ops':'ref_to_string'(Self)",
        reference_extra,
    )
}

pub(crate) fn pid_extra(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        "isAlive" => Some(Document::Str("call 'erlang':'is_process_alive'(Self)")),
        // Forced process termination (erlang:exit(Pid, kill))
        "kill" => Some(Document::Str("call 'erlang':'exit'(Self, 'kill')")),
        // Exit with arbitrary reason (erlang:exit(Pid, Reason))
        "exit:" => {
            let p0 = param(params, 0, "_Reason");
            Some(docvec![
                "call 'erlang':'exit'(Self, ",
                leaf::var(p0.to_string()),
                ")"
            ])
        }
        _ => None,
    }
}

pub(crate) fn reference_extra(selector: &str, _params: &[String]) -> Option<Document<'static>> {
    match selector {
        // demonitor cancels a monitor created by Actor>>monitor
        "demonitor" => Some(Document::Str("call 'erlang':'demonitor'(Self)")),
        _ => None,
    }
}

pub(crate) fn no_extra(_selector: &str, _params: &[String]) -> Option<Document<'static>> {
    None
}

#[cfg(test)]
mod tests {
    use super::super::doc_to_string;
    use super::*;

    // Registry wrapper entry points: assert each wrapper threads the
    // correct `to_string_fn` and `extra_selector` through `generate_opaque_bif`.

    #[test]
    fn test_pid_bif_as_string_and_extra() {
        assert_eq!(
            doc_to_string(generate_pid_bif("asString", &[])),
            Some("call 'beamtalk_primitive':'string_from_binary'(call 'beamtalk_opaque_ops':'pid_to_string'(Self))".to_string())
        );
        assert_eq!(
            doc_to_string(generate_pid_bif("isAlive", &[])),
            Some("call 'erlang':'is_process_alive'(Self)".to_string())
        );
    }

    #[test]
    fn test_port_bif_as_string_and_no_extra() {
        assert_eq!(
            doc_to_string(generate_port_bif("asString", &[])),
            Some("call 'beamtalk_primitive':'string_from_binary'(call 'beamtalk_opaque_ops':'port_to_string'(Self))".to_string())
        );
        // Port has no extra selectors.
        assert_eq!(doc_to_string(generate_port_bif("isAlive", &[])), None);
    }

    #[test]
    fn test_reference_bif_as_string_and_extra() {
        assert_eq!(
            doc_to_string(generate_reference_bif("asString", &[])),
            Some("call 'beamtalk_primitive':'string_from_binary'(call 'beamtalk_opaque_ops':'ref_to_string'(Self))".to_string())
        );
        assert_eq!(
            doc_to_string(generate_reference_bif("demonitor", &[])),
            Some("call 'erlang':'demonitor'(Self)".to_string())
        );
    }

    // pid_extra tests

    #[test]
    fn test_pid_extra_unknown_returns_none() {
        assert_eq!(doc_to_string(pid_extra("unknown", &[])), None);
    }

    // reference_extra tests

    #[test]
    fn test_reference_demonitor() {
        let result = doc_to_string(reference_extra("demonitor", &[]));
        assert_eq!(result, Some("call 'erlang':'demonitor'(Self)".to_string()));
    }

    #[test]
    fn test_reference_extra_unknown_returns_none() {
        assert_eq!(doc_to_string(reference_extra("unknown", &[])), None);
    }

    // no_extra tests

    #[test]
    fn test_no_extra_returns_none() {
        assert_eq!(doc_to_string(no_extra("anything", &[])), None);
    }

    // generate_opaque_bif comparison tests

    #[test]
    fn test_opaque_strict_not_equal() {
        // `=/=` completes the comparison set for opaque identity types.
        let result = doc_to_string(generate_opaque_bif(
            "=/=",
            &["Other".to_string()],
            "call 'beamtalk_opaque_ops':'pid_to_string'(Self)",
            no_extra,
        ));
        assert_eq!(result, Some("call 'erlang':'=/='(Self, Other)".to_string()));
    }

    #[test]
    fn test_opaque_not_equal() {
        let result = doc_to_string(generate_opaque_bif(
            "/=",
            &["Other".to_string()],
            "call 'beamtalk_opaque_ops':'pid_to_string'(Self)",
            no_extra,
        ));
        assert_eq!(result, Some("call 'erlang':'/='(Self, Other)".to_string()));
    }
}
