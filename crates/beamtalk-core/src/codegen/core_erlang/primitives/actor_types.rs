// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Actor type primitive implementations.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Contains BIF generators for BEAM process and concurrency types:
//! `Pid`, `Port`, `Reference`, `Future`, `FileHandle`.

use super::super::document::Document;
use super::{binary_bif, param};
use crate::docvec;

/// Generates BIFs for opaque BEAM types (Pid, Port, Reference) (BT-681).
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
        // BT-2233: strict-inequality completes the comparison set. For opaque
        // identity types (Pid/Port/Reference) `=/=` is equivalent to `/=`.
        "=/=" => binary_bif("=/=", params),
        "asString" => Some(Document::Str(to_string_fn)),
        "hash" => Some(Document::Str("call 'erlang':'phash2'(Self)")),
        _ => extra_selector(selector, params),
    }
}

/// Registry entry point for `Pid` primitives (BT-2234).
pub(crate) fn generate_pid_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    generate_opaque_bif(
        selector,
        params,
        "call 'beamtalk_opaque_ops':'pid_to_string'(Self)",
        pid_extra,
    )
}

/// Registry entry point for `Port` primitives (BT-2234).
pub(crate) fn generate_port_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    generate_opaque_bif(
        selector,
        params,
        "call 'beamtalk_opaque_ops':'port_to_string'(Self)",
        no_extra,
    )
}

/// Registry entry point for `Reference` primitives (BT-2234).
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
        // BT-1553: forced process termination (erlang:exit(Pid, kill))
        "kill" => Some(Document::Str("call 'erlang':'exit'(Self, 'kill')")),
        // BT-1553: exit with arbitrary reason (erlang:exit(Pid, Reason))
        "exit:" => {
            let p0 = param(params, 0, "_Reason");
            Some(docvec!["call 'erlang':'exit'(Self, ", p0.to_string(), ")"])
        }
        _ => None,
    }
}

pub(crate) fn reference_extra(selector: &str, _params: &[String]) -> Option<Document<'static>> {
    match selector {
        // BT-1442: demonitor cancels a monitor created by Actor>>monitor
        "demonitor" => Some(Document::Str("call 'erlang':'demonitor'(Self)")),
        _ => None,
    }
}

pub(crate) fn no_extra(_selector: &str, _params: &[String]) -> Option<Document<'static>> {
    None
}

/// Future primitive implementations (BT-813).
///
/// Futures are BEAM processes returned by async actor message sends.
/// Each instance method delegates to the `beamtalk_future` runtime module.
/// Note: `await`, `awaitForever`, and `await:` are also compiler intrinsics
/// (intercepted by `dispatch_codegen` before reaching here), so these BIFs
/// serve dynamic-dispatch use cases (e.g., `perform:`).
pub(crate) fn generate_future_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    let p0 = param(params, 0, "_Arg0");
    match selector {
        "await" => Some(Document::Str("call 'beamtalk_future':'await'(Self)")),
        "awaitForever" => Some(Document::Str(
            "call 'beamtalk_future':'await_forever'(Self)",
        )),
        "await:" => Some(docvec![
            "call 'beamtalk_future':'await'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "whenResolved:" => Some(docvec![
            "call 'beamtalk_future':'when_resolved'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "whenRejected:" => Some(docvec![
            "call 'beamtalk_future':'when_rejected'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "printString" => Some(super::PRINT_STRING),
        _ => None,
    }
}

/// `FileHandle` primitive implementations (BT-813).
///
/// `FileHandles` are tagged maps produced by `File open:do:`. Instance methods
/// delegate to the `beamtalk_file` runtime module.
pub(crate) fn generate_file_handle_bif(
    selector: &str,
    _params: &[String],
) -> Option<Document<'static>> {
    match selector {
        "lines" => Some(Document::Str("call 'beamtalk_file':'handle_lines'(Self)")),
        "printString" => Some(super::PRINT_STRING),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::super::doc_to_string;
    use super::*;

    // Registry wrapper entry points (BT-2234): assert each wrapper threads the
    // correct `to_string_fn` and `extra_selector` through `generate_opaque_bif`.

    #[test]
    fn test_pid_bif_as_string_and_extra() {
        assert_eq!(
            doc_to_string(generate_pid_bif("asString", &[])),
            Some("call 'beamtalk_opaque_ops':'pid_to_string'(Self)".to_string())
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
            Some("call 'beamtalk_opaque_ops':'port_to_string'(Self)".to_string())
        );
        // Port has no extra selectors.
        assert_eq!(doc_to_string(generate_port_bif("isAlive", &[])), None);
    }

    #[test]
    fn test_reference_bif_as_string_and_extra() {
        assert_eq!(
            doc_to_string(generate_reference_bif("asString", &[])),
            Some("call 'beamtalk_opaque_ops':'ref_to_string'(Self)".to_string())
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

    // generate_opaque_bif comparison tests (BT-2233)

    #[test]
    fn test_opaque_strict_not_equal() {
        // BT-2233: `=/=` completes the comparison set for opaque identity types.
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

    // generate_future_bif tests

    #[test]
    fn test_future_await() {
        let result = doc_to_string(generate_future_bif("await", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_future':'await'(Self)".to_string())
        );
    }

    #[test]
    fn test_future_await_forever() {
        let result = doc_to_string(generate_future_bif("awaitForever", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_future':'await_forever'(Self)".to_string())
        );
    }

    #[test]
    fn test_future_await_with_timeout() {
        let result = doc_to_string(generate_future_bif("await:", &["Timeout".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_future':'await'(Self, Timeout)".to_string())
        );
    }

    #[test]
    fn test_future_when_resolved() {
        let result = doc_to_string(generate_future_bif("whenResolved:", &["Block".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_future':'when_resolved'(Self, Block)".to_string())
        );
    }

    #[test]
    fn test_future_when_rejected() {
        let result = doc_to_string(generate_future_bif("whenRejected:", &["Block".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_future':'when_rejected'(Self, Block)".to_string())
        );
    }

    #[test]
    fn test_future_print_string() {
        let result = doc_to_string(generate_future_bif("printString", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_primitive':'print_string'(Self)".to_string())
        );
    }

    #[test]
    fn test_future_unknown_returns_none() {
        assert_eq!(doc_to_string(generate_future_bif("unknown", &[])), None);
    }

    // generate_file_handle_bif tests

    #[test]
    fn test_file_handle_lines() {
        let result = doc_to_string(generate_file_handle_bif("lines", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_file':'handle_lines'(Self)".to_string())
        );
    }

    #[test]
    fn test_file_handle_print_string() {
        let result = doc_to_string(generate_file_handle_bif("printString", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_primitive':'print_string'(Self)".to_string())
        );
    }

    #[test]
    fn test_file_handle_unknown_returns_none() {
        assert_eq!(
            doc_to_string(generate_file_handle_bif("unknown", &[])),
            None
        );
    }
}
