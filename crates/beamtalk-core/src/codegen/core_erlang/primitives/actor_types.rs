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

/// Pid primitive implementations (BT-681).
///
/// Pids are opaque BEAM process identifiers. Most methods use direct BIFs;
/// `asString` delegates to `beamtalk_opaque_ops` runtime module.
pub(crate) fn generate_pid_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        "=:=" => binary_bif("=:=", params),
        "/=" => binary_bif("/=", params),
        "asString" => Some(Document::Str(
            "call 'beamtalk_opaque_ops':'pid_to_string'(Self)",
        )),
        "hash" => Some(Document::Str("call 'erlang':'phash2'(Self)")),
        "isAlive" => Some(Document::Str("call 'erlang':'is_process_alive'(Self)")),
        _ => None,
    }
}

/// Port primitive implementations (BT-681).
///
/// Ports are opaque BEAM port identifiers.
pub(crate) fn generate_port_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        "=:=" => binary_bif("=:=", params),
        "/=" => binary_bif("/=", params),
        "asString" => Some(Document::Str(
            "call 'beamtalk_opaque_ops':'port_to_string'(Self)",
        )),
        "hash" => Some(Document::Str("call 'erlang':'phash2'(Self)")),
        _ => None,
    }
}

/// Reference primitive implementations (BT-681).
///
/// References are opaque BEAM unique identifiers.
pub(crate) fn generate_reference_bif(
    selector: &str,
    params: &[String],
) -> Option<Document<'static>> {
    match selector {
        "=:=" => binary_bif("=:=", params),
        "/=" => binary_bif("/=", params),
        "asString" => Some(Document::Str(
            "call 'beamtalk_opaque_ops':'ref_to_string'(Self)",
        )),
        "hash" => Some(Document::Str("call 'erlang':'phash2'(Self)")),
        _ => None,
    }
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
        "printString" => Some(Document::Str(
            "call 'beamtalk_primitive':'print_string'(Self)",
        )),
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
        "printString" => Some(Document::Str(
            "call 'beamtalk_primitive':'print_string'(Self)",
        )),
        _ => None,
    }
}
