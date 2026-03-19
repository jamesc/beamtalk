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
        "asString" => Some(Document::Str(to_string_fn)),
        "hash" => Some(Document::Str("call 'erlang':'phash2'(Self)")),
        _ => extra_selector(selector, params),
    }
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
