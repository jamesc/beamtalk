// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Error handling primitive implementations.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Contains BIF generators for error-related classes: `Exception`, `StackFrame`.

use super::super::document::Document;
use super::param;
use crate::docvec;

/// Exception primitive implementations (BT-338).
///
/// Exception field access delegates to `beamtalk_exception_handler` runtime module.
/// This avoids naming conflict: compiled Exception.bt produces `beamtalk_exception`,
/// while the handler module provides the actual implementation.
pub(crate) fn generate_exception_bif(
    selector: &str,
    params: &[String],
) -> Option<Document<'static>> {
    match selector {
        "message" => Some(Document::Str(
            "call 'beamtalk_exception_handler':'dispatch'('message', [], Self)",
        )),
        "hint" => Some(Document::Str(
            "call 'beamtalk_exception_handler':'dispatch'('hint', [], Self)",
        )),
        "kind" => Some(Document::Str(
            "call 'beamtalk_exception_handler':'dispatch'('kind', [], Self)",
        )),
        "selector" => Some(Document::Str(
            "call 'beamtalk_exception_handler':'dispatch'('selector', [], Self)",
        )),
        "errorClass" => Some(Document::Str(
            "call 'beamtalk_exception_handler':'dispatch'('errorClass', [], Self)",
        )),
        "printString" => Some(Document::Str(
            "call 'beamtalk_exception_handler':'dispatch'('printString', [], Self)",
        )),
        "signal" => Some(Document::Str(
            "call 'beamtalk_exception_handler':'dispatch'('signal', [], Self)",
        )),
        "signal:" => {
            let p0 = param(params, 0, "_Msg");
            Some(docvec![
                "call 'beamtalk_exception_handler':'dispatch'('signal:', [",
                p0.to_string(),
                "], Self)",
            ])
        }
        "stackTrace" => Some(Document::Str(
            "call 'beamtalk_exception_handler':'dispatch'('stackTrace', [], Self)",
        )),
        // BT-1524: Class-side signal primitives — raise exceptions without instance allocation.
        "classSignal:" => {
            let p0 = param(params, 0, "_Msg");
            Some(docvec![
                "call 'beamtalk_exception_handler':'class_signal_message'(",
                p0.to_string(),
                ", ClassSelf)"
            ])
        }
        "classSignal" => Some(Document::Str(
            "call 'beamtalk_exception_handler':'class_signal'(ClassSelf)",
        )),
        _ => None,
    }
}

/// `StackFrame` primitive implementations (BT-107).
///
/// `StackFrame` field access delegates to `beamtalk_stack_frame` runtime module.
pub(crate) fn generate_stack_frame_bif(
    selector: &str,
    _params: &[String],
) -> Option<Document<'static>> {
    match selector {
        "method" | "receiverClass" | "arguments" | "sourceLocation" | "moduleName" | "line"
        | "file" | "printString" => Some(docvec![
            "call 'beamtalk_stack_frame':'dispatch'('",
            Document::String(selector.to_owned()),
            "', [], Self)",
        ]),
        _ => None,
    }
}
