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

#[cfg(test)]
mod tests {
    use super::super::doc_to_string;
    use super::*;

    // generate_exception_bif tests

    #[test]
    fn test_exception_message() {
        let result = doc_to_string(generate_exception_bif("message", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_exception_handler':'dispatch'('message', [], Self)".to_string())
        );
    }

    #[test]
    fn test_exception_hint() {
        let result = doc_to_string(generate_exception_bif("hint", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_exception_handler':'dispatch'('hint', [], Self)".to_string())
        );
    }

    #[test]
    fn test_exception_kind() {
        let result = doc_to_string(generate_exception_bif("kind", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_exception_handler':'dispatch'('kind', [], Self)".to_string())
        );
    }

    #[test]
    fn test_exception_selector() {
        let result = doc_to_string(generate_exception_bif("selector", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_exception_handler':'dispatch'('selector', [], Self)".to_string())
        );
    }

    #[test]
    fn test_exception_error_class() {
        let result = doc_to_string(generate_exception_bif("errorClass", &[]));
        assert_eq!(
            result,
            Some(
                "call 'beamtalk_exception_handler':'dispatch'('errorClass', [], Self)".to_string()
            )
        );
    }

    #[test]
    fn test_exception_print_string() {
        let result = doc_to_string(generate_exception_bif("printString", &[]));
        assert_eq!(
            result,
            Some(
                "call 'beamtalk_exception_handler':'dispatch'('printString', [], Self)".to_string()
            )
        );
    }

    #[test]
    fn test_exception_signal() {
        let result = doc_to_string(generate_exception_bif("signal", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_exception_handler':'dispatch'('signal', [], Self)".to_string())
        );
    }

    #[test]
    fn test_exception_signal_with_message() {
        let result = doc_to_string(generate_exception_bif("signal:", &["Msg".to_string()]));
        assert_eq!(
            result,
            Some(
                "call 'beamtalk_exception_handler':'dispatch'('signal:', [Msg], Self)".to_string()
            )
        );
    }

    #[test]
    fn test_exception_stack_trace() {
        let result = doc_to_string(generate_exception_bif("stackTrace", &[]));
        assert_eq!(
            result,
            Some(
                "call 'beamtalk_exception_handler':'dispatch'('stackTrace', [], Self)".to_string()
            )
        );
    }

    #[test]
    fn test_exception_class_signal_with_message() {
        let result = doc_to_string(generate_exception_bif("classSignal:", &["Msg".to_string()]));
        assert_eq!(
            result,
            Some(
                "call 'beamtalk_exception_handler':'class_signal_message'(Msg, ClassSelf)"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_exception_class_signal() {
        let result = doc_to_string(generate_exception_bif("classSignal", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_exception_handler':'class_signal'(ClassSelf)".to_string())
        );
    }

    #[test]
    fn test_exception_unknown_selector_returns_none() {
        assert!(generate_exception_bif("raise", &[]).is_none());
        assert!(generate_exception_bif("new", &[]).is_none());
    }

    // generate_stack_frame_bif tests

    #[test]
    fn test_stack_frame_method() {
        let result = doc_to_string(generate_stack_frame_bif("method", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_stack_frame':'dispatch'('method', [], Self)".to_string())
        );
    }

    #[test]
    fn test_stack_frame_receiver_class() {
        let result = doc_to_string(generate_stack_frame_bif("receiverClass", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_stack_frame':'dispatch'('receiverClass', [], Self)".to_string())
        );
    }

    #[test]
    fn test_stack_frame_arguments() {
        let result = doc_to_string(generate_stack_frame_bif("arguments", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_stack_frame':'dispatch'('arguments', [], Self)".to_string())
        );
    }

    #[test]
    fn test_stack_frame_source_location() {
        let result = doc_to_string(generate_stack_frame_bif("sourceLocation", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_stack_frame':'dispatch'('sourceLocation', [], Self)".to_string())
        );
    }

    #[test]
    fn test_stack_frame_module_name() {
        let result = doc_to_string(generate_stack_frame_bif("moduleName", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_stack_frame':'dispatch'('moduleName', [], Self)".to_string())
        );
    }

    #[test]
    fn test_stack_frame_line() {
        let result = doc_to_string(generate_stack_frame_bif("line", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_stack_frame':'dispatch'('line', [], Self)".to_string())
        );
    }

    #[test]
    fn test_stack_frame_file() {
        let result = doc_to_string(generate_stack_frame_bif("file", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_stack_frame':'dispatch'('file', [], Self)".to_string())
        );
    }

    #[test]
    fn test_stack_frame_print_string() {
        let result = doc_to_string(generate_stack_frame_bif("printString", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_stack_frame':'dispatch'('printString', [], Self)".to_string())
        );
    }

    #[test]
    fn test_stack_frame_unknown_selector_returns_none() {
        assert!(generate_stack_frame_bif("new", &[]).is_none());
        assert!(generate_stack_frame_bif("signal", &[]).is_none());
    }
}
