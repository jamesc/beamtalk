// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Error handling primitive implementations.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Contains BIF generators for error-related classes: `Exception`, `StackFrame`.

use super::param;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;

/// Exception primitive implementations.
///
/// Exception field access delegates to `beamtalk_exception_handler` runtime module.
/// This avoids naming conflict: compiled exception.bt produces `beamtalk_exception`,
/// while the handler module provides the actual implementation.
pub(crate) fn generate_exception_bif(
    selector: &str,
    params: &[String],
) -> Option<Document<'static>> {
    match selector {
        "message" | "hint" | "kind" | "selector" | "errorClass" | "reason" | "details"
        | "printString" | "signal" | "pass" | "stackTrace" => Some(docvec![
            "call 'beamtalk_exception_handler':'dispatch'(",
            leaf::atom(selector.to_owned()),
            ", [], Self)",
        ]),
        "signal:" => {
            let p0 = param(params, 0, "_Msg");
            Some(docvec![
                "call 'beamtalk_exception_handler':'dispatch'('signal:', [",
                leaf::var(p0.to_string()),
                "], Self)",
            ])
        }
        // Class-side signal primitives — raise exceptions without instance
        // allocation. Inside the class method the receiving class is the
        // `ClassSelf` variable; a class-literal send inlines the same call with
        // the class-name atom instead (BT-4115, `generate_class_signal_bif`).
        _ if is_caller_safe_class_primitive(selector) => {
            let args: Vec<Document<'static>> =
                params.iter().map(|p| leaf::var(p.clone())).collect();
            generate_class_signal_bif(selector, &args, leaf::var("ClassSelf"))
        }
        _ => None,
    }
}

/// The class-side `Exception` primitives that are *caller-safe*: they read
/// nothing from the receiving class but its name, touch no class state, and
/// need no class process. A send of one of these to a class literal
/// (`Error signal: m`) is inlined at the call site, raising in the caller
/// instead of making a `gen_server:call` into the class process only to
/// raise there and relay the error back (BT-4115).
///
/// This list is the one place that owns that decision;
/// [`generate_class_signal_bif`] lowers exactly these.
const CALLER_SAFE_CLASS_PRIMITIVES: &[&str] = &[
    "classSignal",
    "classSignal:",
    "classSignal:reason:",
    "classSignalKind:class:selector:hint:",
];

/// Whether the quoted primitive `name` is a caller-safe class primitive (see
/// [`CALLER_SAFE_CLASS_PRIMITIVES`]).
#[must_use]
pub(crate) fn is_caller_safe_class_primitive(name: &str) -> bool {
    CALLER_SAFE_CLASS_PRIMITIVES.contains(&name)
}

/// Lowers a caller-safe class primitive to its `beamtalk_exception_handler`
/// call, given the argument documents and the receiving class's document.
///
/// Two callers share this one selector-to-handler mapping:
/// [`generate_exception_bif`] (the `Exception class` method body, passing the
/// `ClassSelf` variable and the method's parameters) and the class-literal
/// send in `dispatch_codegen` (passing the class-name atom and the threaded
/// argument values). Answers `None` for any other primitive, or when the
/// argument count does not match the primitive.
pub(crate) fn generate_class_signal_bif(
    primitive: &str,
    args: &[Document<'static>],
    class_self: Document<'static>,
) -> Option<Document<'static>> {
    match (primitive, args) {
        ("classSignal", []) => Some(docvec![
            "call 'beamtalk_exception_handler':'class_signal'(",
            class_self,
            ")"
        ]),
        ("classSignal:", [msg]) => Some(docvec![
            "call 'beamtalk_exception_handler':'class_signal_message'(",
            msg.clone(),
            ", ",
            class_self,
            ")"
        ]),
        ("classSignal:reason:", [msg, reason]) => Some(docvec![
            "call 'beamtalk_exception_handler':'class_signal_message_reason'(",
            msg.clone(),
            ", ",
            reason.clone(),
            ", ",
            class_self,
            ")"
        ]),
        // The general, pure-Beamtalk way to signal a named error kind
        // (index_out_of_bounds, empty_collection, or another kind in
        // ExceptionKind) with a hint specific to the call site, instead of the
        // generic user_error kind `self error:` always raises.
        // Kind/Class/Selector/Hint are caller-supplied, so — unlike
        // classSignal:/classSignal — the receiving class plays no role and
        // `class_self` is not threaded through.
        ("classSignalKind:class:selector:hint:", [kind, class, sel, hint]) => Some(docvec![
            "call 'beamtalk_exception_handler':'class_signal_kind'(",
            kind.clone(),
            ", ",
            class.clone(),
            ", ",
            sel.clone(),
            ", ",
            hint.clone(),
            ")"
        ]),
        _ => None,
    }
}

/// `StackFrame` primitive implementations.
///
/// `StackFrame` field access delegates to `beamtalk_stack_frame` runtime module.
pub(crate) fn generate_stack_frame_bif(
    selector: &str,
    _params: &[String],
) -> Option<Document<'static>> {
    match selector {
        "method" | "receiverClass" | "arguments" | "sourceLocation" | "moduleName" | "line"
        | "file" | "printString" => Some(docvec![
            "call 'beamtalk_stack_frame':'dispatch'(",
            leaf::atom(selector.to_owned()),
            ", [], Self)",
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
    fn test_exception_reason() {
        let result = doc_to_string(generate_exception_bif("reason", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_exception_handler':'dispatch'('reason', [], Self)".to_string())
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
    fn test_exception_class_signal_with_message_and_reason() {
        let result = doc_to_string(generate_exception_bif(
            "classSignal:reason:",
            &["Msg".to_string(), "Reason".to_string()],
        ));
        assert_eq!(
            result,
            Some(
                "call 'beamtalk_exception_handler':'class_signal_message_reason'(Msg, Reason, ClassSelf)"
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
    fn test_exception_class_signal_kind() {
        let result = doc_to_string(generate_exception_bif(
            "classSignalKind:class:selector:hint:",
            &[
                "Kind".to_string(),
                "Class".to_string(),
                "Selector".to_string(),
                "Hint".to_string(),
            ],
        ));
        assert_eq!(
            result,
            Some(
                "call 'beamtalk_exception_handler':'class_signal_kind'(Kind, Class, Selector, Hint)"
                    .to_string()
            )
        );
    }

    // BT-4115: the class-literal send's lowering of a caller-safe class
    // primitive passes the class-name atom where the method body passes
    // `ClassSelf`.
    #[test]
    fn test_class_signal_bif_with_class_name_atom() {
        let msg = [leaf::var("Arg1".to_string())];
        let result = doc_to_string(generate_class_signal_bif(
            "classSignal:",
            &msg,
            leaf::atom("Error".to_string()),
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_exception_handler':'class_signal_message'(Arg1, 'Error')".into())
        );
        let result = doc_to_string(generate_class_signal_bif(
            "classSignal",
            &[],
            leaf::atom("MyError".to_string()),
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_exception_handler':'class_signal'('MyError')".into())
        );
    }

    #[test]
    fn test_every_caller_safe_class_primitive_lowers() {
        // The list and the lowering stay one rule: each listed primitive
        // lowers both in the method body and at a class-literal send.
        for &name in CALLER_SAFE_CLASS_PRIMITIVES {
            let arity = name.matches(':').count();
            let params: Vec<String> = (0..arity).map(|i| format!("P{i}")).collect();
            assert!(
                generate_exception_bif(name, &params).is_some(),
                "{name} has no method-body lowering"
            );
            let args: Vec<Document<'static>> = params.into_iter().map(leaf::var).collect();
            assert!(
                generate_class_signal_bif(name, &args, leaf::atom("Error".to_string())).is_some(),
                "{name} has no class-literal lowering"
            );
        }
        assert!(!is_caller_safe_class_primitive("signal:"));
        assert!(!is_caller_safe_class_primitive("classClass"));
        assert!(
            generate_class_signal_bif("classSignal:", &[], leaf::atom("Error".to_string()))
                .is_none(),
            "an argument-count mismatch must not lower"
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
