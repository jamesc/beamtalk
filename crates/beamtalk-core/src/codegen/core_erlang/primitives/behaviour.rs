// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Primitive BIF mappings for Behaviour and Class stdlib classes (ADR 0032 Phase 2).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Maps `@primitive "classXxx"` declarations in `lib/Behaviour.bt` and `lib/Class.bt`
//! to direct calls into `beamtalk_behaviour_intrinsics`. These are thin data-access
//! functions — hierarchy-walking logic lives in pure Beamtalk.

use super::super::document::Document;
use crate::docvec;

/// Zero-arg Behaviour intrinsics: selector name equals the runtime function name.
const BEHAVIOUR_ZERO_ARG: &[&str] = &[
    "classSuperclass",
    "classSubclasses",
    "classAllSubclasses",
    "classLocalMethods",
    "classInstVarNames",
    "classAllSuperclasses",
    "classMethods",
    "classAllInstVarNames",
    "classRemoveFromSystem",
    "classSourceFile",
    "classReload",
    "classDoc",
    // ADR 0068 Phase 2c: Protocol queries
    "classProtocols",
];

/// Generates a `call 'beamtalk_behaviour_intrinsics':'func'(Self)` Document.
fn intrinsic_self(func: &str) -> Document<'static> {
    docvec![
        "call 'beamtalk_behaviour_intrinsics':'",
        func.to_owned(),
        "'(Self)"
    ]
}

/// Generates a `call 'beamtalk_behaviour_intrinsics':'func'(Self, Arg)` Document.
fn intrinsic_self_arg(func: &str, arg: &str) -> Document<'static> {
    docvec![
        "call 'beamtalk_behaviour_intrinsics':'",
        func.to_owned(),
        "'(Self, ",
        arg.to_owned(),
        ")"
    ]
}

/// Generates Core Erlang for a Behaviour primitive.
///
/// These back the `@primitive "classXxx"` method bodies in `lib/Behaviour.bt`.
/// Each maps to a direct call to `beamtalk_behaviour_intrinsics:Func(Self)`.
pub fn generate_behaviour_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    // Check zero-arg intrinsics table first
    if BEHAVIOUR_ZERO_ARG.contains(&selector) {
        return Some(intrinsic_self(selector));
    }

    // Parameterized intrinsics
    match selector {
        "classIncludesSelector"
        | "classCanUnderstand"
        | "classWhichIncludesSelector"
        | "classDocForMethod"
        | "classInheritsFrom"
        | "classIncludesBehaviour"
        | "classSetDoc"
        | "classConformsTo" => {
            let arg = params.first()?;
            Some(intrinsic_self_arg(selector, arg))
        }
        "methodLookup" => {
            let arg = params.first()?;
            Some(docvec![
                "call 'beamtalk_method_resolver':'resolve'(Self, ",
                arg.clone(),
                ")"
            ])
        }
        "classSetMethodDoc" => {
            let sel = params.first()?;
            let doc = params.get(1)?;
            Some(docvec![
                "call 'beamtalk_behaviour_intrinsics':'classSetMethodDoc'(Self, ",
                sel.clone(),
                ", ",
                doc.clone(),
                ")"
            ])
        }
        _ => None,
    }
}

/// Generates Core Erlang for a Class primitive.
///
/// These back the `@primitive "classXxx"` method bodies in `lib/Class.bt`.
pub fn generate_class_bif(selector: &str, _params: &[String]) -> Option<Document<'static>> {
    match selector {
        "className" | "classClass" => Some(intrinsic_self(selector)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::super::doc_to_string;
    use super::*;

    #[test]
    fn test_class_superclass() {
        let result = doc_to_string(generate_behaviour_bif("classSuperclass", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classSuperclass'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_subclasses() {
        let result = doc_to_string(generate_behaviour_bif("classSubclasses", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classSubclasses'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_all_subclasses() {
        let result = doc_to_string(generate_behaviour_bif("classAllSubclasses", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classAllSubclasses'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_local_methods() {
        let result = doc_to_string(generate_behaviour_bif("classLocalMethods", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classLocalMethods'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_includes_selector() {
        let result = doc_to_string(generate_behaviour_bif(
            "classIncludesSelector",
            &["Selector".to_string()],
        ));
        assert_eq!(
            result,
            Some(
                "call 'beamtalk_behaviour_intrinsics':'classIncludesSelector'(Self, Selector)"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_class_inst_var_names() {
        let result = doc_to_string(generate_behaviour_bif("classInstVarNames", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classInstVarNames'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_name() {
        let result = doc_to_string(generate_class_bif("className", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'className'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_class() {
        let result = doc_to_string(generate_class_bif("classClass", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classClass'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_doc() {
        let result = doc_to_string(generate_behaviour_bif("classDoc", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classDoc'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_set_doc() {
        let result = doc_to_string(generate_behaviour_bif(
            "classSetDoc",
            &["DocStr".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classSetDoc'(Self, DocStr)".to_string())
        );
    }

    #[test]
    fn test_class_set_method_doc() {
        let result = doc_to_string(generate_behaviour_bif(
            "classSetMethodDoc",
            &["Selector".to_string(), "DocStr".to_string()],
        ));
        assert_eq!(
            result,
            Some(
                "call 'beamtalk_behaviour_intrinsics':'classSetMethodDoc'(Self, Selector, DocStr)"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_class_doc_for_method() {
        let result = doc_to_string(generate_behaviour_bif(
            "classDocForMethod",
            &["Selector".to_string()],
        ));
        assert_eq!(
            result,
            Some(
                "call 'beamtalk_behaviour_intrinsics':'classDocForMethod'(Self, Selector)"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_class_remove_from_system() {
        let result = doc_to_string(generate_behaviour_bif("classRemoveFromSystem", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classRemoveFromSystem'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_source_file() {
        let result = doc_to_string(generate_behaviour_bif("classSourceFile", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classSourceFile'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_reload() {
        let result = doc_to_string(generate_behaviour_bif("classReload", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classReload'(Self)".to_string())
        );
    }

    #[test]
    fn test_method_lookup() {
        let result = doc_to_string(generate_behaviour_bif(
            "methodLookup",
            &["Selector".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_method_resolver':'resolve'(Self, Selector)".to_string())
        );
    }

    #[test]
    fn test_unknown_behaviour_selector() {
        let result = doc_to_string(generate_behaviour_bif("unknownMethod", &[]));
        assert!(result.is_none());
    }

    #[test]
    fn test_unknown_class_selector() {
        let result = doc_to_string(generate_class_bif("unknownMethod", &[]));
        assert!(result.is_none());
    }
}
